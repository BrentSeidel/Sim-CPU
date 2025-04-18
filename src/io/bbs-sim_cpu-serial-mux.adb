--
--  Author: Brent Seidel
--  Date: 31-Jul-2024
--
--  This file is part of SimCPU.
--  SimCPU is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  SimCPU is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.--
--
with Ada.Streams;
use type Ada.Streams.Stream_Element_Offset;
use type Ada.Streams.Stream_Element;
with Ada.Text_IO;
with Ada.Exceptions;
with GNAT.Sockets;
use type GNAT.Sockets.Port_type;
package body BBS.Sim_CPU.serial.mux is
   --  ----------------------------------------------------------------------
   --  Telnet mux device actions
   --
   --  Set device port and do the network initialiation.  This must be
   --  done only once before using the device.
   --
   procedure init(self : in out mux_tty; ptr : mux_access; port : GNAT.Sockets.Port_Type) is
   begin
      for i in 0 .. 7 loop
         self.chan(i).T.start(ptr, i, port + GNAT.Sockets.Port_type(i), self.host);
      end loop;
   end;
   --
   --  Set which exception to use
   --
   procedure setException(self : in out mux_tty; except : long) is
   begin
      self.int_code := except;
   end;
   --
   --  Write to a port address.
   --  If nothing is connected, the characters are just dropped.
   --
   overriding
   procedure write(self : in out mux_tty; addr : addr_bus; data : data_bus) is
      chan : Integer := Integer(addr - self.base) - 2;
   begin
      if addr = self.base then  --  Ready flags are read only
         null;
      elsif addr = (self.base + 1) then  --  Status port
--         Ada.Text_IO.Put_Line("MUX: Write " & data_bus'Image(data) & " to status port");
         self.int_e := (data and 1) /= 0;
         if (data and 2) /= 0 then
            for i in 0 .. 7 loop    --  Reset command
               self.chan(i).ready := False;
               self.chan(i).char := Character'Val(0);
            end loop;
         end if;
      elsif (addr > (self.base + 1)) and (addr < (self.base + 10)) then  --  Channel ports
--         Ada.Text_IO.Put_Line("MUX: Write to address " & toHex(addr) & ", channel " &
--            toHex(addr - self.base - 2));
         if self.chan(chan).connected then
            self.chan(chan).T.write(Character'Val(Integer(data and 16#FF#)));
         end if;
      end if;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out mux_tty; addr : addr_bus) return data_bus is
      chan : Integer := Integer(addr - self.base) - 2;
   begin
--      Ada.Text_IO.Put_Line("MUX: Reading from address " & toHex(addr));
      if addr = self.base then
         return (if self.chan(0).ready then 1 else 0) +
                (if self.chan(1).ready then 2 else 0) +
                (if self.chan(2).ready then 4 else 0) +
                (if self.chan(3).ready then 8 else 0) +
                (if self.chan(4).ready then 16 else 0) +
                (if self.chan(5).ready then 32 else 0) +
                (if self.chan(6).ready then 64 else 0) +
                (if self.chan(7).ready then 128 else 0);
      elsif addr = (self.base + 1) then
         return (if self.int_e then 1 else 0);
      elsif (addr > (self.base + 1)) and (addr < (self.base + 10)) then
         self.chan(Integer(addr - self.base - 2)).ready := False;
--         Ada.Text_IO.Put_Line("MUX: Returning character code " &
--            toHex(byte(data_bus(Character'Pos(self.chan(chan).char)) and 16#FF#)));
         return data_bus(Character'Pos(self.chan(chan).char));
      end if;
      return 0;
   end;
   --
   --  Close the network connection and halt the tasks.
   --
   overriding
   procedure shutdown(self : in out mux_tty) is
   begin
      --
      --  Abort is used here as this is only called when the program is
      --  terminating and if no telnet connection has been made, the
      --  task will be blocked waiting for a connection.
      --
      for i in 0 .. 7 loop
         abort self.chan(i).T;
      end loop;
   end;
   --
   --  Task type for telnet type server
   --
   task body mux_server is
      data      : mux_access;
      idx       : Integer;
      host      : BBS.Sim_CPU.sim_access;
      exit_flag : Boolean := False;
      sock_ser  : GNAT.Sockets.Socket_Type;  --  Server Socket
      sock_com  : GNAT.Sockets.Socket_Type;  --  Communication Socket
      rx_task   : mux_rx;
      local     : GNAT.Sockets.Sock_Addr_Type;
      s         : GNAT.Sockets.Stream_Access;
   begin
      accept start(self : mux_access; index : Integer; port : GNAT.Sockets.Port_Type; owner : BBS.Sim_CPU.sim_access) do
         data := self;
         idx  := index;
         local.Addr := GNAT.Sockets.Any_Inet_Addr;
         local.Port := port;
         host := owner;
         GNAT.Sockets.Create_Socket(sock_ser, GNAT.Sockets.Family_Inet,
                                 GNAT.Sockets.Socket_Stream);
         GNAT.Sockets.Set_Socket_Option(sock_ser, GNAT.Sockets.Socket_Level,
                                       (GNAT.Sockets.Reuse_Address, True));
         GNAT.Sockets.Bind_Socket(sock_ser, local);
         GNAT.Sockets.Listen_Socket(sock_ser);
      end start;
      loop
         select
            accept write(char : Character) do
               if data.all.chan(idx).connected then
                  String'write(s, "" & char);
               end if;
            end write;
         or
            accept end_task do
               exit_flag := True;
            end end_task;
         or
            delay 0.0;
         end select;
         exit when exit_flag;
         if data.all.chan(idx).disconnecting then
            --
            --  This produces a warning that sock_com may be used before
            --  it's set.  This should not be a problem in normal operation
            --  since connected has to be set True before disconnecting
            --  can be set false.
            --
            pragma Warnings (Off, "Connections happens before disconnecting");
            GNAT.Sockets.Close_Socket(sock_com);
            pragma Warnings (On, "Connections happens before disconnecting");
            data.all.chan(idx).disconnecting := False;
         end if;
         if not data.all.chan(idx).connected then
            --
            --  This call blocks until a connection request comes in.
            --
            GNAT.Sockets.Accept_Socket(sock_ser, sock_com, local);
            s := GNAT.Sockets.Stream(sock_com);
            data.all.chan(idx).connected := True;
            String'write(s, "Mux connected to simulated CPU " & host.name & CRLF);
            rx_task.start(data, idx, sock_com, host);
         end if;
      end loop;
      rx_task.end_task;
      GNAT.Sockets.Close_Socket(sock_ser);
      GNAT.Sockets.Close_Socket(sock_com);
   end mux_server;
   --
   --  Task body for telnet receiver task.  This is intended to only be
   --  used by the main telnet task for reciving characters from the
   --  network connection.
   --
   task body mux_rx is
      exit_flag : Boolean := False;
      data      : mux_access;
      idx       : Integer;
      host      : BBS.Sim_CPU.sim_access;
      sock_com  : GNAT.Sockets.Socket_Type;
      last      : Ada.Streams.Stream_Element_Offset;
      elem      : Ada.Streams.Stream_Element_Array(1 .. 1);
      cmd_state : byte := 0;
   begin
      accept start(self : mux_access; index : Integer;
            sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.sim_access) do
         data := self;
         idx  := index;
         sock_com := sock;
         host := owner;
      end start;
      loop
         select
            accept end_task do
               exit_flag := True;
            end end_task;
            or
            accept start(self : mux_access; index : Integer;
                  sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.sim_access) do
               data := self;
               idx  := index;
               sock_com := sock;
               host := owner;
            end start;
            or
               delay 0.0;
         end select;
         exit when exit_flag;
         if data.all.chan(idx).connected then
            GNAT.Sockets.Receive_Socket(sock_com, elem, last);
--            Ada.Text_IO.Put_Line("MUX: Character received: " & toHex(byte(elem(1))));
            if last = 0 then
               data.all.chan(idx).connected := False;
               data.all.chan(idx).disconnecting := True;
--               Ada.Text_IO.Put_Line("MUX: Closing channel.");
            --
            --  If the client has not read the last character, drop the current
            --  current one.  Buffering could be added at some point, but this
            --  seems to be consistent with the way that CP/M works.
            --
            else
            --
            --  The telnet protocol uses in-band signalling with FF
            --  indicating the start of option signalling.  The sequences
            --  I've seen are:
            --  FF FD 01  (IAC DO echo?)
            --  FF FD 03  (IAC DO supress go ahead?)
            --  There are more defined somewhere.  We just want to ignore
            --  them for now.  If a character FF needs to be sent, it is
            --  sent as FF FF.  It may be that at some point, software
            --  running on the simulator may wish to see these.  At that
            --  point, it could be made into an option.
            --
               if (elem(1) = 16#FF#) and ((cmd_state = 0) or (cmd_state = 3)) then     --  Start of CMD
                  cmd_state := 1;
               elsif (elem(1) = 16#FF#) and (cmd_state = 1) then  --  Escaped FF
                  cmd_state := 0;
               elsif cmd_state = 1 then  --  Ignore next character
                  cmd_state := 2;
               elsif cmd_state = 2 then  --  Ignore next character
                  cmd_state := 3;
               elsif cmd_state = 3 then  --  Reset
                  cmd_state := 0;
               end if;
               if (not data.all.chan(idx).ready) and (cmd_state = 0) then
--                  Ada.Text_IO.Put_Line("MUX: Character stored: " & toHex(byte(elem(1))));
                  data.all.chan(idx).char := Character'Val(elem(1));
                  data.all.chan(idx).ready := True;
               end if;
            end if;
            if data.all.int_e and data.all.chan(idx).ready then
--               Ada.Text_IO.Put_Line("MUX: Sending interrupt " & toHex(data.all.int_code));
               host.interrupt(data.all.int_code);
            end if;
         end if;
      end loop;
   end mux_rx;
end;
