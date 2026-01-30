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
package body BBS.Sim_CPU.io.serial.telnet is
   --  ----------------------------------------------------------------------
   --  Telnet device actions
   --
   --  Set device port and do the network initialiation.  This must be
   --  done only once before using the device.
   --
   procedure init(self : in out tel_tty; ptr : telnet_access; port : GNAT.Sockets.Port_Type) is
   begin
      self.T.start(ptr, port, self.host);
   end;
   --
   --  Set which exception to use
   --
   procedure setException(self : in out tel_tty; except : long) is
   begin
      self.int_code := except;
   end;
   --
   --  Write to a port address.
   --  If nothing is connected, the characters are just dropped.
   --
   overriding
   procedure write(self : in out tel_tty; addr : addr_bus; data : data_bus; size : bus_size; status : out bus_stat) is
   begin
      if (addr = (self.base + 1)) and self.connected then
         self.T.write(Character'Val(Integer(data and 16#FF#)));
      elsif addr = self.base then
         self.int_e := (data and 4) /= 0;
         if (data and 8) /= 0 then  --  Reset command.
            self.ready := False;
            self.char := Character'Val(0);
         end if;
      end if;
   exception
     when e : others =>
       Ada.Text_IO.Put_Line("Exception occured while trying to write to console.");
       Ada.Text_IO.Put_Line("  Name: " & Ada.Exceptions.Exception_Name(e));
       Ada.Text_IO.Put_Line("  Msg:  " & Ada.Exceptions.Exception_Message(e));
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out tel_tty; addr : addr_bus; size : bus_size; status : out bus_stat) return data_bus is
   begin
      if addr = (self.base + 1) then
         self.ready := False;
--         Ada.Text_IO.Put_Line("TTY: Returning character code " & toHex(byte(data_bus(Character'Pos(self.char)) and 16#FF#)));
         return data_bus(Character'Pos(self.char));
      elsif addr = self.base then
         return 0 +
               (if self.ready then 1 else 0) +
               (if self.connected then 2 else 0) +
               (if self.int_e then 4 else 0);
      end if;
      return 0;
   end;
   --
   --  Close the network connection and halt the tasks.
   --
   overriding
   procedure shutdown(self : in out tel_tty) is
   begin
      --
      --  Abort is used here as this is only called when the program is
      --  terminating and if no telnet connection has been made, the
      --  task will be blocked waiting for a connection.
      --
      abort self.T;
--     self.T.end_task;
   end;
   --
   --  Task type for telnet type server
   --
   task body telnet_server is
      data      : telnet_access;
      host      : BBS.Sim_CPU.CPU.sim_access;
      exit_flag : Boolean := False;
      sock_ser  : GNAT.Sockets.Socket_Type;  --  Server Socket
      sock_com  : GNAT.Sockets.Socket_Type;  --  Communication Socket
      rx_task   : telnet_rx;
      local     : GNAT.Sockets.Sock_Addr_Type;
      s         : GNAT.Sockets.Stream_Access;
   begin
      accept start(self : telnet_access; port : GNAT.Sockets.Port_Type; owner : BBS.Sim_CPU.CPU.sim_access) do
         data := self;
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
--      Ada.Text_IO.Put_Line("TTY: Telnet server started.");
      loop
         select
            accept write(char : Character) do
               String'write(s, "" & char);
            end write;
         or
            accept end_task do
               exit_flag := True;
            end end_task;
         or
            delay 0.0;
         end select;
         exit when exit_flag;
         if data.all.disconnecting then
            --
            --  This produces a warning that sock_com may be used before
            --  it's set.  This should not be a problem in normal operation
            --  since connected has to be set True before disconnecting
            --  can be set false.
            --
            pragma Warnings (Off, "Connections happens before disconnecting");
            GNAT.Sockets.Close_Socket(sock_com);
            pragma Warnings (On, "Connections happens before disconnecting");
            data.all.disconnecting := False;
         end if;
         if not data.all.connected then
            --
            --  This call blocks until a connection request comes in.
            --
            GNAT.Sockets.Accept_Socket(sock_ser, sock_com, local);
            s := GNAT.Sockets.Stream(sock_com);
            data.all.connected := True;
            String'write(s, "Connected to simulated CPU " & host.name & CRLF);
            rx_task.start(data, sock_com, host);
         end if;
      end loop;
      rx_task.end_task;
      GNAT.Sockets.Close_Socket(sock_ser);
      GNAT.Sockets.Close_Socket(sock_com);
   end telnet_server;
   --
   --  Task body for telnet receiver task.  This is intended to only be
   --  used by the main telnet task for reciving characters from the
   --  network connection.
   --
   task body telnet_rx is
      exit_flag : Boolean := False;
      data      : telnet_access;
      host      : BBS.Sim_CPU.CPU.sim_access;
      sock_com  : GNAT.Sockets.Socket_Type;
      last      : Ada.Streams.Stream_Element_Offset;
      elem      : Ada.Streams.Stream_Element_Array(1 .. 1);
      cmd_state : byte := 0;
   begin
      accept start(self : telnet_access; sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.CPU.sim_access) do
         data := self;
         sock_com := sock;
         host := owner;
      end start;
      loop
         select
            accept end_task do
               exit_flag := True;
            end end_task;
            or
            accept start(self : telnet_access; sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.CPU.sim_access) do
               data := self;
               sock_com := sock;
               host := owner;
            end start;
            or
               delay 0.0;
         end select;
         exit when exit_flag;
         if data.all.connected then
            GNAT.Sockets.Receive_Socket(sock_com, elem, last);
--            Ada.Text_IO.Put_Line("TTY: Character received: " & toHex(byte(elem(1))));
            if last = 0 then
               data.all.connected := False;
               data.all.disconnecting := True;
--               Ada.Text_IO.Put_Line("TTY: Receiver disconnecting");
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
               if (not data.all.ready) and (cmd_state = 0) then
--                  Ada.Text_IO.Put_Line("TTY: Character stored: " & toHex(byte(elem(1))));
                  data.all.char := Character'Val(elem(1));
                  data.all.ready := True;
               end if;
            end if;
            if data.all.int_e and data.all.ready then
--               Ada.Text_IO.Put_Line("TTY: Sending interrupt " & toHex(data.all.int_code));
               host.interrupt(data.all.int_code);
            end if;
         end if;
      end loop;
   end telnet_rx;
end;
