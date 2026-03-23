--
--  Author: Brent Seidel
--  Date: 20-Jan-2026
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
--  with SimCPU. If not, see <https://www.gnu.org/licenses/>.
--
--
with Ada.Streams;
use type Ada.Streams.Stream_Element_Offset;
use type Ada.Streams.Stream_Element;
with Ada.Text_IO;
with Ada.Exceptions;
package body BBS.Sim_CPU.io.serial.dl11 is
   --  ----------------------------------------------------------------------
   --  Telnet device actions
   --
   --  Set device port and do the network initialiation.  This must be
   --  done only once before using the device.
   --
   procedure init(self : in out dl11x; ptr : dl11_access; port : GNAT.Sockets.Port_Type) is
   begin
      self.T.start(ptr, port, self.host);
   end;
   --
   --  Set which exception to use.  The RX vector is the LSB of except.  The TX
   --  vector is the next MSB of except.
   --
   procedure setException(self : in out dl11x; except : long) is
   begin
      self.rx_vect := byte(except and 16#FF#);
      self.tx_vect := byte(except/16#100# and 16#FF#);
   end;
   --
   --  Write to a port address.
   --  If nothing is connected, the characters are just dropped.
   --
   overriding
   procedure write(self : in out dl11x; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat) is
      offset : constant addr_bus := addr - self.base;
   begin
      case size is
         when bits8 =>
            if (word(self.host.trace) and 2) = 2 then
               Ada.Text_IO.Put("DL11: Writing byte " & toOct(byte(data and 16#FF#)) & " to ");
            end if;
            case offset is
               when off_rx_statl =>  --  LSB of receive status register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("RCSR LSB");
                  end if;
                  self.rx_en := (data and 64) /= 0;  --  Receive interrupt enable
               when off_rx_statm =>  --  MSB of receive status register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("RCSR MSB");
                  end if;
               when off_rx_datal =>  --  LSB of reciver buffer register (character received)
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("RBUF LSB");
                  end if;
                  self.rx_done := False;
                  self.ready := False;
               when off_rx_datam =>  --  MSB of reciver buffer register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("RBUF MSB");
                  end if;
                  self.rx_done := False;
                  self.ready := False;
               when off_tx_statl =>   --  LSB of transmitter status register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("XCSR LSB");
                  end if;
                  self.tx_en := (data and 64) /= 0;  --  Transmit interrupt enable
                  if self.tx_rdy and ((data and 64) /= 0) then
                     self.host.interrupt(long(self.tx_vect));
                  end if;
               when off_tx_statm =>  --  MSB of transmitter status register (unused)
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("XCSR MSB");
                  end if;
               when off_tx_datal =>  --  LSB of transmitter buffer register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("XBUF LSB");
                  end if;
                  if self.connected then
                     self.T.write(Character'Val(Integer(data and 16#FF#)));
                  end if;
               when off_tx_datam =>  --  MSB of transmitter buffer register (unused)
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("XBUF MSB");
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
         when bits16 =>
            if (word(self.host.trace) and 2) = 2 then
               Ada.Text_IO.Put("DL11: Writing word " & toOct(word(data and 16#FFFF#)) & " to ");
            end if;
            case offset is
               when off_rx_statl =>  --  receive status register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("RCSR");
                  end if;
                  self.rx_en := (data and 64) /= 0;  --  Receive interrupt enable
               when off_rx_datal =>  --  reciver buffer register (character received)
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("RBUF");
                  end if;
                  self.rx_done := False;
                  self.ready := False;
               when off_tx_statl =>   --  transmitter status register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("XCSR");
                  end if;
                  self.tx_en := (data and 64) /= 0;  --  Transmit interrupt enable
                  if self.tx_rdy and ((data and 64) /= 0) then
                     self.host.interrupt(long(self.tx_vect));
                  end if;
               when off_tx_datal =>  --  transmitter buffer register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("XBUF");
                  end if;
                  if self.connected then
                     self.T.write(Character'Val(Integer(data and 16#FF#)));
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
         when others =>
            status := BUS_NONE;
      end case;
   exception
     when e : others =>
       Ada.Text_IO.Put_Line("Exception occured while trying to write to DL11.");
       Ada.Text_IO.Put_Line("  Name: " & Ada.Exceptions.Exception_Name(e));
       Ada.Text_IO.Put_Line("  Msg:  " & Ada.Exceptions.Exception_Message(e));
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out dl11x; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus is
      offset : constant addr_bus := addr - self.base;
      temp   : data_bus := 0;
   begin
      case size is
         when bits8 =>
            if (word(self.host.trace) and 2) = 2 then
               Ada.Text_IO.Put("DL11: Reading byte from ");
            end if;
            case offset is
               when off_rx_statl =>  --  LSB of receive status register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put("RCSR LSB");
                  end if;
                  temp := (if self.rx_done then 128 else 0) +
                    (if self.rx_en then 64 else 0);
               when off_rx_statm =>  --  MSB of receive status register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put("RCSR MSB");
                  end if;
                  temp := (if self.rx_act then 8 else 0);
               when off_rx_datal =>  --  LSB of reciver buffer register (character received)
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put("RBUF LSB");
                  end if;
                  self.rx_done := False;
                  self.ready := False;
                  temp := data_bus(Character'Pos(self.char));
               when off_rx_datam =>  --  MSB of reciver buffer register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put("RBUF MSB");
                  end if;
                  self.rx_done := False;
                  self.ready := False;
                  temp := 0;
               when off_tx_statl =>   --  LSB of transmitter status register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put("XCSR LSB");
                  end if;
                  temp := (if self.tx_rdy then 128 else 0) +
                    (if self.tx_en then 64 else 0);
               when off_tx_statm =>  --  MSB of transmitter status register (unused)
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put("XCSR MSB");
                  end if;
                  temp := 0;
               when off_tx_datal =>  --  LSB of transmitter buffer register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put("XBUF LSB");
                  end if;
                  temp := 0;
               when off_tx_datam =>  --  MSB of transmitter buffer register (unused)
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put("XBUF MSB");
                  end if;
                  temp := 0;
               when others =>
                  status := BUS_NONE;
            end case;
            if (word(self.host.trace) and 2) = 2 then
               Ada.Text_IO.Put_Line(" value " & toOct(byte(temp)));
            end if;
         when bits16 =>
            if (word(self.host.trace) and 2) = 2 then
               Ada.Text_IO.Put("DL11: Reading byte from ");
            end if;
            case offset is
               when off_rx_statl =>  --  Receive status register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put("RCSR");
                  end if;
                  temp := (if self.rx_act then 2048 else 0) +
                    (if self.rx_done then 128 else 0) +
                    (if self.rx_en then 64 else 0);
               when off_rx_datal =>  --  LSB of reciver buffer register (character received)
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put("RBUF");
                  end if;
                  self.rx_done := False;
                  self.ready := False;
                  temp := data_bus(Character'Pos(self.char));
               when off_tx_statl =>   --  LSB of transmitter status register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put("XCSR");
                  end if;
                  temp := (if self.tx_rdy then 128 else 0) +
                    (if self.tx_en then 64 else 0);
               when off_tx_datal =>  --  LSB of transmitter buffer register
                  if (word(self.host.trace) and 2) = 2 then
                     Ada.Text_IO.Put("XBUF");
                  end if;
                  temp := 0;
               when others =>
                  status := BUS_NONE;
            end case;
            if (word(self.host.trace) and 2) = 2 then
               Ada.Text_IO.Put_Line(" value " & toOct(word(temp)));
            end if;
         when others =>
            status := BUS_NONE;
      end case;
      return temp;
   end;
   --
   --  Close the network connection and halt the tasks.
   --
   overriding
   procedure shutdown(self : in out dl11x) is
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
   task body dl11_server is
      data      : dl11_access;
      host      : BBS.Sim_CPU.CPU.sim_access;
      exit_flag : Boolean := False;
      sock_ser  : GNAT.Sockets.Socket_Type;  --  Server Socket
      sock_com  : GNAT.Sockets.Socket_Type;  --  Communication Socket
      rx_task   : dl11_rx;
      local     : GNAT.Sockets.Sock_Addr_Type;
      s         : GNAT.Sockets.Stream_Access;
      sending   : Boolean := False;
      c_to_send : Character;
   begin
      accept start(self : dl11_access; port : GNAT.Sockets.Port_Type; owner : BBS.Sim_CPU.CPU.sim_access) do
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
         data.all.tx_rdy := True;
      end start;
      if (word(host.trace) and 2) = 2 then
         Ada.Text_IO.Put_Line("DL11: Telnet server started.");
      end if;
      loop
         select
            accept write(char : Character) do
               c_to_send := char;
               sending := True;
            end write;
         or
            accept end_task do
               exit_flag := True;
            end end_task;
         or
            delay 0.0;
         end select;
         exit when exit_flag;
         if sending then
            sending := False;
            String'write(s, "" & c_to_send);
            if data.all.tx_en then
--               Ada.Text_IO.Put_Line("DL11: TX of character with interrupt.");
               data.all.tx_rdy := False;
               delay character_delay;
               data.all.tx_rdy := True;
               host.interrupt(long(data.all.tx_vect));
            end if;
         end if;
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
   end dl11_server;
   --
   --  Task body for telnet receiver task.  This is intended to only be
   --  used by the main telnet task for reciving characters from the
   --  network connection.
   --
   task body dl11_rx is
      exit_flag : Boolean := False;
      data      : dl11_access;
      host      : BBS.Sim_CPU.CPU.sim_access;
      sock_com  : GNAT.Sockets.Socket_Type;
      last      : Ada.Streams.Stream_Element_Offset;
      elem      : Ada.Streams.Stream_Element_Array(1 .. 1);
      cmd_state : byte := 0;
   begin
      accept start(self : dl11_access; sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.CPU.sim_access) do
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
            accept start(self : dl11_access; sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.CPU.sim_access) do
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
            if (word(host.trace) and 2) = 2 then
               Ada.Text_IO.Put_Line("DL11: Character received: " & toHex(byte(elem(1))));
            end if;
            if last = 0 then
               data.all.connected := False;
               data.all.disconnecting := True;
               if (word(host.trace) and 2) = 2 then
                  Ada.Text_IO.Put_Line("DL11: Receiver disconnecting");
               end if;
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
                  if (word(host.trace) and 2) = 2 then
                     Ada.Text_IO.Put_Line("DL11: Character stored: " & toHex(byte(elem(1))));
                     end if;
                  data.all.char := Character'Val(elem(1));
                  data.all.ready := True;
               end if;
            end if;
            if data.all.rx_en and data.all.ready then
               data.all.rx_act := True;
               delay character_delay;
               data.all.rx_act := False;
               if (word(host.trace) and 2) = 2 then
                  Ada.Text_IO.Put_Line("DL11: Sending interrupt " & toHex(data.all.rx_vect));
               end if;
               host.interrupt(long(data.all.rx_vect));
            end if;
            data.all.rx_done := True;
         end if;
      end loop;
   end dl11_rx;
end;
