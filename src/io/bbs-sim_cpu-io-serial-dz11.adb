--
--  Author: Brent Seidel
--  Date: 20-Aug-2026
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
with Ada.Streams;
use type Ada.Streams.Stream_Element_Offset;
use type Ada.Streams.Stream_Element;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with GNAT.Sockets;
use type GNAT.Sockets.Port_type;
package body BBS.Sim_CPU.io.serial.DZ11 is
   function RBUF_to_word is new Ada.Unchecked_Conversion(source => tRBUF,
                                                         target => word);
   function word_to_RBUF is new Ada.Unchecked_Conversion(source => word,
                                                         target => tRBUF);

   function CSR_to_word is new Ada.Unchecked_Conversion(source => tCSR,
                                                        target => word);
   function word_to_CSR is new Ada.Unchecked_Conversion(source => word,
                                                        target => tCSR);

   function word_to_LPR is new Ada.Unchecked_Conversion(source => word,
                                                        target => tLPR);

   function TCR_to_word is new Ada.Unchecked_Conversion(source => tTCR,
                                                        target => word);
   function word_to_TCR is new Ada.Unchecked_Conversion(source => word,
                                                        target => tTCR);

   function word_to_TDR is new Ada.Unchecked_Conversion(source => word,
                                                        target => tTDR);

   --  ----------------------------------------------------------------------
   --  DZ11 device actions
   --
   --  Set device port and do the network initialiation.  This must be
   --  done only once before using the device.
   --
   procedure init(self : in out DZ11; ptr : DZ11_access; port : GNAT.Sockets.Port_Type) is
   begin
      for i in 0 .. 7 loop
         self.chan(i).T.start(ptr, i, port + GNAT.Sockets.Port_type(i), self.host);
      end loop;
   end;
   --
   --  Set which exception to use.  There are actually separate transmit and
   --  receive vectors.  It appears (as in, not clearly documented) that the
   --  receive vector is first and the transmit vector immediately follows.
   --
   procedure setException(self : in out DZ11; except : long) is
   begin
      self.vector := except;
   end;
   --
   --  Reset the controller
   --
   procedure reset(self : in out DZ11) is
   begin
      self.CSR.maint := False;
      self.CSR.clr   := False;
      self.CSR.mse   := False;
      self.CSR.rx_ie := False;
      self.CSR.tline := 0;
      self.CSR.sae   := False;
      self.CSR.sa    := False;
      self.CSR.trdy  := False;
      while self.silo.current_use > 0 loop
         self.silo.dequeue(self.RBUF);
      end loop;
      for c of self.chan loop
         c.LPR.rcvron := False;
      end loop;
      self.TCR := word_to_TCR(0);
      self.TDR := word_to_TDR(0);
   end;
   --
   --  Process clear command in CSR
   --
   procedure clear(self : in out DZ11) is
   begin
      self.CSR.maint := False;
      self.CSR.clr   := False;
      self.CSR.mse   := False;
      self.CSR.rx_ie := False;
      self.CSR.tline := 0;
      self.CSR.sae   := False;
      self.CSR.sa    := False;
      self.CSR.trdy  := False;
      while self.silo.current_use > 0 loop
         self.silo.dequeue(self.RBUF);
      end loop;
      for c of self.chan loop
         c.LPR.rcvron := False;
      end loop;
      self.TCR.line0_en := False;
      self.TCR.line1_en := False;
      self.TCR.line2_en := False;
      self.TCR.line3_en := False;
      self.TCR.line4_en := False;
      self.TCR.line5_en := False;
      self.TCR.line6_en := False;
      self.TCR.line7_en := False;
      self.TDR := word_to_TDR(0);
   end;
   --
   --  Check if a transmit interrupt should be sent.
   --
   procedure check_tx(self : in out DZ11) is
      status : constant array (uint3) of Boolean := (self.TCR.line0_en, self.TCR.line1_en,
                                                     self.TCR.line2_en, self.TCR.line3_en,
                                                     self.TCR.line4_en, self.TCR.line5_en,
                                                     self.TCR.line6_en, self.TCR.line7_en);
   begin
      if not self.CSR.tie then  --  TX interrupt not enabled
         return;
      end if;
      for i in uint3'Range loop
         if status(i) then
            self.CSR.tline := i;
            self.CSR.trdy  := True;
            if self.CSR.tie then
               self.host.interrupt(self.vector + 4);
            end if;
            return;
         end if;
      end loop;
   end;
   --
   --  Write to a port address.
   --  If nothing is connected, the characters are just dropped.
   --
   overriding
   procedure write(self : in out DZ11; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat) is
      offset : constant byte := byte((addr - self.base) and 16#FF#);
      bvalue : constant byte := byte(data and 16#FF#);
      wvalue : constant word := word(data and 16#FFFF#);
      CSRmsk : constant word := 16#A780#;  --  Mask for the read only CSR bits
      temp   : word;
   begin
      status := BUS_SUCC;
      case size is
         when bits8 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("DZ11: Writing byte " & toOct(bvalue) & " to address " & toOct(addr));
            end if;
            case offset is
               when CSRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" CSR lsb");
                  end if;
                  temp := CSR_to_word(self.CSR) and CSRmsk;
                  self.CSR := word_to_CSR(temp or (word(bvalue) and not CSRmsk));
                  if self.CSR.clr then
                     self.clear;
                  end if;
               when CSRmsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" CSR msb");
                  end if;
                  temp := CSR_to_word(self.CSR) and CSRmsk;
                  self.CSR := word_to_CSR(temp or (word(bvalue)*16#100# and not CSRmsk));
                  self.check_tx;
               when LPRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *LPR lsb");
                  end if;
                  status := BUS_NONE;
               when LPRmsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *LPR msb");
                  end if;
                  status := BUS_NONE;
               when TCRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" TCR lsb");
                  end if;
                  temp := TCR_to_word(self.TCR) and 16#FF00#;
                  self.TCR := word_to_TCR(temp or word(bvalue));
                  self.check_tx;
               when TCRmsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" TCR msb");
                  end if;
                  temp := TCR_to_word(self.TCR) and 16#FF#;
                  self.TCR := word_to_TCR(temp or word(bvalue)*16#100#);
               when TDRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" TDR lsb");
                  end if;
                  self.TDR.tbuf := bvalue;
                  if self.chan(Integer(self.CSR.tline)).connected then
                     self.chan(Integer(self.CSR.tline)).T.write(Character'Val(Integer(self.TDR.tbuf)));
                  else
                     if self.CSR.tie then
                        self.host.interrupt(self.vector + 4 + 16#10_00_0000#);
                     end if;
                  end if;
               when TDRmsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *TDR msb");
                  end if;
                  status := BUS_NONE;
               when others =>
                  status := BUS_NONE;
            end case;
         when bits16 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("DZ11: Writing word " & toOct(wvalue) & " to address " & toOct(addr));
            end if;
            case offset is
               when CSRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" CSR");
                  end if;
                  temp := CSR_to_word(self.CSR) and CSRmsk;
                  self.CSR := word_to_CSR(temp or (wvalue and not CSRmsk));
                  if self.CSR.clr then
                     self.clear;
                  end if;
                  self.check_tx;
               when LPRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" LPR");
                  end if;
                  temp := wvalue and 7;
                  self.chan(Integer(temp)).LPR := word_to_LPR(wvalue);
               when TCRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" TCR");
                  end if;
                  self.TCR := word_to_TCR(wvalue);
                  self.check_tx;
               when TDRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" TDR");
                  end if;
                  self.TDR := word_to_TDR(wvalue);
                  if self.chan(Integer(self.CSR.tline)).connected then
                     self.chan(Integer(self.CSR.tline)).T.write(Character'Val(Integer(self.TDR.tbuf)));
                  else
                     if self.CSR.tie then
                        self.host.interrupt(self.vector + 4 + 16#10_00_0000#);
                     end if;
                  end if;
               when others =>
                  status := BUS_NONE;
            end case;
         when others =>
            status := BUS_NONE;
      end case;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out DZ11; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus is
      offset    : constant byte := byte((addr - self.base) and 16#FF#);
      ret_val   : word := 0;
   begin
      status := BUS_SUCC;
      case size is
         when bits8 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("DZ11: Reading byte from address " & toOct(addr));
            end if;
            case offset is
               when CSRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", CSR lsb");
                  end if;
                  ret_val := CSR_to_word(self.CSR) and 16#FF#;
               when CSRmsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", CSR msb");
                  end if;
                  ret_val := CSR_to_word(self.CSR)/16#100# and 16#FF#;
               when LPRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *LPR lsb");
                  end if;
                  status := BUS_NONE;
               when LPRmsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *LPR msb");
                  end if;
                  status := BUS_NONE;
               when TCRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", TCR lsb");
                  end if;
                  ret_val := TCR_to_word(self.TCR) and 16#FF#;
               when TCRmsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", TCR msb");
                  end if;
                  ret_val := TCR_to_word(self.TCR)/16#100# and 16#FF#;
               when TDRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *MSR lsb");
                  end if;
                  status := BUS_NONE;
               when TDRmsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *MSR msb");
                  end if;
                  status := BUS_NONE;
               when others =>
                  status := BUS_NONE;
            end case;
         when bits16 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("DZ11: Reading word from address " & toOct(addr));
            end if;
            case offset is
               when CSRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", CSR");
                  end if;
                  ret_val := CSR_to_word(self.CSR);
               when LPRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", RBUF");
                  end if;
                  if self.silo.current_use > 0 then
                     self.silo.dequeue(self.RBUF);
                     ret_val := RBUF_to_word(self.RBUF);
                  else
                     ret_val := 0;
                  end if;
               when TCRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", TCR");
                  end if;
                  ret_val := TCR_to_word(self.TCR);
               when MSRlsb =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(", *MSR");
                  end if;
                  status := BUS_NONE;
               when others =>
                  status := BUS_NONE;
            end case;
         when others =>
            status := BUS_NONE;
      end case;
      if self.host.trace.io or debug then
         Ada.Text_IO.Put_Line(", value " & toOct(ret_val));
      end if;
      return data_bus(ret_val);
   end;
   --
   --  Close the network connection and halt the tasks.
   --
   overriding
   procedure shutdown(self : in out DZ11) is
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
   task body DZ11_server is
      data      : DZ11_access;
      idx       : Integer;
      host      : BBS.Sim_CPU.CPU.sim_access;
      exit_flag : Boolean := False;
      sock_ser  : GNAT.Sockets.Socket_Type;  --  Server Socket
      sock_com  : GNAT.Sockets.Socket_Type;  --  Communication Socket
      rx_task   : DZ11_rx;
      local     : GNAT.Sockets.Sock_Addr_Type;
      s         : GNAT.Sockets.Stream_Access;
   begin
      accept start(self : DZ11_access; index : Integer; port : GNAT.Sockets.Port_Type; owner : BBS.Sim_CPU.CPU.sim_access) do
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
                  if data.all.CSR.tie then
                     host.interrupt(data.all.vector + 4 + 16#10_00_0000#);
                  end if;
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
            String'write(s, "DZ11 connected to simulated CPU " & host.name & CRLF);
            rx_task.start(data, idx, sock_com, host);
         end if;
      end loop;
      rx_task.end_task;
      GNAT.Sockets.Close_Socket(sock_ser);
      GNAT.Sockets.Close_Socket(sock_com);
   end DZ11_server;
   --
   --  Task body for telnet receiver task.  This is intended to only be
   --  used by the main telnet task for reciving characters from the
   --  network connection.
   --
   task body DZ11_rx is
      exit_flag : Boolean := False;
      data      : DZ11_access;                               --  Owning DZ11
      idx       : Integer;                                   --  Line number
      host      : BBS.Sim_CPU.CPU.sim_access;                --  Host simulator
      sock_com  : GNAT.Sockets.Socket_Type;
      last      : Ada.Streams.Stream_Element_Offset;
      elem      : Ada.Streams.Stream_Element_Array(1 .. 1);
      cmd_state : byte := 0;
      RBUF      : tRBUF;                                     --  Receive buffer
   begin
      accept start(self : DZ11_access; index : Integer;
            sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.CPU.sim_access) do
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
            accept start(self : DZ11_access; index : Integer;
                  sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.CPU.sim_access) do
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
            if host.trace.io or debug then
               Ada.Text_IO.Put_Line("DZ11: Character received on channel " & Integer'Image(idx) & ": " & toHex(byte(elem(1))));
            end if;
            if last = 0 then
               data.all.chan(idx).connected := False;
               data.all.chan(idx).disconnecting := True;
               if host.trace.io or debug then
                  Ada.Text_IO.Put_Line("DZ11: Closing channel.");
               end if;
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
               if (cmd_state = 0) then
                  RBUF.rbuf    := byte(elem(1));
                  RBUF.rxline  := uint3(idx);
                  RBUF.perror  := False;
                  RBUF.ferror  := False;
                  RBUF.overrun := False;
                  RBUF.valid   := True;
                  data.all.silo.enqueue(RBUF);
               end if;
            end if;
            if data.all.CSR.rx_ie then
               if data.all.CSR.sae then
                  if data.all.silo.current_use >= 16 then
                     host.interrupt(data.all.vector);
                     if host.trace.io or debug then
                        Ada.Text_IO.Put_Line("DZ11: Sending silo alarm interrupt " & toHex(data.all.vector));
                     end if;
                  end if;
               else
                  host.interrupt(data.all.vector);
                  if host.trace.io or debug then
                     Ada.Text_IO.Put_Line("DZ11: Sending RX interrupt " & toHex(data.all.vector));
                  end if;
               end if;
            end if;
         end if;
      end loop;
   end DZ11_rx;
   --
begin
   devs.append((title'Access, desc'Access));
end;
