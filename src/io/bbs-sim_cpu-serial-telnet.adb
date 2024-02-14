with Ada.Streams;
use type Ada.Streams.Stream_Element_Offset;
with Ada.Text_IO;
with Ada.Exceptions;
package body BBS.Sim_CPU.serial.telnet is
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
   procedure write(self : in out tel_tty; addr : addr_bus; data : data_bus) is
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
   function read(self : in out tel_tty; addr : addr_bus) return data_bus is
   begin
      if addr = (self.base + 1) then
         self.ready := False;
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
   --  Get the base address
   --
   overriding
   function getBase(self : in out tel_tty) return addr_bus is
   begin
      return self.base;
   end;
   --
   --  Set the base address
   --
   overriding
   procedure setBase(self : in out tel_tty; base : addr_bus) is
   begin
      self.base := base;
   end;
   --
   --  Set the owner (used mainly for DMA and interrupts)
   --
   overriding
   procedure setOwner(self : in out tel_tty; owner : sim_access) is
   begin
      self.host := owner;
   end;
   --
   --  Close the network connection and halt the tasks.
   --
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
      host      : BBS.Sim_CPU.sim_access;
      exit_flag : Boolean := False;
      sock_ser  : GNAT.Sockets.Socket_Type;  --  Server Socket
      sock_com  : GNAT.Sockets.Socket_Type;  --  Communication Socket
      rx_task   : telnet_rx;
      local     : GNAT.Sockets.Sock_Addr_Type;
      s         : GNAT.Sockets.Stream_Access;
   begin
      accept start(self : telnet_access; port : GNAT.Sockets.Port_Type; owner : BBS.Sim_CPU.sim_access) do
         data := self;
         local.Addr := GNAT.Sockets.Any_Inet_Addr;
         local.Port := port;
         host := owner;
      end start;
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
         if not data.all.connected then
            GNAT.Sockets.Create_Socket(sock_ser, GNAT.Sockets.Family_Inet,
                                    GNAT.Sockets.Socket_Stream);
            GNAT.Sockets.Set_Socket_Option(sock_ser, GNAT.Sockets.Socket_Level,
                                          (GNAT.Sockets.Reuse_Address, True));
            GNAT.Sockets.Bind_Socket(sock_ser, local);
            GNAT.Sockets.Listen_Socket(sock_ser);
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
      host      : BBS.Sim_CPU.sim_access;
      sock_com  : GNAT.Sockets.Socket_Type;
      last      : Ada.Streams.Stream_Element_Offset;
      elem      : Ada.Streams.Stream_Element_Array(1 .. 1);
   begin
      accept start(self : telnet_access; sock : GNAT.Sockets.Socket_Type; owner : BBS.Sim_CPU.sim_access) do
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
               delay 0.0;
         end select;
         exit when exit_flag;
         if data.all.connected then
            GNAT.Sockets.Receive_Socket(sock_com, elem, last);
            if last = 0 then
               data.all.connected := False;
            --
            --  If the client has not read the last character, drop the current
            --  current one.  Buffering could be added at some point, but this
            --  seems to be consistent with the way that CP/M works.
            --
            elsif not data.all.ready then
               data.all.char := Character'Val(elem(1));
               data.all.ready := True;
--               Ada.Text_IO.Put_Line("TTY: Character received.");
            end if;
            if data.all.int_e then
--               Ada.Text_IO.Put_Line("TTY: Sending interrupt " & toHex(data.all.int_code));
               host.interrupt(data.all.int_code);
            end if;
         end if;
      end loop;
   end telnet_rx;
end;
