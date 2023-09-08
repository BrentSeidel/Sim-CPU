package body BBS.Sim_CPU.serial.telnet is
   --  ----------------------------------------------------------------------
   --  8 bit telnet device actions
   --
   --  Set device port and do the network initialiation.  This must be
   --  done before using the device.
   --
   procedure init(self : in out tel_tty; ptr : telnet_access; port : GNAT.Sockets.Port_Type) is
   begin
     GNAT.Sockets.Initialize(True);
     self.T.start(ptr, port);
   end;
   --
   --  Write to a port address
   --
   overriding
   procedure write(self : in out tel_tty; addr : addr_bus; data : data_bus) is
   begin
      if addr = self.base then
        self.T.write(Character'Val(Integer(data and 16#FF#)));
      end if;
   end;
   --
   --  Read from a port address
   --
   overriding
   function read(self : in out tel_tty; addr : addr_bus) return data_bus is
   begin
      if addr = self.base then
         if self.ready then
            self.ready := False;
         end if;
         return data_bus(Character'Pos(self.char));
      elsif addr = (self.base + 1) then
         if self.ready then
            return 1;
         end if;
         Ada.Text_IO.Get_Immediate(self.char, self.ready);  --  ***
         if self.ready then
            return 1;
         else
            return 0;
         end if;
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
   --  Task type for telnet type server
   --
   task body telnet_server is
     data        : telnet_access;
     exit_flag   : Boolean := False;
     starting    : Boolean := False;
     started     : Boolean := False;
     sock_rx : GNAT.Sockets.Socket_Type;
     sock_tx : GNAT.Sockets.Socket_Type;
     local   : GNAT.Sockets.Sock_Addr_Type;
     s : GNAT.Sockets.Stream_Access;
   begin
     loop
       select
         accept start(self : telnet_access; port : GNAT.Sockets.Port_Type) do
           if not started then
             data := self;
             local.Addr := GNAT.Sockets.Any_Inet_Addr;
             local.Port := port;
             starting := True;
           end if;
         end start;
       or
         accept end_task do
           exit_flag := true;
         end end_task;
       or
         accept write(char : Character) do
           String'write(s, "" & char);
         end write;
       end select;
       exit when exit_flag;
       if starting then
         GNAT.Sockets.Create_Socket(sock_rx, GNAT.Sockets.Family_Inet,
                                    GNAT.Sockets.Socket_Stream);
         GNAT.Sockets.Set_Socket_Option(sock_rx, GNAT.Sockets.Socket_Level,
                                        (GNAT.Sockets.Reuse_Address, True));
         GNAT.Sockets.Bind_Socket(sock_rx, local);
         GNAT.Sockets.Listen_Socket(sock_rx);
         --
         --  This call blocks until a connection request comes in.
         --
         GNAT.Sockets.Accept_Socket(sock_rx, sock_tx, local);
         s := GNAT.Sockets.Stream(sock_rx);
         started := True;
       end if;
     end loop;
   end telnet_server;
end;
