with BBS.embed;
use type BBS.embed.uint32;
with Ada.Strings.Fixed;
with Ada.Text_IO;
package body BBS.Sim_CPU is
   --
   --  Simulator switches and lights
   --
   function get_lr_data(self : in out simulator) return data_bus is
   begin
      return self.lr_data;
   end;
   --
   function get_lr_addr(self : in out simulator) return addr_bus is
   begin
      return self.lr_data;
   end;
   --
   function get_lr_ctrl(self : in out simulator) return ctrl_mode is
   begin
      return self.lr_ctl;
   end;
   --
   procedure set_sr_ad(self : in out simulator; value : ad_bus) is
   begin
      self.sr_ad := value;
   end;
   --
   procedure set_sr_ctrl(self : in out simulator; value : ctrl_mode) is
   begin
      self.sr_ctl := value;
   end;
   --
   --  Utility functions
   --
   hex_digit : String := "0123456789ABCDEF";
   --
   function toHex(value : byte) return String is
   begin
      return hex_digit(Integer(value/16#10#) + 1) & hex_digit(Integer(value and 16#0F#) + 1);
   end;
   --
   function toHex(value : word) return String is
   begin
      return hex_digit(Integer(value/16#1000#) + 1) &
        hex_digit(Integer((value/16#100#) and 16#0F#) + 1) &
        hex_digit(Integer((value/16#10#) and 16#0F#) + 1) &
        hex_digit(Integer(value and 16#0F#) + 1);
   end;
   --
   --  Return a value from a hexidecimal string
   --
   function toHex(s : String) return BBS.embed.uint32 is
      v : BBS.embed.uint32 := 0;
   begin
      for i in s'Range loop
         exit when not isHex(s(i));
         v := v*16#10# + hexDigit(s(i));
      end loop;
      return v;
   end;
   --
   --  Return the hexidecimal digit
   --
   function hexDigit(c : Character) return BBS.embed.uint32 is
   begin
      case c is
         when '0' =>
            return 0;
         when '1' =>
            return 1;
         when '2' =>
            return 2;
         when '3' =>
            return 3;
         when '4' =>
            return 4;
         when '5' =>
            return 5;
         when '6' =>
            return 6;
         when '7' =>
            return 7;
         when '8' =>
            return 8;
         when '9' =>
            return 9;
         when 'A' | 'a' =>
            return 10;
         when 'B' | 'b' =>
            return 11;
         when 'C' | 'c' =>
            return 12;
         when 'D' | 'd' =>
            return 13;
         when 'E' | 'e' =>
            return 14;
         when 'F' | 'f' =>
            return 15;
         when others =>
            return 0;
      end case;
   end;
   --
   --  ----------------------------------------------------------------------
   --  I/O device actions
   --
   function getSize(self : in out io_device) return addr_bus is (self.size);
   function getBase(self : in out io_device) return addr_bus is (self.base);
   --  ----------------------------------------------------------------------
   --
   --  Routines to help parsing data files.  This is to help avoid duplication
   --  of code in the child packages.
   --
   --  Parse a line of an Intex Hex file.
   --  s     - The input string to parse
   --  count - The number of data bytes in the record
   --  addr  - The memory address for the data bytes
   --  rec   - The record type
   --  data  - An array containing the data bytes (0 .. count) are valid
   --  valid - True for a valid record parsed.
   --
   procedure IntelHex(s : String; count : out byte; addr : out word; rec : out byte;
                      data : out page; valid : out Boolean) is
      start : Natural := Ada.Strings.Fixed.Index(s, ":");
      ptr   : Natural := start + 1;
      t1    : BBS.embed.uint32;
      t2    : BBS.embed.uint32;
      check : byte;
   begin
      count := 0;
      addr  := 0;
      rec   := 1;
      data  := (others => 0);
      valid := False;
      --
      --  Get byte count
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := byte(t1);
      count := byte(t1);
      --
      -- Get address
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + byte(t1);
      if isHex(s(ptr)) then
         t2 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t2 := t2*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + byte(t2);
      addr := word(t1*16#100# + t2);
      --
      --  Get record type
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + byte(t1);
      rec := byte(t1);
      --
      --  Get data
      --
      if count > 0 then
         for i in 0 .. (count - 1) loop
            if isHex(s(ptr)) then
               t1 := hexDigit(s(ptr));
            else
               return;
            end if;
            ptr := ptr + 1;
            if isHex(s(ptr)) then
               t1 := t1*16 + hexDigit(s(ptr));
            else
               return;
            end if;
            ptr := ptr + 1;
            check := check + byte(t1);
            data(Integer(i)) := byte(t1);
         end loop;
      end if;
      --
      --  Check checksum
      --
      if isHex(s(ptr)) then
         t1 := hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      if isHex(s(ptr)) then
         t1 := t1*16 + hexDigit(s(ptr));
      else
         return;
      end if;
      ptr := ptr + 1;
      check := check + byte(t1);
      if check = 0 then
         valid := True;
      else
         Ada.Text_IO.Put_Line("Checksum value for " & s & " is " & toHex(check));
      end if;
   end;
   --
end BBS.Sim_CPU;
