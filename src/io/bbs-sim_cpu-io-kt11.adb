--
--  Author: Brent Seidel
--  Date: 7-Jul-2025
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
--  KT11 memory manager for for PDP-11
--
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with BBS.Sim_CPU.cpu.pdp11;
package body BBS.Sim_CPU.io.kt11 is
   --
   function MMR0_to_word is new Ada.Unchecked_Conversion(source => mmr0_type,
                                                         target => word);
   function word_to_MMR0 is new Ada.Unchecked_Conversion(source => word,
                                                         target => mmr0_type);
   --
   function PDR_to_word is new Ada.Unchecked_Conversion(source => pdr,
                                                        target => word);
   function word_to_PDR is new Ada.Unchecked_Conversion(source => word,
                                                        target => pdr);
   --
   procedure setException(self : in out kt11; except : long) is
   begin
      self.vector := except;
   end;
   --
   procedure reset(self : in out kt11) is
   begin
      self.mmr0 := word_to_MMR0(0);
      self.mmr1 := 0;
      self.mmr2 := 0;
      if self.host.trace.io then
         Ada.Text_IO.Put_Line("KT11: Reset commanded by bus");
      end if;
   end;
   --
   function read(self : in out kt11; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus is
      ret_val : data_bus := 0;
      num     : Integer;
      config  : constant BBS.Sim_CPU.cpu.pdp11.features := BBS.Sim_CPU.cpu.pdp11.pdp11_access(self.host).getConfig;
   begin
      status := BUS_NONE;
      case size is
         when bits8 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("KT11: Reading byte from address " & toOct(addr));
            end if;
            case addr is
               when sid_pdr_start .. sid_pdr_end + 1 =>
                  num := Integer(addr - sid_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *Supervisor I/D PDR" & Integer'Image(num));
                  end if;
                  if config.has_super then
                     null;
                  end if;
               when sd_pdr_start .. sd_pdr_end + 1 =>
                  num := Integer(addr - sd_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *Supervisor I PDR" & Integer'Image(num));
                  end if;
                  if config.has_super and config.has_ID then
                     null;
                  end if;
               when sid_par_start .. sid_par_end + 1 =>
                  num := Integer(addr - sid_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *Supervisor I/D PAR" & Integer'Image(num));
                  end if;
                  if config.has_super then
                     null;
                  end if;
               when sd_par_start .. sd_par_end + 1 =>
                  num := Integer(addr - sd_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *Supervisor I PAR" & Integer'Image(num));
                  end if;
                  if config.has_super and config.has_super then
                     null;
                  end if;
               when kid_pdr_start .. kid_pdr_end + 1 =>
                  num := Integer(addr - kid_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *Kernel I/D PDR" & Integer'Image(num));
                  end if;
               when kd_pdr_start .. kd_pdr_end + 1 =>
                  num := Integer(addr - kd_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *Kernel I PDR" & Integer'Image(num));
                  end if;
                  if config.has_ID then
                     null;
                  end if;
               when kid_par_start .. kid_par_end + 1 =>
                  num := Integer(addr - kid_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *Kernel I/D PAR" & Integer'Image(num));
                  end if;
               when kd_par_start .. kd_par_end + 1 =>
                  num := Integer(addr - kd_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *Kernel I PAR" & Integer'Image(num));
                  end if;
               when uid_pdr_start .. uid_pdr_end + 1 =>
                  num := Integer(addr - uid_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *User I/D PDR" & Integer'Image(num));
                  end if;
               when ud_pdr_start .. ud_pdr_end + 1 =>
                  num := Integer(addr - ud_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *User I PDR" & Integer'Image(num));
                  end if;
               when uid_par_start .. uid_par_end + 1 =>
                  num := Integer(addr - uid_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *User I/D PAR" & Integer'Image(num));
                  end if;
               when ud_par_start .. ud_par_end + 1 =>
                  num := Integer(addr - ud_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *User I PAR" & Integer'Image(num));
                  end if;
               when mmr0_sr0 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" *MMR0/SR0 lsb");
                  end if;
                  ret_val := data_bus(mmr0_to_word(self.mmr0) and 16#FF#);
                  status := BUS_SUCC;
               when mmr0_sr0 + 1 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" MMR0/SR0 msb");
                  end if;
                  ret_val := data_bus(mmr0_to_word(self.mmr0) and 16#FF00#)/16#100#;
                  status := BUS_SUCC;
               when mmr1_sr1 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" MMR1/SR1 lsb");
                  end if;
                  ret_val := data_bus(self.mmr1 and 16#FF#);
                  status := BUS_SUCC;
               when mmr1_sr1 + 1 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" MMR1/SR1 msb");
                  end if;
                  ret_val := data_bus(self.mmr1 and 16#FF00#)/16#100#;
                  status := BUS_SUCC;
               when mmr2_sr2 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" MMR2/SR2 lsb");
                  end if;
                  ret_val := data_bus(self.mmr2 and 16#FF#);
                  status := BUS_SUCC;
               when mmr2_sr2 + 1 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" MMR2/SR2 msb");
                  end if;
                  ret_val := data_bus(self.mmr2 and 16#FF00#)/16#100#;
                  status := BUS_SUCC;
               when others =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" Unknown register");
                  end if;
            end case;
         when bits16 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("KT11: Reading word from address " & toOct(addr));
            end if;
            case addr is
               when sid_pdr_start .. sid_pdr_end =>
                  num := Integer(addr - sid_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" Supervisor I/D PDR" & Integer'Image(num));
                  end if;
                  if config.has_super then
                     ret_val := data_bus(PDR_to_word(self.sid_pdr(num)));
                     status := BUS_SUCC;
                  end if;
               when sd_pdr_start .. sd_pdr_end =>
                  num := Integer(addr - sd_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" Supervisor I PDR" & Integer'Image(num));
                  end if;
                  if config.has_super and config.has_ID then
                     ret_val := data_bus(PDR_to_word(self.sd_pdr(num)));
                     status := BUS_SUCC;
                  end if;
               when sid_par_start .. sid_par_end =>
                  num := Integer(addr - sid_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" Supervisor I/D PAR" & Integer'Image(num));
                  end if;
                  if config.has_super then
                     ret_val := data_bus(self.sid_par(num));
                     status := BUS_SUCC;
                  end if;
               when sd_par_start .. sd_par_end =>
                  num := Integer(addr - sd_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" Supervisor I PAR" & Integer'Image(num));
                  end if;
                  if config.has_super and config.has_ID then
                     ret_val := data_bus(self.sd_par(num));
                     status := BUS_SUCC;
                  end if;
               when kid_pdr_start .. kid_pdr_end =>
                  num := Integer(addr - kid_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" Kernel I/D PDR" & Integer'Image(num));
                  end if;
                  ret_val := data_bus(PDR_to_word(self.kid_pdr(num)));
                  status := BUS_SUCC;
               when kd_pdr_start .. kd_pdr_end =>
                  num := Integer(addr - kd_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" Kernel I PDR" & Integer'Image(num));
                  end if;
                  if config.has_ID then
                     ret_val := data_bus(PDR_to_word(self.kd_pdr(num)));
                     status := BUS_SUCC;
                  end if;
               when kid_par_start .. kid_par_end =>
                  num := Integer(addr - kid_par_start)/2;
                  ret_val := data_bus(self.kid_par(num));
                  status  := BUS_SUCC;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" Kernel I/D PAR" & Integer'Image(num));
                  end if;
               when kd_par_start .. kd_par_end =>
                  num := Integer(addr - kd_par_start)/2;
                  if config.has_ID then
                     ret_val := data_bus(self.kd_par(num));
                     status  := BUS_SUCC;
                  else
                     Ada.Text_IO.Put(" disabled User I/D PDR" & Integer'Image(num));
                  end if;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" Kernel I PAR" & Integer'Image(num));
                  end if;
               when uid_pdr_start .. uid_pdr_end =>
                  num := Integer(addr - uid_pdr_start)/2;
                  if config.has_user then
                     ret_val := data_bus(PDR_to_word(self.uid_pdr(num)));
                     status  := BUS_SUCC;
                  end if;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" User I/D PDR" & Integer'Image(num));
                  end if;
               when ud_pdr_start .. ud_pdr_end =>
                  num := Integer(addr - ud_pdr_start)/2;
                  if config.has_user and config.has_ID then
                     ret_val := data_bus(PDR_to_word(self.ud_pdr(num)));
                     status := BUS_SUCC;
                  end if;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" User I PDR" & Integer'Image(num));
                  end if;
               when uid_par_start .. uid_par_end =>
                  num := Integer(addr - uid_par_start)/2;
                  if config.has_user then
                     ret_val := data_bus(self.uid_par(num));
                     status := BUS_SUCC;
                  else
                     Ada.Text_IO.Put(" disabled User I/D PAR" & Integer'Image(num));
                  end if;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" User I/D PAR" & Integer'Image(num));
                  end if;
               when ud_par_start .. ud_par_end =>
                  num := Integer(addr - ud_par_start)/2;
                  if config.has_user and config.has_ID then
                     ret_val := data_bus(self.ud_par(num));
                     status := BUS_SUCC;
                  end if;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" User I PAR" & Integer'Image(num));
                  end if;
               when mmr0_sr0 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" MMR0/SR0");
                  end if;
                  ret_val := data_bus(mmr0_to_word(self.mmr0));
                  status := BUS_SUCC;
               when mmr1_sr1 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" MMR1/SR1");
                  end if;
                  ret_val := data_bus(self.mmr1);
                  status := BUS_SUCC;
               when mmr2_sr2 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" MMR2/SR2");
                  end if;
                  ret_val := data_bus(self.mmr2);
                  status := BUS_SUCC;
               when others =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put(" Unknown register");
                  end if;
            end case;
         when others =>
            ret_val := 0;
            status := BUS_NONE;
      end case;
      if self.host.trace.io or debug then
         Ada.Text_IO.Put_Line(", value " & toOct(ret_val));
      end if;
      return ret_val;
   end;
   --
   procedure write(self : in out kt11; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat) is
      config : constant BBS.Sim_CPU.cpu.pdp11.features := BBS.Sim_CPU.cpu.pdp11.pdp11_access(self.host).getConfig;
      num    : Integer;
      bvalue : constant byte := byte(data and 16#FF#);
      wvalue : constant word := word(data and 16#FFFF#);
      mask18 : constant word := 16#7F4E#;
   begin
      status := BUS_NONE;
      case size is
         when bits8 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("KT11: Writing byte " & toOct(byte(data and 16#FF#)) & " to address " & toOct(addr));
            end if;
            case addr is
               when sid_pdr_start .. sid_pdr_end + 1 =>
                  num := Integer(addr - sid_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Supervisor I/D PDR" & Integer'Image(num));
                  end if;
               when sd_pdr_start .. sd_pdr_end + 1 =>
                  num := Integer(addr - sd_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Supervisor I PDR" & Integer'Image(num));
                  end if;
               when sid_par_start .. sid_par_end + 1 =>
                  num := Integer(addr - sid_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Supervisor I/D PAR" & Integer'Image(num));
                  end if;
               when sd_par_start .. sd_par_end + 1 =>
                  num := Integer(addr - sd_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Supervisor I PAR" & Integer'Image(num));
                  end if;
               when kid_pdr_start .. kid_pdr_end + 1 =>
                  num := Integer(addr - kid_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Kernel I/D PDR" & Integer'Image(num));
                  end if;
               when kd_pdr_start .. kd_pdr_end + 1 =>
                  num := Integer(addr - kd_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Kernel I PDR" & Integer'Image(num));
                  end if;
               when kid_par_start .. kid_par_end + 1 =>
                  num := Integer(addr - kid_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Kernel I/D PAR" & Integer'Image(num));
                  end if;
               when kd_par_start .. kd_par_end + 1 =>
                  num := Integer(addr - kd_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Kernel I PAR" & Integer'Image(num));
                  end if;
               when uid_pdr_start .. uid_pdr_end + 1 =>
                  num := Integer(addr - uid_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *User I/D PDR" & Integer'Image(num));
                  end if;
               when ud_pdr_start .. ud_pdr_end + 1 =>
                  num := Integer(addr - ud_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *User I PDR" & Integer'Image(num));
                  end if;
               when uid_par_start .. uid_par_end + 1 =>
                  num := Integer(addr - uid_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *User I/D PAR" & Integer'Image(num));
                  end if;
               when ud_par_start .. ud_par_end + 1 =>
                  num := Integer(addr - ud_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *User I PAR" & Integer'Image(num));
                  end if;
               when mmr0_sr0 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" MMR0/SR0 lsb");
                  end if;
                  self.mmr0 := word_to_mmr0((mmr0_to_word(self.mmr0) and 16#FF00#) or (wvalue and 16#FF#));
                  status := BUS_SUCC;
               when mmr0_sr0 + 1 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" MMR0/SR0 msb");
                  end if;
                  self.mmr0 := word_to_mmr0((mmr0_to_word(self.mmr0) and 16#FF#) or (wvalue and 16#FF00#));
                  status := BUS_SUCC;
               when mmr1_sr1 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" MMR1/SR1 lsb");
                  end if;
                  self.mmr1 := (self.mmr1 and 16#FF00#) or (wvalue and 16#FF#);
                  status := BUS_SUCC;
               when mmr1_sr1 + 1 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" MMR1/SR1 msb");
                  end if;
                  self.mmr1 := (self.mmr1 and 16#FF#) or (wvalue and 16#FF00#);
                  status := BUS_SUCC;
               when mmr2_sr2 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" MMR2/SR2 lsb");
                  end if;
                  self.mmr2 := (self.mmr2 and 16#FF00#) or (wvalue and 16#FF#);
                  status := BUS_SUCC;
               when mmr2_sr2 + 1 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" MMR2/SR2 msb");
                  end if;
                  self.mmr2 := (self.mmr2 and 16#FF#) or (wvalue and 16#FF00#);
                  status := BUS_SUCC;
               when others =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Unknown register");
                  end if;
            end case;
         when bits16 =>
            if self.host.trace.io or debug then
               Ada.Text_IO.Put("KT11: Writing word " & toOct(word(data and 16#FFFF#)) & " to address " & toOct(addr));
            end if;
            case addr is
               when sid_pdr_start .. sid_pdr_end =>
                  num := Integer(addr - sid_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Supervisor I/D PDR" & Integer'Image(num));
                  end if;
               when sd_pdr_start .. sd_pdr_end =>
                  num := Integer(addr - sd_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Supervisor I PDR" & Integer'Image(num));
                  end if;
               when sid_par_start .. sid_par_end =>
                  num := Integer(addr - sid_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Supervisor I/D PAR" & Integer'Image(num));
                  end if;
               when sd_par_start .. sd_par_end =>
                  num := Integer(addr - sd_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *Supervisor I PAR" & Integer'Image(num));
                  end if;
               when kid_pdr_start .. kid_pdr_end =>
                  num := Integer(addr - kid_pdr_start)/2;
                  if config.has_mmu22 then
                     self.kid_pdr(num) := word_to_pdr(wvalue);
                  else
                     self.kid_pdr(num) := word_to_pdr(wvalue and mask18);
                  end if;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" Kernel I/D PDR" & Integer'Image(num));
                  end if;
                  status := BUS_SUCC;
               when kd_pdr_start .. kd_pdr_end =>
                  num := Integer(addr - kd_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" Kernel I PDR" & Integer'Image(num));
                  end if;
                  if config.has_ID then
                     self.kd_pdr(num) := word_to_pdr(wvalue);
                     status := BUS_SUCC;
                  end if;
               when kid_par_start .. kid_par_end =>
                  num := Integer(addr - kid_par_start)/2;
                  self.kid_par(num) := wvalue;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" Kernel I/D PAR" & Integer'Image(num));
                  end if;
                  status := BUS_SUCC;
               when kd_par_start .. kd_par_end =>
                  num := Integer(addr - kd_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" Kernel I PAR" & Integer'Image(num));
                  end if;
                  if config.has_ID then
                     self.kd_par(num) := wvalue;
                     status := BUS_SUCC;
                  end if;
               when uid_pdr_start .. uid_pdr_end =>
                  num := Integer(addr - uid_pdr_start)/2;
                  if config.has_user then
                     if config.has_mmu22 then
                        self.uid_pdr(num) := word_to_pdr(wvalue);
                     else
                        self.uid_pdr(num) := word_to_pdr(wvalue and mask18);
                     end if;
                     status := BUS_SUCC;
                  else
                     Ada.Text_IO.Put_Line(" disabled User I/D PDR" & Integer'Image(num));
                  end if;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" User I/D PDR" & Integer'Image(num));
                  end if;
               when ud_pdr_start .. ud_pdr_end =>
                  num := Integer(addr - ud_pdr_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" User I PDR" & Integer'Image(num));
                  end if;
                  if config.has_user and config.has_ID then
                     self.ud_pdr(num) := word_to_pdr(wvalue);
                     status := BUS_SUCC;
                  end if;
               when uid_par_start .. uid_par_end =>
                  num := Integer(addr - uid_par_start)/2;
                  if config.has_user then
                     self.uid_par(num) := wvalue;
                     status := BUS_SUCC;
                  else
                     Ada.Text_IO.Put_Line(" disabled User I/D PAR" & Integer'Image(num));
                  end if;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" User I/D PAR" & Integer'Image(num));
                  end if;
               when ud_par_start .. ud_par_end =>
                  num := Integer(addr - ud_par_start)/2;
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" User I PAR" & Integer'Image(num));
                  end if;
                  if config.has_user and config.has_ID then
                     self.ud_par(num) := wvalue;
                     status := BUS_SUCC;
                  end if;
               when mmr0_sr0 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" MMR0/SR0");
                  end if;
                  self.mmr0 := word_to_mmr0(wvalue);
                  status := BUS_SUCC;
               when mmr1_sr1 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MMR1/SR1");
                  end if;
                  self.mmr1 := wvalue;
                  status := BUS_SUCC;
               when mmr2_sr2 =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" *MMR2/SR2");
                  end if;
                  self.mmr2 := wvalue;
                  status := BUS_SUCC;
               when others =>
                  if self.host.trace.io or debug then
                     Ada.Text_IO.Put_Line(" Unknown register");
                  end if;
            end case;
         when others =>
            status := BUS_NONE;
      end case;
   end;
   --
   --  Perform address translation for logical bus access
   --
   --  Access control field is 3 bits of which the upper two are used by the KT11-D
   --  00x - Non resident
   --  01x - Read only
   --  10x - Illegal
   --  11x - Read/Write
   --
   --  Add checks for page length and for growing up/down
   --
   function translate(self : in out kt11; addr : addr_bus; mode : proc_mode;
                      addr_kind : addr_type; rw : Boolean) return addr_bus is
      config : constant BBS.Sim_CPU.cpu.pdp11.features := BBS.Sim_CPU.cpu.pdp11.pdp11_access(self.host).getConfig;
      num    : Integer;
      cpar   : addr_bus;
      cpdr   : pdr;
      taddr  : addr_bus;
   begin
      if (not self.mmr0.enable) or (self.mmr0.maint and not rw) then
         if addr < base_io_start then
            return addr;
         elsif addr <= base_io_end then
            return addr + (ub_io_start - base_io_start);
         else
            Ada.Text_IO.Put_Line("MMU: Address out of range for 16 bit mode.");
            return bad_addr;
         end if;
      else
         num   := Integer((addr and 16#E000#)/16#1000#)/2;
         taddr := addr and 16#1FFF#;
         case mode is
            when PROC_KERN =>
               cpdr := self.kid_pdr(num);
               if self.reloc_valid(cpdr, addr, rw) then
                  if config.has_MMU22 then
                     cpar := addr_bus(self.kid_par(num));
                  else
                     cpar := addr_bus(self.kid_par(num) and 16#0FFF#);
                  end if;
                  taddr := taddr + cpar*16#40#;
               else
                  taddr := bad_addr;
               end if;
               return taddr;
            when PROC_SUP =>
               if config.has_super then
                  null;
               else
                  Ada.Text_IO.Put_Line("MMU: Supervisor processor mode not enabled.");
                  return bad_addr;
               end if;
            when PROC_USER =>
               if config.has_user then
                  cpdr := self.uid_pdr(num);
                  if self.reloc_valid(cpdr, addr, rw) then
                     if config.has_MMU22 then
                        cpar := addr_bus(self.uid_par(num));
                     else
                        cpar := addr_bus(self.uid_par(num) and 16#0FFF#);
                     end if;
                     taddr := taddr + cpar*16#40#;
                  else
                     taddr := bad_addr;
                  end if;
                  return taddr;
               else
                  Ada.Text_IO.Put_Line("MMU: Supervisor processor mode not enabled.");
                  return bad_addr;
               end if;
            when others =>
               Ada.Text_IO.Put_Line("MMU: Unused processor mode not supported.");
               return bad_addr;
         end case;
         Ada.Text_IO.Put_Line("MMU: Enabling is not yet supported");
         return bad_addr;
      end if;
   end;
   --
   --  Check to see if relocation is valid.  If not, set the appropriate bits
   --  in MMR0.
   --
   function reloc_valid(self : in out kt11; cpdr : in out pdr; addr : addr_bus;
                        rw : Boolean) return Boolean is
      block : constant uint8 := uint8((addr and 16#1FC0#)/16#40#);
      lenf  : Boolean := (not cpdr.ed and (block <= uint8(cpdr.plf)+1)) or
        (cpdr.ed and (block >= uint8(8#177# - cpdr.plf)-1));
   begin
      if cpdr.plf = 0 then
         lenf := True;
      end if;
      if not lenf then
         Ada.Text_IO.Put_Line("MMU: Page length failure: addr " & toOct(addr) &
                                ", block " & toOct(word(block)) & ", PLF " &
                                toOct(byte(cpdr.plf)) & ", expansion " & Boolean'Image(cpdr.ed));
      end if;
      if ((cpdr.acf = 6) or ((cpdr.acf = 2) and not rw)) and lenf then
         if rw then
            cpdr.w := True;
         end if;
         self.mmr0.tabsent := False;
         self.mmr0.tlength := False;
         self.mmr0.tread   := False;
         return True;
      end if;
      self.mmr0.page    := uint3((addr and 16#E000#)/16#2000#);
      self.mmr0.tabsent := (cpdr.acf = 0) or (cpdr.acf = 1);
      self.mmr0.tlength := not lenf;
      self.mmr0.tread   := (cpdr.acf = 2) or (cpdr.acf = 3);
      Ada.Text_IO.Put_Line("KT11: Relocation failed MMR0: " & toOct(MMR0_to_word(self.mmr0)));
      Ada.Text_IO.Put_Line("KT11: Relocation failed PDR: " & toOct(PDR_to_word(cpdr)));
      for i in 0 .. 7 loop
         Ada.Text_IO.Put_Line("KT11: User page " & Integer'Image(i) & " PDR is " & toOct(PDR_to_word(self.uid_pdr(i))) &
                                " PAR " & Integer'Image(i) & " is " & toOct(self.uid_par(i)));
      end loop;
      for i in 0 .. 7 loop
         Ada.Text_IO.Put_Line("KT11: Kernel page " & Integer'Image(i) & " PDR is " & toOct(PDR_to_word(self.kid_pdr(i))) &
                                " PAR " & Integer'Image(i) & " is " & toOct(self.kid_par(i)));
      end loop;
      self.host.interrupt(self.vector);
      return False;
   end;
end;
