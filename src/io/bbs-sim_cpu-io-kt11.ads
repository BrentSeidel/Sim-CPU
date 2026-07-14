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
--  KT11 memory manager for for PDP-11.  Possibly includes configurable support
--  for 22 bit addressing, supervisor mode, and separate I/D space.
--
package BBS.Sim_CPU.io.kt11 is
   --
   --  Address map for KT11.  This is, I believe the full set for all PDP-11
   --  MMU options.  If the model does not have supervisor mode, the super PDR/PARs
   --  are not used.  If there is no separate I/D space, only the I/D PDR/PARs
   --  are used.  MMR1 is only used in some models (probably depending on 22-bit
   --  addressing, supervisor space, and separate I and D space).
   --
   --  770200-372 or 770200-376 Unibus map*
   --
   --  772200 - Super I/D PDR 0*
   --  772202 - Super I/D PDR 1*
   --  772204 - Super I/D PDR 2*
   --  772206 - Super I/D PDR 3*
   --  772210 - Super I/D PDR 4*
   --  772212 - Super I/D PDR 5*
   --  772214 - Super I/D PDR 6*
   --  772216 - Super I/D PDR 7*
   --
   --  772220 - Super D PDR 0*
   --  772222 - Super D PDR 1*
   --  772224 - Super D PDR 2*
   --  772226 - Super D PDR 3*
   --  772230 - Super D PDR 4*
   --  772232 - Super D PDR 5*
   --  772234 - Super D PDR 6*
   --  772236 - Super D PDR 7*
   --
   --  772240 - Super I/D PAR 0
   --  772242 - Super I/D PAR 1
   --  772243 - Super I/D PAR 2
   --  772246 - Super I/D PAR 3
   --  772250 - Super I/D PAR 4
   --  772252 - Super I/D PAR 5
   --  772253 - Super I/D PAR 6
   --  772256 - Super I/D PAR 7
   --
   --  772260 - Super D PAR 0*
   --  772262 - Super D PAR 1*
   --  772263 - Super D PAR 2*
   --  772266 - Super D PAR 3*
   --  772270 - Super D PAR 4*
   --  772272 - Super D PAR 5*
   --  772273 - Super D PAR 6*
   --  772276 - Super D PAR 7*
   --
   --  772300 - Kernel I/D PDR 0
   --  772302 - Kernel I/D PDR 1
   --  772304 - Kernel I/D PDR 2
   --  772306 - Kernel I/D PDR 3
   --  772310 - Kernel I/D PDR 4
   --  772312 - Kernel I/D PDR 5
   --  772314 - Kernel I/D PDR 6
   --  772316 - Kernel I/D PDR 7
   --
   --  772320 - Kernel D PDR 0*
   --  772322 - Kernel D PDR 1*
   --  772324 - Kernel D PDR 2*
   --  772326 - Kernel D PDR 3*
   --  772330 - Kernel D PDR 4*
   --  772332 - Kernel D PDR 5*
   --  772334 - Kernel D PDR 6*
   --  772336 - Kernel D PDR 7(
   --
   --  772340 - Kernel I/D PAR 0
   --  772342 - Kernel I/D PAR 1
   --  772343 - Kernel I/D PAR 2
   --  772346 - Kernel I/D PAR 3
   --  772350 - Kernel I/D PAR 4
   --  772352 - Kernel I/D PAR 5
   --  772353 - Kernel I/D PAR 6
   --  772356 - Kernel I/D PAR 7
   --
   --  772360 - Kernel D PAR 0*
   --  772362 - Kernel D PAR 1*
   --  772363 - Kernel D PAR 2*
   --  772366 - Kernel D PAR 3*
   --  772370 - Kernel D PAR 4*
   --  772372 - Kernel D PAR 5*
   --  772373 - Kernel D PAR 6*
   --  772376 - Kernel D PAR 7*
   --
   --  772516 - MMR3*
   --
   --  777572 - MMR0 (SR0)
   --  777574 - MMR1 (SR1)*
   --  777576 - MMR2 (SR2)
   --
   --  777600 - User I/D PDR 0
   --  777602 - User I/D PDR 1
   --  777604 - User I/D PDR 2
   --  777606 - User I/D PDR 3
   --  777610 - User I/D PDR 4
   --  777612 - User I/D PDR 5
   --  777614 - User I/D PDR 6
   --  777616 - User I/D PDR 7
   --
   --  777620 - User D PDR 0*
   --  777622 - User D PDR 1*
   --  777624 - User D PDR 2*
   --  777626 - User D PDR 3*
   --  777630 - User D PDR 4*
   --  777632 - User D PDR 5*
   --  777634 - User D PDR 6*
   --  777636 - User D PDR 7*
   --
   --  777640 - User I/D PAR 0
   --  777642 - User I/D PAR 1
   --  777644 - User I/D PAR 2
   --  777646 - User I/D PAR 3
   --  777650 - User I/D PAR 4
   --  777652 - User I/D PAR 5
   --  777654 - User I/D PAR 6
   --  777656 - User I/D PAR 7
   --
   --  777660 - User D PAR 0*
   --  777662 - User D PAR 1*
   --  777664 - User D PAR 2*
   --  777666 - User D PAR 3*
   --  777670 - User D PAR 4*
   --  777672 - User D PAR 5*
   --  777674 - User D PAR 6*
   --  777676 - User D PAR 7*
   --
   --  Note that these addresses are not contiguous.
   --
   --  *Registers not in basic KT11-D
   --
   --  The I/O device object for simulated MMU
   --
   type kt11 is new BBS.Sim_CPU.io.io_device with private;
   type kt11_access is access all kt11'Class;
   --
   --  ----------------------------------------------------------------------
   --  I/O device actions
   --
   procedure write(self : in out kt11; addr : addr_bus; data : data_bus; size : bus_size; status : in out bus_stat);
   function read(self : in out kt11; addr : addr_bus; size : bus_size; status : in out bus_stat) return data_bus;
   function getSize(self : in out kt11) return addr_bus is (0);
   function name(self : in out kt11) return String is ("KT11");
   function description(self : in out kt11) return String is ("KT11 Memory Management Unit");
   procedure setException(self : in out kt11; except : long);
   function dev_class(self : in out kt11) return dev_type is (MM);
   procedure reset(self : in out kt11);
   procedure shutdown(self : in out kt11) is null;
   --
   --  MMU action to translate addresses
   --
   --  Perform address translation for logical bus access.
   --  mode is the processor mode
   --  addr_kind is address type
   --  rw is False for read and True for write
   --
   function translate(self : in out kt11; addr : addr_bus; mode : proc_mode;
                 addr_kind : addr_type; rw : Boolean) return addr_bus;
   --
private
   --
   --  Enable/Disable debugging message for this device specifically
   --
   debug : constant Boolean := True;
   --
   --  Register definitions
   --  The definitions here are for the full MMU set which cover 22 bit addresses,
   --  supervisor mode, and separate I/D space.  Some MMUs only use a subset of
   --  these definitions.
   --
   --  Page address register is just a word, bits 0-11 are used for 18-bit addressing
   --  and all bits are used for 22-bit addressing
   --
   --  Page descriptor register
   --
   type pdr is record
      acf : uint3;    --  Access control field (bit 0 not used in KT11-D)
      ed  : Boolean;  --  Expansion direction
      u0  : uint2;    --  unused
      w   : Boolean;  --  Page has been written to (dirty)
      a   : Boolean;  --  Cause an access trap if requested by ACF*
      plf : uint7;    --  Page length field
      u1  : Boolean;  --  unused
   end record;
   for pdr use record
      acf at 0 range  0 ..  2;
      ed  at 0 range  3 ..  3;
      u0  at 0 range  4 ..  5;
      w   at 0 range  6 ..  6;
      a   at 0 range  7 ..  7;
      plf at 0 range  8 .. 14;
      u1  at 0 range 15 .. 15;
   end record;
   --
   --  Status Register 0 (SR0)/Memory Management Register 0 (MMR0)
   --
   type mmr0_type is record
      enable   : Boolean;  --  Enable relocation
      page     : uint3;    --  Page number
      i_d      : Boolean;  --  Page Address Space I/D*
      mode     : uint2;    --  Page Mode
      complete : Boolean;  --  Instruction complete*
      maint    : Boolean;  --  Maintenance mode
      etrap    : Boolean;  --  Enable memory management trap*
      unused   : uint2;    --  Unused
      mtrap    : Boolean;  --  Memory management trap occured*
      tread    : Boolean;  --  Read only abort
      tlength  : Boolean;  --  Page length abort
      tabsent  : Boolean;  --  Non-resident abort
   end record;
   for mmr0_type use record
      enable   at 0 range  0 ..  0;
      page     at 0 range  1 ..  3;
      i_d      at 0 range  4 ..  4;
      mode     at 0 range  5 ..  6;
      complete at 0 range  7 ..  7;
      maint    at 0 range  8 ..  8;
      etrap    at 0 range  9 ..  9;
      unused   at 0 range 10 .. 11;
      mtrap    at 0 range 12 .. 12;
      tread    at 0 range 13 .. 13;
      tlength  at 0 range 14 .. 14;
      tabsent  at 0 range 15 .. 15;
   end record;
   for mmr0_type'size use 16;
   --
   type pdrs is array (0 .. 7) of pdr;
   type pars is array (0 .. 7) of word;
   type kt11 is new BBS.Sim_CPU.io.io_device with record
      sid_pdr : pdrs;  --  Supervisor I and I/D PDRs
      sd_pdr  : pdrs;  --  Supervisor D PDRs
      sid_par : pars;  --  Supervisor I and I/D PARs
      sd_par  : pars;  --  Supervisor D PARs
      kid_pdr : pdrs;  --  Kernel I and I/D PDRs
      kd_pdr  : pdrs;  --  Kernel D PDRs
      kid_par : pars;  --  Kernel I and I/D PARs
      kd_par  : pars;  --  Kernel D PARs
      uid_pdr : pdrs;  --  User I and I/D PDRs
      ud_pdr  : pdrs;  --  User D PDRs
      uid_par : pars;  --  User I and I/D PARs
      ud_par  : pars;  --  User D PARs
      mmr0    : mmr0_type;  --  Memory Management Register 0
      mmr1    : word;  --  Memory Management Register 1
      mmr2    : word;  --  Memory Management Register 2
      vector  : long;  --  Exception vector
   end record;
   --
   --  Constants for I/O page
   --
   base_io_start : constant addr_bus := 8#160_000#;  --  Start of I/O page in unmapped CPU address
   base_io_end   : constant addr_bus := 8#177_777#;  --  End of I/O page in unmapped CPU address
   ub_io_start   : constant addr_bus := 8#760_000#;  --  Start of Unibus I/O page
   ub_io_end     : constant addr_bus := 8#777_777#;  --  End of Unibus I/O page (and max Unibus address)
   bad_addr      : constant addr_bus := 16#FFFF_FFFF#;  --  Out of range address to indicate errors
   --
   --  Constants for PARs/PDRs and other registers
   --
   sid_pdr_start : constant addr_bus := 8#772200#;
   sid_pdr_end   : constant addr_bus := 8#772216#;
   --
   sd_pdr_start  : constant addr_bus := 8#772220#;
   sd_pdr_end    : constant addr_bus := 8#772236#;
   --
   sid_par_start : constant addr_bus := 8#772240#;
   sid_par_end   : constant addr_bus := 8#772256#;
   --
   sd_par_start  : constant addr_bus := 8#772260#;
   sd_par_end    : constant addr_bus := 8#772276#;
   --
   kid_pdr_start : constant addr_bus := 8#772300#;
   kid_pdr_end   : constant addr_bus := 8#772316#;
   --
   kd_pdr_start  : constant addr_bus := 8#772320#;
   kd_pdr_end    : constant addr_bus := 8#772336#;
   --
   kid_par_start : constant addr_bus := 8#772340#;
   kid_par_end   : constant addr_bus := 8#772356#;
   --
   kd_par_start  : constant addr_bus := 8#772360#;
   kd_par_end    : constant addr_bus := 8#772376#;
   --
   uid_pdr_start : constant addr_bus := 8#777600#;
   uid_pdr_end   : constant addr_bus := 8#777616#;
   --
   ud_pdr_start  : constant addr_bus := 8#777620#;
   ud_pdr_end    : constant addr_bus := 8#777636#;
   --
   uid_par_start : constant addr_bus := 8#777640#;
   uid_par_end   : constant addr_bus := 8#777656#;
   --
   ud_par_start  : constant addr_bus := 8#777660#;
   ud_par_end    : constant addr_bus := 8#777676#;
   --
   mmr0_sr0      : constant addr_bus := 8#777572#;
   mmr1_sr1      : constant addr_bus := 8#777574#;
   mmr2_sr2      : constant addr_bus := 8#777576#;
   --
   --  Functions/procedures for relocating pages
   --
   --  Check to see if relocation is valid.  If not, set the appropriate bits
   --  in MMR0.
   --
   function reloc_valid(self : in out kt11; cpdr : in out pdr; addr : addr_bus;
                       rw : Boolean) return Boolean;
end;
