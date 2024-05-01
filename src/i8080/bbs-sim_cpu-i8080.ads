package BBS.Sim_CPU.i8080 is
   --
   --  The simple Intel 8080 simulator inheriting from Sim.simulator.
   --
   type i8080 is new simulator with private;
   --
   memory_size : constant word := 2**16;
   --
   --  The trace level is interpreted as follows for this simulator:
   --  Bit  Use
   --   0   List instructions being traced
   --   1   List I/O operations
   --   2   Unused
   --   3   Unused
   --   4   Unused
   --   5   Unused
   --   6   Unused
   --   7   Unused
   --
   --  Variants of processor
   --
   type variants_i8080 is (var_8080,
                           var_8085,
                           var_z80);
   --
   --  ----------------------------------------------------------------------
   --  Simulator control
   --
   --  Called first to initialize the simulator
   --
   overriding
   procedure init(self : in out i8080);
   --
   --  Called once when Start/Stop switch is moved to start position
   --
   overriding
   procedure start(self : in out i8080);
   --
   --  Called to start simulator execution at a specific address.
   --
   overriding
   procedure start(self : in out i8080; addr : addr_bus);
   --
   --  Called once per frame when start/stop is in the start position and run/pause
   --  is in the run position.
   --
   overriding
   procedure run(self : in out i8080);
   --
   --  Called once when the Deposit switch is moved to the Deposit position.
   --
   overriding
   procedure deposit(self : in out i8080);
   --
   --  Called once when the Examine switch is moved to the Examine position.
   --
   overriding
   procedure examine(self : in out i8080);
   --
   --  This loads data from a file specified by "name" into the simulator memory.
   --
   overriding
   procedure load(self : in out i8080; name : String);
   --
   --  Called to attach an I/O device to a simulator at a specific address.  Bus
   --  is simulator dependent as some CPUs have separate I/O and memory space.
   --  For bus:
   --    0 - I/O space
   --    1 - Memory space (currently unimplemented)
   --
   overriding
   procedure attach_io(self : in out i8080; io_dev : io_access;
                       base_addr : addr_bus; bus : bus_type);
   --
   --  ----------------------------------------------------------------------
   --  Simulator information
   --
   --  Called to get simulator name
   --
   overriding
   function name(self : in out i8080) return String is ("i8080");
   --
   --  Called to get simulator memory size
   --
   overriding
   function mem_size(self : in out i8080) return addr_bus is (uint32(memory_size));
   --
   --  Called to get number of registers
   --
   overriding
   function registers(self : in out i8080) return uint32;
   --
   --  Called to get number of variants
   --
   overriding
   function variants(self : in out i8080) return Natural is (3);
   --
   --  Called to get variant name
   --
   overriding
   function variant(self : in out i8080; v : natural) return String is
        (variants_i8080'Image(variants_i8080'Val(v)));
   --
   --  Called to get current variant index
   --
   overriding
   function variant(self : in out i8080) return Natural;
   --
   --  Called to set variant
   --
   overriding
   procedure variant(self : in out i8080; v : natural);
   --
   --  Interrupt status.  Returns simulator dependent status of interrupts
   --
   overriding
   function intStatus(self : in out i8080) return int32;
   --
   --  Input/Output debugging
   --
   overriding
   function lastOutAddr(self : in out i8080) return addr_bus;
   overriding
   function lastOutData(self : in out i8080) return data_bus;
   overriding
   procedure overrideIn(self : in out i8080; addr : in addr_bus; data : in data_bus);
   --
   --  ----------------------------------------------------------------------
   --  Simulator data
   --
   --  Called to set a memory value
   --
   overriding
   procedure set_mem(self : in out i8080; mem_addr : addr_bus;
                     data : data_bus);
   --
   --  Called to read a memory value
   --
   overriding
   function read_mem(self : in out i8080; mem_addr : addr_bus) return
     data_bus;
   --
   --  Called to get register name
   --
   overriding
   function reg_name(self : in out i8080; num : uint32)
                     return String;
   --
   --  Called to get register value as a number
   --
   overriding
   function read_reg(self : in out i8080; num : uint32)
                     return data_bus;
   --
   --  Called to get register value as a string (useful for flag registers)
   --
   overriding
   function read_reg(self : in out i8080; num : uint32)
                     return String;
   --
   --  Called to set register value
   --
   overriding
   procedure set_reg(self : in out i8080; num : uint32;
                     data : data_bus) is null;
   --
   --  Called to check if the CPU is halted
   --
   overriding
   function halted(self : in out i8080) return Boolean;
   --
   --  This clears the halted flag allowing processing to continue.
   --
   overriding
   procedure continue_proc(self : in out i8080);
   --
   --  Set and clear breakpoints.  The implementation is up to the specific simulator.
   --
   procedure setBreak(self : in out i8080; addr : addr_bus);
   procedure clearBreak(self : in out i8080; addr : addr_bus);
   --
   --  Unimplemented instruction response
   --
   procedure unimplemented(self : in out i8080; addr : word; data : byte);

private
   --
   type reg_id is (reg_a,    --  Accumulator (8 bits)
                   reg_psw,  --  Status word
                   reg_b,    --  B register (8 bits)
                   reg_c,    --  C register (8 bits)
                   reg_bc,   --  B & C registers (16 bits)
                   reg_d,    --  D register (8 bits)
                   reg_e,    --  E register (8 bits)
                   reg_de,   --  D & E registers (16 bits)
                   reg_h,    --  H register (8 bits)
                   reg_l,    --  L register (8 bits)
                   reg_hl,   --  H & L register (16 bits)
                   reg_sp,   --  Stack pointer (16 bits)
                   reg_pc);  --  Program counter (16 bits)
   --
   type status_word is record
      carry   : Boolean := False;
      unused0 : Boolean := True;
      parity  : Boolean := False;
      unused1 : Boolean := False;
      aux_carry : Boolean := False;
      unused2 : Boolean := False;
      zero    : Boolean := False;
      sign    : Boolean := False;
   end record;
   --
   for status_word use record
      carry   at 0 range 0 .. 0;
      unused0 at 0 range 1 .. 1;
      parity  at 0 range 2 .. 2;
      unused1 at 0 range 3 .. 3;
      aux_carry at 0 range 4 .. 4;
      unused2 at 0 range 5 .. 5;
      zero    at 0 range 6 .. 6;
      sign    at 0 range 7 .. 7;
   end record;
   --
   for status_word'Size use 8;
   --
   type mem_array is array (0 .. memory_size - 1) of byte;
   type io_array is array (byte'Range) of io_access;
   --
   type i8080 is new simulator with record
      addr : word := 0;
      temp_addr : word := 0;
      a  : byte := 0;
      psw : status_word;
      b  : byte := 0;
      c  : byte := 0;
      d  : byte := 0;
      e  : byte := 0;
      h  : byte := 0;
      l  : byte := 0;
      sp : word := 0;
      pc : word := 0;
      mem : mem_array := (others => 0);
      io_ports     : io_array := (others => null);
      intr         : Boolean := False;
      cpu_halt     : Boolean := False;
      int_enable   : Boolean := False;
      break_enable : Boolean := False;
      break_point  : word;
      last_out_addr : addr_bus := 0;
      last_out_data : data_bus := 0;
      in_override  : Boolean := False;
      in_over_addr : addr_bus := 0;
      in_over_data : data_bus := 0;
      cpu_model    : variants_i8080 := var_8080;
   end record;
   --
   subtype reg8_index is byte range 0 .. 7;
   subtype reg16_index is byte range 0 .. 3;
   --
   --  Code for the instruction processing.
   --
   procedure decode(self : in out i8080);
   function get_next(self : in out i8080) return byte;
   procedure check_intr(self : in out i8080) is null;
   --
   --
   procedure reg8(self : in out i8080; reg : reg8_index; value : byte);
   function reg8(self : in out i8080; reg : reg8_index) return byte;
   procedure mod8(self  : in out i8080; reg : reg8_index; dir : Integer);
   --
   --  LXI and PUSH/POP have different reg16 indices.  V = 0 selects the LXI
   --  version and V = 1 selects the PUSH/POP version.
   --
   procedure reg16(self : in out i8080; reg : reg16_index; value : word; v : Natural);
   function reg16(self : in out i8080; reg : reg16_index; v : Natural) return word;
   procedure setf(self : in out i8080; value : byte);
   function addf(self : in out i8080; v1 : byte; v2 : byte; c : Boolean) return byte;
   function subf(self : in out i8080; v1 : byte; v2 : byte; c : Boolean) return byte;
   function dad(self  : in out i8080; v1 : word; v2 : word) return word;
   procedure mod16(self  : in out i8080; reg : reg16_index; dir : Integer);
   --
   --  All memory accesses should be routed through these functions so that they
   --  can do checks for memory-mapped I/O or shared memory.
   --
   procedure memory(self : in out i8080; addr : word; value : byte; mode : addr_type);
   function memory(self : in out i8080; addr : word; mode : addr_type) return byte;
   --
   --  Handle I/O port accesses
   --
   procedure port(self : in out i8080; addr : byte; value : byte);
   function port(self : in out i8080; addr : byte) return byte;
   --
   --  Common code for Jump, Call, and Return
   --
   procedure jump(self : in out i8080; go : Boolean);
   procedure call(self : in out i8080; go : Boolean);
   procedure ret(self : in out i8080; go : Boolean);
   --
   --  Op code type
   --
   type opcode is (OP_NOP,     OP_LXI_B,   OP_STAX_B,  OP_INX_B,   OP_INR_B,   OP_DCR_B,   OP_MVI_B,   OP_RLC,
                   OP_08,      OP_DAD_B,   OP_LDAX_B,  OP_DCX_B,   OP_INR_C,   OP_DCR_C,   OP_MVI_C,   OP_RRC,
                   OP_10,      OP_LXI_D,   OP_STAX_D,  OP_INX_D,   OP_INR_D,   OP_DCR_D,   OP_MVI_D,   OP_RAL,
                   OP_18,      OP_DAD_D,   OP_LDAX_D,  OP_DCX_D,   OP_INR_E,   OP_DCR_E,   OP_MVI_E,   OP_RAR,
                   RIM,        OP_LXI_H,   OP_SHLD,    OP_INX_H,   OP_INR_H,   OP_DCR_H,   OP_MVI_H,   OP_DAA,
                   OP_28,      OP_DAD_H,   OP_LHLD,    OP_DCX_H,   OP_INR_L,   OP_DCR_L,   OP_MVI_L,   OP_CMA,
                   SIM,        OP_LXI_SP , OP_STA,     OP_INX_SP,  OP_INR_M,   OP_DCR_M,   OP_MVI_M,   OP_STC,
                   OP_38 ,     OP_DAD_SP,  OP_LDA,     OP_DCX_SP,  OP_INR_A,   OP_DCR_A,   OP_MVI_A,   OP_CMC,
                   OP_MOV_B_B, OP_MOV_B_C, OP_MOV_B_D, OP_MOV_B_E, OP_MOV_B_H, OP_MOV_B_L, OP_MOV_B_M, OP_MOV_B_A,
                   OP_MOV_C_B, OP_MOV_C_C, OP_MOV_C_D, OP_MOV_C_E, OP_MOV_C_H, OP_MOV_C_L, OP_MOV_C_M, OP_MOV_C_A,
                   OP_MOV_D_B, OP_MOV_D_C, OP_MOV_D_D, OP_MOV_D_E, OP_MOV_D_H, OP_MOV_D_L, OP_MOV_D_M, OP_MOV_D_A,
                   OP_MOV_E_B, OP_MOV_E_C, OP_MOV_E_D, OP_MOV_E_E, OP_MOV_E_H, OP_MOV_E_L, OP_MOV_E_M, OP_MOV_E_A,
                   OP_MOV_H_B, OP_MOV_H_C, OP_MOV_H_D, OP_MOV_H_E, OP_MOV_H_H, OP_MOV_H_L, OP_MOV_H_M, OP_MOV_H_A,
                   OP_MOV_L_B, OP_MOV_L_C, OP_MOV_L_D, OP_MOV_L_E, OP_MOV_L_H, OP_MOV_L_L, OP_MOV_L_M, OP_MOV_L_A,
                   OP_MOV_M_B, OP_MOV_M_C, OP_MOV_M_D, OP_MOV_M_E, OP_MOV_M_H, OP_MOV_M_L, OP_HLT,     OP_MOV_M_A,
                   OP_MOV_A_B, OP_MOV_A_C, OP_MOV_A_D, OP_MOV_A_E, OP_MOV_A_H, OP_MOV_A_L, OP_MOV_A_M, OP_MOV_A_A,
                   OP_ADD_B,   OP_ADD_C,   OP_ADD_D,   OP_ADD_E,   OP_ADD_H,   OP_ADD_L,   OP_ADD_M,   OP_ADD_A,
                   OP_ADC_B,   OP_ADC_C,   OP_ADC_D,   OP_ADC_E,   OP_ADC_H,   OP_ADC_L,   OP_ADC_M,   OP_ADC_A,
                   OP_SUB_B,   OP_SUB_C,   OP_SUB_D,   OP_SUB_E,   OP_SUB_H,   OP_SUB_L,   OP_SUB_M,   OP_SUB_A,
                   OP_SBB_B,   OP_SBB_C,   OP_SBB_D,   OP_SBB_E,   OP_SBB_H,   OP_SBB_L,   OP_SBB_M,   OP_SBB_A,
                   OP_ANA_B,   OP_ANA_C,   OP_ANA_D,   OP_ANA_E,   OP_ANA_H,   OP_ANA_L,   OP_ANA_M,   OP_ANA_A,
                   OP_XRA_B,   OP_XRA_C,   OP_XRA_D,   OP_XRA_E,   OP_XRA_H,   OP_XRA_L,   OP_XRA_M,   OP_XRA_A,
                   OP_ORA_B,   OP_ORA_C,   OP_ORA_D,   OP_ORA_E,   OP_ORA_H,   OP_ORA_L,   OP_ORA_M,   OP_ORA_A,
                   OP_CMP_B,   OP_CMP_C,   OP_CMP_D,   OP_CMP_E,   OP_CMP_H,   OP_CMP_L,   OP_CMP_M,   OP_CMP_A,
                   OP_RNZ,     OP_POP_B,   OP_JNZ,     OP_JMP,     OP_CNZ,     OP_PUSH_B,  OP_ADI,     OP_RST_0,
                   OP_RZ,      OP_RET,     OP_JZ,      OP_0CBH,    OP_CZ,      OP_CALL,    OP_ACI,     OP_RST_1,
                   OP_RNC,     OP_POP_D,   OP_JNC,     OP_OUT,     OP_CNC,     OP_PUSH_D,  OP_SUI,     OP_RST_2,
                   OP_RC,      OP_0D9H,    OP_JC,      OP_IN,      OP_CC,      OP_0DDH,    OP_SBI,     OP_RST_3,
                   OP_RPO,     OP_POP_H,   OP_JPO,     OP_XTHL,    OP_CPO,     OP_PUSH_H,  OP_ANI,     OP_RST_4,
                   OP_RPE,     OP_PCHL,    OP_JPE,     OP_XCHG,    OP_CPE,     OP_0EDH,    OP_XRI,     OP_RST_5,
                   OP_RP,      OP_POP_PSW, OP_JP,      OP_DI,      OP_CP,      OP_PUSH_PSW, OP_ORI,    OP_RST_6,
                   OP_RM,      OP_SPHL,    OP_JM,      OP_EI,      OP_CM,      OP_0FDH,    OP_CPI,     OP_RST_7);
for opcode use(OP_NOP     => 16#00#, OP_LXI_B   => 16#01#, OP_STAX_B  => 16#02#, OP_INX_B   => 16#03#,
               OP_INR_B   => 16#04#, OP_DCR_B   => 16#05#, OP_MVI_B   => 16#06#, OP_RLC     => 16#07#,
               OP_08      => 16#08#, OP_DAD_B   => 16#09#, OP_LDAX_B  => 16#0A#, OP_DCX_B   => 16#0B#,
               OP_INR_C   => 16#0C#, OP_DCR_C   => 16#0D#, OP_MVI_C   => 16#0E#, OP_RRC     => 16#0F#,
               OP_10      => 16#10#, OP_LXI_D   => 16#11#, OP_STAX_D  => 16#12#, OP_INX_D   => 16#13#,
               OP_INR_D   => 16#14#, OP_DCR_D   => 16#15#, OP_MVI_D   => 16#16#, OP_RAL     => 16#17#,
               OP_18      => 16#18#, OP_DAD_D   => 16#19#, OP_LDAX_D  => 16#1A#, OP_DCX_D   => 16#1B#,
               OP_INR_E   => 16#1C#, OP_DCR_E   => 16#1D#, OP_MVI_E   => 16#1E#, OP_RAR     => 16#1F#,
               RIM        => 16#20#, OP_LXI_H   => 16#21#, OP_SHLD    => 16#22#, OP_INX_H   => 16#23#,
               OP_INR_H   => 16#24#, OP_DCR_H   => 16#25#, OP_MVI_H   => 16#26#, OP_DAA     => 16#27#,
               OP_28      => 16#28#, OP_DAD_H   => 16#29#, OP_LHLD    => 16#2A#, OP_DCX_H   => 16#2B#,
               OP_INR_L   => 16#2C#, OP_DCR_L   => 16#2D#, OP_MVI_L   => 16#2E#, OP_CMA     => 16#2F#,
               SIM        => 16#30#, OP_LXI_SP  => 16#31#, OP_STA     => 16#32#, OP_INX_SP  => 16#33#,
               OP_INR_M   => 16#34#, OP_DCR_M   => 16#35#, OP_MVI_M   => 16#36#, OP_STC     => 16#37#,
               OP_38      => 16#38#, OP_DAD_SP  => 16#39#, OP_LDA     => 16#3A#, OP_DCX_SP  => 16#3B#,
               OP_INR_A   => 16#3C#, OP_DCR_A   => 16#3D#, OP_MVI_A   => 16#3E#, OP_CMC     => 16#3F#,
               OP_MOV_B_B => 16#40#, OP_MOV_B_C => 16#41#, OP_MOV_B_D => 16#42#, OP_MOV_B_E => 16#43#,
               OP_MOV_B_H => 16#44#, OP_MOV_B_L => 16#45#, OP_MOV_B_M => 16#46#, OP_MOV_B_A => 16#47#,
               OP_MOV_C_B => 16#48#, OP_MOV_C_C => 16#49#, OP_MOV_C_D => 16#4A#, OP_MOV_C_E => 16#4B#,
               OP_MOV_C_H => 16#4C#, OP_MOV_C_L => 16#4D#, OP_MOV_C_M => 16#4E#, OP_MOV_C_A => 16#4F#,
               OP_MOV_D_B => 16#50#, OP_MOV_D_C => 16#51#, OP_MOV_D_D => 16#52#, OP_MOV_D_E => 16#53#,
               OP_MOV_D_H => 16#54#, OP_MOV_D_L => 16#55#, OP_MOV_D_M => 16#56#, OP_MOV_D_A => 16#57#,
               OP_MOV_E_B => 16#58#, OP_MOV_E_C => 16#59#, OP_MOV_E_D => 16#5A#, OP_MOV_E_E => 16#5B#,
               OP_MOV_E_H => 16#5C#, OP_MOV_E_L => 16#5D#, OP_MOV_E_M => 16#5E#, OP_MOV_E_A => 16#5F#,
               OP_MOV_H_B => 16#60#, OP_MOV_H_C => 16#61#, OP_MOV_H_D => 16#62#, OP_MOV_H_E => 16#63#,
               OP_MOV_H_H => 16#64#, OP_MOV_H_L => 16#65#, OP_MOV_H_M => 16#66#, OP_MOV_H_A => 16#67#,
               OP_MOV_L_B => 16#68#, OP_MOV_L_C => 16#69#, OP_MOV_L_D => 16#6A#, OP_MOV_L_E => 16#6B#,
               OP_MOV_L_H => 16#6C#, OP_MOV_L_L => 16#6D#, OP_MOV_L_M => 16#6E#, OP_MOV_L_A => 16#6F#,
               OP_MOV_M_B => 16#70#, OP_MOV_M_C => 16#71#, OP_MOV_M_D => 16#72#, OP_MOV_M_E => 16#73#,
               OP_MOV_M_H => 16#74#, OP_MOV_M_L => 16#75#, OP_HLT     => 16#76#, OP_MOV_M_A => 16#77#,
               OP_MOV_A_B => 16#78#, OP_MOV_A_C => 16#79#, OP_MOV_A_D => 16#7A#, OP_MOV_A_E => 16#7B#,
               OP_MOV_A_H => 16#7C#, OP_MOV_A_L => 16#7D#, OP_MOV_A_M => 16#7E#, OP_MOV_A_A => 16#7F#,
               OP_ADD_B   => 16#80#, OP_ADD_C   => 16#81#, OP_ADD_D   => 16#82#, OP_ADD_E   => 16#83#,
               OP_ADD_H   => 16#84#, OP_ADD_L   => 16#85#, OP_ADD_M   => 16#86#, OP_ADD_A   => 16#87#,
               OP_ADC_B   => 16#88#, OP_ADC_C   => 16#89#, OP_ADC_D   => 16#8A#, OP_ADC_E   => 16#8B#,
               OP_ADC_H   => 16#8C#, OP_ADC_L   => 16#8D#, OP_ADC_M   => 16#8E#, OP_ADC_A   => 16#8F#,
               OP_SUB_B   => 16#90#, OP_SUB_C   => 16#91#, OP_SUB_D   => 16#92#, OP_SUB_E   => 16#93#,
               OP_SUB_H   => 16#94#, OP_SUB_L   => 16#95#, OP_SUB_M   => 16#96#, OP_SUB_A   => 16#97#,
               OP_SBB_B   => 16#98#, OP_SBB_C   => 16#99#, OP_SBB_D   => 16#9A#, OP_SBB_E   => 16#9B#,
               OP_SBB_H   => 16#9C#, OP_SBB_L   => 16#9D#, OP_SBB_M   => 16#9E#, OP_SBB_A   => 16#9F#,
               OP_ANA_B   => 16#A0#, OP_ANA_C   => 16#A1#, OP_ANA_D   => 16#A2#, OP_ANA_E   => 16#A3#,
               OP_ANA_H   => 16#A4#, OP_ANA_L   => 16#A5#, OP_ANA_M   => 16#A6#, OP_ANA_A   => 16#A7#,
               OP_XRA_B   => 16#A8#, OP_XRA_C   => 16#A9#, OP_XRA_D   => 16#AA#, OP_XRA_E   => 16#AB#,
               OP_XRA_H   => 16#AC#, OP_XRA_L   => 16#AD#, OP_XRA_M   => 16#AE#, OP_XRA_A   => 16#AF#,
               OP_ORA_B   => 16#B0#, OP_ORA_C   => 16#B1#, OP_ORA_D   => 16#B2#, OP_ORA_E   => 16#B3#,
               OP_ORA_H   => 16#B4#, OP_ORA_L   => 16#B5#, OP_ORA_M   => 16#B6#, OP_ORA_A   => 16#B7#,
               OP_CMP_B   => 16#B8#, OP_CMP_C   => 16#B9#, OP_CMP_D   => 16#BA#, OP_CMP_E   => 16#BB#,
               OP_CMP_H   => 16#BC#, OP_CMP_L   => 16#BD#, OP_CMP_M   => 16#BE#, OP_CMP_A   => 16#BF#,
               OP_RNZ     => 16#C0#, OP_POP_B   => 16#C1#, OP_JNZ     => 16#C2#, OP_JMP     => 16#C3#,
               OP_CNZ     => 16#C4#, OP_PUSH_B  => 16#C5#, OP_ADI     => 16#C6#, OP_RST_0   => 16#C7#,
               OP_RZ      => 16#C8#, OP_RET     => 16#C9#, OP_JZ      => 16#CA#, OP_0CBH    => 16#CB#,
               OP_CZ      => 16#CC#, OP_CALL    => 16#CD#, OP_ACI     => 16#CE#, OP_RST_1   => 16#CF#,
               OP_RNC     => 16#D0#, OP_POP_D   => 16#D1#, OP_JNC     => 16#D2#, OP_OUT     => 16#D3#,
               OP_CNC     => 16#D4#, OP_PUSH_D  => 16#D5#, OP_SUI     => 16#D6#, OP_RST_2   => 16#D7#,
               OP_RC      => 16#D8#, OP_0D9H    => 16#D9#, OP_JC      => 16#DA#, OP_IN      => 16#DB#,
               OP_CC      => 16#DC#, OP_0DDH    => 16#DD#, OP_SBI     => 16#DE#, OP_RST_3   => 16#DF#,
               OP_RPO     => 16#E0#, OP_POP_H   => 16#E1#, OP_JPO     => 16#E2#, OP_XTHL    => 16#E3#,
               OP_CPO     => 16#E4#, OP_PUSH_H  => 16#E5#, OP_ANI     => 16#E6#, OP_RST_4   => 16#E7#,
               OP_RPE     => 16#E8#, OP_PCHL    => 16#E9#, OP_JPE     => 16#EA#, OP_XCHG    => 16#EB#,
               OP_CPE     => 16#EC#, OP_0EDH    => 16#ED#, OP_XRI     => 16#EE#, OP_RST_5   => 16#EF#,
               OP_RP      => 16#F0#, OP_POP_PSW => 16#F1#, OP_JP      => 16#F2#, OP_DI      => 16#F3#,
               OP_CP      => 16#F4#, OP_PUSH_PSW => 16#F5#, OP_ORI    => 16#F6#, OP_RST_6   => 16#F7#,
               OP_RM      => 16#F8#, OP_SPHL    => 16#F9#, OP_JM      => 16#FA#, OP_EI      => 16#FB#,
               OP_CM      => 16#FC#, OP_0FDH    => 16#FD#, OP_CPI     => 16#FE#, OP_RST_7   => 16#FF#);
   for opcode'Size use 8;
   --
end BBS.Sim_CPU.i8080;

