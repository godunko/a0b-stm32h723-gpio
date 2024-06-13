
--  pragma Restrictions (No_Elaboration_Code);

pragma Ada_2022;

--  with A0B.ARMv7M;               use A0B.ARMv7M;
--  with A0B.ARMv7M.CMSIS;         use A0B.ARMv7M.CMSIS;
--  with A0B.ARMv7M.NVIC;
--  with A0B.SVD.STM32H723.EXTI;   use A0B.SVD.STM32H723.EXTI;
with A0B.STM32H723.SVD.GPIO;   use A0B.STM32H723.SVD.GPIO;
--  with A0B.SVD.STM32H723.RCC;    use A0B.SVD.STM32H723.RCC;
--  with A0B.SVD.STM32H723.SYSCFG; use A0B.SVD.STM32H723.SYSCFG;
--  with A0B.Types.GCC_Builtins;   use A0B.Types.GCC_Builtins;

with A0B.Types;

package body A0B.STM32H723.GPIO is

   --  type STM32_Pin_Access is access all STM32_Pin'Class;
   --
   --  type EXTI_Descriptor is limited record
   --     IO : STM32_Pin_Access;
   --     SO : aliased Ada.Synchronous_Task_Control.Suspension_Object;
   --     CB : A0B.Callbacks.Callback;
   --  end record;
   --
   --  subtype EXTI_Line_Identifier is IO_Line_Identifier;
   --
   --  EXTI0_Interrupt     : constant A0B.ARMv7M.External_Interrupt_Number := 6;
   --  EXTI1_Interrupt     : constant A0B.ARMv7M.External_Interrupt_Number := 7;
   --  EXTI2_Interrupt     : constant A0B.ARMv7M.External_Interrupt_Number := 8;
   --  EXTI3_Interrupt     : constant A0B.ARMv7M.External_Interrupt_Number := 9;
   --  EXTI4_Interrupt     : constant A0B.ARMv7M.External_Interrupt_Number := 10;
   --  EXTI9_5_Interrupt   : constant A0B.ARMv7M.External_Interrupt_Number := 23;
   --  EXTI15_10_Interrupt : constant A0B.ARMv7M.External_Interrupt_Number := 40;
   --
   --  EXTI0_Mask     : constant := 2#0000_0000_0000_0001#;
   --  EXTI1_Mask     : constant := 2#0000_0000_0000_0010#;
   --  EXTI2_Mask     : constant := 2#0000_0000_0000_0100#;
   --  EXTI3_Mask     : constant := 2#0000_0000_0000_1000#;
   --  EXTI4_Mask     : constant := 2#0000_0000_0001_0000#;
   --  EXTI9_5_Mask   : constant := 2#0000_0011_1110_0000#;
   --  EXTI15_10_Mask : constant := 2#1111_1100_0000_0000#;
   --
   --  EXTI_Line : array (EXTI_Line_Identifier) of aliased EXTI_Descriptor;

   subtype GPIO_Alternative_Function is A0B.Types.Unsigned_4;

   type Line_Descriptor (Supported : Boolean := False) is record
      case Supported is
         when False =>
            null;

         when True =>
            Controller           : GPIO_Controller_Identifier;
            Line                 : GPIO_Line_Identifier;
            Alternative_Function : GPIO_Alternative_Function;
      end case;
   end record with Pack;

   type Line_Descriptor_Array is array (0 .. 3) of Line_Descriptor with Pack;

   AF : constant array (Function_Line) of Line_Descriptor_Array :=
     [FMC_A0     => [(True, F, 0, 12), others => <>],
      FMC_A1     => [(True, F, 1, 12), others => <>],
      FMC_A2     => [(True, F, 2, 12), others => <>],
      FMC_A3     => [(True, F, 3, 12), others => <>],
      FMC_A4     => [(True, F, 4, 12), others => <>],
      FMC_A5     => [(True, F, 5, 12), others => <>],
      FMC_A6     => [(True, F, 12, 12), others => <>],
      FMC_A7     => [(True, F, 13, 12), others => <>],
      FMC_A8     => [(True, F, 14, 12), others => <>],
      FMC_A9     => [(True, F, 15, 12), others => <>],
      FMC_A10    => [(True, G, 0, 12), others => <>],
      FMC_A11    => [(True, G, 1, 12), others => <>],
      FMC_A12    => [(True, G, 2, 12), others => <>],
      FMC_A13    => [(True, G, 3, 12), others => <>],
      FMC_A14    => [(True, G, 4, 12), others => <>],
      FMC_A15    => [(True, G, 5, 12), others => <>],
      FMC_A16    => [(True, D, 11, 12), others => <>],
      FMC_A17    => [(True, D, 12, 12), others => <>],
      FMC_A18    => [(True, D, 13, 12), others => <>],
      FMC_A19    => [(True, A, 0, 12), (True, E, 3, 12), others => <>],
      FMC_A20    => [(True, E, 4, 12), others => <>],
      FMC_A21    => [(True, E, 5, 12), others => <>],
      FMC_A22    => [(True, C, 4, 1), (True, E, 6, 12), others => <>],
      FMC_A23    => [(True, E, 2, 12), others => <>],
      FMC_A24    => [(True, G, 13, 12), others => <>],
      FMC_A25    => [(True, C, 0, 9), (True, G, 14, 12), others => <>],
      FMC_AD0    => [(True, D, 14, 12), others => <>],
      FMC_AD1    => [(True, D, 15, 12), others => <>],
      FMC_AD2    => [(True, D, 0, 12), others => <>],
      FMC_AD3    => [(True, D, 1, 12), others => <>],
      FMC_AD4    => [(True, E, 7, 12), others => <>],
      FMC_AD5    => [(True, E, 8, 12), others => <>],
      FMC_AD6    => [(True, C, 12, 1), (True, E, 9, 12), others => <>],
      FMC_AD7    => [(True, D, 2, 1), (True, E, 10, 12), others => <>],
      FMC_AD8    => [(True, A, 4, 12), (True, E, 11, 12), others => <>],
      FMC_AD9    => [(True, A, 5, 12), (True, E, 12, 12), others => <>],
      FMC_AD10   => [(True, B, 14, 12), (True, E, 13, 12), others => <>],
      FMC_AD11   => [(True, B, 15, 12), (True, E, 14, 12), others => <>],
      FMC_AD12   => [(True, C, 0, 1), (True, E, 15, 12), others => <>],
      FMC_AD13   => [(True, D, 8, 12), others => <>],
      FMC_AD14   => [(True, D, 9, 12), others => <>],
      FMC_AD15   => [(True, D, 10, 12), others => <>],
      FMC_D0     => [(True, D, 14, 12), others => <>],
      FMC_D1     => [(True, D, 15, 12), others => <>],
      FMC_D2     => [(True, D, 0, 12), others => <>],
      FMC_D3     => [(True, D, 1, 12), others => <>],
      FMC_D4     => [(True, E, 7, 12), others => <>],
      FMC_D5     => [(True, E, 8, 12), others => <>],
      FMC_D6     => [(True, C, 12, 1), (True, E, 9, 12), others => <>],
      FMC_D7     => [(True, D, 2, 1), (True, E, 10, 12), others => <>],
      FMC_D8     => [(True, A, 4, 12), (True, E, 11, 12), others => <>],
      FMC_D9     => [(True, A, 5, 12), (True, E, 12, 12), others => <>],
      FMC_D10    => [(True, B, 14, 12), (True, E, 13, 12), others => <>],
      FMC_D11    => [(True, B, 15, 12), (True, E, 14, 12), others => <>],
      FMC_D12    => [(True, C, 0, 1), (True, E, 15, 12), others => <>],
      FMC_D13    => [(True, D, 8, 12), others => <>],
      FMC_D14    => [(True, D, 9, 12), others => <>],
      FMC_D15    => [(True, D, 10, 12), others => <>],
      FMC_ALE    => [(True, D, 12, 12), others => <>],
      FMC_BA0    => [(True, G, 4, 12), others => <>],
      FMC_BA1    => [(True, G, 5, 12), others => <>],
      FMC_CLE    => [(True, D, 11, 12), others => <>],
      FMC_CLK    => [(True, D, 3, 12), others => <>],
      FMC_INT    => [(True, C, 8, 10), (True, G, 7, 12), others => <>],
      FMC_NBL0   => [(True, E, 0, 12), others => <>],
      FMC_NBL1   => [(True, E, 1, 12), others => <>],
      FMC_NCAS   => [(True, G, 15, 12), others => <>],
      FMC_NCE    => [(True, C, 8, 9), (True, G, 9, 12), others => <>],
      FMC_NE1    => [(True, C, 7, 9), (True, D, 7, 12), others => <>],
      FMC_NE2    => [(True, C, 8, 9), (True, G, 9, 12), others => <>],
      FMC_NE3    => [(True, G, 6, 9), (True, G, 10, 12), others => <>],
      FMC_NE4    => [(True, G, 12, 12), others => <>],
      FMC_NL     => [(True, B, 7, 12), others => <>],
      FMC_NOE    => [(True, D, 4, 12), others => <>],
      FMC_NRAS   => [(True, F, 11, 12), others => <>],
      FMC_NWAIT  => [(True, C, 6, 9), (True, D, 6, 12), others => <>],
      FMC_NWE    => [(True, D, 5, 12), others => <>],
      FMC_SDCKE0 => [(True, C, 3, 12), (True, C, 5, 12), others => <>],
      FMC_SDCKE1 => [(True, B, 5, 12), others => <>],
      FMC_SDCLK  => [(True, G, 8, 12), others => <>],
      FMC_SDNE0  => [(True, C, 2, 12), (True, C, 4, 12), others => <>],
      FMC_SDNE1  => [(True, B, 6, 12), others => <>],
      FMC_SDNWE  => [(True, A, 7, 12), (True, C, 0, 12), others => <>],

      LPTIM1_ETR => [(True, E, 0, 1), (True, G, 14, 1), others => <>],
      LPTIM1_IN1 => [(True, D, 12, 1), (True, G, 12, 1), others => <>],
      LPTIM1_IN2 => [(True, E, 1, 1), (True, G, 11, 1), others => <>],
      LPTIM1_OUT => [(True, D, 13, 1), (True, G, 13, 1), others => <>],
      LPTIM2_ETR => [(True, B, 11, 3), (True, E, 0, 4), others => <>],
      LPTIM2_IN1 => [(True, B, 10, 3), (True, D, 12, 3), others => <>],
      LPTIM2_IN2 => [(True, D, 11, 3), others => <>],
      LPTIM2_OUT => [(True, B, 13, 3), others => <>],
      LPTIM3_OUT => [(True, A, 1, 3), others => <>],
      LPTIM4_OUT => [(True, A, 2, 3), others => <>],
      LPTIM5_OUT => [(True, A, 3, 3), others => <>],

      PSSI_D0    => [(True, A, 9, 13), (True, C, 6, 13), others => <>],
      PSSI_D1    => [(True, A, 10, 13), (True, C, 7, 13), others => <>],
      PSSI_D2    => [(True, B, 13, 13), (True, C, 8, 13), (True, E, 0, 13), (True, G, 10, 13), others => <>],
      PSSI_D3    => [(True, C, 9, 13), (True, E, 1, 13), (True, G, 11, 13), others => <>],
      PSSI_D4    => [(True, C, 11, 13), (True, E, 4, 13), others => <>],
      PSSI_D5    => [(True, B, 6, 13), (True, D, 3, 13), others => <>],
      PSSI_D6    => [(True, B, 8, 13), (True, E, 5, 13), others => <>],
      PSSI_D7    => [(True, B, 9, 13), (True, E, 6, 13), others => <>],
      PSSI_D8    => [(True, C, 10, 13), others => <>],
      PSSI_D9    => [(True, C, 12, 13), others => <>],
      PSSI_D10   => [(True, B, 5, 13), (True, D, 6, 13), others => <>],
      PSSI_D11   => [(True, D, 2, 13), (True, F, 10, 13), others => <>],
      PSSI_D12   => [(True, D, 12, 13), (True, F, 11, 13), (True, G, 6, 13), others => <>],
      PSSI_D13   => [(True, D, 13, 13), (True, G, 7, 13), (True, G, 15, 13), others => <>],
      PSSI_D14   => [(True, A, 5, 13), others => <>],
      PSSI_D15   => [(True, C, 5, 4), (True, F, 10, 4), others => <>],
      PSSI_DE    => [(True, A, 4, 13), others => <>],
      PSSI_PDCK  => [(True, A, 6, 13), others => <>],
      PSSI_RDY   => [(True, B, 7, 13), (True, G, 9, 13), others => <>],

      SPI6_MISO  => [(True, A, 6, 8), (True, B, 4, 8), (True, G, 12, 5), others => <>],
      SPI6_MOSI  => [(True, A, 7, 8), (True, B, 5, 8), (True, G, 14, 5), others => <>],
      SPI6_NSS   => [(True, A, 0, 5), (True, A, 4, 8), (True, A, 15, 7), (True, G, 8, 5)],
      SPI6_SCK   => [(True, A, 5, 8), (True, B, 3, 8), (True, C, 12, 5), (True, G, 13, 5)]];

   --  procedure EXTI0_Handler
   --    with Export, Convention => C, External_Name => "EXTI0_Handler";
   --  procedure EXTI1_Handler
   --    with Export, Convention => C, External_Name => "EXTI1_Handler";
   --  procedure EXTI2_Handler
   --    with Export, Convention => C, External_Name => "EXTI2_Handler";
   --  procedure EXTI3_Handler
   --    with Export, Convention => C, External_Name => "EXTI3_Handler";
   --  procedure EXTI4_Handler
   --    with Export, Convention => C, External_Name => "EXTI4_Handler";
   --  procedure EXTI9_5_Handler
   --    with Export, Convention => C, External_Name => "EXTI9_5_Handler";
   --  procedure EXTI15_10_Handler
   --    with Export, Convention => C, External_Name => "EXTI15_10_Handler";
   --  --  EXTI* interrupt handlers
   --
   --  procedure Enable_Clock (Self : in out GPIO_Controller);
   --  --  Enable clock of the given GPIO controller.

   procedure Set_Pull_Mode
     (Self : aliased in out GPIO_Line'Class;
      To   : Pull_Mode);

   procedure Set_Output_Speed
     (Self : aliased in out GPIO_Line'Class;
      To   : Output_Speed);

   procedure Set_Output_Mode
     (Self : aliased in out GPIO_Line'Class;
      To   : Output_Mode);

   ------------------------------------
   -- Configure_Alternative_Function --
   ------------------------------------

   procedure Configure_Alternative_Function
     (Self  : aliased in out GPIO_Line'Class;
      Line  : Function_Line;
      Mode  : Output_Mode  := Push_Pull;
      Speed : Output_Speed := Low;
      Pull  : Pull_Mode    := No) is
   begin
      --  Enable_Clock (Self.Controller.all);

      for Descriptor of AF (Line) loop
         if Descriptor.Controller = Self.Controller.Identifier
           and Descriptor.Line = Self.Identifier
         then
            Self.Set_Output_Speed (Speed);
            Self.Set_Output_Mode (Mode);
            Self.Set_Pull_Mode (Pull);

            if Self.Identifier < 8 then
               Self.Controller.Peripheral.AFRL.Arr
                 (Integer (Self.Identifier)) :=
                   Descriptor.Alternative_Function;

            else
               Self.Controller.Peripheral.AFRH.Arr
                 (Integer (Self.Identifier)) :=
                   Descriptor.Alternative_Function;
            end if;

            Self.Controller.Peripheral.MODER.Arr
              (Integer (Self.Identifier)) := 2#10#;
            --  Alternate function

            return;
         end if;
      end loop;

      raise Program_Error;
   end Configure_Alternative_Function;

   --  --------------------
   --  -- Configure_EXTI --
   --  --------------------
   --
   --  procedure Configure_EXTI
   --    (Self : aliased in out STM32_Pin'Class;
   --     Mode : EXTI_Mode;
   --     Pull : Pull_Mode := No) is
   --  begin
   --     pragma Assert
   --       (EXTI_Line (Self.Line).IO = null
   --          or EXTI_Line (Self.Line).IO = Self'Unchecked_Access);
   --     EXTI_Line (Self.Line).IO := Self'Unchecked_Access;
   --     --  Link EXTI line with GPIO line
   --
   --     Self.Configure_Input (Pull);
   --     --  Configure IO line in input mode, it is required for EXTI, enable
   --     --  pull-up/pull-down when requested.
   --
   --     RCC_Periph.APB4ENR.SYSCFGEN := True;
   --     Data_Synchronization_Barrier;
   --     --  Enable clock of the SYSCFG controller.
   --
   --     --  Select GPIO controller to be used to generate external interrupt.
   --
   --     case Self.Line is
   --        when 0 .. 3 =>
   --           SYSCFG_Periph.EXTICR1.EXTI.Arr (Integer (Self.Line)) :=
   --             Self.Controller.Identifier;
   --
   --        when 4 .. 7 =>
   --           SYSCFG_Periph.EXTICR2.EXTI.Arr (Integer (Self.Line)) :=
   --             Self.Controller.Identifier;
   --
   --        when 8 .. 11 =>
   --           SYSCFG_Periph.EXTICR3.EXTI.Arr (Integer (Self.Line)) :=
   --             Self.Controller.Identifier;
   --
   --        when 12 .. 15 =>
   --           SYSCFG_Periph.EXTICR4.EXTI.Arr (Integer (Self.Line)) :=
   --             Self.Controller.Identifier;
   --     end case;
   --
   --     --  Select which edge(s) generates interrupt.
   --
   --     case Mode is
   --        when Both_Edge =>
   --           EXTI_Periph.RTSR1.TR.Arr (Integer (Self.Line)) := True;
   --           EXTI_Periph.FTSR1.TR.Arr (Integer (Self.Line)) := True;
   --
   --        when Rising_Edge =>
   --           EXTI_Periph.RTSR1.TR.Arr (Integer (Self.Line)) := True;
   --           EXTI_Periph.FTSR1.TR.Arr (Integer (Self.Line)) := False;
   --
   --        when Falling_Edge =>
   --           EXTI_Periph.RTSR1.TR.Arr (Integer (Self.Line)) := False;
   --           EXTI_Periph.FTSR1.TR.Arr (Integer (Self.Line)) := True;
   --     end case;
   --  end Configure_EXTI;

   ---------------------
   -- Configure_Input --
   ---------------------

   procedure Configure_Input
     (Self : aliased in out GPIO_Line'Class;
      Pull : Pull_Mode := No) is
   begin
      --  Enable_Clock (Self.Controller.all);

      Self.Set_Pull_Mode (Pull);

      Self.Controller.Peripheral.MODER.Arr
        (Integer (Self.Identifier)) := 2#00#;
      --  Input mode
   end Configure_Input;

   ----------------------
   -- Configure_Output --
   ----------------------

   procedure Configure_Output
     (Self  : aliased in out GPIO_Line'Class;
      Mode  : Output_Mode  := Push_Pull;
      Speed : Output_Speed := Low;
      Pull  : Pull_Mode    := No) is
   begin
      --  Enable_Clock (Self.Controller.all);

      Self.Set_Output_Mode (Mode);
      Self.Set_Output_Speed (Speed);
      Self.Set_Pull_Mode (Pull);

      Self.Controller.Peripheral.MODER.Arr
        (Integer (Self.Identifier)) := 2#01#;
      --  General purpose output mode
   end Configure_Output;

   --  -----------------------
   --  -- Disable_Interrupt --
   --  -----------------------
   --
   --  overriding procedure Disable_Interrupt (Self : in out STM32_Pin) is
   --     use type A0B.Types.Unsigned_32;
   --
   --     Old_IMR : constant A0B.Types.Unsigned_32 := EXTI_Periph.CPUIMR1.Val;
   --     Mask    : constant A0B.Types.Unsigned_32 :=
   --       A0B.Types.Shift_Left (1, Integer (Self.Line));
   --     New_IMR : constant A0B.Types.Unsigned_32 := Old_IMR and not Mask;
   --
   --  begin
   --     --  Disable external interrupt
   --
   --     EXTI_Periph.CPUIMR1.Val := New_IMR;
   --
   --     --  Disable NVIC interrupt
   --
   --     if (Old_IMR and EXTI0_Mask) /= 0
   --       and (New_IMR and EXTI0_Mask) = 0
   --     then
   --        NVIC.ICER (EXTI0_Interrupt) := True;
   --     end if;
   --
   --     if (Old_IMR and EXTI1_Mask) /= 0
   --       and (New_IMR and EXTI1_Mask) = 0
   --     then
   --        NVIC.ICER (EXTI1_Interrupt) := True;
   --     end if;
   --
   --     if (Old_IMR and EXTI2_Mask) /= 0
   --       and (New_IMR and EXTI2_Mask) = 0
   --     then
   --        NVIC.ICER (EXTI2_Interrupt) := True;
   --     end if;
   --
   --     if (Old_IMR and EXTI3_Mask) /= 0
   --       and (New_IMR and EXTI3_Mask) = 0
   --     then
   --        NVIC.ICER (EXTI3_Interrupt) := True;
   --     end if;
   --
   --     if (Old_IMR and EXTI4_Mask) /= 0
   --       and (New_IMR and EXTI4_Mask) = 0
   --     then
   --        NVIC.ICER (EXTI4_Interrupt) := True;
   --     end if;
   --
   --     if (Old_IMR and EXTI9_5_Mask) /= 0
   --       and (New_IMR and EXTI9_5_Mask) = 0
   --     then
   --        NVIC.ICER (EXTI9_5_Interrupt) := True;
   --     end if;
   --
   --     if (Old_IMR and EXTI15_10_Mask) /= 0
   --       and (New_IMR and EXTI15_10_Mask) = 0
   --     then
   --        NVIC.ICER (EXTI15_10_Interrupt) := True;
   --     end if;
   --  end Disable_Interrupt;
   --
   --  ------------------
   --  -- Enable_Clock --
   --  ------------------
   --
   --  procedure Enable_Clock (Self : in out GPIO_Controller) is
   --  begin
   --     case Self.Identifier is
   --        when A =>
   --           RCC_Periph.AHB4ENR.GPIOAEN := True;
   --
   --        when B =>
   --           RCC_Periph.AHB4ENR.GPIOBEN := True;
   --
   --        when C =>
   --           RCC_Periph.AHB4ENR.GPIOCEN := True;
   --
   --        when D =>
   --           RCC_Periph.AHB4ENR.GPIODEN := True;
   --
   --        when E =>
   --           RCC_Periph.AHB4ENR.GPIOEEN := True;
   --
   --        when F =>
   --           RCC_Periph.AHB4ENR.GPIOFEN := True;
   --
   --        when G =>
   --           RCC_Periph.AHB4ENR.GPIOGEN := True;
   --
   --        when H =>
   --           RCC_Periph.AHB4ENR.GPIOHEN := True;
   --
   --        when J =>
   --           RCC_Periph.AHB4ENR.GPIOJEN := True;
   --
   --        when K =>
   --           RCC_Periph.AHB4ENR.GPIOKEN := True;
   --
   --        when others =>
   --           raise Program_Error;
   --     end case;
   --
   --     Data_Synchronization_Barrier;
   --  end Enable_Clock;
   --
   --  ----------------------
   --  -- Enable_Interrupt --
   --  ----------------------
   --
   --  overriding procedure Enable_Interrupt (Self : in out STM32_Pin) is
   --     use type A0B.Types.Unsigned_32;
   --
   --     Old_IMR : constant A0B.Types.Unsigned_32 := EXTI_Periph.CPUIMR1.Val;
   --     Mask    : constant A0B.Types.Unsigned_32 :=
   --       A0B.Types.Shift_Left (1, Integer (Self.Line));
   --     New_IMR : constant A0B.Types.Unsigned_32 := Old_IMR or Mask;
   --
   --  begin
   --     --  Clear pending status and enable external interrupt
   --
   --     EXTI_Periph.CPUPR1.PR.Val := A0B.Types.Unsigned_22 (Mask);
   --     EXTI_Periph.CPUIMR1.Val   := New_IMR;
   --
   --     --  Enable NVIC interrupt
   --
   --     if (Old_IMR and EXTI0_Mask) = 0
   --       and (New_IMR and EXTI0_Mask) /= 0
   --     then
   --        NVIC.ICPR (EXTI0_Interrupt) := True;
   --        NVIC.ISER (EXTI0_Interrupt) := True;
   --        --  NVIC.IPR (EXTI0_Interrupt) := bla...;
   --        --  XXX How to get priority without depend      null;
   --     end if;
   --
   --     if (Old_IMR and EXTI1_Mask) = 0
   --       and (New_IMR and EXTI1_Mask) /= 0
   --     then
   --        NVIC.ICPR (EXTI1_Interrupt) := True;
   --        NVIC.ISER (EXTI1_Interrupt) := True;
   --        --  NVIC.IPR (EXTI1_Interrupt) := bla...;
   --        --  XXX How to get priority without depend      null;
   --     end if;
   --
   --     if (Old_IMR and EXTI2_Mask) = 0
   --       and (New_IMR and EXTI2_Mask) /= 0
   --     then
   --        NVIC.ICPR (EXTI2_Interrupt) := True;
   --        NVIC.ISER (EXTI2_Interrupt) := True;
   --        --  NVIC.IPR (EXTI2_Interrupt) := bla...;
   --        --  XXX How to get priority without depend      null;
   --     end if;
   --
   --     if (Old_IMR and EXTI3_Mask) = 0
   --       and (New_IMR and EXTI3_Mask) /= 0
   --     then
   --        NVIC.ICPR (EXTI3_Interrupt) := True;
   --        NVIC.ISER (EXTI3_Interrupt) := True;
   --        --  NVIC.IPR (EXTI3_Interrupt) := bla...;
   --        --  XXX How to get priority without depend      null;
   --     end if;
   --
   --     if (Old_IMR and EXTI4_Mask) = 0
   --       and (New_IMR and EXTI4_Mask) /= 0
   --     then
   --        NVIC.ICPR (EXTI4_Interrupt) := True;
   --        NVIC.ISER (EXTI4_Interrupt) := True;
   --        --  NVIC.IPR (EXTI4_Interrupt) := bla...;
   --        --  XXX How to get priority without depend      null;
   --     end if;
   --
   --     if (Old_IMR and EXTI9_5_Mask) = 0
   --       and (New_IMR and EXTI9_5_Mask) /= 0
   --     then
   --        NVIC.ICPR (EXTI9_5_Interrupt) := True;
   --        NVIC.ISER (EXTI9_5_Interrupt) := True;
   --        --  NVIC.IPR (EXTI9_5_Interrupt) := bla...;
   --        --  XXX How to get priority without depend      null;
   --     end if;
   --
   --     if (Old_IMR and EXTI15_10_Mask) = 0
   --       and (New_IMR and EXTI15_10_Mask) /= 0
   --     then
   --        NVIC.ICPR (EXTI15_10_Interrupt) := True;
   --        NVIC.ISER (EXTI15_10_Interrupt) := True;
   --        --  NVIC.IPR (EXTI15_10_Interrupt) := bla...;
   --        --  XXX How to get priority without depend      null;
   --     end if;
   --  end Enable_Interrupt;
   --
   --  ------------------
   --  -- EXTI_Handler --
   --  ------------------
   --
   --  procedure EXTI_Handler (Pending_Mask : A0B.Types.Unsigned_32) is
   --     use type A0B.Types.Unsigned_32;
   --
   --     Status : A0B.Types.Unsigned_32 :=
   --       A0B.Types.Unsigned_32 (EXTI_Periph.CPUPR1.PR.Val)
   --         and Pending_Mask;
   --     Line   : Integer;
   --     Mask   : A0B.Types.Unsigned_32;
   --
   --  begin
   --     while Status /= 0 loop
   --        Line   := Integer (ctz (Status));
   --        Mask   := A0B.Types.Shift_Left (1, Line);
   --        Status := @ and (not Mask);
   --
   --        EXTI_Periph.CPUPR1.PR.Arr (Line) := True;
   --        --  Clear interrupt pending bit, it should be done by software.
   --
   --        Ada.Synchronous_Task_Control.Set_True
   --          (EXTI_Line (EXTI_Line_Identifier (Line)).SO);
   --        A0B.Callbacks.Emit (EXTI_Line (EXTI_Line_Identifier (Line)).CB);
   --     end loop;
   --  end EXTI_Handler;
   --
   --  -------------------
   --  -- EXTI0_Handler --
   --  -------------------
   --
   --  procedure EXTI0_Handler is
   --  begin
   --     EXTI_Handler (EXTI0_Mask);
   --  end EXTI0_Handler;
   --
   --  -------------------
   --  -- EXTI1_Handler --
   --  -------------------
   --
   --  procedure EXTI1_Handler is
   --  begin
   --     EXTI_Handler (EXTI1_Mask);
   --  end EXTI1_Handler;
   --
   --  -----------------------
   --  -- EXTI15_10_Handler --
   --  -----------------------
   --
   --  procedure EXTI15_10_Handler is
   --  begin
   --     EXTI_Handler (EXTI15_10_Mask);
   --  end EXTI15_10_Handler;
   --
   --  -------------------
   --  -- EXTI2_Handler --
   --  -------------------
   --
   --  procedure EXTI2_Handler is
   --  begin
   --     EXTI_Handler (EXTI2_Mask);
   --  end EXTI2_Handler;
   --
   --  -------------------
   --  -- EXTI3_Handler --
   --  -------------------
   --
   --  procedure EXTI3_Handler is
   --  begin
   --     EXTI_Handler (EXTI3_Mask);
   --  end EXTI3_Handler;
   --
   --  -------------------
   --  -- EXTI4_Handler --
   --  -------------------
   --
   --  procedure EXTI4_Handler is
   --  begin
   --     EXTI_Handler (EXTI4_Mask);
   --  end EXTI4_Handler;
   --
   --  ---------------------
   --  -- EXTI9_5_Handler --
   --  ---------------------
   --
   --  procedure EXTI9_5_Handler is
   --  begin
   --     EXTI_Handler (EXTI9_5_Mask);
   --  end EXTI9_5_Handler;

   ---------
   -- Get --
   ---------

   overriding function Get (Self : GPIO_Line) return Boolean is
   begin
      return
        Self.Controller.Peripheral.IDR.ID.Arr (Integer (Self.Identifier));
   end Get;

   ---------
   -- Set --
   ---------

   overriding procedure Set (Self : GPIO_Line; To : Boolean) is
      Aux : BSRR_Register;

   begin
      case To is
         when False =>
            Aux.BR.Arr (Integer (Self.Identifier)) := True;

         when True =>
            Aux.BS.Arr (Integer (Self.Identifier)) := True;
      end case;

      Self.Controller.Peripheral.BSRR := Aux;
   end Set;

   --  ------------------
   --  -- Set_Callback --
   --  ------------------
   --
   --  overriding procedure Set_Callback
   --    (Self : in out STM32_Pin; Callback : A0B.Callbacks.Callback) is
   --  begin
   --     pragma Assert (EXTI_Line (Self.Line).IO = Self'Unchecked_Access);
   --
   --     EXTI_Line (Self.Line).CB := Callback;
   --  end Set_Callback;
   --
   --  -----------------------
   --  -- Suspension_Object --
   --  -----------------------
   --
   --  overriding function Suspension_Object
   --    (Self : aliased in out STM32_Pin)
   --     return not null access Ada.Synchronous_Task_Control.Suspension_Object is
   --  begin
   --     pragma Assert (EXTI_Line (Self.Line).IO = Self'Unchecked_Access);
   --
   --     return EXTI_Line (Self.Line).SO'Access;
   --  end Suspension_Object;

   ---------------------
   -- Set_Output_Mode --
   ---------------------

   procedure Set_Output_Mode
     (Self : aliased in out GPIO_Line'Class;
      To   : Output_Mode) is
   begin
      case To is
         when Push_Pull =>
            Self.Controller.Peripheral.OTYPER.OT.Arr
              (Integer (Self.Identifier)) := False;

         when Open_Drain =>
            Self.Controller.Peripheral.OTYPER.OT.Arr
              (Integer (Self.Identifier)) := True;
      end case;
   end Set_Output_Mode;

   ----------------------
   -- Set_Output_Speed --
   ----------------------

   procedure Set_Output_Speed
     (Self : aliased in out GPIO_Line'Class;
      To   : Output_Speed) is
   begin
      case To is
         when Low =>
            Self.Controller.Peripheral.OSPEEDR.Arr
              (Integer (Self.Identifier)) := 2#00#;

         when Medium =>
            Self.Controller.Peripheral.OSPEEDR.Arr
              (Integer (Self.Identifier)) := 2#01#;

         when High =>
            Self.Controller.Peripheral.OSPEEDR.Arr
              (Integer (Self.Identifier)) := 2#10#;

         when Very_High =>
            Self.Controller.Peripheral.OSPEEDR.Arr
              (Integer (Self.Identifier)) := 2#11#;
      end case;
   end Set_Output_Speed;

   -------------------
   -- Set_Pull_Mode --
   -------------------

   procedure Set_Pull_Mode
     (Self : aliased in out GPIO_Line'Class;
      To   : Pull_Mode) is
   begin
      case To is
         when No =>
            Self.Controller.Peripheral.PUPDR.Arr
              (Integer (Self.Identifier)) := 2#00#;

         when Pull_Up =>
            Self.Controller.Peripheral.PUPDR.Arr
              (Integer (Self.Identifier)) := 2#01#;

         when Pull_Down =>
            Self.Controller.Peripheral.PUPDR.Arr
              (Integer (Self.Identifier)) := 2#10#;
      end case;
   end Set_Pull_Mode;

end A0B.STM32H723.GPIO;
