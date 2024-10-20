--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  pragma Restrictions (No_Elaboration_Code);

with A0B.Callbacks;
with A0B.EXTI;
with A0B.GPIO;
with A0B.STM32H723.SVD.GPIO;
private with A0B.Types;

package A0B.STM32H723.GPIO
  with Preelaborate
is

   type GPIO_Controller;

   type GPIO_Controller_Identifier is (A, B, C, D, E, F, G, H) with Size => 4;
   for GPIO_Controller_Identifier use
     (A => 0, B => 1, C => 2, D => 3, E => 4, F => 5, G => 6, H => 7);

   type GPIO_Line_Identifier is range 0 .. 15;

   type Pull_Mode is (No, Pull_Up, Pull_Down);

   type Output_Mode is (Push_Pull, Open_Drain);

   type Output_Speed is (Low, Medium, High, Very_High);

   type EXTI_Mode is (Both_Edge, Rising_Edge, Falling_Edge);

   type GPIO_Line
     (Controller : not null access GPIO_Controller;
      Identifier : GPIO_Line_Identifier) is
        limited new A0B.GPIO.Input_Line
          and A0B.GPIO.Output_Line
          and A0B.EXTI.External_Interrupt_Line with null record;

   procedure Configure_Input
     (Self : aliased in out GPIO_Line'Class;
      Pull : Pull_Mode := No);

   procedure Configure_Output
     (Self  : aliased in out GPIO_Line'Class;
      Mode  : Output_Mode  := Push_Pull;
      Speed : Output_Speed := Low;
      Pull  : Pull_Mode    := No);

   procedure Configure_Alternative_Function
     (Self  : aliased in out GPIO_Line'Class;
      Line  : Function_Line;
      Mode  : Output_Mode  := Push_Pull;
      Speed : Output_Speed := Low;
      Pull  : Pull_Mode    := No);

   procedure Configure_EXTI
     (Self : aliased in out GPIO_Line'Class;
      Mode : EXTI_Mode;
      Pull : Pull_Mode := No);

   overriding procedure Enable_Interrupt (Self : in out GPIO_Line);

   overriding procedure Disable_Interrupt (Self : in out GPIO_Line);

   overriding procedure Set_Callback
     (Self : in out GPIO_Line; Callback : A0B.Callbacks.Callback);

   overriding function Get (Self : GPIO_Line) return Boolean;

   overriding procedure Set (Self : GPIO_Line; To : Boolean);

   type GPIO_Controller
     (Peripheral : not null access A0B.STM32H723.SVD.GPIO.GPIO_Peripheral;
      Identifier : GPIO_Controller_Identifier) is
        limited null record;

   GPIOA : aliased GPIO_Controller
     (Peripheral => A0B.STM32H723.SVD.GPIO.GPIOA_Periph'Access,
      Identifier => A);
   GPIOB : aliased GPIO_Controller
     (Peripheral => A0B.STM32H723.SVD.GPIO.GPIOB_Periph'Access,
      Identifier => B);
   GPIOC : aliased GPIO_Controller
     (Peripheral => A0B.STM32H723.SVD.GPIO.GPIOC_Periph'Access,
      Identifier => C);
   GPIOD : aliased GPIO_Controller
     (Peripheral => A0B.STM32H723.SVD.GPIO.GPIOD_Periph'Access,
      Identifier => D);
   GPIOE : aliased GPIO_Controller
     (Peripheral => A0B.STM32H723.SVD.GPIO.GPIOE_Periph'Access,
      Identifier => E);
   GPIOF : aliased GPIO_Controller
     (Peripheral => A0B.STM32H723.SVD.GPIO.GPIOF_Periph'Access,
      Identifier => F);
   GPIOG : aliased GPIO_Controller
     (Peripheral => A0B.STM32H723.SVD.GPIO.GPIOG_Periph'Access,
      Identifier => G);
   GPIOH : aliased GPIO_Controller
     (Peripheral => A0B.STM32H723.SVD.GPIO.GPIOH_Periph'Access,
      Identifier => H);

   --  GPIO A

   PA0  : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 0);
   PA1  : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 1);
   PA2  : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 2);
   PA3  : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 3);
   PA4  : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 4);
   PA5  : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 5);
   PA6  : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 6);
   PA7  : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 7);
   PA8  : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 8);
   PA9  : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 9);
   PA10 : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 10);
   PA11 : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 11);
   PA12 : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 12);
   PA13 : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 13);
   PA14 : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 14);
   PA15 : aliased GPIO_Line (Controller => GPIOA'Access, Identifier => 15);

   --  GPIO B

   PB0  : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 0);
   PB1  : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 1);
   PB2  : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 2);
   PB3  : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 3);
   PB4  : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 4);
   PB5  : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 5);
   PB6  : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 6);
   PB7  : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 7);
   PB8  : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 8);
   PB9  : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 9);
   PB10 : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 10);
   PB11 : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 11);
   PB12 : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 12);
   PB13 : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 13);
   PB14 : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 14);
   PB15 : aliased GPIO_Line (Controller => GPIOB'Access, Identifier => 15);

   --  GPIO C

   PC0  : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 0);
   PC1  : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 1);
   PC2  : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 2);
   PC3  : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 3);
   PC4  : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 4);
   PC5  : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 5);
   PC6  : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 6);
   PC7  : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 7);
   PC8  : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 8);
   PC9  : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 9);
   PC10 : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 10);
   PC11 : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 11);
   PC12 : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 12);
   PC13 : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 13);
   PC14 : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 14);
   PC15 : aliased GPIO_Line (Controller => GPIOC'Access, Identifier => 15);

   --  GPIO D

   PD0  : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 0);
   PD1  : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 1);
   PD2  : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 2);
   PD3  : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 3);
   PD4  : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 4);
   PD5  : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 5);
   PD6  : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 6);
   PD7  : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 7);
   PD8  : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 8);
   PD9  : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 9);
   PD10 : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 10);
   PD11 : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 11);
   PD12 : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 12);
   PD13 : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 13);
   PD14 : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 14);
   PD15 : aliased GPIO_Line (Controller => GPIOD'Access, Identifier => 15);

   --  GPIO E

   PE0  : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 0);
   PE1  : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 1);
   PE2  : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 2);
   PE3  : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 3);
   PE4  : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 4);
   PE5  : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 5);
   PE6  : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 6);
   PE7  : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 7);
   PE8  : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 8);
   PE9  : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 9);
   PE10 : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 10);
   PE11 : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 11);
   PE12 : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 12);
   PE13 : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 13);
   PE14 : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 14);
   PE15 : aliased GPIO_Line (Controller => GPIOE'Access, Identifier => 15);

   --  GPIO F

   PF0  : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 0);
   PF1  : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 1);
   PF2  : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 2);
   PF3  : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 3);
   PF4  : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 4);
   PF5  : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 5);
   PF6  : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 6);
   PF7  : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 7);
   PF8  : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 8);
   PF9  : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 9);
   PF10 : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 10);
   PF11 : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 11);
   PF12 : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 12);
   PF13 : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 13);
   PF14 : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 14);
   PF15 : aliased GPIO_Line (Controller => GPIOF'Access, Identifier => 15);

   --  GPIO G

   PG0  : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 0);
   PG1  : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 1);
   PG2  : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 2);
   PG3  : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 3);
   PG4  : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 4);
   PG5  : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 5);
   PG6  : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 6);
   PG7  : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 7);
   PG8  : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 8);
   PG9  : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 9);
   PG10 : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 10);
   PG11 : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 11);
   PG12 : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 12);
   PG13 : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 13);
   PG14 : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 14);
   PG15 : aliased GPIO_Line (Controller => GPIOG'Access, Identifier => 15);

   --  GPIO H

   PH0  : aliased GPIO_Line (Controller => GPIOH'Access, Identifier => 0);
   PH1  : aliased GPIO_Line (Controller => GPIOH'Access, Identifier => 1);

private

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

end A0B.STM32H723.GPIO;
