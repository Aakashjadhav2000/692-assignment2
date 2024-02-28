with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces.C; use Interfaces.C;

procedure Traffic_Light_Control is

   type File_Descriptor is new Interfaces.C.int;
   type Path_String is array(Positive range <>) of Character;
   type State_String is array(Positive range <>) of Character;

   MAX_PATH_LENGTH : constant := 50;
   BASE_PATH : constant := "/sys/class/gpio/gpio";
   
   LED1_RED_PATH, LED1_YELLOW_PATH, LED1_GREEN_PATH,
   LED2_RED_PATH, LED2_YELLOW_PATH, LED2_GREEN_PATH : Path_String(MAX_PATH_LENGTH);
   
   function To_C_String is new Ada.Unchecked_Conversion(String, Path_String);

   procedure Set_GPIO_Value(Path : Path_String; Value : State_String) is
      Fd : File_Descriptor;
      Len : Interfaces.C.size_t := Path'Length;
      Val : String := Value & ASCII.CR; -- Adding Carriage Return since you might need it for the write operation
   begin
      Fd := Open(Path(1..Len), O_WRONLY);
      if Fd >= 0 then
         declare
            use Interfaces.C;
         begin
            Write(Fd, Val(1)'Address, Val'Length);
            Close(Fd);
         end;
      else
         Put_Line("Error opening file: " & Path(1..Len));
      end if;
   end Set_GPIO_Value;

   procedure Set_GPIO_Direction_Output(Path : Path_String; Dir : String) is
      Fd : File_Descriptor;
      Len : Interfaces.C.size_t := Path'Length;
   begin
      Fd := Open(Path(1..Len), O_WRONLY);
      if Fd >= 0 then
         declare
            use Interfaces.C;
         begin
            Write(Fd, Dir & ASCII.CR);
            Close(Fd);
         end;
      else
         Put_Line("Error opening file: " & Path(1..Len));
      end if;
   end Set_GPIO_Direction_Output;

   procedure Configure_Traffic_Light(Red_Path, Yellow_Path, Green_Path : Path_String; State : State_String) is
   begin
      Set_GPIO_Value(Red_Path, State);
      Set_GPIO_Value(Yellow_Path, State);
      Set_GPIO_Value(Green_Path, State);
   end Configure_Traffic_Light;

   -- Main procedure
   procedure Main is
      Green_Sig_Time : Float;
      Yellow_Sig_Time : constant := 5.0; -- Yellow signal time in seconds
      All_Red_Time : constant := 2.0;    -- All red time in seconds
      Pin_Number : Interfaces.C.uint32_t;
   begin
      Put_Line("Enter GPIO pin number for LED1 Red: ");
      Get(Pin_Number);
      LED1_RED_PATH := To_C_String(BASE_PATH & Integer'Image(Pin_Number) & "/value");

      Put_Line("Enter GPIO pin number for LED1 Yellow: ");
      Get(Pin_Number);
      LED1_YELLOW_PATH := To_C_String(BASE_PATH & Integer'Image(Pin_Number) & "/value");

      Put_Line("Enter GPIO pin number for LED1 Green: ");
      Get(Pin_Number);
      LED1_GREEN_PATH := To_C_String(BASE_PATH & Integer'Image(Pin_Number) & "/value");

      Put_Line("Enter GPIO pin number for LED2 Red: ");
      Get(Pin_Number);
      LED2_RED_PATH := To_C_String(BASE_PATH & Integer'Image(Pin_Number) & "/value");

      Put_Line("Enter GPIO pin number for LED2 Yellow: ");
      Get(Pin_Number);
      LED2_YELLOW_PATH := To_C_String(BASE_PATH & Integer'Image(Pin_Number) & "/value");

      Put_Line("Enter GPIO pin number for LED2 Green: ");
      Get(Pin_Number);
      LED2_GREEN_PATH := To_C_String(BASE_PATH & Integer'Image(Pin_Number) & "/value");

      Put_Line("Enter green signal time in minutes (can be a fraction): ");
      Get(Green_Sig_Time);

      -- Set the directions of the GPIO pins for the LEDs
      Set_GPIO_Direction_Output(LED1_RED_PATH, "out");
      Set_GPIO_Direction_Output(LED1_YELLOW_PATH, "out");
      Set_GPIO_Direction_Output(LED1_GREEN_PATH, "out");
      Set_GPIO_Direction_Output(LED2_RED_PATH, "out");
      Set_GPIO_Direction_Output(LED2_YELLOW_PATH, "out");
      Set_GPIO_Direction_Output(LED2_GREEN_PATH, "out");

      loop
         -- Set the first traffic light to green and the second to red
         Configure_Traffic_Light(LED1_RED_PATH, LED1_YELLOW_PATH, LED1_GREEN_PATH, "001");
         Configure_Traffic_Light(LED2_RED_PATH, LED2_YELLOW_PATH, LED2_GREEN_PATH, "100");

         delay (Milliseconds => Integer(Green_Sig_Time * 60.0 * 1000.0)); -- Wait for green signal time

         -- Set the first traffic light to yellow
         Configure_Traffic_Light(LED1_RED_PATH, LED1_YELLOW_PATH, LED1_GREEN_PATH, "010");

         delay (Milliseconds => Integer(Yellow_Sig_Time * 1000.0)); -- Wait for 5 seconds

         -- Set the first traffic light to red
         Configure_Traffic_Light(LED1_RED_PATH, LED1_YELLOW_PATH, LED1_GREEN_PATH, "100");

         delay (Milliseconds => Integer(All_Red_Time * 1000.0)); -- Wait for 2 seconds with both sides red

         -- Set the second traffic light to green
         Configure_Traffic_Light(LED2_RED_PATH, LED2_YELLOW_PATH, LED2_GREEN_PATH, "001");

         delay (Milliseconds => Integer(Green_Sig_Time * 60.0 * 1000.0)); -- Wait for green signal time

         -- Set the second traffic light to yellow
         Configure_Traffic_Light(LED2_RED_PATH, LED2_YELLOW_PATH, LED2_GREEN_PATH, "010");

         delay (Milliseconds => Integer(Yellow_Sig_Time * 1000.0)); -- Wait for 5 seconds

         -- Set the second traffic light to red
         Configure_Traffic_Light(LED2_RED_PATH, LED2_YELLOW_PATH, LED2_GREEN_PATH, "100");

         delay (Milliseconds => Integer(All_Red_Time * 1000.0)); -- Wait for 2 seconds with both sides red
      end loop;
   end Main;

begin
   Main;
end Traffic_Light_Control;
