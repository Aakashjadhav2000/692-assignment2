with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams; use Ada.Streams;

procedure Traffic_Light is
   subtype Path_Type is String(1 .. 50);

   BASE_PATH : constant String := "/sys/class/gpio/gpio";

   procedure Set_GPIO_Value(Path : Path_Type; Value : Character) is
      File : Stream_IO.File_Type;
      Data : Stream_Element_Array(1 .. 1) := (1 => Stream_Element'Val(Character'Pos(Value)));
   begin
      Stream_IO.Open(File, Stream_IO.Out_File, Path);
      Stream_IO.Write(File, Data);
      Stream_IO.Flush(File);
      Stream_IO.Close(File);
   end Set_GPIO_Value;

   procedure Set_GPIO_Direction_Output(GPIO_Pin : Path_Type; Dir : String) is
      File : Stream_IO.File_Type;
      Data : Stream_Element_Array(1 .. Dir'Length) := (others => Stream_Element'Val(Character'Pos(' ')));
   begin
      for I in 1 .. Dir'Length loop
         Data(I) := Stream_Element'Val(Character'Pos(Dir(I)));
      end loop;

      Stream_IO.Open(File, Stream_IO.Out_File, GPIO_Pin & "/direction");
      Stream_IO.Write(File, Data);
      Stream_IO.Flush(File);
      Stream_IO.Close(File);
   end Set_GPIO_Direction_Output;

   LED1_Red_Path, LED1_Yellow_Path, LED1_Green_Path : Path_Type;
   LED2_Red_Path, LED2_Yellow_Path, LED2_Green_Path : Path_Type;
   Green_Sig_Time : Float := 1.0; -- Default green signal time in minutes
   Yellow_Sig_Time : constant Natural := 5; -- Yellow signal time in seconds
   All_Red_Time : constant Natural := 2; -- All red time in seconds
   Pin_Number : Natural;
begin
   -- Get GPIO pin numbers from the user and construct the paths
   Put("Enter GPIO pin number for LED1 Red: ");
   Get(Pin_Number);
   LED1_Red_Path := BASE_PATH & Integer'Image(Pin_Number) & "/value";

   -- Repeat for other LEDs...

   -- Set the directions of the GPIO pins for the LEDs
   Set_GPIO_Direction_Output(LED1_Red_Path(1 .. LED1_Red_Path'Last - 6), "out");
   -- Repeat for other LEDs...

   -- Main loop
   loop
      -- Set the first traffic light to green and the second to red
      Set_GPIO_Value(LED1_Red_Path, '0');
      Set_GPIO_Value(LED1_Yellow_Path, '0');
      Set_GPIO_Value(LED1_Green_Path, '1');
      Set_GPIO_Value(LED2_Red_Path, '1');
      Set_GPIO_Value(LED2_Yellow_Path, '0');
      Set_GPIO_Value(LED2_Green_Path, '0');
      delay Duration(Green_Sig_Time * 60.0);

      -- Set the first traffic light to yellow
      Set_GPIO_Value(LED1_Red_Path, '0');
      Set_GPIO_Value(LED1_Yellow_Path, '1');
      Set_GPIO_Value(LED1_Green_Path, '0');
      delay Duration(Yellow_Sig_Time);

      -- Set the first traffic light to red
      Set_GPIO_Value(LED1_Red_Path, '1');
      Set_GPIO_Value(LED1_Yellow_Path, '0');
      Set_GPIO_Value(LED1_Green_Path, '0');

      -- Set the second traffic light to red and yellow
      Set_GPIO_Value(LED2_Red_Path, '1');
      Set_GPIO_Value(LED2_Yellow_Path, '1');
      Set_GPIO_Value(LED2_Green_Path, '0');
      delay Duration(All_Red_Time);

      -- Set the second traffic light to green
      Set_GPIO_Value(LED2_Red_Path, '0');
      Set_GPIO_Value(LED2_Yellow_Path, '0');
      Set_GPIO_Value(LED2_Green_Path, '1');
      delay Duration(Green_Sig_Time * 60.0);

      -- Set the second traffic light to yellow
      Set_GPIO_Value(LED2_Red_Path, '0');
      Set_GPIO_Value(LED2_Yellow_Path, '1');
      Set_GPIO_Value(LED2_Green_Path, '0');
      delay Duration(Yellow_Sig_Time);

      -- Set the second traffic light to red
      Set_GPIO_Value(LED2_Red_Path, '1');
      Set_GPIO_Value(LED2_Yellow_Path, '0');
      Set_GPIO_Value(LED2_Green_Path, '0');

      -- Set the first traffic light to red and yellow
      Set_GPIO_Value(LED1_Red_Path, '1');
      Set_GPIO_Value(LED1_Yellow_Path, '1');
      Set_GPIO_Value(LED1_Green_Path, '0');
      delay Duration(All_Red_Time);
   end loop;
end Traffic_Light;