with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure Traffic_Signal is

   type Light_Color is (Red, Yellow, Green);
   type Traffic_Light is record
      Red_Pin    : Integer;
      Yellow_Pin : Integer;
      Green_Pin  : Integer;
   end record;

   -- Time durations
   Yellow_Signal_Time : constant Time_Span := Seconds(5);
   All_Red_Time       : constant Time_Span := Seconds(2);
   Green_Signal_Time  : Time_Span;

   -- Traffic lights
   Light1, Light2 : Traffic_Light;

   procedure Set_Light (L : in Traffic_Light; Color : in Light_Color) is
   begin
      -- Set the traffic light to the specified color
      -- This is where you would set the GPIO pins
      case Color is
         when Red    => Put_Line("Light " & Integer'Image(L.Red_Pin) & ": Red");
         when Yellow => Put_Line("Light " & Integer'Image(L.Yellow_Pin) & ": Yellow");
         when Green  => Put_Line("Light " & Integer'Image(L.Green_Pin) & ": Green");
      end case;
   end Set_Light;

   procedure Cycle_Lights is
      Next_Time : Time;
   begin
      loop
         -- Set Light1 to Green and Light2 to Red
         Set_Light(Light1, Green);
         Set_Light(Light2, Red);
         Next_Time := Clock + Green_Signal_Time;
         delay until Next_Time;

         -- Set Light1 to Yellow
         Set_Light(Light1, Yellow);
         Next_Time := Clock + Yellow_Signal_Time;
         delay until Next_Time;

         -- Set Light1 to Red
         Set_Light(Light1, Red);
         Next_Time := Clock + All_Red_Time;
         delay until Next_Time;

         -- Set Light2 to Green
         Set_Light(Light2, Green);
         Next_Time := Clock + Green_Signal_Time;
         delay until Next_Time;

         -- Set Light2 to Yellow
         Set_Light(Light2, Yellow);
         Next_Time := Clock + Yellow_Signal_Time;
         delay until Next_Time;

         -- Set Light2 to Red
         Set_Light(Light2, Red);
         Next_Time := Clock + All_Red_Time;
         delay until Next_Time;
      end loop;
   end Cycle_Lights;

begin
   -- Get GPIO pin numbers from the user
   Put("Enter GPIO pin number for Light1 Red: ");
   Get(Light1.Red_Pin);
   Put("Enter GPIO pin number for Light1 Yellow: ");
   Get(Light1.Yellow_Pin);
   Put("Enter GPIO pin number for Light1 Green: ");
   Get(Light1.Green_Pin);

   Put("Enter GPIO pin number for Light2 Red: ");
   Get(Light2.Red_Pin);
   Put("Enter GPIO pin number for Light2 Yellow: ");
   Get(Light2.Yellow_Pin);
   Put("Enter GPIO pin number for Light2 Green: ");
   Get(Light2.Green_Pin);

   -- Get green signal time in seconds
   Put("Enter green signal time in seconds: ");
   declare
      Temp : Float;
   begin
      Get(Temp);
      Green_Signal_Time := Seconds(Temp);
   end;

   -- Start the traffic light cycle
   Cycle_Lights;
end Traffic_Signal;
