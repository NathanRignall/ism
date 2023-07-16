with Ada.Text_IO;
with Interfaces;

use type Interfaces.Unsigned_8;

package body Application.Control is

   procedure Initialize is
   begin

      Ada.Text_IO.Put_Line ("Control Initialize");

   end Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type) is

      Write_Payload_Array : Library.Ipc.Payload_Array_Type := ( 0 => 100, others => 0);

      Read_Payload_Array : Library.Ipc.Payload_Array_Type := ( others => 0);

   begin

      case Cycle is

         when Types.Schedule.S_200ms =>
            Application.Core_Ipc.Write(Payload_Array => Write_Payload_Array);

            delay 0.1;

            Application.Core_Ipc.Read(Payload_Array => Read_Payload_Array);

            if Write_Payload_Array(0) = Read_Payload_Array(0) then
               Ada.Text_IO.Put_Line("Match");
            else 
               Ada.Text_IO.Put_Line("NO Match");
            end if;

         when others =>
            null;
            
      end case;

   end Schedule;

end Application.Control;
