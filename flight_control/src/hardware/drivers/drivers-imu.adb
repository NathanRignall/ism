with Ada.Text_IO;
with Ada.Streams;

package body Drivers.Imu is

   procedure Receive_Packet (This : in out Imu_Type);

   procedure Initialize
     (This : in out Imu_Type; Ethernet : Drivers.Ethernet.Ethernet_Access_Type)
   is
   begin

      This.Ethernet := Ethernet;

   end Initialize;

   procedure Schedule
     (This : in out Imu_Type; Cycle : Types.Schedule.Cycle_Type)
   is
   begin

      case Cycle is

         when Types.Schedule.S_20ms =>

            if This.Ethernet.Is_New_Data then

               This.Receive_Packet;

            end if;

         when others =>
            null;

      end case;

   end Schedule;

   procedure Receive_Packet (This : in out Imu_Type) is

      Source_Address : Drivers.Ethernet.Address_V4_Type;
      Source_Port    : Drivers.Ethernet.Port_Type;

      Data : Ada.Streams.Stream_Element_Array (1 .. 1_031) := (others => 0);
      Last : Ada.Streams.Stream_Element_Offset;

      New_Packet : Packet_Type;
      for New_Packet'Address use Data'Address;

   begin

      -- get the packet from transport
      This.Ethernet.Receive (Source_Address, Source_Port, Data, Last);

      --  -- overlay the data on the packet
      Ada.Text_IO.Put_Line (New_Packet'Image);

   end Receive_Packet;

end Drivers.Imu;
