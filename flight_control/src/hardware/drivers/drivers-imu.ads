with Interfaces;

with Drivers.Ethernet;

with Types.Schedule;

package Drivers.Imu is

   type Packet_Type is
     record
         Acceleration_X : Interfaces.Unsigned_16;
         Acceleration_Y : Interfaces.Unsigned_16;
         Acceleration_Z : Interfaces.Unsigned_16;
     end record;
   for Packet_Type use
     record
         Acceleration_X at 0 range 0 .. 15;
         Acceleration_Y at 0 range 16 .. 31;
         Acceleration_Z at 0 range 32 .. 47;
     end record;
   for Packet_Type'Size use 48;

   type Imu_Type is tagged private;

   type Imu_Access_Type is access Imu_Type'Class;

   procedure Initialize
     (This : in out Imu_Type; Ethernet : Drivers.Ethernet.Ethernet_Access_Type);

   procedure Schedule (This : in out Imu_Type; Cycle : Types.Schedule.Cycle_Type);

private

   type Imu_Type is tagged record
      Ethernet : Drivers.Ethernet.Ethernet_Access_Type;
   end record;

end Drivers.Imu;
