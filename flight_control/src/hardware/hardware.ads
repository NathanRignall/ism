with Drivers.Ethernet;
with Drivers.Imu;

with Types.Schedule;

package Hardware is

   Ipc_Core : Drivers.Ethernet.Ethernet_Access_Type :=
     new Drivers.Ethernet.Ethernet_Type;
   Edge     : Drivers.Ethernet.Ethernet_Access_Type :=
     new Drivers.Ethernet.Ethernet_Type;
   Radio    : Drivers.Ethernet.Ethernet_Access_Type :=
     new Drivers.Ethernet.Ethernet_Type;
   Cloud    : Drivers.Ethernet.Ethernet_Access_Type :=
     new Drivers.Ethernet.Ethernet_Type;

   Imu : Drivers.Imu.Imu_Access_Type := new Drivers.Imu.Imu_Type;

   procedure Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type);

end Hardware;
