with Drivers.Ethernet;

with Types.Schedule;

package Hardware is

   Radio    : Drivers.Ethernet.Ethernet_Access_Type :=
     new Drivers.Ethernet.Ethernet_Type;
   Cloud    : Drivers.Ethernet.Ethernet_Access_Type :=
     new Drivers.Ethernet.Ethernet_Type;

   procedure Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type);

end Hardware;
