with Ada.Environment_Variables;
with Ada.Text_IO;

package body Hardware is

   procedure Initialize is

      -- load the environment variables

      Radio_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("RADIO_RX_PORT");

      Cloud_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("CLOUD_RX_PORT");

      -- create the addreseses and ports for the interfaces
      Radio_Rx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Radio_Rx_Port_Env);

      Cloud_Rx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Cloud_Rx_Port_Env);

      -- create the addresses for the interfaces
      Interface_Address : constant Drivers.Ethernet.Address_V4_Type :=
        Drivers.Ethernet.Address_V4_Type'(0, 0, 0, 0);

   begin

      Ada.Text_IO.Put_Line ("Hardware Initialize");

      -- initialize the interfaces
      Radio.Initialize (Interface_Address, Radio_Rx_Port);
      Cloud.Initialize (Interface_Address, Cloud_Rx_Port);

   end Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type) is
   begin

      null;

   end Schedule;

end Hardware;
