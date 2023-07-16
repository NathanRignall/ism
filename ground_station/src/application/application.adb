with Ada.Environment_Variables;

with Application.State;

with Hardware;
with Drivers.Ethernet;

with Types.Physics;

package body Application is

   procedure Initialize is

      -- load the environment variables
      Device_Identifier_Env : constant String :=
        Ada.Environment_Variables.Value ("DEVICE_IDENTIFIER");

      Radio_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("RADIO_RX_PORT");

      Radio_Tx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("RADIO_TX_PORT");

      CLoud_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("CLOUD_RX_PORT");

      Cloud_Tx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("CLOUD_TX_PORT");

      -- create the addreseses and ports for the interfaces
      Device_Identifier : constant Library.Network.Device_Identifier_Type :=
        Library.Network.Device_Identifier_Type'Value (Device_Identifier_Env);

      Radio_Multicast_Address : constant Drivers.Ethernet.Address_V4_Type :=
        Drivers.Ethernet.Address_V4_Type'(127, 0, 0, 1);

      Radio_Rx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Radio_Rx_Port_Env);

      Radio_Tx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Radio_Tx_Port_Env);

      Cloud_Server_Address : constant Drivers.Ethernet.Address_V4_Type :=
        Drivers.Ethernet.Address_V4_Type'(127, 0, 0, 1);

      Cloud_Rx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (CLoud_Rx_Port_Env);

      Cloud_Tx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Cloud_Tx_Port_Env);

   begin

      -- initialize the applications
      Network.Initialize
        (Device_Identifier       => Device_Identifier, Radio => Hardware.Radio,
         Cloud                   => Hardware.Cloud,
         Radio_Multicast_Address => Radio_Multicast_Address,
         Radio_Tx_Port           => Radio_Tx_Port,
         Cloud_Server_Address    => Cloud_Server_Address,
         Cloud_Tx_Port           => Cloud_Tx_Port);

      Telemetry.Initialize (Network => Network);

   end Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type) is
   begin

      -- schedule the applications
      Network.Schedule (Cycle);
      Telemetry.Schedule (Cycle);

   end Schedule;

end Application;
