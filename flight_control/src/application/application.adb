with Ada.Environment_Variables;

with Application.Control;
with Application.Estimation;
with Application.State;

with Hardware;
with Drivers.Ethernet;

with Types.Physics;

package body Application is

   procedure Initialize is

      -- load the environment variables
      Device_Identifier_Env : constant String :=
        Ada.Environment_Variables.Value ("DEVICE_IDENTIFIER");

      Latitude_Env : constant String :=
        Ada.Environment_Variables.Value ("INITIAL_LATITUDE");

      Longitude_Env : constant String :=
        Ada.Environment_Variables.Value ("INITIAL_LONGITUDE");

      Velocity_X_Env : constant String :=
        Ada.Environment_Variables.Value ("INITIAL_VELOCITY_X");

      Velocity_Y_Env : constant String :=
        Ada.Environment_Variables.Value ("INITIAL_VELOCITY_Y");

      Ipc_Core_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("IPC_CORE_RX_PORT");

      Ipc_Core_Tx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("IPC_CORE_TX_PORT");

      Edge_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("EDGE_RX_PORT");

      Edge_Tx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("EDGE_TX_PORT");

      Radio_Multicast_Address_Env : constant String :=
        Ada.Environment_Variables.Value ("RADIO_MULTICAST_ADDRESS");

      Radio_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("RADIO_RX_PORT");

      Radio_Tx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("RADIO_TX_PORT");

      Cloud_Server_Address_Env : constant String :=
        Ada.Environment_Variables.Value ("CLOUD_SERVER_ADDRESS");

      CLoud_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("CLOUD_RX_PORT");

      Cloud_Tx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("CLOUD_TX_PORT");

      -- create the addreseses and ports for the interfaces
      Device_Identifier : constant Library.Network.Device_Identifier_Type :=
        Library.Network.Device_Identifier_Type'Value (Device_Identifier_Env);

      Ipc_Core_Rx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Ipc_Core_Rx_Port_Env);

      Ipc_Core_Tx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Ipc_Core_Tx_Port_Env);

      Radio_Multicast_Address : constant Drivers.Ethernet.Address_V4_Type :=
        Drivers.Ethernet.Address_V4_Type'
          ((1 =>
              Drivers.Ethernet.Address_Octet_Type'Value
                (Radio_Multicast_Address_Env (1 .. 3)),
            2 =>
              Drivers.Ethernet.Address_Octet_Type'Value
                (Radio_Multicast_Address_Env (5 .. 7)),
            3 =>
              Drivers.Ethernet.Address_Octet_Type'Value
                (Radio_Multicast_Address_Env (9 .. 11)),
            4 =>
              Drivers.Ethernet.Address_Octet_Type'Value
                (Radio_Multicast_Address_Env (13 .. 15))));

      Radio_Rx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Radio_Rx_Port_Env);

      Radio_Tx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Radio_Tx_Port_Env);

      Cloud_Server_Address : constant Drivers.Ethernet.Address_V4_Type :=
        Drivers.Ethernet.Address_V4_Type'
          ((1 =>
              Drivers.Ethernet.Address_Octet_Type'Value
                (Cloud_Server_Address_Env (1 .. 3)),
            2 =>
              Drivers.Ethernet.Address_Octet_Type'Value
                (Cloud_Server_Address_Env (5 .. 7)),
            3 =>
              Drivers.Ethernet.Address_Octet_Type'Value
                (Cloud_Server_Address_Env (9 .. 11)),
            4 =>
              Drivers.Ethernet.Address_Octet_Type'Value
                (Cloud_Server_Address_Env (13 .. 15))));

      Cloud_Rx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (CLoud_Rx_Port_Env);

      Cloud_Tx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Cloud_Tx_Port_Env);

   begin

      -- load the initial values on the state
      Application.State.Core_State.Physical_State.Position.Latitude :=
        Types.Physics.Latitude_Type'Value (Latitude_Env);

      Application.State.Core_State.Physical_State.Position.Longitude :=
        Types.Physics.Longitude_Type'Value (Longitude_Env);

      Application.State.Core_State.Physical_State.Velocity_Vector
        (Types.Physics.X) :=
        Types.Physics.Velocity_Type'Value (Velocity_X_Env);

      Application.State.Core_State.Physical_State.Velocity_Vector
        (Types.Physics.Y) :=
        Types.Physics.Velocity_Type'Value (Velocity_Y_Env);

      -- initialize demo applications
      Application.Estimation.Initialize;
      Application.Control.Initialize;

      Core_Ipc.Initialize
        (Ethernet => Hardware.Ipc_Core, Port => Ipc_Core_Tx_Port);

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

      -- schedule demo applications
      Estimation.Schedule (Cycle);
      --  Control.Schedule (Cycle);

      -- schedule the applications
      --  Network.Schedule (Cycle);
      --  Telemetry.Schedule (Cycle);

   end Schedule;

end Application;
