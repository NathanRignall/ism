with Ada.Environment_Variables;
with Ada.Text_IO;

package body Hardware is

   procedure Initialize is

      -- load the environment variables
      Ipc_Core_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("IPC_CORE_RX_PORT");

      Edge_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("EDGE_RX_PORT");

      Radio_Multicast_Address_Env : constant String :=
        Ada.Environment_Variables.Value ("RADIO_MULTICAST_ADDRESS");

      Radio_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("RADIO_RX_PORT");

      CLoud_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("CLOUD_RX_PORT");

      -- create the addreseses and ports for the interfaces
      Edge_Rx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Edge_Rx_Port_Env);

      Ipc_Core_Rx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Ipc_Core_Rx_Port_Env);

      Radio_Multicast_Address :
        constant Drivers.Ethernet.Address_V4_Access_Type :=
        new Drivers.Ethernet.Address_V4_Type'
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

      Cloud_Rx_Port : constant Drivers.Ethernet.Port_Type :=
        Drivers.Ethernet.Port_Type'Value (Cloud_Rx_Port_Env);

      -- create the addresses for the interfaces
      Interface_Address : constant Drivers.Ethernet.Address_V4_Type :=
        Drivers.Ethernet.Address_V4_Type'(0, 0, 0, 0);

   begin

      Ada.Text_IO.Put_Line ("Hardware Initialize");

      -- initialize the interfaces
      Ipc_Core.Initialize (Interface_Address, Ipc_Core_Rx_Port);
      Edge.Initialize (Interface_Address, Edge_Rx_Port);
      Radio.Initialize (Interface_Address, Radio_Rx_Port);
      Cloud.Initialize (Interface_Address, Cloud_Rx_Port);

      -- join the multicast group for the radio
      Radio.Join_Multicast_Group (Radio_Multicast_Address.all);
  
      -- initialize the aditional drivers
      Imu.Initialize (Edge);

   end Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type) is
   begin

      Imu.Schedule (Cycle);

   end Schedule;

end Hardware;
