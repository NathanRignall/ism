with Ada.Environment_Variables;
with Ada.Text_IO;

package body Hardware is

   procedure Initialize is

      -- load the environment variables
      Radio_Multicast_Address_Env : constant String :=
        Ada.Environment_Variables.Value ("RADIO_MULTICAST_ADDRESS");

      Ipc_Core_Rx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("IPC_CORE_RX_PORT");

      Ipc_Core_Tx_Port_Env : constant String :=
        Ada.Environment_Variables.Value ("IPC_CORE_TX_PORT");

      -- create the addresses using the environment variables
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

      -- create the addresses for the interfaces
      Interface_Address : constant Drivers.Ethernet.Address_V4_Type :=
        Drivers.Ethernet.Address_V4_Type'(0, 0, 0, 0);

   begin

      Ada.Text_IO.Put_Line ("Hardware Initialize");

      -- initialize the interfaces
      Ipc_Core.Initialize (Interface_Address, 4_001);
      Edge.Initialize (Interface_Address, 4_002);
      Radio.Initialize (Interface_Address, 4_003);
      Cloud.Initialize (Interface_Address, 4_004);

      Imu.Initialize (Edge);

      -- join the multicast group for the radio
      --  Radio.Join_Multicast_Group (Radio_Multicast_Address.all);

   end Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type) is
   begin

      Imu.Schedule (Cycle);

      case Cycle is

         when Types.Schedule.S_200ms =>
            Ada.Text_IO.Put_Line ("Hardware Schedule 200ms");

         when others =>
            null;

      end case;

   end Schedule;

end Hardware;
