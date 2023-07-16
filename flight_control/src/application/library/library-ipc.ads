with Interfaces;

with Drivers.Ethernet;

package Library.Ipc is

   type Ipc_Type is tagged private;

   type Ipc_Access_Type is access Ipc_Type'Class;

   type Payload_Index_Type is mod 2**10;
   for Payload_Index_Type'Size use 10;
   Payload_Index_Default : constant Payload_Index_Type := 0;

   type Payload_Array_Type is
     array (Payload_Index_Type) of Interfaces.Unsigned_8;
   for Payload_Array_Type'Size use 8_192;
   Payload_Array_Default : constant Payload_Array_Type :=
     Payload_Array_Type'(others => 0);

   Ipc_Address : Drivers.Ethernet.Address_V4_Type :=
     Drivers.Ethernet.Address_V4_Type'(1 => 127, 2 => 0, 3 => 0, 4 => 1);

   procedure Initialize
     (This : in out Ipc_Type; Ethernet : Drivers.Ethernet.Ethernet_Access_Type;
      Port :        Drivers.Ethernet.Port_Type);

   procedure Write
     (This : in out Ipc_Type; Payload_Array : Payload_Array_Type);

   procedure Read
     (This : in out Ipc_Type; Payload_Array : out Payload_Array_Type);

private

   type Ipc_Type is tagged record
      Ethernet : Drivers.Ethernet.Ethernet_Access_Type;
      Port     : Drivers.Ethernet.Port_Type;
   end record;

end Library.Ipc;
