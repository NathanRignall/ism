with Ada.Streams;
with Ada.Text_IO;

package body Library.Ipc is

   procedure Initialize (This : in out Ipc_Type; Ethernet : Drivers.Ethernet.Ethernet_Access_Type; Port : Drivers.Ethernet.Port_Type) is
   begin

      Ada.Text_IO.Put_Line ("Ipc Initialize");

      This.Ethernet := Ethernet;
      This.Port := Port;

   end Initialize;

   procedure Send_Packet (This : in out Ipc_Type; Payload_Array: Payload_Array_Type ) is 
      
      Last : Ada.Streams.Stream_Element_Offset;

      New_Data : Ada.Streams.Stream_Element_Array (1 .. 1_024);
      for New_Data'Address use Payload_Array'Address;

   begin

      This.Ethernet.Send(Address => Ipc_Address, Port => This.Port, Data => New_Data, Last => Last);

   end Send_Packet;

   procedure Receive_Packet (This : in out Ipc_Type; Payload_Array: out Payload_Array_Type; Source_Address_Port : out Drivers.Ethernet.Address_Port_Type) is

      Data : Ada.Streams.Stream_Element_Array (1 .. 1_024) := (others => 0);
      Last : Ada.Streams.Stream_Element_Offset;

      New_Payload_Array : Payload_Array_Type;
      for New_Payload_Array'Address use Data'Address;

   begin

      This.Ethernet.Receive(Source_Address_Port.Address, Source_Address_Port.Port, Data, Last);

   end Receive_Packet;

   procedure Write (This : in out Ipc_Type; Payload_Array: Payload_Array_Type) is
   begin

      Send_Packet(This, Payload_Array);

   end Write;

   procedure Read (This : in out Ipc_Type; Payload_Array: out Payload_Array_Type) is

      Source_Address_Port : Drivers.Ethernet.Address_Port_Type;

   begin

      -- loop for a maximum of 32 times
      for Index in 1 .. 32 loop

         -- check if there is a new packet
         if This.Ethernet.Is_New_Data then

            -- get the packet
            Receive_Packet (This, Payload_Array, Source_Address_Port);

            Ada.Text_IO.Put_Line("Data");
            
         else

            -- exit the loop as there is no new data
            exit;

         end if;

      end loop;

   end Read;

end Library.Ipc;
