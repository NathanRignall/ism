with Library.Network;
with Library.Telemetry;
with Library.Ipc;

with Types.Schedule;

package Application is

   Network   : Library.Network.Network_Access_Type := new Library.Network.Network_Type;
   Telemetry : Library.Telemetry.Telemetry_Access_Type := new Library.Telemetry.Telemetry_Type;
   Core_Ipc  : Library.Ipc.Ipc_Access_Type := new Library.Ipc.Ipc_Type;

   procedure Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type);

end Application;


-- planning
-- the flight control processes and produces an output for the motor (for example)
-- this output is put in a shared buffer
-- the oposite flight computer checks the shared buffer if they match
-- if they match the message is sent (if master)
-- if they do not match processes is reset

-- ipc / buffer replacement
-- buffer has write and read
