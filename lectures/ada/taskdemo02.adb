with Ada.Integer_Text_Io, Ada.Text_Io ;
use Ada.Integer_Text_Io, Ada.Text_Io ;

procedure Taskdemo02 is

   task type A_Type(LocalN : Integer) is
      entry input(NN : in Integer);
      entry output(NN : out Integer);
   end A_Type ;
   type A_Access is access all A_Type;
   
   task type B_Type(Server : A_Access) is
   end B_Type ;

   A : aliased A_Type(10);
   B : B_Type(A'Access);
   
   task body A_Type is
      N : Integer;
   begin
      accept input(NN : in integer) do
	 N := NN;
      end input;
      Put(N);
      New_Line;
      delay 0.1;
      accept output(NN : out Integer) do
	 NN := localN*N;
      end output;
   end;
   
   task body B_Type is
      N : Integer;
   begin
      delay 0.1;
      Server.input(5);
      Server.output(N);
      Put(N);
      New_Line;
   end;      

begin
   null;
end Taskdemo02;
   
