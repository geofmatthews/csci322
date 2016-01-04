with Ada.Integer_Text_Io, Ada.Text_Io ;
use Ada.Integer_Text_Io, Ada.Text_Io ;

procedure Taskdemo01 is

   task type A_Type is
      entry input(NN : in Integer);
      entry output(NN : out Integer);
   end A_Type ;
   task type B_Type  is
   end B_Type ;

   A : A_Type;
   B : B_Type;
   
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
	 NN := N*N;
      end output;
   end;
   
   task body B_Type is
      N : Integer;
   begin
      delay 0.1;
      A.input(5);
      A.output(N);
      Put(N);
      New_Line;
   end;      

begin
   null;
end Taskdemo01;
   
