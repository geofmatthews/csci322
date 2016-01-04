with Ada.Integer_Text_Io, Ada.Text_Io ;
use Ada.Integer_Text_Io, Ada.Text_Io ;

procedure Taskdemo00 is

   task type A_Type(localN : Integer) is
      entry Get(NN : in Integer);
   end A_Type ;
   task type B_Type  is
      entry Get(NN : in Integer);
   end B_Type ;

   A : A_Type(10);
   B : B_Type;
   
   task body A_Type is
      N : Integer;
   begin
      accept Get(NN : in integer) do
	 N := NN;
      end Get;
      Put(N);
      New_Line;
      delay 0.1;
      B.Get(localN*N);
   end;
   
   task body B_Type is
      N : Integer;
   begin
      delay 0.1;
      A.Get(5);
      accept Get(NN : in integer) do
	 N := NN;
      end Get;
      Put(N);
      New_Line;
   end;      

begin
   null;
end Taskdemo00;
   
