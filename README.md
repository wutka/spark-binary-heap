# SPARK Ada Binary Heap

This library implements a binary heap in two ways:

The `Simple_Binary_Heap` package implements a binary heap
where the items stored in the heap have a weight and are
compared with the supplied "<" function.

The `Binary_Heap` package implements a binary heap where
items and their weights are separated and you supply the
type for each. The reason for this form is that otherwise
if you were doing something like a shortest distance algorithm,
you would likely want to store a location and a weight, but
not necessarily have to creat your own record that contained
both the location and the weight. `Binary_Heap` takes care
of that for you.

Here is an example program using both of these:
```ada
with Simple_Binary_Heap;
with Binary_Heap;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

procedure Binheap_Test is
   --  Create a simple binary heap for storing integers
   package IntBinheap is new Simple_Binary_Heap (
      Element_Type => Integer,
      Default_Element  => 0,
      "<" => "<");

   --  Create a record to store
   type Coord_Type is record
      X : Integer := 0;
      Y : Integer := 0;
   end record;

   --   Create a binary heap for storing coordinates with
   --   integer weights.
   package Coordheap is new Binary_Heap (
      Element_Type => Coord_Type,
      Default_Element => (0, 0),
      Weight_Type => Integer,
      Default_Weight => 0,
      "<" => "<");

   Heap : IntBinheap.Binary_Heap (1000);
   Coord_Heap : Coordheap.Binary_Heap (1000);
   X : Integer;
   Coord : Coord_Type;
   Coord_Weight : Integer;
begin
   --  Insert some values in to the int heap
   IntBinheap.Insert (Heap, 10);
   IntBinheap.Insert (Heap, 5);
   IntBinheap.Insert (Heap, 2);
   IntBinheap.Insert (Heap, 1);

   --  Pull off the first value and print it
   IntBinheap.Top (Heap, X);
   Put (X);
   New_Line;

   --  Insert some more values
   IntBinheap.Insert (Heap, 7);
   IntBinheap.Insert (Heap, 3);

   --  Print everything in the heap
   while not IntBinheap.Is_Empty (Heap) loop
      IntBinheap.Top (Heap, X);
      Put (X);
      New_Line;
   end loop;

   --  Insert some coordinates and weights into the coord heap
   Coordheap.Insert (Coord_Heap, (1, 2), 10);
   Coordheap.Insert (Coord_Heap, (3, 4), 1);
   Coordheap.Insert (Coord_Heap, (1, 1), 7);
   Coordheap.Insert (Coord_Heap, (19, 20), 6);
   Coordheap.Insert (Coord_Heap, (2, 2), 2);

   --  Print their values and weights
   while not Coordheap.Is_Empty (Coord_Heap) loop
      Coordheap.Top (Coord_Heap, Coord, Coord_Weight);
      Put ("(");
      Put (Coord.X);
      Put (",");
      Put (Coord.Y);
      Put (") = ");
      Put (Coord_Weight);
      New_Line;
   end loop;
exception
   when others => Put_Line ("Exception");

end Binheap_Test;
```
