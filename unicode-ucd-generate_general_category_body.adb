------------------------------------------------------------------------------
--                                                                          --
--                             Unicode Utilities                            --
--                                                                          --
--                 Unicode Character Database (UCD) Facilities              --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai (ANNEXI-STRAYLINE)                                        --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--                                                                          --
--      * Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--                                                                          --
--      * Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--                                                                          --
--      * Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     --
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A --
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT      --
--  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT        --
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,   --
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY   --
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT     --
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE   --
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.    --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Ada.Streams.Stream_IO;

with Hex;
with Hex.Unsigned_8;
with Hex.Unsigned_24;

with Unicode.General_Category;        use Unicode.General_Category;
with Unicode.General_Category.Alias;

procedure Unicode.UCD.Generate_General_Category_Body
  (UnicodeData_Path: in String := "UnicodeData.txt";
   Body_Path       : in String := "aura-unicode-general_category.adb")
is
   
   package SIO renames Ada.Streams.Stream_IO;
   package GC_Alias renames Unicode.General_Category.Alias;
   
   subtype Set_Case is Hex.Set_Case;
   use all type Set_Case;
   
   subtype General_Category_Type 
     is Unicode.General_Category.General_Category_Type;
   use all type General_Category_Type;
   
   -- Codepoint Value (0 - 16#10FFFF#, (up to FFFFFF is fine also in this 
   -- implementation)
   use type Hex.Unsigned_24.Unsigned_24;
   subtype Codepoint_Value is Hex.Unsigned_24.Unsigned_24;
   
   procedure Encode_Codepoint (Value   : in     Codepoint_Value;
                               Buffer  :    out String;
                               Use_Case: in     Set_Case := Upper_Case)
     renames Hex.Unsigned_24.Encode;
   
   
   -- Hash Key
   use type Hex.Unsigned_8.Unsigned_8;
   subtype Key_Hash is Hex.Unsigned_8.Unsigned_8 
     range 0 .. 16#10#;
   
   procedure Encode_Hash (Value   : in     Key_Hash;
                         Buffer  :    out String;
                         Use_Case: in     Set_Case := Upper_Case)
     renames Hex.Unsigned_8.Encode;
   
   
   -- Reference Hash Table ----------------------------------------------------
   type Bucket_Item;
   type Bucket_Item_Access is access Bucket_Item;
   
   type Codepoint_Range is
      record
         First: Codepoint_Value;
         Last : Codepoint_Value;
      end record;
   
   type Bucket_Item is
      record
         Codepoints: Codepoint_Range;
         Category  : General_Category_Type;
         
         Next      : Bucket_Item_Access := null;
      end record;
   
   procedure Free is new Ada.Unchecked_Deallocation 
        (Object => Bucket_Item,
         Name   => Bucket_Item_Access);
   
   
   type Bucket is
      record
         First, Last: Bucket_Item_Access := null;
      end record;
   
   
   type Hash_Table is array (Key_Hash) of Bucket;
   
   
   ----------------------
   -- Deallocate_Table --
   ----------------------
   procedure Deallocate_Table (Table: in out Hash_Table) is
      
      I,N: Bucket_Item_Access;
   begin
      for B of Table loop
         I := B.First;
         
         while I /= null loop
            N := I.Next;
            Free (I);
            I := N;
         end loop;
         
         B.First := null;
         B.Last  := null;
      end loop;
   end Deallocate_Table;
   
   
   ----------
   -- Hash --
   ----------
   -- Numerous approaches were tried, but simply going by Unicode planes as the
   -- hash keeps the rages as cohesive as possible (no wonder)
   function Hash (Codepoint: Codepoint_Value) return Key_Hash
     is (Key_Hash (Codepoint / 16#1_0000#))
     with Inline;
   
   
   -----------------
   -- Append_Item --
   -----------------
   procedure Append_Item (To       : in out Bucket;
                          Codepoint: in     Codepoint_Value;
                          Category : in     General_Category_Type)
   with Inline is 
      New_Item: Bucket_Item_Access 
        := new Bucket_Item'
          (Codepoints => (others => Codepoint),
           Category   => Category,
           Next       => null);
   begin
      if To.First = null then
         To.First := New_Item;
         To.Last  := New_Item;
         
      else
         To.Last.Next  := New_Item;
         To.Last       := New_Item;
      end if;
   end Append_Item;
   
   
   -- UnicodeData.txt Parsing -------------------------------------------------
   
   type UnicodeData_Range is (First, Last, Single);
   
   --------------------------
   -- Identify_Point_Range --
   --------------------------
   -- UnicodeData.txt encodes codepoint ranges unusually. Instead of having a
   -- range in Property 0 (P0), it has a special tag in P1, where the tag is
   -- in angle brackets, and always ends with either ", First>" or, ", Last>".
   --
   -- This subprogram gives a quick answer to for each entry
   
   function Identify_Point_Range (E: UCD_Entry) return UnicodeData_Range
   with Inline is
      P1: constant String := E.Property(1);
      
      P_First: constant String := ", First>";
      P_Last : constant String := ", Last>";
      
   begin
      if P1(P1'Last) = '>' 
        and then P1'Length >= P_First'Length
      then
         if P1(P1'Last - P_First'Length + 1 .. P1'Last) = P_First then
            return First;
         elsif P1(P1'Last - P_Last'Length + 1 .. P1'Last) = P_Last then
            return Last;
         else
            return Single;
         end if;

      else
         return Single;
      end if;
   end Identify_Point_Range;
   
   
   ----------------
   -- Load_Table --
   ----------------
   -- Parses the UnicodeData.txt file and loads the data into the hash table,
   -- including filling in any ranges encountered
   procedure Load_Table (File : in     SIO.File_Type;
                         Table: in out Hash_Table;
                         Total:    out Natural)
   is
      E: UCD_Entry;
      Stream: constant SIO.Stream_Access := SIO.Stream (File);
      
      Codepoint: Codepoint_Value;
      Category : General_Category_Type;
      
      pragma Assertion_Policy (Check);
   begin
      Total := 0;
      
      while not SIO.End_Of_File (File) loop
         E := Next_Entry (Stream);
         
         pragma Assert (E.First_Codepoint = E.Last_Codepoint);
         -- If this fails, the data is either bad, or Unicode changed the
         -- format, either way we wouldn't handle this properly, since
         -- we're expecting ranges to be expressed via P1
         
         Codepoint := Codepoint_Value 
           (Wide_Wide_Character'Pos (E.First_Codepoint));
         
         Category  := GC_Alias.Category_By_Alias (E.Property(2));
         
         Append_Item (To        => Table(Hash (Codepoint)),
                      Codepoint => Codepoint,
                      Category  => Category);
         
         Total := Total + 1;
         
         -- Check for a range and add those too
         if Identify_Point_Range (E) = First then
            declare
               Start, Finish: Codepoint_Value; 
            begin
               Start := Codepoint_Value
                 (Wide_Wide_Character'Pos
                    (Wide_Wide_Character'Succ (E.First_Codepoint)));
               
               -- The next entry had better be a "Last" range
               E := Next_Entry (Stream);
               pragma Assert (Identify_Point_Range (E) = Last);
               
               Finish := Codepoint_Value
                 (Wide_Wide_Character'Pos (E.First_Codepoint));
               
               for C in Start .. Finish loop
                  Append_Item (To        => Table(Hash (C)),
                               Codepoint => C,
                               Category  => Category);
                  Total := Total + 1;
               end loop;
            end;   
         end if;
         
      end loop;
   end Load_Table;
   
   
   ---------------------
   -- Collapse_Ranges --
   ---------------------
   -- Scans each bucket of contiguous ranges of the same category and collapses
   -- them into single entries
   procedure Collapse_Ranges (Table: in out Hash_Table) is
   begin
      for K in Key_Hash loop
         declare
            B: Bucket renames Table(K);
            Base : Bucket_Item_Access := B.First;
            Reach: Bucket_Item_Access;
         begin
            while Base /= null loop
               Reach := Base.Next;
               while Reach /= null loop
                  exit when 
                    (Reach.Category /= Base.category
                       or else (Reach.Codepoints.First 
                                  /= (Base.Codepoints.Last + 1)));
                  
                  -- Absorb Reach
                  Base.Codepoints.Last := Reach.Codepoints.Last;
                  
                  -- Isolate & Free Reach
                  Base.Next := Reach.Next;
                  Free (Reach);
                  
                  Reach := Base.Next;
               end loop;
               
               Base := Reach;
            end loop;
         end;
      end loop;
   end Collapse_Ranges;
   
   
   -- Main --------------------------------------------------------------------
   
   UnicodeData: SIO.File_Type;
   Ada_File   : SIO.File_Type;
   Ada_Stream : SIO.Stream_Access;
   
   Table: Hash_Table;
   
   Loaded_Codepoints: Natural;
   
   
   -- Formatting
   Tab              : constant String := (1 .. 3 => ' ');
   New_Line_Sequence: constant String := (1 => Ada.Characters.Latin_1.LF);
   
   Indent: Natural := 0;
   
   
   procedure Put (S: in String) with Inline is
   begin
      String'Write (Ada_Stream, S);
   end Put;
   
   procedure Do_Indent with Inline is
   begin
      for I in 1 .. Indent loop
         Put (Tab);
      end loop;
   end Do_Indent;
   
   procedure Put_Line (S: in String) with Inline is
   begin
      Do_Indent;
      Put (S & New_Line_Sequence);
   end Put_Line;
   
   procedure New_Line (Count: Natural := 1) with Inline is
   begin

      
      for I in 1 .. Count loop
         Put (New_Line_Sequence);
      end loop;
   end New_Line;
   
begin
   declare
      use SIO;
   begin
      Open (File => UnicodeData,
            Mode => In_File,
            Name => UnicodeData_Path);
   end;
   
   Load_Table (File => UnicodeData,
               Table => Table,
               Total => Loaded_Codepoints);
   
   SIO.Close (UnicodeData);
   
   -- Let's get to it
   SIO.Create (File => Ada_File,
               Name => Body_Path);
   Ada_Stream := SIO.Stream (Ada_File);
   
   -- Some stats
   Put_Line ("-- ******* " 
               & "THIS FILE IS AUTOMATICALLY GENERATED "
               & "******* --");
   Put_Line ("--  "
               & "- See Unicode.UCD.Generate_General_Category_Body -"
               & "  --");
   New_Line (2);
   Put_Line ("-- UnicodeData.txt Load Statistics --");
   
   declare
      Totals: array (General_Category_Type) of Natural := (others => 0);
      Loaded_Calc: Natural := 0;
      Grand_Total: Natural := 0;
      I: Bucket_Item_Access;
   begin
      for B of Table loop
         I := B.First;
         
         while I /= null loop
            Totals(I.Category) := Totals(I.Category) + 1;
            I := I.Next;
         end loop;
      end loop;
      
      -- Confirm our totals
      for N of Totals loop
         Loaded_Calc := Loaded_Calc + N;
      end loop;
      
      -- UnicodeData.txt does not contain "Other_Not_Assigned (Cn)"
      -- so we need to calculate it ourselves
      Totals(Other_Not_Assigned) := 16#110000# - Loaded_Calc;
      
      for N of Totals loop
         Grand_Total := Grand_Total + N;
      end loop;
      
      declare
         Target_Length: constant := 26;
      begin
         for C in General_Category_Type loop
            declare
               C_Image: constant String := General_Category_Type'Image (C);

               Padding: constant String(1 .. (Target_Length - C_Image'Length))
                 := (others => ' ');
            begin
               Put_Line ("--  " & C_Image
                           & Padding
                           & '(' & GC_Alias.General_Category_Aliases(C)
                           & ") ="
                           & Natural'Image (Totals(C)));
            end;  
         end loop;
         
         declare
            GT: constant String := "Grand Total";
            Padding: constant String(1 .. (Target_Length - GT'Length))
              := (others => ' ');
         begin
            Put_Line (String'(1 .. Target_Length + 20 => '-'));
            Put_Line ("--  " & GT & Padding & "     =" 
                        & Natural'Image(Grand_Total)); 
         end;
      end;
      
      Put_Line ("--");
      Put_Line ("-- Sanity Checks --");
      
      if Grand_Total = 16#110000# then
         Put ("--  [PASS] ");
      else
         Put ("--  [FAIL] ");
      end if;
      
      Put_Line ("Grand total = 16#110000# (U+000000 .. U+10FFFF)");
      
      
      if Loaded_Codepoints = Loaded_Calc then
         Put ("--  [PASS] ");
      else
         Put ("--  [FAIL] ");
      end if;
      
      Put_Line ("Total loaded (" & Natural'Image (Loaded_Codepoints)
                  & " ) = Hash table totals excluding Cn (" 
                  & Natural'Image (Loaded_Calc)
                  & " )");
      
      
   end;
   
   -- Collapse the table
   Collapse_Ranges (Table);
   
   
   -- OK, now lets do the actual file
   New_Line (2);
   Put_Line ("package body Unicode.General_Category is");
   
   Indent := 1;
   
   -- Types and hash function
   Put_Line ("type Codepoint is mod 2**24;");
   New_Line;
   
   
   -- Now we need to insert a function for each plane (bucket) with it's own
   -- case statement for all the ranges in that bucket
   declare
      procedure Generate_Plane_Table (Table: Hash_Table; Key: Key_Hash) is
         B: Bucket renames Table(Key);
         
         Hash_Hex: String (1 .. 4);
         Plane: String renames Hash_Hex (3 .. 4);
         
         Codepoint_Hex: String (1 .. 6);
         
         I: Bucket_Item_Access := B.First;
      begin
         Encode_Hash (Value  => Key,
                      Buffer => Hash_Hex);
         
         Indent := 1;
         New_Line;
         Put_Line ("function Plane_" & Plane 
                     & "_Lookup (C: Codepoint) "
                     & "return General_Category_Type is");
         
         Indent := 2;
         if I = null then
            -- Nothing here, this is just a direct return
            Put_Line ("(" & General_Category_Type'Image (Other_Not_Assigned)
                        & ") with Inline;");
            return;
         end if;
         
         Put_Line ("(case C is");
         
         Indent := 3;
         
         while I /= null loop
            Do_Indent;
            Put ("when ");
            
            Encode_Codepoint (Value  => I.Codepoints.First,
                              Buffer => Codepoint_Hex);
            
            Put ("16#" & Codepoint_Hex & "# ");
            
            if I.Codepoints.Last > I.Codepoints.First then
               Encode_Codepoint (Value  => I.Codepoints.Last,
                                 Buffer => Codepoint_Hex);
               Put (".. 16#" & Codepoint_Hex & "# ");
            else
               Put ("      " & "      "      & "  ");
            end if;
            
            Put ("=> " & General_Category_Type'Image (I.Category) & ',');
            
            New_Line;
            
            I := I.Next;
         end loop;
         
         New_Line;
         Put_Line ("when others => " 
                     & General_Category_Type'Image (Other_Not_Assigned)
                     & ")");
         Indent := 2;
         Put_Line ("with Inline;");
         
      end Generate_Plane_Table;
      
   begin
      for K in Key_Hash loop
         Generate_Plane_Table (Table, K);
      end loop;
   end;
   
   
   -- Finally, The actual body for General_Category
   Indent := 1;
   New_Line (2);
   Put_Line ("----------------------");
   Put_Line ("-- General_Category --");
   Put_Line ("----------------------");
   
   Indent := 1;
   Put_Line ("function General_Category (C: Wide_Wide_Character)");
   Put_Line ("                          return General_Category_Type");
   Put_Line ("is");
   
   Indent := 2;
   Put_Line ("CP: constant Codepoint "
               & ":= Codepoint (Wide_Wide_Character'Pos(C));");
   
   Indent := 1;
   Put_Line ("begin");
   
   Indent := 2;
   Put_Line ("return");
   
   Indent := 3;
   Put_Line ("(case CP is");
   
   declare
      procedure Generate_Dispatch_Line (Key: Key_Hash) is
         Hash_Hex: String (1 .. 4);
         Plane: String renames Hash_Hex (3 .. 4);
         
         Start_Hex, End_Hex: String (1 .. 6);
         
         C: Codepoint_Value := ((Codepoint_Value(Key) + 1) * 16#1_0000#) - 1;
      begin
         Encode_Hash (Value  => Key,
                      Buffer => Hash_Hex);
         
         Encode_Codepoint (Value  => C and 16#FF_0000#,
                           Buffer => Start_Hex);
         
         Encode_Codepoint (Value  => C,
                           Buffer => End_Hex);
         
         Indent := 4;
         
         Put_Line ("when 16#" & Start_Hex 
                     & "# .. " 
                     & "16#" & End_Hex 
                     & "# => "
                     & "Plane_" & Plane & "_Lookup (CP),");

      end Generate_Dispatch_Line;
      
   begin
      for K in Key_Hash loop
         Generate_Dispatch_Line (K);
      end loop;
      
      New_Line;
      Put_Line ("when others => "
                  & General_Category_Type'Image (Other_Not_Assigned)
                  & ");");
   end;
   
   
   Indent := 1;
   Put_Line ("end General_Category;");
   
   Indent := 0;
   New_Line;
   Put_Line ("end Unicode.General_Category;");
   
   SIO.Close (Ada_File);
   Deallocate_Table (Table);
   
exception
   when others =>
      Deallocate_Table (Table);
      SIO.Close (UnicodeData);
      SIO.Close (Ada_File);
      raise;
      
end Unicode.UCD.Generate_General_Category_Body;
