------------------------------------------------------------------------------
--                                                                          --
--                             Unicode Utilities                            --
--                                                                          --
--                        Normalization Form Utilities                      --
--                        Quick Check Query Facilities                      --
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
with Ada.IO_Exceptions;

with Hex;
with Hex.Unsigned_8;
with Hex.Unsigned_24;

with Unicode.Normalization.Quick_Check;
 use Unicode.Normalization.Quick_Check;

procedure Unicode.UCD.Generate_Normalization_Quick_Check_Body is
   
   package SIO renames Ada.Streams.Stream_IO;
   
   subtype Set_Case is Hex.Set_Case;
   use all type Set_Case;
   
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
         QC_Match  : Quick_Check_Result;
         
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
   procedure Append_Item (To        : in out Bucket;
                          Codepoints: in     Codepoint_Range;
                          Match     : in     Quick_Check_Result)
   with Inline is 
      New_Item: Bucket_Item_Access 
        := new Bucket_Item'
          (Codepoints => Codepoints,
           QC_Match   => Match,
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
   
   
   
   ----------------
   -- Load_Table --
   ----------------
   -- Parses the DerivedNormalizationProps.txt file and loads the data into the
   -- hash table
   procedure Load_Table (File : in     SIO.File_Type;
                         Table: in out Hash_Table;
                         Total:    out Natural)
   is
      E: UCD_Entry;
      Stream: constant SIO.Stream_Access := SIO.Stream (File);
      
      Codepoints : Codepoint_Range;
      Match_State: Quick_Check_Result;
      
      Hash_First, Hash_Last: Key_Hash;
      
      QC_Table: constant String := "NF" & Form_Code & "_QC";
      
   begin
      Total := 0;
      
      while not SIO.End_Of_File (File) loop
         begin
            E := Next_Entry (Stream);
         exception
            when Ada.IO_Exceptions.End_Error => exit;
            when others => raise;
         end;
         
         if E.Property(1) = QC_Table then
            Codepoints.First := Codepoint_Value 
              (Wide_Wide_Character'Pos (E.First_Codepoint));
            Codepoints.Last := Codepoint_Value 
              (Wide_Wide_Character'Pos (E.Last_Codepoint));
            
            case Wide_Wide_Character'(E.Property(2)(1)) is
               when 'N' => Match_State := No;
               when 'M' => Match_State := Maybe;
               when others =>
                  raise Bad_Data with "Invalid quick check code";
            end case;
            
            Hash_First := Hash (Codepoints.First);
            Hash_Last  := Hash (Codepoints.Last);
            
            
            -- Check for a range that bridges two codepages (therefore
            -- needs to be split between "buckets".
            -- Note - this really shouldn't happen with Unicode -
            -- codepages are there for a reason, but I'm no expert.
            --
            -- This is for completeness, good-form, and future-proofing
            if Hash_First /= Hash_Last then
               declare
                  Upper_Half: Codepoint_Range;
                  Lower_Half: Codepoint_Range;
               begin
                  Upper_Half.First := Codepoints.First;
                  Lower_Half.First := (Codepoint_Value 
                                         (Hash_Last) * 16#1_0000#);
                  Upper_Half.Last  := Lower_Half.First - 1;
                  Lower_Half.Last  := Codepoints.Last;
                  
                  Append_Item (To         => Table(Hash_First),
                               Codepoints => Upper_Half,
                               Match      => Match_State);
                  
                  Append_Item (To         => Table(Hash_Last),
                               Codepoints => Lower_Half,
                               Match      => Match_State);
               end;
            else
               Append_Item (To         => Table(Hash_First),
                            Codepoints => Codepoints,
                            Match      => Match_State);
            end if;
            
            Total := Total + 1;
         end if;
      end loop;
      
   exception
      when Bad_Data => raise;
      when others =>
         raise Bad_Data with "Error loading QC table.";
         
   end Load_Table;
   
   
   
   -- Main --------------------------------------------------------------------
   
   NormProps : SIO.File_Type;
   Ada_File  : SIO.File_Type;
   Ada_Stream: SIO.Stream_Access;
   
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
      Open (File => NormProps,
            Mode => In_File,
            Name => NormProps_Path);
   end;
   
   Load_Table (File => NormProps,
               Table => Table,
               Total => Loaded_Codepoints);
   
   SIO.Close (NormProps);
   
   -- Let's get to it
   SIO.Create (File => Ada_File,
               Name => Body_Path);
   Ada_Stream := SIO.Stream (Ada_File);
   
   -- Some stats
   Put_Line ("-- ************** " 
               & "THIS FILE IS AUTOMATICALLY GENERATED "
               & "************* --");
   Put_Line ("--  "
               & "- See Unicode.UCD.Generate_"
               & "Normalization_Quick_Check_Body.adb -"
               & "  --");
   New_Line (2);
   Put_Line ("-- DerivedNormalizationProps.txt --");
   Put_Line ("-- Records loaded (" 
               & "NF" & Form_Code & "_QC)"
               & " =" & Natural'Image (Loaded_Codepoints));
   
   
   -- OK, now lets do the actual file
   Indent := 0;
   
   New_Line (2);
   Put_Line ("function Unicode.Normalization.Quick_Check."
               & Form_Code & " (C: Wide_Wide_Character)");
   Put_Line (String'(1..15 => ' ')
               & "return Quick_Check_Result");
   Put_Line ("is");
   
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
                     & "return Quick_Check_Result is");
         
         Indent := 2;
         if I = null then
            -- Nothing here, this is just a direct return
            Put_Line ("(" & Quick_Check_Result'Image (Yes)
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
            
            Put ("=> " & Quick_Check_Result'Image (I.QC_Match) & ',');
            
            New_Line;
            
            I := I.Next;
         end loop;
         
         New_Line;
         Put_Line ("when others => " 
                     & Quick_Check_Result'Image (Yes)
                     & ")");
         Indent := 2;
         Put_Line ("with Inline;");
         
      end Generate_Plane_Table;
      
   begin
      for K in Key_Hash loop
         Generate_Plane_Table (Table, K);
      end loop;
   end;
   
   
   -- The actual body
   New_Line (2);
   
   Indent := 1;
   Put_Line ("CP: constant Codepoint "
               & ":= Codepoint (Wide_Wide_Character'Pos(C));");
   
   Indent := 0;
   Put_Line ("begin");
   
   Indent := 1;
   Put_Line ("return (case CP is");
   
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
         
         Indent := 2;
         
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
                  & Quick_Check_Result'Image (Yes)
                  & ");");
   end;
   
   
   Indent := 0;
   New_Line;
   Put_Line ("end Unicode.Normalization.Quick_Check." & Form_Code & ';');
   
   SIO.Close (Ada_File);
   Deallocate_Table (Table);
   
exception
   when others =>
      Deallocate_Table (Table);
      SIO.Close (NormProps);
      SIO.Close (Ada_File);
      raise;
      
end Unicode.UCD.Generate_Normalization_Quick_Check_Body;

