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

with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Ada.Streams.Stream_IO;

with Hex;
with Hex.Unsigned_8;
with Hex.Unsigned_24;

procedure Unicode.UCD.Generate_Case_Folding.Simple
  (CaseFolding_Path: in String := "CaseFolding.txt";
   Body_Path       : in String := "aura-unicode-case_folding-simple.adb")
is
   
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
   
   function Decode_Codepoint (S: in String) return Codepoint_Value
     renames Hex.Unsigned_24.Decode;
   
   -- Hash Key
   use type Hex.Unsigned_8.Unsigned_8;
   subtype Key_Hash is Hex.Unsigned_8.Unsigned_8;
   
   procedure Encode_Hash (Value   : in     Key_Hash;
                          Buffer  :    out String;
                          Use_Case: in     Set_Case := Upper_Case)
     renames Hex.Unsigned_8.Encode;
   
   
   -- Reference Hash Table ----------------------------------------------------
   type Bucket_Item;
   type Bucket_Item_Access is access Bucket_Item;
   
   type Codepoint_Map is
      record
         From: Codepoint_Value;
         To  : Codepoint_Value;
      end record;
   
   type Bucket_Item is
      record
         Map : Codepoint_Map;
         Next: Bucket_Item_Access := null;
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
   -- Simple case folding is spread fairly randomly amongst 1,411 codepoints.
   -- We need a hash to distribute this as evenly as possible amongst 16
   -- buckets
   function Hash (Codepoint: Codepoint_Value) return Key_Hash
   with Inline is
      C: Codepoint_Value := Codepoint;
   begin
      return K: Key_Hash := 0 do
         for I in 1 .. 3 loop
            K := K xor Key_Hash (C and 16#FF#);
            C := C / 16#100#;
         end loop;
      end return;
   end Hash;
   
   
   -----------------
   -- Append_Item --
   -----------------
   procedure Append_Item (To      : in out Bucket;
                          Map     : in     Codepoint_Map)
   with Inline is 
      New_Item: Bucket_Item_Access 
        := new Bucket_Item'
          (Map      => Map,
           Next     => null);
   begin
      if To.First = null then
         To.First := New_Item;
         To.Last  := New_Item;
         
      else
         To.Last.Next  := New_Item;
         To.Last       := New_Item;
      end if;
   end Append_Item;
   
   
   -- CaseFolding.txt Parsing -------------------------------------------------
   
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
      
      Map: Codepoint_Map;
      
      pragma Assertion_Policy (Check);
   begin
      Total := 0;
      
      while not SIO.End_Of_File (File) loop
         begin
            E := Next_Entry (Stream);
         exception
            when Ada.IO_Exceptions.End_Error => exit;
            when others => raise;
         end;
         
         pragma Assert (E.First_Codepoint = E.Last_Codepoint);
         -- If this fails, the data is either bad, or Unicode changed the
         -- format, either way we wouldn't handle this properly. We're
         -- expecting exactly a one-to-one relationship for any mappings
         -- in Simple Case Folding
         
         -- Only load "Common" (C) and "Simple" (S) entries
         if E.Property(1) in "C" | "S" then
            Map := (From => Codepoint_Value 
                      (Wide_Wide_Character'Pos (E.First_Codepoint)),
                    To => Decode_Codepoint (E.Property(2)));
         
            Append_Item (To        => Table(Hash (Map.From)),
                         Map       => Map);
         
            Total := Total + 1;
         end if;
      end loop;
   end Load_Table;
   
   
   
   -- Main --------------------------------------------------------------------
   
   CaseFolding_File: SIO.File_Type;
   Ada_File   : SIO.File_Type;
   Ada_Stream : SIO.Stream_Access;
   
   Table: Hash_Table;
   
   Loaded_Maps: Natural;
   
   
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
      Open (File => CaseFolding_File,
            Mode => In_File,
            Name => CaseFolding_Path);
   end;
   
   Load_Table (File  => CaseFolding_File,
               Table => Table,
               Total => Loaded_Maps);
   
   SIO.Close (CaseFolding_File);
   
   -- Let's get to it
   SIO.Create (File => Ada_File,
               Name => Body_Path);
   Ada_Stream := SIO.Stream (Ada_File);
   
   -- Some stats
   Put_Line ("-- ******* " 
               & "THIS FILE IS AUTOMATICALLY GENERATED "
               & "******* --");
   Put_Line ("--  "
               & " - See Unicode.UCD.Generate_Case_Folding.Simple -"
               & "   --");
   New_Line (2);
   Put_Line ("-- CaseFolding.txt: C+S maps loaded =" 
               & Natural'Image (Loaded_Maps)); 
   
   
   -- OK, now lets do the actual file
   Indent := 0;
   New_Line (2);
   Put_Line ("function Unicode.Case_Folding.Simple "
               & "(C: Wide_Wide_Character)");
   Put_Line ("                                     "
               & "return Wide_Wide_Character");
   Put_Line ("is");
             
   Indent := 1;
   -- Types and hash function
   Put_Line ("type Codepoint is mod 2**24;");
   Put_Line ("type Key_Hash  is mod 2**8;");
   New_Line;
   
   Put_Line ("function Hash (C: Codepoint) return Key_Hash with Inline is");
   
   Indent := 2;
   Put_Line ("T: Codepoint := C;");
   
   Indent := 1;
   Put_Line ("begin");
   
   Indent := 2;
   Put_Line ("return K: Key_Hash := 0 do");
   
   Indent := 3;
   Put_Line ("for I in 1 .. 3 loop");
   
   Indent := 4;
   Put_Line ("K := K xor Key_Hash (T and 16#FF#);");
   Put_Line ("T := T / 16#100#;");
   
   Indent := 3;
   Put_Line ("end loop;");
   
   Indent := 2;
   Put_Line ("end return;");
   
   Indent := 1;
   Put_Line ("end Hash;");
   
   
   -- Now we need to insert a function for each bucket with it's own
   -- case statement for all the ranges in that bucket
   declare
      procedure Generate_Hash_Table (Table: Hash_Table; Key: Key_Hash) is
         B: Bucket renames Table(Key);
         
         Bucket_Hex   : String (1 .. 2);
         Codepoint_Hex: String (1 .. 6);
         
         I: Bucket_Item_Access := B.First;
      begin
         Encode_Hash (Value  => Key,
                      Buffer => Bucket_Hex);
         
         Indent := 1;
         New_Line;
         Put_Line ("function Bucket_" & Bucket_Hex
                     & " (C: Codepoint) "
                     & "return Wide_Wide_Character is");
         
         Indent := 2;
         if I = null then
            -- Nothing here, just an identity function
            Put_Line ("(Wide_Wide_Character'Val (C))"
                        & " with Inline;");
            return;
         end if;
         
         Put_Line ("(case C is");
         
         Indent := 3;
         
         while I /= null loop
            Do_Indent;
            Put ("when ");
            
            Encode_Codepoint (Value  => I.Map.From,
                              Buffer => Codepoint_Hex);
            
            Put ("16#" & Codepoint_Hex & "# ");
            
            Encode_Codepoint (Value  => I.Map.To,
                              Buffer => Codepoint_Hex);
            
            Put ("=> Wide_Wide_Character'Val (16#"
                   & Codepoint_Hex & "#),");
            
            New_Line;
            
            I := I.Next;
         end loop;
         
         New_Line;
         Put_Line ("when others => Wide_Wide_Character'Val (C))");
         
         Indent := 2;
         Put_Line ("with Inline;");
         
      end Generate_Hash_Table;
      
   begin
      for K in Key_Hash loop
         Generate_Hash_Table (Table, K);
      end loop;
   end;
   
   
   -- Finally, The actual body
   Indent := 0;
   New_Line (2);
   Put_Line ("begin");
   
   
   Indent := 1;
   Put_Line ("-- This shouldn't happen..");
   Put_Line ("if Wide_Wide_Character'Pos(C) > 16#10FFFF# then");
   
   Indent := 2;
   Put_Line ("return C;");
   
   Indent := 1;
   Put_Line ("end if;");
   
   New_Line (2);
   
   Put_Line ("declare");
   
   Indent := 2;
   Put_Line ("CP: constant Codepoint ");
   
   Indent := 3;
   Put_Line (":= Codepoint (Wide_Wide_Character'Pos (C));");
   
   Indent := 2;
   Put_Line ("K: constant Key_Hash := Hash (CP);");
   
   Indent := 1;
   Put_Line ("begin");
   
   Indent := 2;
   Put_Line ("case K is");
   
   Indent := 3;
   
   declare
      Bucket_Hex: String (1 .. 2);
   begin
      for K in Key_Hash loop
         Encode_Hash (Value  => K,
                      Buffer => Bucket_Hex);
         Put_Line ("when 16#" & Bucket_Hex & "# => " &
                     "return Bucket_" & Bucket_Hex
                     & " (CP);");
      end loop;
   end;
   
   Indent := 2;
   Put_Line ("end case;");
   
   Indent := 1;
   Put_Line ("end;");
   
   Indent := 0;
   New_Line;
   Put_Line ("end Unicode.Case_Folding.Simple;");
   
   SIO.Close (Ada_File);
   Deallocate_Table (Table);
   
exception
   when others =>
      Deallocate_Table (Table);
      SIO.Close (CaseFolding_File);
      SIO.Close (Ada_File);
      raise;
      
end Unicode.UCD.Generate_Case_Folding.Simple;
