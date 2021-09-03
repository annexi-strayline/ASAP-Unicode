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

with Ada.Streams;

private with Ada.Characters.Conversions;
private with Ada.Strings.Wide_Wide_Unbounded;
private with Ada.Containers.Vectors;


package Unicode.UCD is
   
   Bad_Data: exception;
   
   type UCD_Entry is tagged private with Preelaborable_Initialization;
   
   function Next_Entry 
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class)
     return UCD_Entry;
   -- Parse from the beginning of a line in a UCD file, returning the result.
   --
   -- This should never fail when used correctly with a properly formatted 
   -- file. Therefore a parsing error is considered fatal for a given stream.
   -- Once a Bad_Data exception is raised, it should be expected that
   -- subsequent calls to Next_Extry will also fail.
   --
   -- Next_Entry is strict in parsing and makes no assuptions.
   
   function First_Codepoint (E: UCD_Entry) return Wide_Wide_Character;
   function Last_Codepoint  (E: UCD_Entry) return Wide_Wide_Character;
   -- Returns the first and last codepoint values for the entry.
   
   function Property_Count  (E: UCD_Entry) return Natural;
   -- Returns the number of properties contained in the entry
   
   function Property (E: UCD_Entry; Index: Positive) return String;
   function Property (E: UCD_Entry; Index: Positive) return Wide_Wide_String;
   -- Returns the value of the Property at the given index.
   -- Raises CONSTRAINT_ERROR if the property index does not exist, or if
   -- the property cannot be represented in a String
   
   function Comment (E: UCD_Entry) return String;
   function Comment (E: UCD_Entry) return Wide_Wide_String;
   -- Returns the value of the comment (if any) at the end of the entry
   -- (not including the initial '#')
   
private
   
   package WWU renames Ada.Strings.Wide_Wide_Unbounded;
   
   use type WWU.Unbounded_Wide_Wide_String;
   subtype Unbounded_Wide_Wide_String is WWU.Unbounded_Wide_Wide_String;
   
   package Property_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Unbounded_Wide_Wide_String);
   
   use type Property_Vectors.Vector;
   subtype Property_Vector is Property_Vectors.Vector;
   
   type UCD_Entry is tagged
      record
         First, Last: Wide_Wide_Character;
         Properties : Property_Vector;
         Comment    : Unbounded_Wide_Wide_String;
      end record;
   
   function First_Codepoint (E: UCD_Entry) return Wide_Wide_Character
     is (E.First);
   
   function Last_Codepoint  (E: UCD_Entry) return Wide_Wide_Character
     is (E.Last);
   
   function Property_Count (E: UCD_Entry) return Natural
     is (Natural (E.Properties.Length));
   
   
   function Property (E: UCD_Entry; Index: Positive) return Wide_Wide_String
     is (WWU.To_Wide_Wide_String (E.Properties(Index)));
   
   function Property (E: UCD_Entry; Index: Positive) return String
     is (Ada.Characters.Conversions.To_String 
           (Wide_Wide_String'(E.Property(Index))));
   
   function Comment (E: UCD_Entry) return Wide_Wide_String
     is (WWU.To_Wide_Wide_String (E.Comment));
   
   function Comment (E: UCD_Entry) return String
     is (Ada.Characters.Conversions.To_String 
           (WWU.To_Wide_Wide_String (E.Comment)));
   
end Unicode.UCD;
