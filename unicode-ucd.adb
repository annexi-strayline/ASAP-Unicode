------------------------------------------------------------------------------
--                                                                          --
--                             Unicode Utilities                            --
--                                                                          --
--                 Unicode Character Database (UCD) Utilities               --
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

with Ada.Exceptions;             use Ada;
with Ada.IO_Exceptions;
with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;
with Ada.Strings;

with Hex;
with Hex.Unsigned_24;

with Unicode.UTF8_Stream_Decoder;

package body Unicode.UCD is
   
   use type Hex.Hex_Character;
   
   ----------------
   -- Next_Entry --
   ----------------
   function Next_Entry 
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class)
     return UCD_Entry
   is
      use type Ada.Streams.Root_Stream_Type;
      subtype Root_Stream_Type is Ada.Streams.Root_Stream_Type;
      
      function Next_Character 
        (UTF8_Stream: not null access Root_Stream_Type'Class
           := Stream)
        return Wide_Wide_Character
        renames Unicode.UTF8_Stream_Decoder.Decode_Next;
      
      function Next_Character 
        (UTF8_Stream: not null access Root_Stream_Type'Class
           := Stream)
        return Character
        renames Unicode.UTF8_Stream_Decoder.Decode_Next;
      
      CR: Character renames Ada.Characters.Latin_1.CR;
      LF: Character renames Ada.Characters.Latin_1.LF;
      
      WW_CR: constant Wide_Wide_Character
        := Wide_Wide_Character'Val (Character'Pos (CR));
      WW_LF: constant Wide_Wide_Character
        := Wide_Wide_Character'Val (Character'Pos (LF));
      
      WWC: Wide_Wide_Character;
      C  : Character;
      
      
      -- Seek_Entry_Start --
      ----------------------
      -- Attempts to seek to the first digit of the initial codepoint of the
      -- next entry. If successful, C contains the first hex digit of the
      -- entry, otherwise Bad_Data is raised.
      
      procedure Seek_Entry_Start with Inline is 
      begin
         -- We should be positioned at the start of a line.
         
         -- The first thing we want to find is the value of the first
         -- codepoint of a poential range. We expect to find spaces,
         -- '#' or a hexidecimal digit, anything else and we bail.
         -- if it's a '#', we then look for a single LF character before
         -- trying again (on the next line).
         loop
            begin
               C := Next_Character;
            exception
               -- Check for an end of file condition - this can happen when
               -- the very last lines are comments. If we have an end error
               -- here, we'll just propegate it
               when IO_Exceptions.End_Error => raise;
               
               -- Otherwise, it's probably a wide-wide character or bad utf-8,
               -- either case, this shouldn't happen. yet
               when e: others =>
                  raise Bad_Data with
                    "Exception while looking for codepoint range: " &
                    Ada.Exceptions.Exception_Information (e);
            end;
            
            exit when C in Hex.Hex_Character;
            
            case C is
               when '#' =>
                  -- Comment. Comments can be any UTF-8 character,
                  -- and we should discard those until we find a 
                  -- single Line-feed or a Carriage-return followed
                  -- by a line-feed (why not, we'll place nice)
                  begin
                     loop
                        WWC := Next_Character;
                        exit when WWC = WW_LF;
                        
                        if WWC = WW_CR then
                           WWC := Next_Character;
                           if WWC /= WW_LF then
                              raise Bad_Data with
                                "CR not followed by LF.";
                           end if;
                        end if;
                     end loop;
                  exception
                     when Bad_Data => raise;
                     when e: others =>
                        raise Bad_Data with
                          "Exception while looking for end of comment - " &
                          "where comment is before the first entry " & 
                          " of the file: " &
                          Ada.Exceptions.Exception_Information (e);
                  end;
                  
                  -- Leading whitespace is ignored
               when CR =>
                  C := Next_Character;
                  if C /= LF then
                     raise Bad_Data with "CR not followed by LF.";
                  end if;
                  
               when LF | ' ' =>
                  -- This is fine, skip
                  null;
                  
               when others =>
                  -- We already left the loop if it was a valid hexadecimal
                  -- digit, so this must be something else entirely.
                  -- a ';' would definately be illegal since we really need to
                  -- know at least the first codepoint!
                  raise Bad_Data with
                    "Unexpected '" & C 
                    & "' while looking for first codepoint.";
            end case;
         end loop;
         
      exception
         when Bad_Data | IO_Exceptions.End_Error => raise;
         when e: others =>
            raise Bad_Data with "Unexpected exception while seeking " 
              & "first codepoint: "
              & Ada.Exceptions.Exception_Information (e);
      end Seek_Entry_Start;
      
      
      -- Parse_Codepoints --
      ----------------------
      -- Following a call to Seek_Entry_Start, C is expected to cointain
      -- a single Hex digit. This procedure attempts to parse the codepoint
      -- range or singleton
      procedure Parse_Codepoints (First, Last: out Wide_Wide_Character)
      with Inline
      is
         use Hex.Unsigned_24;
         Codepoint_Hex: String (1 .. 6);
         
         Last_Digit: Natural;
         Is_Range  : Boolean;
         
         procedure Load_Codepoint_String with Inline is
         begin
            -- First (most significant) digit is loaded "manually",
            -- we are here to fill-out the rest
            
            Is_Range := False;
            
            for I in 2 .. Codepoint_Hex'Last + 1 loop
               C := Next_Character;
               
               case C is
                  when Hex.Hex_Character =>
                     if I > Codepoint_Hex'Last then
                        raise Bad_Data with
                          "Codepoint is longer than the allowed maximum";
                     else
                        Codepoint_Hex(I) := C;
                     end if;
                     
                  when '.' =>
                     -- Next character *must* be '.' also.
                     if Next_Character /= Wide_Wide_Character'('.') then
                        raise Bad_Data 
                          with "First codepoint incorrectly terminated.";
                     else
                        -- Nice, we're done with this one, and we have a
                        -- range!
                        Last_Digit := I - 1;
                        Is_Range   := True;
                        exit;
                     end if;
                     
                  when ';' =>
                     -- Singleton value
                     Last_Digit := I - 1;
                     exit;
                     
                  when ' ' =>
                     -- Shall be a singleton value ("ABCD .. DEF0" is not
                     -- valid, only "ABCD..DEF0" is legal according to 
                     -- Unicode. So spaces shall only exist when trailing a
                     -- property - and that means it needs to end with ';'
                     
                     -- CR/LF is not legal - as the line is not complete
                     -- (same with comments)
                     while C = ' ' loop
                        C := Next_Character;
                     end loop;
                     
                     if C /= ';' then
                        raise Bad_Data with "Invalid termination of first "
                          & "codepoint.";
                     end if;
                     
                     Last_Digit := I - 1;
                     exit;
                     
                  when others =>
                     -- Invalid!
                     raise Bad_Data with "Invalid character in " 
                       & "first codepoint";
               end case;
            end loop;
         end Load_Codepoint_String;
         
      begin
         Codepoint_Hex(1) := C;
         Load_Codepoint_String;
         
         -- If we made it here, we should have a proper string to convert
         First := Wide_Wide_Character'Val
           (Decode (Codepoint_Hex (1 .. Last_Digit)));
         
         -- Next we check if it is a range, in which case we then need to
         -- try to get the last character of that range.
         if Is_Range then
            -- A valid range means that there is no space or LF after the
            -- "..", and therefore the next character had better be a
            -- hex digit
            C := Next_Character;
            
            if C not in Hex.Hex_Character then
               raise Bad_Data with "Invalid codepoint range";
            end if;
            
            Codepoint_Hex(1) := C;
            Load_Codepoint_String;
            
            -- This should not report another range!
            if Is_Range then
               raise Bad_Data with "Invalid codepoint range";
            end if;
            
            Last
              := Wide_Wide_Character'Val
                (Decode (Codepoint_Hex (1 .. Last_Digit)));
         else
            -- Singleton codepoint
            Last := First;
         end if;
         
      exception
         when Bad_Data => raise;
         when e: others =>
            raise Bad_Data 
              with "Unexpected exception when parsing codepoint: "
                & Ada.Exceptions.Exception_Information (e);

      end Parse_Codepoints;
      
      
      -- Parse_Properties --
      ----------------------
      -- We've consumed the ';' delimiting the beginning of property 1.
      -- Parse_Properties then loads each property until the end of the line,
      -- except for any final comment
      -- 1. Leading spaces (following the last ';') are insignificant
      -- 2. Empty properties are signficiant
      -- 3. Trailing spaces are insignificant (read: should be trimed)
      -- 4. Every property must _begin_ with ';', but can end with either
      --    LF (or CRLF), or '#' for a comment.
      --
      -- WWC will be left containing whichever character ended the sequence,
      -- which will always be either LF (WW_LF) or '#'
      
      procedure Parse_Properties (Properties: in out Property_Vector)
      with Inline is
         procedure Skip_Leading_Spaces with Inline is
         begin
            loop
               WWC := Next_Character;
               exit when WWC /= ' ';
            end loop;
         end Skip_Leading_Spaces;
         
         procedure Load_Property with Inline is
            use WWU;
            use Ada.Strings;
         
            Chunk: Wide_Wide_String (1 .. 80) := (others => ' ');
            Chunk_Last: Natural := Chunk'First - 1;
            -- Most properties will fit into a single chunk! This method makes
            -- adding each bit to the unbounded string more efficient than if
            -- we did it one character at a time!
            
            Property: Unbounded_Wide_Wide_String;
         begin
            loop
               if Chunk_Last = Chunk'Last then
                  -- Time to purge
                  Append (Source   => Property,
                          New_Item => Chunk);
                  Chunk_Last := Chunk'First - 1;
               end if;
               
               case WWC is
                  when ';' | '#' | WW_LF =>
                     -- End of the road
                     exit;
                     
                  when WW_CR =>
                     WWC := Next_Character;
                     if WWC /= WW_LF then
                        raise Bad_Data with "CR not followed with LF";
                     end if;
                     
                     exit;
                     
                  when others =>
                     Chunk_Last := Chunk_Last + 1;
                     Chunk(Chunk_Last) := WWC;
               end case;
               
               -- Skip_Leading_Spaces would have the first character
               -- already loaded
               WWC := Next_Character;
               
            end loop;
            
            -- Last purge
            if Chunk_Last >= Chunk'First then
               Append (Source   => Property,
                       New_Item => Chunk(Chunk'First .. Chunk_Last));
            end if;
            
            -- Trim trailing spaces
            Trim (Source => Property,
                  Side   => Right);
            
            -- Slap it on the end of the vector
            Properties.Append (Property);
            
         end Load_Property;
         
      begin
         loop
            Skip_Leading_Spaces;
            Load_Property;
            
            exit when WWC /= ';';
         end loop;   
      exception
         when e: others =>
            raise Bad_Data with "Properties malformed: " &
              Ada.Exceptions.Exception_Information (e);
      end Parse_Properties;
      
      
      -- Load_Comment --
      ------------------
      -- To be called only after reaching '#'. Loads everything following up
      -- until LF or CRLF.
      function Load_Comment return Unbounded_Wide_Wide_String
      with Inline is
         use WWU;
         
         Chunk: Wide_Wide_String (1 .. 80) := (others => ' ');
         Chunk_Last: Natural := Chunk'First - 1;
      begin
         return Comment: Unbounded_Wide_Wide_String do
            loop
               if Chunk_Last = Chunk'Last then
                  -- purge
                  Append (Source   => Comment,
                          New_Item => Chunk);
                  Chunk_Last := Chunk'First - 1;
               end if;
               
               WWC := Next_Character;
               exit when WWC in WW_CR | WW_LF;
               
               Chunk_Last := Chunk_Last + 1;
               Chunk(Chunk_Last) := WWC;
            end loop;
            
            if WWC = WW_CR then
               WWC := Next_Character;
               if WWC /= WW_LF then
                  raise Bad_Data with "CR not followed with LF";
               end if;
            end if;
            
            -- Load last chunk
            if Chunk_Last >= Chunk'First then
               Append (Source   => Comment,
                       New_Item => Chunk(Chunk'First .. Chunk_Last));
            end if;
         end return;
      end Load_Comment;
      
      
   -- Next_Entry Body ---------------------------------------------------------
   begin
      return E: UCD_Entry do
         -- Initial conditions - reserving some space in the vector
         -- prevents undue memory copying every time the vector is
         -- expanded (as can be expected in some implementations)
         E.Properties.Reserve_Capacity (20);
         
         Seek_Entry_Start;
         Parse_Codepoints (E.First, E.Last);
         Parse_Properties (E.Properties);
         
         -- WWC should now have either LF or '#'
         case WWC is
            when '#' => 
               -- We also have a comment to load!
               E.Comment := Load_Comment;
               
            when WW_LF =>
               -- That's it
               null;
               
            when others =>
               raise Program_Error with
                 "Parse_Properties failed inappropriately.";
         end case;
      end return;
   end Next_Entry;
   
end Unicode.UCD;
   
