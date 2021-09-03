------------------------------------------------------------------------------
--                                                                          --
--                             Unicode Utilities                            --
--                            UTF-8 Stream Decoder                          --
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


package body Unicode.UTF8_Stream_Decoder.Codec 
  with SPARK_Mode => On
is
   pragma Assertion_Policy (Ignore);
   
   pragma Suppress (Index_Check);
   pragma Suppress (Length_Check);
   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);
   
   --
   -- Supprting functions
   --
   
   function Validate_First (Octet: Stream_Element) return Boolean

     is (((Octet and 2#1100_0000#) /= 2#1000_0000#)
           -- Not a continuation byte
           
           and then 
           (          (Octet and 2#1000_0000#) = 0
              or else (Octet and 2#1110_0000#) = 2#1100_0000#
              or else (Octet and 2#1111_0000#) = 2#1110_0000#
              or else (Octet and 2#1111_1000#) = 2#1111_0000#
           )
           -- The leading bits must be valid Byte 1 permutations
           -- for 1,2,3, and 4 total bytes, respectively
           
           and then
           (Octet not in 16#C0# .. 16#C1# | 16#F5# .. 16#FF#))
           -- Specifically prohibited in any UTF-8 stream
           -- as per STD63/RFC3629, Section 1
     with Inline => True, Global => null;
     
   -- Returns True if and only if Octet is a valid firsy byte of a
   -- continuation.
   
   
   function Indicated_Continuation_Bytes (First_Octet: Stream_Element)
                                         return Stream_Element_Offset
     is (case (First_Octet and 2#1111_0000#) is
            when 2#0000_0000# .. 2#0111_0000# => 0,
            when 2#1100_0000# .. 2#1101_0000# => 1,
            when 2#1110_0000#                 => 2,
            when 2#1111_0000#                 => 3,
            when others                       => 4)  -- Invalid value
     with 
     Global => null,
     Pre    => Validate_First (First_Octet),
     Post   => Indicated_Continuation_Bytes'Result in 0 .. 3,
     Inline => True;
   -- Given a valid first byte of a UTF-8 sequence, returns the number of
   -- continuation bytes that are expected to follow
   
   
   function Validate_Continuation (Octet: Stream_Element) return Boolean
     is ((Octet and 2#11_000000#) = 2#10_000000#)
   with Global => null,
     Inline => True;
   -- Simply validates that Octet is a valid continuation byte.
   
   
   function Validate_Sequence (Sequence: Sequence_Array)
                              return Boolean
     is 
     (Sequence'Length in 1 .. 4
        
        and then Sequence'First in 1 .. 4
        and then Sequence'Last >= Sequence'First
        
        and then Validate_First (Sequence(Sequence'First))
        
        and then 
        (Indicated_Continuation_Bytes (Sequence(Sequence'First)) + 1 
           = Sequence'Length)
        
        and then
        (case Sequence'Length is
            when 1 =>
              (Sequence'First in 1 .. 4
                 and then Sequence'Last = Sequence'First),
            when 2 =>
              (Sequence'First in 1 .. 3
                 and then Sequence'Last = Sequence'First + 1),
            when 3 =>
              (Sequence'First in 1 .. 2
                 and then Sequence'Last = Sequence'First + 2),
            when 4 =>
              (Sequence'First = 1
                 and then Sequence'Last = 4),
            when others =>
               False)
        -- The SPARK tools needed to be told this..
        
        and then 
        (for all Octet of 
           Sequence(Sequence'First + 1 .. Sequence'Last)
           => Validate_Continuation (Octet)))
   with
     Inline => True,
     Global => null;
   -- Validates that a given sequence has a legal formation. Does not check the
   -- actual encoded value - that should be done with Verified_Combine
   
   
   procedure Verified_Combine (Sequence: in     Sequence_Array;
                               Result  :    out Wide_Wide_Character;
                               Status  :    out Decode_Status) 
   with 
     Inline => True,
     Global => null,
     Pre    => Validate_Sequence (Sequence),
     Post   => (if Status = Success then 
                  (case Sequence'Length is
                      when 1 =>
                         Wide_Wide_Character'Pos (Result)
                           in 16#000000# .. 16#00007F#,
                      when 2 =>
                         Wide_Wide_Character'Pos (Result)
                           in 16#000080# .. 16#0007FF#,
                      when 3 =>
                         Wide_Wide_Character'Pos (Result)
                           in 16#000800# .. 16#00D7FF# 
                            | 16#00E000# .. 16#00FDCF#
                            | 16#00FDF0# .. 16#00FFFD#,
                         -- IETF STD63/RFC3629 Section 3:
                         -- UTF-16 "surrogate pairs"
                         -- (U+D800 .. U+DFFF) are prohibited
                         --
                         -- Unicode Corrigendum #9 - Noncharacter
                         -- codepoints:
                         -- U+FDD0 .. U+FDEF
                         -- U+nFFFE + U+nFFFF
                      when 4 =>
                         Wide_Wide_Character'Pos (Result)
                           in 16#010000# .. 16#01FFFD#
                            | 16#020000# .. 16#02FFFD#
                            | 16#030000# .. 16#03FFFD#
                            | 16#040000# .. 16#04FFFD#
                            | 16#050000# .. 16#05FFFD#
                            | 16#060000# .. 16#06FFFD#
                            | 16#070000# .. 16#07FFFD#
                            | 16#080000# .. 16#08FFFD#
                            | 16#090000# .. 16#09FFFD#
                            | 16#0A0000# .. 16#0AFFFD#
                            | 16#0B0000# .. 16#0BFFFD#
                            | 16#0C0000# .. 16#0CFFFD#
                            | 16#0D0000# .. 16#0DFFFD#
                            | 16#0E0000# .. 16#0EFFFD#
                            | 16#0F0000# .. 16#0FFFFD#
                            | 16#100000# .. 16#10FFFD#,
                         -- U+10000 .. U+10FFFF except for:
                         --  Unicode Corrigendum #9 - Noncharacter
                         --  codepoints:
                         --  U+nFFFE + U+nFFFF
                      when others =>
                         False)
                else
                  ((Status in Overlong | Codepoint_Excursion) and
                     Result = Unicode_Replacement_Character));
                
   -- Combines a validated single UTF-8 sequence encoded character into
   -- a Wide_Wide_Character.
   
   
   ----------------------
   -- Verified_Combine --
   ----------------------
   procedure Verified_Combine (Sequence: in     Sequence_Array;
                               Result  :    out Wide_Wide_Character;
                               Status  :    out Decode_Status)
   is
      type Encoded_Codepoint is mod 2**32;
      
      Codepoint: Encoded_Codepoint;
      
      procedure Codepoint_Shift_And_Add (Continuation_Byte: in Stream_Element)
      with Inline => True,
        Global => (In_Out => Codepoint),
        Pre => (Validate_Continuation (Continuation_Byte)
                  and then Codepoint <= 16#FFFF#),
        -- This basically means that we're not calling it
        -- more than 4 times.
        Post => (Codepoint = (Codepoint'Old * 2**6) 
                   + Encoded_Codepoint(Continuation_Byte and 2#00_111111#));
        
      procedure Codepoint_Shift_And_Add (Continuation_Byte: in Stream_Element) 
      is
      begin
         Codepoint := (Codepoint * 2**6) 
           + Encoded_Codepoint (Continuation_Byte and 2#00_111111#);
         
      end Codepoint_Shift_And_Add;
      
   begin
      
      pragma Assert (Validate_Sequence(Sequence));
      
      case Sequence'Length is
         when 1 =>
            pragma Assert (Sequence'First in 1 .. 4);
            pragma Assert (Sequence'Last = Sequence'First);
            
            Codepoint 
              := Encoded_Codepoint (Sequence(Sequence'First) and 2#0_1111111#);
            Result := Wide_Wide_Character'Val (Codepoint);
            Status := Success;
            pragma Assert (Wide_Wide_Character'Pos (Result)
                             in 16#000000# .. 16#00007F#);
            return;
            
         when 2 =>
            pragma Assert (Sequence'First in 1 .. 3);
            pragma Assert (Sequence'Last = Sequence'First + 1);
            
            Codepoint 
              := Encoded_Codepoint (Sequence(Sequence'First) and 2#000_11111#);
               
            -- Overlong check -- 
            -- If the leading byte shall not be zero or one. If it was zero or
            -- one, then that would mean the full value would be a 7-bit value,
            -- which could be encoded in a single byte.
            
            if Codepoint < 2 then
               Result := Unicode_Replacement_Character;
               Status := Overlong;
               return;
               
            else
               pragma Assert (Codepoint in 2 .. 16#7F#);
               
               Codepoint_Shift_And_Add (Sequence(Sequence'First + 1));
               Result := Wide_Wide_Character'Val (Codepoint);
               Status := Success;
               pragma Assert (Wide_Wide_Character'Pos (Result)
                                in 16#000080# .. 16#0007FF#);
               return;
            end if;
               
         when 3 =>
            pragma Assert (Sequence'First in 1 .. 2);
            pragma Assert (Sequence'Last = Sequence'First + 2);
            
            Codepoint 
              := Encoded_Codepoint (Sequence(Sequence'First) and 2#0000_1111#);
              
            declare
               Second_Byte: Stream_Element
                 renames Sequence(Sequence'First + 1);
               
               Third_Byte: Stream_Element
                 renames Sequence(Sequence'First + 2);
            begin
               -- Overlong check --
               -- The leading byte may encode zero. This would be needed to
               -- encode any 12-bit value, since a two byte sequence can only
               -- encode 5 + 6 = 11 bits.
               --
               -- This means that if the first byte encodes zero, the second
               -- byte must be greater than 1F, since 0 .. 1F would be a five-
               -- bit value meaning the total value would be 5 + 6 bits.
               
               if Codepoint = 0 
                 and then (Second_Byte and 2#00_111111#) <= 16#1F#
               then
                  Result := Unicode_Replacement_Character;
                  Status := Overlong;
                  return;
                  
               else
                  Codepoint_Shift_And_Add (Second_Byte);
                  Codepoint_Shift_And_Add (Third_Byte);
                  
                  if Codepoint in 
                      16#D800# .. 16#DFFF#
                    | 16#FDD0# .. 16#FDEF#
                    | 16#FFFE# .. 16#FFFF#
                  then
                     -- IETF STD63/RFC3629 Section 3:
                     -- UTF-16 "surrogate pairs"
                     -- (U+D800 .. U+DFFF) are prohibited
                     --
                     -- Unicode Corrigendum #9 - Noncharacter
                     -- codepoints:
                     -- U+FDD0 .. U+FDEF
                     -- U+nFFFE + U+nFFFF

                     Result := Unicode_Replacement_Character;
                     Status := Codepoint_Excursion;
                     return;
                  end if;
                  
                  Result := Wide_Wide_Character'Val (Codepoint);
                  Status := Success;

                  return;
                  
               end if;
               
            end;
            
         when 4 =>
            pragma Assert (Sequence'First = 1);
            pragma Assert (Sequence'Last = Sequence'First + 3);
            
            Codepoint 
              := Encoded_Codepoint (Sequence(Sequence'First) and 2#00000_111#);
              
            declare
               Second_Byte: Stream_Element
                 renames Sequence(Sequence'First + 1);
               
               Third_Byte: Stream_Element
                 renames Sequence(Sequence'First + 2);
               
               Fourth_Byte: Stream_Element
                 renames Sequence(Sequence'First + 3);
            begin
               
               -- Overlong check --
               -- For a 3-byte sequence, the largest value that can be stored
               -- is 4-bits + 6 + 6 = 16 bits. For a 4-byte sequence, if the
               -- third and fourth bytes account for 6 + 6 = 12 bits, and we
               -- assume they are all 1's, and we allow the first byte to 
               -- encode zero, we then must have more than the first 4 bits set
               -- in the second byte - i.e. the second byte must encode a value
               -- that is > 16#F# (< 16#10#).
               --
               -- If the first byte encodes anything > 0, this forces the value
               -- to be at least 1 + 6 + 6 + 6 = 19 bits, which is valid.
               
               -- Excursion check --
               -- However, if the first byte encodes something > 0, it must not
               -- encode anything larger than 16#10FFFF# (as per the Unicode
               -- standard). 
               --
               -- It turns-out that there are two separate cases we need to
               -- check for. One is if the first three bits encode anything
               -- larger than 2#100# (anything > 4), since this will definately
               -- encode a value larger than 16#10FFFF#.
               --
               -- Alternately, if the value encoded by the first byte is
               -- exactly 4 (2#100#), we need to also check that the second
               -- byte does not encode anything larger than 2#001111#
               -- (anything > 16#F#).
               --
               -- Since this check is exclusive of the overlong check
               -- (it only applies when the first byte is not zero), we can
               -- encode this into a case statement, allowing for an efficient 
               -- jump-table to be constructed by the compiler
               
               case Codepoint is
                  when 0 =>
                     -- Possible overlong
                     if (Second_Byte and 2#00_111111#) < 16#10# then
                        Result := Unicode_Replacement_Character;
                        Status := Overlong;
                        return;
                     end if;
                     
                  when 2#001# .. 2#011# =>
                     -- This is fine
                     null;
                     
                  when 2#100# =>
                     -- Possible excursion
                     if (Second_Byte and 2#00_111111#) > 2#001111# then
                        Result := Unicode_Replacement_Character;
                        Status := Codepoint_Excursion;
                        return;
                     end if;
                     
                     -- Otherwise, we're ok
                     
                  when others =>
                     -- Definite excursion
                     Result := Unicode_Replacement_Character;
                     Status := Codepoint_Excursion;
                     return;
               end case;
               
               Codepoint_Shift_And_Add (Second_Byte);
               Codepoint_Shift_And_Add (Third_Byte);
               Codepoint_Shift_And_Add (Fourth_Byte);
               
               -- Check Unicode Noncharacter codepoints
               -- U+nFFFE .. U+nFFFF
               if (Codepoint and 16#FFFF#) in 16#FFFE# .. 16#FFFF# then
                  Result := Unicode_Replacement_Character;
                  Status := Codepoint_Excursion;

               else
                  Result := Wide_Wide_Character'Val (Codepoint);
                  Status := Success;
               end if;
                                
               return;
            end;
            
            
         when others =>
            raise Program_Error;
            -- Postcondition of Indicated_Continuation_Bytes excludes this
            -- possibility
      end case;
      
      -- Unreachable
      pragma Assert (False);
      
   end Verified_Combine;
   
   
   ----------------
   -- Try_Decode --
   ----------------
   procedure Try_Decode (Sequence          : in     Sequence_Array;
                         Last              :    out Sequence_Index;
                         Continuation_Bytes:    out Stream_Element_Count;
                         Result            :    out Wide_Wide_Character;
                         Status            :    out Decode_Status)
   is begin

      Continuation_Bytes := 0;
      Result := Unicode_Replacement_Character;
   
      if Sequence'Length = 0 then
         Last := Sequence_Index'First;
         Status := Short_Load;
         return;
      elsif not Validate_First (Sequence(Sequence'First)) then
         Last := Sequence'First;
         Status := Bad_Sequence;
         return;
      end if;
      
      pragma Assert (Validate_First (Sequence(Sequence'First)));
      
      Continuation_Bytes 
        := Indicated_Continuation_Bytes (Sequence(Sequence'First));
      pragma Assert (Continuation_Bytes in 0 .. 3);
      
      if Sequence'Length < Continuation_Bytes + 1 then
         Last   := Sequence'First;
         Status := Short_Load;
         return;
      end if;
      
      Last := Sequence'First + Continuation_Bytes;
         
      if Last > Sequence'First
        and then
        (for some Octet of Sequence (Sequence'First + 1 .. Last)
           => not Validate_Continuation (Octet))
      then
         Result := Unicode_Replacement_Character;
         Status := Bad_Sequence;
         return;
      end if;
      
      
      pragma Assert (Validate_Sequence 
                       (Sequence(Sequence'First .. Last)));
      
      Verified_Combine 
        (Sequence => Sequence(Sequence'First .. Last),
         Result   => Result,
         Status   => Status);
      
      pragma Assert (Status in Success | Overlong | Codepoint_Excursion);
      
   end Try_Decode;
   
end Unicode.UTF8_Stream_Decoder.Codec;
