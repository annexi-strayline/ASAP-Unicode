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

with Ada.Characters.Conversions;
with Unicode.UTF8_Stream_Decoder.Codec;

package body Unicode.UTF8_Stream_Decoder is
   
   --
   -- Stream Decoders
   --
   
   -----------------
   -- Decode_Next --   (Wide_Wide_Character)
   -----------------
   function Decode_Next (UTF8_Stream : not null access Root_Stream_Type'Class)
                        return Wide_Wide_Character
   is
      use Codec;
      
      Buffer: Sequence_Array (1 .. 4) := (others => 0);
      
      Last, Continuation: Stream_Element_Offset;
      Status: Decode_Status;
      
   begin
      return Result: Wide_Wide_Character do
         -- Load next octet (hoping for a valid starting octet)
         Stream_Element'Read (UTF8_Stream, Buffer(1));
         
         -- Phase 1, check for sync and then mult-byte sequence
         Try_Decode (Sequence           => Buffer(1 .. 1),
                     Last               => Last,
                     Continuation_Bytes => Continuation,
                     Result             => Result,
                     Status             => Status);
         
         -- See if we have an indicated multi-byte condition
         if Status = Short_Load then
            -- Load the expected number of octets and re-run
            
            declare
               -- The (verified) postcondition of Try_Decode promises that
               -- Continuation will be 1 .. 3. Therefore we know that the
               -- above range will never be larger than 2 .. 3 + 1 = 4
               
               pragma Suppress (Index_Check);
               pragma Suppress (Length_Check);
               pragma Suppress (Overflow_Check);
               pragma Suppress (Range_Check);
               -- 2 .. 2, 2 .. 3, 2 .. 4 - all ok
               
               -- Also note that the components of Sequence_Array is
               -- a modular type - and therefore Overflow_Check and 
               -- Range_Check do not apply to those (the return values)
               -- anyways.
            begin
               Sequence_Array'Read 
                 (UTF8_Stream, Buffer(2 .. Continuation + 1));
            end;
            
            -- Run Try_Decode again for the final Result
            Try_Decode (Sequence           => Buffer(1 .. Continuation + 1),
                        Last               => Last,
                        Continuation_Bytes => Continuation,
                        Result             => Result,
                        Status             => Status);
            
         end if;
         
         -- Note that the postcondition of Try_Decode promises that if Status
         -- is not "Success", then Result will always be
         -- Unicode_Replacement_Character
         
      end return;
   end Decode_Next;
   
   
   -----------------
   -- Decode_Next --   (Wide_Character)
   -----------------
   function Decode_Next (UTF8_Stream : not null access Root_Stream_Type'Class)
                        return Wide_Character
   is 
      use Ada.Characters.Conversions;
      
      Full_Char: Wide_Wide_Character 
        := Decode_Next (UTF8_Stream);
   begin
      if not Is_Wide_Character (Full_Char) then
         raise Insufficient_Width
           with "Encoded character is not within the range of Wide_Character";
      else
         return To_Wide_Character (Full_Char);
      end if;
   end Decode_Next;
   
   
   -----------------
   -- Decode_Next --   (Character)
   -----------------
   function Decode_Next (UTF8_Stream : not null access Root_Stream_Type'Class)
                        return Character
   is 
      use Ada.Characters.Conversions;
      
      Full_Char: Wide_Wide_Character 
        := Decode_Next (UTF8_Stream);
   begin
      if not Is_Character (Full_Char) then
         raise Insufficient_Width
           with "Encoded character is not within the range of Character";
      else
         return To_Character (Full_Char);
      end if;
   end Decode_Next;
   
   
   -- Buffer Decoders ---------------------------------------------------------
   
   -----------------
   -- Decode_Next --   (Wide_Wide_Character)
   -----------------
   procedure Decode_Next (Buffer      : in     Stream_Element_Array;
                          Last        :    out Stream_Element_Offset;
                          Result      :    out Wide_Wide_Character)
   is
      use Codec;
      
      Start       : Stream_Element_Offset := Buffer'First;
      Continuation: Stream_Element_Offset;
      Status      : Decode_Status;
      
      Sequence_Last: Sequence_Index;
      
   begin
      if Buffer'Length = 0 then
         raise Short_Buffer with "Buffer is empty.";
      end if;
      
      Last := Buffer'First;
      
      -- Phase 1, check for sync and then mult-byte sequence
      Try_Decode 
        (Sequence           => Sequence_Array'(1 => Buffer(Buffer'First)),
         Last               => Sequence_Last,
         Continuation_Bytes => Continuation,
         Result             => Result,
         Status             => Status);
      
      -- See if we have an indicated multi-byte condition
      if Status = Short_Load then
         
         -- Check that we can actually provide the required number of
         -- continuation bytes.
         if Buffer'First + Continuation > Buffer'Last then
            raise Short_Buffer;
         end if;
         
         -- Re-run with the Load the expected number of octets
         
         declare
            -- The (verified) postcondition of Try_Decode promises that
            -- Continuation will be 1 .. 3. Therefore we know that the
            -- above range will never be larger than 2 .. 3 + 1 = 4
            
            pragma Suppress (Index_Check);
            pragma Suppress (Length_Check);
            pragma Suppress (Overflow_Check);
            pragma Suppress (Range_Check);
            -- 2 .. 2, 2 .. 3, 2 .. 4 - all ok
            
            -- Also note that the components of Sequence_Array is
            -- a modular type - and therefore Overflow_Check and 
            -- Range_Check do not apply to those (the return values)
            -- anyways.
            
            Sequence: constant Sequence_Array(1 .. 1 + Continuation)
              := Sequence_Array
                (Buffer(Buffer'First .. Buffer'First + Continuation));
            -- 1 + Continuation must be: 2, 3, 4
            -- Buffer'First + Continuation must be <= Buffer'Last, due
            -- to the if statement above
            
         begin
            -- Run Try_Decode again for the final Result
            Try_Decode (Sequence           => Sequence,
                        Last               => Sequence_Last,
                        Continuation_Bytes => Continuation,
                        Result             => Result,
                        Status             => Status);
         end;
      end if;
      
      Last := Buffer'First + Continuation;
      
      if Status /= Success then
         Result := Unicode_Replacement_Character;
      end if;
         
   end Decode_Next;
   
   
   -----------------
   -- Decode_Next --   (Wide_Character)
   -----------------
   procedure Decode_Next (Buffer      : in     Stream_Element_Array;
                          Last        :    out Stream_Element_Offset;
                          Result      :    out Wide_Character)
      
   is 
      use Ada.Characters.Conversions;
      
      Full_Char: Wide_Wide_Character;
      Temp_Last: Stream_Element_Offset;
      
   begin
      Decode_Next (Buffer       => Buffer,
                   Last         => Temp_Last,
                   Result       => Full_Char);
      
      if not Is_Wide_Character (Full_Char) then
         raise Insufficient_Width
           with "Encoded character is not within the range of Wide_Character";
      else
         Result := To_Wide_Character (Full_Char);
         Last   := Temp_Last;
      end if;
   end Decode_Next;
   
   
   -----------------
   -- Decode_Next --   (Character)
   -----------------
   procedure Decode_Next (Buffer      : in     Stream_Element_Array;
                          Last        :    out Stream_Element_Offset;
                          Result      :    out Character)
   is 
      use Ada.Characters.Conversions;
      
      Full_Char: Wide_Wide_Character;
      Temp_Last: Stream_Element_Offset;

   begin
      Decode_Next (Buffer       => Buffer,
                   Last         => Temp_Last,
                   Result       => Full_Char);
      
      if not Is_Character (Full_Char) then
         raise Insufficient_Width
           with "Encoded character is not within the range of Character";
      else
         Result := To_Character (Full_Char);
         Last   := Temp_Last;
      end if;
   end Decode_Next;
   
end Unicode.UTF8_Stream_Decoder;
