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

-- Formally verified codec

private package Unicode.UTF8_Stream_Decoder.Codec
  with Pure, SPARK_Mode => On
is
   pragma Assertion_Policy (Ignore);
   
   pragma Assert (Stream_Element'Modulus = 2**8);
   -- Re-assertion for the encompasing package, to help the SPARK tools
   
   subtype Sequence_Index is Stream_Element_Offset range 1 .. 4;
   type    Sequence_Array is array (Sequence_Index range <>) of Stream_Element;
   -- A restricted analogue to Stream_Element_Array which is digestible by the
   -- SPARK tools - used internally for the various sequence validation/
   -- processing steps.
   
   
   type Decode_Status is
     (Success,
      Short_Load,           -- Additional bytes required
      Bad_Sequence,         -- 1. First octet was illegal (RFS3629)
                            -- 2. First octet was invalid (encoded more than 3
                            --    continuation bytes
                            -- 3. Expected continuation bytes were invalid
                            --    (not in 2#10_000000# .. 2#10_111111)
                            -- not properly marked as continuation bytes
      Overlong,             -- An illegal overlong condition was found.
      Codepoint_Excursion); -- The encoded sequence is not a legal Unicode
                            -- codepoint
   
   procedure Try_Decode (Sequence          : in     Sequence_Array;
                         Last              :    out Sequence_Index;
                         Continuation_Bytes:    out Stream_Element_Count;
                         Result            :    out Wide_Wide_Character;
                         Status            :    out Decode_Status) 
   with 
     Inline => True,  -- For performance, and since this package is not
                      -- embedded in the parent package because it SPARK_Mode
                      -- can only be applied to library-level packages
     Global => null,
     Post => ((Continuation_Bytes in 0 .. 3)
                and then (if Status = Success then
                             Last = Sequence'First + Continuation_Bytes
                             
                             and then 
                             (for all Octet of 
                                Sequence(Sequence'First .. Last) =>
                                  Octet not in 16#C0# | 16#C1# 
                                  | 16#F5# .. 16#FF#)
                             -- STD63/RFC3629 Section 1 - Prohibited octets
                             -- in any UTF-8 text
                             
                             and then
                             (case Continuation_Bytes is
                                 when 0 =>
                                    Wide_Wide_Character'Pos (Result)
                                      in 16#000000# .. 16#00007F#,
                                 when 1 =>
                                    Wide_Wide_Character'Pos (Result)
                                      in 16#000080# .. 16#0007FF#,
                                 when 2 =>
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
                                 when 3 =>
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
                                    -- There are no others
                                    False)
                          
                          
                          elsif Status = Short_Load then
                             Sequence'Length = 0 
                             
                             or else
                             Continuation_Bytes in 1 .. 3
                          
                          else
                             Result = Unicode_Replacement_Character
                             
                             and then
                             (if Sequence'Length > 0 then 
                                 Last in Sequence'Range
                              else
                                 Last = Sequence_Index'First)));
                             -- Used by the non-spark portions to implement
                             -- a very efficient two-step process
   
   -- Try_Decode attempts to decode a Character, and indicates the reson for
   -- failure if not successful. Indented to be called from full Ada, this
   -- procedure carries the full contract of a correct decoding result in the
   -- postcondtion which has been fully verified.
   --
   -- Try_Decode is invoked by the Decode_Next subprograms in the parent
   -- package
   
end Unicode.UTF8_Stream_Decoder.Codec;
