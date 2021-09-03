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

-- Rationale
-- =========
--
-- The language-defined package Ada.Strings.UTF_Encoding generally requires a
-- complete String type of a UTF-8 encoded octet string (UTF_8_String). In
-- streaming applications with potentially very large sizes (such as a JSON
-- object, or a program source), the UTF_8_String passed to Decode will often
-- need to be chunked off the larger stream. This chunking must be aware enough
-- of UTF-8 to properly ensure that each (potentially multi-byte) encoded
-- character placed into that buffer is not truncated.
--
-- This package handles the inline processing of streams or arbitrarily-sized
-- buffers, where such chunking would otherwise be required to use the
-- predefined packages.
--
-- This package is specifically designed for use in high-volume, high-integrity
-- server environments. The binary decoding codec is fully verified.
--
-- For encoding to UTF-8, the language-defined package should be sufficient,
-- due to the lack of synchronization issues with the source data.


with Ada.Streams;

package Unicode.UTF8_Stream_Decoder with Pure is
   
   pragma Assert (Ada.Streams.Stream_Element'Modulus = 2**8);
   -- This is important for ensuring that Decode_Next will not have either
   -- constant Synchronization_Errors or dropped characters in the very
   -- unlikely event that Stream_Element is not an octet
   
   Insufficient_Width: exception;
   Short_Buffer      : exception;
   
   
   -- Ada Stream Decoder ------------------------------------------------------
   
   subtype Root_Stream_Type is Ada.Streams.Root_Stream_Type;

   
   function Decode_Next (UTF8_Stream: not null access Root_Stream_Type'Class)
                        return Character;
   
   function Decode_Next (UTF8_Stream: not null access Root_Stream_Type'Class)
                        return Wide_Character;
   
   function Decode_Next (UTF8_Stream: not null access Root_Stream_Type'Class)
                        return Wide_Wide_Character;
   -- Attempts to decode a UTF-8 byte sequence from UTF8_Stream, and returns
   -- the corresponding Character type.
   --
   -- The Unicode standard provides a specific 'REPLACEMENT CHARACTER' (U+FFFD)
   -- for use in error conditions when decoding. Decode_Next will return a
   -- single replacement character for each invalid octet (byte) it encounters.
   --
   -- For the overload returning Character, this replacement character will
   -- induce an Insufficient_Width exception (see below).
   --
   -- Any invalid actual encoding (invalid Unicode codepoints) are always 
   -- returned as 'REPLACEMENT CHARACTER'. Insufficient_Width only applies
   -- to returns to Character or Wide_Character.
   --
   -- Where possible a 'REPLACEMENT CHARACTER' will correspond to an entire
   -- sequence (maximum of 4 octets). Where the leading byte is not valid
   -- or allowed in UTF-8, the 'REPLACEMENT CHARACTER' corresponds to that
   -- single octet.
   --
   -- Any invalid sequences as per IETF STD63/RFC3629 (UTF-8) are resolved as
   -- a 'REPLACEMENT CHARACTER'. This includes
   -- * Octets that shall never appear in UTF-8 text (Section 1):
   --   (16#C0#, 16#C1#, 16#F5# .. 16#FF#)
   -- * Codepoints that are reserved for UTF-16 "surrogate pairs" (Section 3):
   --   (U+D800 .. U+DFFF)
   --
   -- Any "Noncharacter" codepoints in Unicode are resolved as a 'REPLACEMENT
   -- CHARACTER', in accordance with Unicode Corrigendum #9 (2013-Jan-30).
   -- These codepoints are for internal use only and should not be interchanged
   -- As Ada natively supports Wide_Wide_Character/Strings, UTF-8 encodoing/
   -- decoding is not expected to be used internally.
   --
   -- Unicode defines "Noncharacter" codepoints as being:
   -- * U+nFFFE, U+nFFFF
   -- * U+FDD0 .. U+FDEF
   --
   -- Any exceptions raised by attempts to read from UTF8_Stream are
   -- propagated.
   --
   -- -- Explicit Raises --
   -- *  Insufficient_Width: (Character and Wide_Character returns only)
   --                        Raised if the next character encoded in the UTF-8
   --                        stream is out of range of the return character
   --                        type. 
   --
   --                        The Wide_Wide_Character overload will never raise
   --                        this exception.
   --
   --                        -- Warning: Character return type --
   --                        If the stream has any errors, which are
   --                        normally replaced with 'REPLACEMENT CHARACTER'
   --                        (U+FFFD), this exception will be raised instead,
   --                        since 
   
   
   
   -- High-efficiency Buffer Decoder ------------------------------------------
   
   -- The internal decode codec is fully verified as far as possible, and so
   -- relevant checks have been disabled. Using buffer decoding allows for the
   -- fastest possible processing.
   
   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;
   use type Ada.Streams.Stream_Element_Count;
   use type Ada.Streams.Stream_Element_Array;
   
   subtype Stream_Element is Ada.Streams.Stream_Element;
   subtype Stream_Element_Offset is Ada.Streams.Stream_Element_Offset;
   subtype Stream_Element_Count  is Ada.Streams.Stream_Element_Count;
   subtype Stream_Element_Array  is Ada.Streams.Stream_Element_Array;
   
   
   procedure Decode_Next (Buffer      : in     Stream_Element_Array;
                          Last        :    out Stream_Element_Offset;
                          Result      :    out Character);
   
   procedure Decode_Next (Buffer      : in     Stream_Element_Array;
                          Last        :    out Stream_Element_Offset;
                          Result      :    out Wide_Character);
   
   procedure Decode_Next (Buffer      : in     Stream_Element_Array;
                          Last        :    out Stream_Element_Offset;
                          Result      :    out Wide_Wide_Character);
   -- Attempts to decode a UTF-8 byte sequence from Buffer, and returns
   -- the corresponding Character type. 
   --
   -- The Unicode standard provides a specific 'REPLACEMENT CHARACTER' (U+FFFD)
   -- for use in error conditions when decoding. Decode_Next will return a
   -- single replacement character for each invalid octet (byte) it encounters.
   -- Last will be set to the index of that octet.
   --
   -- For the overload returning Character, this replacement character will
   -- induce an Insufficient_Width exception (see below).
   --
   -- Any invalid actual encoding (invalid Unicode codepoints) are always 
   -- returned as 'REPLACEMENT CHARACTER'. Insufficient_Width only applies
   -- to returns to Character or Wide_Character.
   --
   -- Where possible a 'REPLACEMENT CHARACTER' will correspond to an entire
   -- sequence (maximum of 4 octets). Where the leading byte is not valid
   -- or allowed in UTF-8, the 'REPLACEMENT CHARACTER' corresponds to that
   -- single octet.
   --
   -- Any invalid sequences as per IETF STD63/RFC3629 (UTF-8) are resolved as
   -- a 'REPLACEMENT CHARACTER'. This includes
   -- * Octets that shall never appear in UTF-8 text (Section 1):
   --   (16#C0#, 16#C1#, 16#F5# .. 16#FF#)
   -- * Codepoints that are reserved for UTF-16 "surrogate pairs" (Section 3):
   --   (U+D800 .. U+DFFF)
   --
   -- Any "Noncharacter" codepoints in Unicode are resolved as a 'REPLACEMENT
   -- CHARACTER', in accordance with Unicode Corrigendum #9 (2013-Jan-30).
   -- These codepoints are for internal use only and should not be interchanged
   -- As Ada natively supports Wide_Wide_Character/Strings, UTF-8 encodoing/
   -- decoding is not expected to be used internally.
   --
   -- Any exceptions raised by attempts to read from UTF8_Stream are
   -- propagated.
   --
   -- -- Explicit Raises --
   -- *  Insufficient_Width: (Character and Wide_Character returns only)
   --                        Raised if the next character encoded in the buffer
   --                        stream is out of range of the return character
   --                        type. 
   --
   --                        The Wide_Wide_Character overload will never raise
   --                        this exception.
   --
   --                        -- Warning: Character return type --
   --                        If the stream has any errors, which are
   --                        normally replaced with 'REPLACEMENT CHARACTER'
   --                        (U+FFFD), this exception will be raised instead,
   --                        since 
   -- * Short_Buffer       : Raised if Buffer'First indexes a byte that
   --                        encodes a valid UTF-8 initial byte which also
   --                        indicates more continuation bytes than exist
   --                        in the remaining elements of Buffer
   
   
end Unicode.UTF8_Stream_Decoder;
