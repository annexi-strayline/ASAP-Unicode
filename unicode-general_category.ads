------------------------------------------------------------------------------
--                                                                          --
--                             Unicode Utilities                            --
--                                                                          --
--                      General Category Query Utility                      --
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

package Unicode.General_Category with Pure is 
   
   -- Unicode Standard 12.0.0, Chapter 4.5, Table 4-4
   type General_Category_Type is
     (Letter_Uppercase,          -- Letter, uppercase          (Lu)
      Letter_Lowercase,          -- Letter, lowercase          (Ll)
      Letter_Titlecase,          -- Letter, titlecase          (Lt)
      Letter_Modifier,           -- Letter, modifier           (Lm)
      Letter_Other,              -- Letter, other              (Lo)
      
      Mark_Nonspacing,           -- Mark, nonspacing           (Mn)
      Mark_Spacing_Combining,    -- Mark, spacing combining    (Mc)
      Mark_Enclosing,            -- Mark, enclosing            (Me)
      
      Number_Decimal_Digit,      -- Number, decimal digit      (Nd)
      Number_Letter,             -- Number, letter             (Nl)
      Number_Other,              -- Number, other              (No)
      
      Punctuation_Connector,     -- Punctuation, connector     (Pc)
      Punctuation_Dash,          -- Punctuation, dash          (Pd)
      Punctuation_Open,          -- Punctuation, open          (Ps)
      Punctuation_Close,         -- Punctuation, close         (Pe)
      Punctuation_Initial_Quote, -- Punctuation, initial quote (Pi)
      Punctuation_Final_Quote,   -- Punctuation, final quote   (Pf)
      Punctuation_Other,         -- Punctuation, other         (Po)
      
      Symbol_Math,               -- Symbol, math               (Sm)
      Symbol_Currency,           -- Symbol, currency           (Sc)
      Symbol_Modifier,           -- Symbol, modifier           (Sk)
      Symbol_Other,              -- Symbol, other              (So)
      
      Separator_Space,           -- Separator, space           (Zs)
      Separator_Line,            -- Separator, line            (Zl)
      Separator_Paragraph,       -- Separator, paragraph       (Zp)
      
      Other_Control,             -- Other, control             (Cc)
      Other_Format,              -- Other, format              (Cf)
      Other_Surrogate,           -- Other, surrogate           (Cs)
      Other_Private_Use,         -- Other, private use         (Co)
      Other_Not_Assigned);       -- Other, not assigned        (Cn)

   
   function General_Category (C: Wide_Wide_Character) 
                             return General_Category_Type;
   -- Returns the General_Category value of C, as per the UCD.
   -- Codepoints not listed in UCD (including codepoints above U+10FFFF)
   -- return "Other_Not_Assigned"
   
   function Is_Category (C       : Wide_Wide_Character; 
                         Category: General_Category_Type)
                        return Boolean
     is (General_Category (C) = Category);
   -- Returns True if Character C is of the General_Category Category.
   --
   -- Useful in renames with a default set for Category.
   
     
end Unicode.General_Category;
