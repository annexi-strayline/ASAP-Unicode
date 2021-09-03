------------------------------------------------------------------------------
--                                                                          --
--                             Unicode Utilities                            --
--                                                                          --
--                      General Category Query Utility                      --
--                                                                          --
--                       Alias Lookup and Conversion                        --
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

package Unicode.General_Category.Alias with Pure is
   
   subtype General_Category_Alias_String is String (1 .. 2);
   
   type General_Category_Alias_List   
     is array (General_Category_Type) of General_Category_Alias_String
     with Preelaborable_Initialization;
      
   General_Category_Aliases: constant General_Category_Alias_List
     := (Letter_Uppercase          => "Lu",
         Letter_Lowercase          => "Ll",
         Letter_Titlecase          => "Lt",
         Letter_Modifier           => "Lm",
         Letter_Other              => "Lo",
         
         Mark_Nonspacing           => "Mn",
         Mark_Spacing_Combining    => "Mc",
         Mark_Enclosing            => "Me",
         
         Number_Decimal_Digit      => "Nd",
         Number_Letter             => "Nl",
         Number_Other              => "No",
         
         Punctuation_Connector     => "Pc",
         Punctuation_Dash          => "Pd",
         Punctuation_Open          => "Ps",
         Punctuation_Close         => "Pe",
         Punctuation_Initial_Quote => "Pi",
         Punctuation_Final_Quote   => "Pf",
         Punctuation_Other         => "Po",
         
         Symbol_Math               => "Sm",
         Symbol_Currency           => "Sc",
         Symbol_Modifier           => "Sk",
         Symbol_Other              => "So",
         
         Separator_Space           => "Zs",
         Separator_Line            => "Zl",
         Separator_Paragraph       => "Zp",
         
         Other_Control             => "Cc",
         Other_Format              => "Cf",
         Other_Surrogate           => "Cs",
         Other_Private_Use         => "Co",
         Other_Not_Assigned        => "Cn");
   
   
   function Category_By_Alias (Alias: in General_Category_Alias_String)
                              return General_Category_Type;
   -- Raises CONSTRAINT_ERROR if Alias does not identify a General_Category
   -- value.
   
end Unicode.General_Category.Alias;
