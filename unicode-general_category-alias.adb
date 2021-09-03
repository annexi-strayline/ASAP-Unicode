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

package body Unicode.General_Category.Alias is
   
   -----------------------
   -- Category_By_Alias --
   -----------------------
   function Category_By_Alias (Alias: in General_Category_Alias_String)
                              return General_Category_Type
   is
      procedure Bad_Alias with No_Return is
      begin
         raise Constraint_Error with
           "Alias " & '"' & Alias & '"' 
           & " does not identify a General_Category";
      end Bad_Alias;

      GCA: General_Category_Alias_List renames General_Category_Aliases;
   begin
      
      case Alias(1) is
         when 'C' =>
            case Alias(2) is
               when 'c' => return Other_Control;
               when 'f' => return Other_Format;
               when 's' => return Other_Surrogate;
               when 'o' => return Other_Private_Use;
               when 'n' => return Other_Not_Assigned;
                  
               when others => Bad_Alias;
            end case;
         
         when 'L' =>
            case Alias(2) is
               when 'l' => return Letter_Lowercase;
               when 'm' => return Letter_Modifier;
               when 'o' => return Letter_Other;
               when 't' => return Letter_Titlecase;
               when 'u' => return Letter_Uppercase;
                  
               when others => Bad_Alias;
            end case;
            
         when 'M' =>
            case Alias(2) is
               when 'c' => return Mark_Spacing_Combining;
               when 'e' => return Mark_Enclosing;
               when 'n' => return Mark_Nonspacing;
                  
               when others => Bad_Alias;
            end case;
            
         when 'N' =>
            case Alias(2) is
               when 'd' => return Number_Decimal_Digit;
               when 'l' => return Number_Letter;
               when 'o' => return Number_Other;
                  
               when others => Bad_Alias;
            end case;
            
         when 'P' =>
            case Alias(2) is
               when 'c' => return Punctuation_Connector;
               when 'd' => return Punctuation_Dash;
               when 'e' => return Punctuation_Close;
               when 'f' => return Punctuation_Final_Quote;
               when 'i' => return Punctuation_Initial_Quote;
               when 'o' => return Punctuation_Other;
               when 's' => return Punctuation_Open;
                  
               when others => Bad_Alias;
            end case;
            
         when 'S' =>
            case Alias(2) is
               when 'c' => return Symbol_Currency;
               when 'k' => return Symbol_Modifier;
               when 'm' => return Symbol_Math;
               when 'o' => return Symbol_Other;
                  
               when others => Bad_Alias;
            end case;
            
         when 'Z' =>
            case Alias(2) is
               when 'l' => return Separator_Line;
               when 'p' => return Separator_Paragraph;
               when 's' => return Separator_Space;
                  
               when others => Bad_Alias;
            end case;
            
         when others =>
            Bad_Alias;
         
      end case;
      
   end Category_By_Alias;
   
end Unicode.General_Category.Alias;
