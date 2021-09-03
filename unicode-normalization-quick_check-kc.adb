------------------------------------------------------------------------------
--                                                                          --
--                             Unicode Utilities                            --
--                                                                          --
--                        Normalization Form Utilities                      --
--                        Quick Check Query Facilities                      --
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

-- Generated: 2019-08-18
-- DerivedNormalizationProps.txt source
-- https://www.unicode.org/Public/UCD/latest/ucd/DerivedNormalizationProps.txt


-- ************* THIS FILE IS AUTOMATICALLY GENERATED ************ --
-- - See Unicode.UCD.Generate_Normalization_Quick_Check_Body.adb - --


-- DerivedNormalizationProps.txt --
-- Records loaded (NFKC_QC) = 428


function Unicode.Normalization.Quick_Check.KC (C: Wide_Wide_Character)
                                              return Quick_Check_Result
is
   type Codepoint is mod 2**24;


   function Plane_00_Lookup (C: Codepoint) return Quick_Check_Result is
      (case C is
         when 16#0000A0#               => NO,
         when 16#0000A8#               => NO,
         when 16#0000AA#               => NO,
         when 16#0000AF#               => NO,
         when 16#0000B2# .. 16#0000B3# => NO,
         when 16#0000B4#               => NO,
         when 16#0000B5#               => NO,
         when 16#0000B8#               => NO,
         when 16#0000B9#               => NO,
         when 16#0000BA#               => NO,
         when 16#0000BC# .. 16#0000BE# => NO,
         when 16#000132# .. 16#000133# => NO,
         when 16#00013F# .. 16#000140# => NO,
         when 16#000149#               => NO,
         when 16#00017F#               => NO,
         when 16#0001C4# .. 16#0001CC# => NO,
         when 16#0001F1# .. 16#0001F3# => NO,
         when 16#0002B0# .. 16#0002B8# => NO,
         when 16#0002D8# .. 16#0002DD# => NO,
         when 16#0002E0# .. 16#0002E4# => NO,
         when 16#000340# .. 16#000341# => NO,
         when 16#000343# .. 16#000344# => NO,
         when 16#000374#               => NO,
         when 16#00037A#               => NO,
         when 16#00037E#               => NO,
         when 16#000384# .. 16#000385# => NO,
         when 16#000387#               => NO,
         when 16#0003D0# .. 16#0003D6# => NO,
         when 16#0003F0# .. 16#0003F2# => NO,
         when 16#0003F4# .. 16#0003F5# => NO,
         when 16#0003F9#               => NO,
         when 16#000587#               => NO,
         when 16#000675# .. 16#000678# => NO,
         when 16#000958# .. 16#00095F# => NO,
         when 16#0009DC# .. 16#0009DD# => NO,
         when 16#0009DF#               => NO,
         when 16#000A33#               => NO,
         when 16#000A36#               => NO,
         when 16#000A59# .. 16#000A5B# => NO,
         when 16#000A5E#               => NO,
         when 16#000B5C# .. 16#000B5D# => NO,
         when 16#000E33#               => NO,
         when 16#000EB3#               => NO,
         when 16#000EDC# .. 16#000EDD# => NO,
         when 16#000F0C#               => NO,
         when 16#000F43#               => NO,
         when 16#000F4D#               => NO,
         when 16#000F52#               => NO,
         when 16#000F57#               => NO,
         when 16#000F5C#               => NO,
         when 16#000F69#               => NO,
         when 16#000F73#               => NO,
         when 16#000F75# .. 16#000F79# => NO,
         when 16#000F81#               => NO,
         when 16#000F93#               => NO,
         when 16#000F9D#               => NO,
         when 16#000FA2#               => NO,
         when 16#000FA7#               => NO,
         when 16#000FAC#               => NO,
         when 16#000FB9#               => NO,
         when 16#0010FC#               => NO,
         when 16#001D2C# .. 16#001D2E# => NO,
         when 16#001D30# .. 16#001D3A# => NO,
         when 16#001D3C# .. 16#001D4D# => NO,
         when 16#001D4F# .. 16#001D6A# => NO,
         when 16#001D78#               => NO,
         when 16#001D9B# .. 16#001DBF# => NO,
         when 16#001E9A# .. 16#001E9B# => NO,
         when 16#001F71#               => NO,
         when 16#001F73#               => NO,
         when 16#001F75#               => NO,
         when 16#001F77#               => NO,
         when 16#001F79#               => NO,
         when 16#001F7B#               => NO,
         when 16#001F7D#               => NO,
         when 16#001FBB#               => NO,
         when 16#001FBD#               => NO,
         when 16#001FBE#               => NO,
         when 16#001FBF# .. 16#001FC1# => NO,
         when 16#001FC9#               => NO,
         when 16#001FCB#               => NO,
         when 16#001FCD# .. 16#001FCF# => NO,
         when 16#001FD3#               => NO,
         when 16#001FDB#               => NO,
         when 16#001FDD# .. 16#001FDF# => NO,
         when 16#001FE3#               => NO,
         when 16#001FEB#               => NO,
         when 16#001FED# .. 16#001FEF# => NO,
         when 16#001FF9#               => NO,
         when 16#001FFB#               => NO,
         when 16#001FFD# .. 16#001FFE# => NO,
         when 16#002000# .. 16#00200A# => NO,
         when 16#002011#               => NO,
         when 16#002017#               => NO,
         when 16#002024# .. 16#002026# => NO,
         when 16#00202F#               => NO,
         when 16#002033# .. 16#002034# => NO,
         when 16#002036# .. 16#002037# => NO,
         when 16#00203C#               => NO,
         when 16#00203E#               => NO,
         when 16#002047# .. 16#002049# => NO,
         when 16#002057#               => NO,
         when 16#00205F#               => NO,
         when 16#002070#               => NO,
         when 16#002071#               => NO,
         when 16#002074# .. 16#002079# => NO,
         when 16#00207A# .. 16#00207C# => NO,
         when 16#00207D#               => NO,
         when 16#00207E#               => NO,
         when 16#00207F#               => NO,
         when 16#002080# .. 16#002089# => NO,
         when 16#00208A# .. 16#00208C# => NO,
         when 16#00208D#               => NO,
         when 16#00208E#               => NO,
         when 16#002090# .. 16#00209C# => NO,
         when 16#0020A8#               => NO,
         when 16#002100# .. 16#002101# => NO,
         when 16#002102#               => NO,
         when 16#002103#               => NO,
         when 16#002105# .. 16#002106# => NO,
         when 16#002107#               => NO,
         when 16#002109#               => NO,
         when 16#00210A# .. 16#002113# => NO,
         when 16#002115#               => NO,
         when 16#002116#               => NO,
         when 16#002119# .. 16#00211D# => NO,
         when 16#002120# .. 16#002122# => NO,
         when 16#002124#               => NO,
         when 16#002126#               => NO,
         when 16#002128#               => NO,
         when 16#00212A# .. 16#00212D# => NO,
         when 16#00212F# .. 16#002131# => NO,
         when 16#002133# .. 16#002134# => NO,
         when 16#002135# .. 16#002138# => NO,
         when 16#002139#               => NO,
         when 16#00213B#               => NO,
         when 16#00213C# .. 16#00213F# => NO,
         when 16#002140#               => NO,
         when 16#002145# .. 16#002149# => NO,
         when 16#002150# .. 16#00215F# => NO,
         when 16#002160# .. 16#00217F# => NO,
         when 16#002189#               => NO,
         when 16#00222C# .. 16#00222D# => NO,
         when 16#00222F# .. 16#002230# => NO,
         when 16#002329#               => NO,
         when 16#00232A#               => NO,
         when 16#002460# .. 16#00249B# => NO,
         when 16#00249C# .. 16#0024E9# => NO,
         when 16#0024EA#               => NO,
         when 16#002A0C#               => NO,
         when 16#002A74# .. 16#002A76# => NO,
         when 16#002ADC#               => NO,
         when 16#002C7C# .. 16#002C7D# => NO,
         when 16#002D6F#               => NO,
         when 16#002E9F#               => NO,
         when 16#002EF3#               => NO,
         when 16#002F00# .. 16#002FD5# => NO,
         when 16#003000#               => NO,
         when 16#003036#               => NO,
         when 16#003038# .. 16#00303A# => NO,
         when 16#00309B# .. 16#00309C# => NO,
         when 16#00309F#               => NO,
         when 16#0030FF#               => NO,
         when 16#003131# .. 16#00318E# => NO,
         when 16#003192# .. 16#003195# => NO,
         when 16#003196# .. 16#00319F# => NO,
         when 16#003200# .. 16#00321E# => NO,
         when 16#003220# .. 16#003229# => NO,
         when 16#00322A# .. 16#003247# => NO,
         when 16#003250#               => NO,
         when 16#003251# .. 16#00325F# => NO,
         when 16#003260# .. 16#00327E# => NO,
         when 16#003280# .. 16#003289# => NO,
         when 16#00328A# .. 16#0032B0# => NO,
         when 16#0032B1# .. 16#0032BF# => NO,
         when 16#0032C0# .. 16#0033FF# => NO,
         when 16#00A69C# .. 16#00A69D# => NO,
         when 16#00A770#               => NO,
         when 16#00A7F8# .. 16#00A7F9# => NO,
         when 16#00AB5C# .. 16#00AB5F# => NO,
         when 16#00F900# .. 16#00FA0D# => NO,
         when 16#00FA10#               => NO,
         when 16#00FA12#               => NO,
         when 16#00FA15# .. 16#00FA1E# => NO,
         when 16#00FA20#               => NO,
         when 16#00FA22#               => NO,
         when 16#00FA25# .. 16#00FA26# => NO,
         when 16#00FA2A# .. 16#00FA6D# => NO,
         when 16#00FA70# .. 16#00FAD9# => NO,
         when 16#00FB00# .. 16#00FB06# => NO,
         when 16#00FB13# .. 16#00FB17# => NO,
         when 16#00FB1D#               => NO,
         when 16#00FB1F# .. 16#00FB28# => NO,
         when 16#00FB29#               => NO,
         when 16#00FB2A# .. 16#00FB36# => NO,
         when 16#00FB38# .. 16#00FB3C# => NO,
         when 16#00FB3E#               => NO,
         when 16#00FB40# .. 16#00FB41# => NO,
         when 16#00FB43# .. 16#00FB44# => NO,
         when 16#00FB46# .. 16#00FBB1# => NO,
         when 16#00FBD3# .. 16#00FD3D# => NO,
         when 16#00FD50# .. 16#00FD8F# => NO,
         when 16#00FD92# .. 16#00FDC7# => NO,
         when 16#00FDF0# .. 16#00FDFB# => NO,
         when 16#00FDFC#               => NO,
         when 16#00FE10# .. 16#00FE16# => NO,
         when 16#00FE17#               => NO,
         when 16#00FE18#               => NO,
         when 16#00FE19#               => NO,
         when 16#00FE30#               => NO,
         when 16#00FE31# .. 16#00FE32# => NO,
         when 16#00FE33# .. 16#00FE34# => NO,
         when 16#00FE35#               => NO,
         when 16#00FE36#               => NO,
         when 16#00FE37#               => NO,
         when 16#00FE38#               => NO,
         when 16#00FE39#               => NO,
         when 16#00FE3A#               => NO,
         when 16#00FE3B#               => NO,
         when 16#00FE3C#               => NO,
         when 16#00FE3D#               => NO,
         when 16#00FE3E#               => NO,
         when 16#00FE3F#               => NO,
         when 16#00FE40#               => NO,
         when 16#00FE41#               => NO,
         when 16#00FE42#               => NO,
         when 16#00FE43#               => NO,
         when 16#00FE44#               => NO,
         when 16#00FE47#               => NO,
         when 16#00FE48#               => NO,
         when 16#00FE49# .. 16#00FE4C# => NO,
         when 16#00FE4D# .. 16#00FE4F# => NO,
         when 16#00FE50# .. 16#00FE52# => NO,
         when 16#00FE54# .. 16#00FE57# => NO,
         when 16#00FE58#               => NO,
         when 16#00FE59#               => NO,
         when 16#00FE5A#               => NO,
         when 16#00FE5B#               => NO,
         when 16#00FE5C#               => NO,
         when 16#00FE5D#               => NO,
         when 16#00FE5E#               => NO,
         when 16#00FE5F# .. 16#00FE61# => NO,
         when 16#00FE62#               => NO,
         when 16#00FE63#               => NO,
         when 16#00FE64# .. 16#00FE66# => NO,
         when 16#00FE68#               => NO,
         when 16#00FE69#               => NO,
         when 16#00FE6A# .. 16#00FE6B# => NO,
         when 16#00FE70# .. 16#00FE72# => NO,
         when 16#00FE74#               => NO,
         when 16#00FE76# .. 16#00FEFC# => NO,
         when 16#00FF01# .. 16#00FF03# => NO,
         when 16#00FF04#               => NO,
         when 16#00FF05# .. 16#00FF07# => NO,
         when 16#00FF08#               => NO,
         when 16#00FF09#               => NO,
         when 16#00FF0A#               => NO,
         when 16#00FF0B#               => NO,
         when 16#00FF0C#               => NO,
         when 16#00FF0D#               => NO,
         when 16#00FF0E# .. 16#00FF0F# => NO,
         when 16#00FF10# .. 16#00FF19# => NO,
         when 16#00FF1A# .. 16#00FF1B# => NO,
         when 16#00FF1C# .. 16#00FF1E# => NO,
         when 16#00FF1F# .. 16#00FF20# => NO,
         when 16#00FF21# .. 16#00FF3A# => NO,
         when 16#00FF3B#               => NO,
         when 16#00FF3C#               => NO,
         when 16#00FF3D#               => NO,
         when 16#00FF3E#               => NO,
         when 16#00FF3F#               => NO,
         when 16#00FF40#               => NO,
         when 16#00FF41# .. 16#00FF5A# => NO,
         when 16#00FF5B#               => NO,
         when 16#00FF5C#               => NO,
         when 16#00FF5D#               => NO,
         when 16#00FF5E#               => NO,
         when 16#00FF5F#               => NO,
         when 16#00FF60#               => NO,
         when 16#00FF61#               => NO,
         when 16#00FF62#               => NO,
         when 16#00FF63#               => NO,
         when 16#00FF64# .. 16#00FF65# => NO,
         when 16#00FF66# .. 16#00FF6F# => NO,
         when 16#00FF70#               => NO,
         when 16#00FF71# .. 16#00FF9D# => NO,
         when 16#00FF9E# .. 16#00FF9F# => NO,
         when 16#00FFA0# .. 16#00FFBE# => NO,
         when 16#00FFC2# .. 16#00FFC7# => NO,
         when 16#00FFCA# .. 16#00FFCF# => NO,
         when 16#00FFD2# .. 16#00FFD7# => NO,
         when 16#00FFDA# .. 16#00FFDC# => NO,
         when 16#00FFE0# .. 16#00FFE1# => NO,
         when 16#00FFE2#               => NO,
         when 16#00FFE3#               => NO,
         when 16#00FFE4#               => NO,
         when 16#00FFE5# .. 16#00FFE6# => NO,
         when 16#00FFE8#               => NO,
         when 16#00FFE9# .. 16#00FFEC# => NO,
         when 16#00FFED# .. 16#00FFEE# => NO,
         when 16#000300# .. 16#000304# => MAYBE,
         when 16#000306# .. 16#00030C# => MAYBE,
         when 16#00030F#               => MAYBE,
         when 16#000311#               => MAYBE,
         when 16#000313# .. 16#000314# => MAYBE,
         when 16#00031B#               => MAYBE,
         when 16#000323# .. 16#000328# => MAYBE,
         when 16#00032D# .. 16#00032E# => MAYBE,
         when 16#000330# .. 16#000331# => MAYBE,
         when 16#000338#               => MAYBE,
         when 16#000342#               => MAYBE,
         when 16#000345#               => MAYBE,
         when 16#000653# .. 16#000655# => MAYBE,
         when 16#00093C#               => MAYBE,
         when 16#0009BE#               => MAYBE,
         when 16#0009D7#               => MAYBE,
         when 16#000B3E#               => MAYBE,
         when 16#000B56#               => MAYBE,
         when 16#000B57#               => MAYBE,
         when 16#000BBE#               => MAYBE,
         when 16#000BD7#               => MAYBE,
         when 16#000C56#               => MAYBE,
         when 16#000CC2#               => MAYBE,
         when 16#000CD5# .. 16#000CD6# => MAYBE,
         when 16#000D3E#               => MAYBE,
         when 16#000D57#               => MAYBE,
         when 16#000DCA#               => MAYBE,
         when 16#000DCF#               => MAYBE,
         when 16#000DDF#               => MAYBE,
         when 16#00102E#               => MAYBE,
         when 16#001161# .. 16#001175# => MAYBE,
         when 16#0011A8# .. 16#0011C2# => MAYBE,
         when 16#001B35#               => MAYBE,
         when 16#003099# .. 16#00309A# => MAYBE,

         when others => YES)
      with Inline;

   function Plane_01_Lookup (C: Codepoint) return Quick_Check_Result is
      (case C is
         when 16#01D15E# .. 16#01D164# => NO,
         when 16#01D1BB# .. 16#01D1C0# => NO,
         when 16#01D400# .. 16#01D454# => NO,
         when 16#01D456# .. 16#01D49C# => NO,
         when 16#01D49E# .. 16#01D49F# => NO,
         when 16#01D4A2#               => NO,
         when 16#01D4A5# .. 16#01D4A6# => NO,
         when 16#01D4A9# .. 16#01D4AC# => NO,
         when 16#01D4AE# .. 16#01D4B9# => NO,
         when 16#01D4BB#               => NO,
         when 16#01D4BD# .. 16#01D4C3# => NO,
         when 16#01D4C5# .. 16#01D505# => NO,
         when 16#01D507# .. 16#01D50A# => NO,
         when 16#01D50D# .. 16#01D514# => NO,
         when 16#01D516# .. 16#01D51C# => NO,
         when 16#01D51E# .. 16#01D539# => NO,
         when 16#01D53B# .. 16#01D53E# => NO,
         when 16#01D540# .. 16#01D544# => NO,
         when 16#01D546#               => NO,
         when 16#01D54A# .. 16#01D550# => NO,
         when 16#01D552# .. 16#01D6A5# => NO,
         when 16#01D6A8# .. 16#01D6C0# => NO,
         when 16#01D6C1#               => NO,
         when 16#01D6C2# .. 16#01D6DA# => NO,
         when 16#01D6DB#               => NO,
         when 16#01D6DC# .. 16#01D6FA# => NO,
         when 16#01D6FB#               => NO,
         when 16#01D6FC# .. 16#01D714# => NO,
         when 16#01D715#               => NO,
         when 16#01D716# .. 16#01D734# => NO,
         when 16#01D735#               => NO,
         when 16#01D736# .. 16#01D74E# => NO,
         when 16#01D74F#               => NO,
         when 16#01D750# .. 16#01D76E# => NO,
         when 16#01D76F#               => NO,
         when 16#01D770# .. 16#01D788# => NO,
         when 16#01D789#               => NO,
         when 16#01D78A# .. 16#01D7A8# => NO,
         when 16#01D7A9#               => NO,
         when 16#01D7AA# .. 16#01D7C2# => NO,
         when 16#01D7C3#               => NO,
         when 16#01D7C4# .. 16#01D7CB# => NO,
         when 16#01D7CE# .. 16#01D7FF# => NO,
         when 16#01EE00# .. 16#01EE03# => NO,
         when 16#01EE05# .. 16#01EE1F# => NO,
         when 16#01EE21# .. 16#01EE22# => NO,
         when 16#01EE24#               => NO,
         when 16#01EE27#               => NO,
         when 16#01EE29# .. 16#01EE32# => NO,
         when 16#01EE34# .. 16#01EE37# => NO,
         when 16#01EE39#               => NO,
         when 16#01EE3B#               => NO,
         when 16#01EE42#               => NO,
         when 16#01EE47#               => NO,
         when 16#01EE49#               => NO,
         when 16#01EE4B#               => NO,
         when 16#01EE4D# .. 16#01EE4F# => NO,
         when 16#01EE51# .. 16#01EE52# => NO,
         when 16#01EE54#               => NO,
         when 16#01EE57#               => NO,
         when 16#01EE59#               => NO,
         when 16#01EE5B#               => NO,
         when 16#01EE5D#               => NO,
         when 16#01EE5F#               => NO,
         when 16#01EE61# .. 16#01EE62# => NO,
         when 16#01EE64#               => NO,
         when 16#01EE67# .. 16#01EE6A# => NO,
         when 16#01EE6C# .. 16#01EE72# => NO,
         when 16#01EE74# .. 16#01EE77# => NO,
         when 16#01EE79# .. 16#01EE7C# => NO,
         when 16#01EE7E#               => NO,
         when 16#01EE80# .. 16#01EE89# => NO,
         when 16#01EE8B# .. 16#01EE9B# => NO,
         when 16#01EEA1# .. 16#01EEA3# => NO,
         when 16#01EEA5# .. 16#01EEA9# => NO,
         when 16#01EEAB# .. 16#01EEBB# => NO,
         when 16#01F100# .. 16#01F10A# => NO,
         when 16#01F110# .. 16#01F12E# => NO,
         when 16#01F130# .. 16#01F14F# => NO,
         when 16#01F16A# .. 16#01F16C# => NO,
         when 16#01F190#               => NO,
         when 16#01F200# .. 16#01F202# => NO,
         when 16#01F210# .. 16#01F23B# => NO,
         when 16#01F240# .. 16#01F248# => NO,
         when 16#01F250# .. 16#01F251# => NO,
         when 16#0110BA#               => MAYBE,
         when 16#011127#               => MAYBE,
         when 16#01133E#               => MAYBE,
         when 16#011357#               => MAYBE,
         when 16#0114B0#               => MAYBE,
         when 16#0114BA#               => MAYBE,
         when 16#0114BD#               => MAYBE,
         when 16#0115AF#               => MAYBE,

         when others => YES)
      with Inline;

   function Plane_02_Lookup (C: Codepoint) return Quick_Check_Result is
      (case C is
         when 16#02F800# .. 16#02FA1D# => NO,

         when others => YES)
      with Inline;

   function Plane_03_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_04_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_05_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_06_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_07_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_08_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_09_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_0A_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_0B_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_0C_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_0D_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_0E_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_0F_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;

   function Plane_10_Lookup (C: Codepoint) return Quick_Check_Result is
      (YES) with Inline;


   CP: constant Codepoint := Codepoint (Wide_Wide_Character'Pos(C));
begin
   return (case CP is
      when 16#000000# .. 16#00FFFF# => Plane_00_Lookup (CP),
      when 16#010000# .. 16#01FFFF# => Plane_01_Lookup (CP),
      when 16#020000# .. 16#02FFFF# => Plane_02_Lookup (CP),
      when 16#030000# .. 16#03FFFF# => Plane_03_Lookup (CP),
      when 16#040000# .. 16#04FFFF# => Plane_04_Lookup (CP),
      when 16#050000# .. 16#05FFFF# => Plane_05_Lookup (CP),
      when 16#060000# .. 16#06FFFF# => Plane_06_Lookup (CP),
      when 16#070000# .. 16#07FFFF# => Plane_07_Lookup (CP),
      when 16#080000# .. 16#08FFFF# => Plane_08_Lookup (CP),
      when 16#090000# .. 16#09FFFF# => Plane_09_Lookup (CP),
      when 16#0A0000# .. 16#0AFFFF# => Plane_0A_Lookup (CP),
      when 16#0B0000# .. 16#0BFFFF# => Plane_0B_Lookup (CP),
      when 16#0C0000# .. 16#0CFFFF# => Plane_0C_Lookup (CP),
      when 16#0D0000# .. 16#0DFFFF# => Plane_0D_Lookup (CP),
      when 16#0E0000# .. 16#0EFFFF# => Plane_0E_Lookup (CP),
      when 16#0F0000# .. 16#0FFFFF# => Plane_0F_Lookup (CP),
      when 16#100000# .. 16#10FFFF# => Plane_10_Lookup (CP),

      when others => YES);

end Unicode.Normalization.Quick_Check.KC;
