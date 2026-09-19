unit ucpUtils;

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$H+}
{$M+}

interface

uses
  SysUtils, Variants, Classes;

type
  TUnicodeCategory = (
    ucLetter,
    ucMark,
    ucNumber,
    ucSeparator,
    ucSymbol,
    ucPunctuation,
    ucOther
  );

  TUnicodeRange = record
    First: Cardinal;
    Last: Cardinal;
    Category: TUnicodeCategory;
  end;

function UnicodeCategory(Code: Cardinal): TUnicodeCategory;
function IsLetter(Code: Cardinal): Boolean;
function IsMark(Code: Cardinal): Boolean;
function IsNumber(Code: Cardinal): Boolean;
function IsSeparator(Code: Cardinal): Boolean;
function IsSymbol(Code: Cardinal): Boolean;
function IsPunctuation(Code: Cardinal): Boolean;

function CodePointAt(const Text: UnicodeString; Pos: Integer): Cardinal;
function CodePointSize(Code: Cardinal): Integer;
function CharAt(const Text: UnicodeString; Pos: Integer): UnicodeString;
function GraphemeToUserPart(const Text: UnicodeString; Pos: Integer): UnicodeString;
function GraphemeToNextPart(const Text: UnicodeString; Pos: Integer): UnicodeString;

type
  TMBToWC_Proc = procedure(S: AnsiChar; var R: WideChar);
  TWCToMB_Proc = procedure(S: WideChar; var R: AnsiChar);

function ucpAnsiToUnicode(const S: AnsiString): WideString; overload;
function ucpAnsiToUnicode(const S: AnsiString; Proc: Tmbtowc_proc): WideString; overload;

function ucpUnicodeToAnsi(const S: WideString): AnsiString; overload;
function ucpUnicodeToAnsi(const S: WideString; Proc: Twctomb_proc): AnsiString; overload;

procedure ucpInstall(MBToWCProc: Tmbtowc_proc; WCtoMBProc: Twctomb_proc{$IFDEF FPC}; Hook: Boolean{$ENDIF});

implementation

uses
  ucp1250; //the default code page

const
  UnicodeRanges: array[0..2248] of TUnicodeRange = (
    (First: $0000; Last: $001F; Category: ucOther),
    (First: $0020; Last: $0020; Category: ucSeparator),
    (First: $0021; Last: $0023; Category: ucPunctuation),
    (First: $0024; Last: $0024; Category: ucSymbol),
    (First: $0025; Last: $002A; Category: ucPunctuation),
    (First: $002B; Last: $002B; Category: ucSymbol),
    (First: $002C; Last: $002F; Category: ucPunctuation),
    (First: $0030; Last: $0039; Category: ucNumber),
    (First: $003A; Last: $003B; Category: ucPunctuation),
    (First: $003C; Last: $003E; Category: ucSymbol),
    (First: $003F; Last: $0040; Category: ucPunctuation),
    (First: $0041; Last: $005A; Category: ucLetter),
    (First: $005B; Last: $005D; Category: ucPunctuation),
    (First: $005E; Last: $005E; Category: ucSymbol),
    (First: $005F; Last: $005F; Category: ucPunctuation),
    (First: $0060; Last: $0060; Category: ucSymbol),
    (First: $0061; Last: $007A; Category: ucLetter),
    (First: $007B; Last: $007B; Category: ucPunctuation),
    (First: $007C; Last: $007C; Category: ucSymbol),
    (First: $007D; Last: $007D; Category: ucPunctuation),
    (First: $007E; Last: $007E; Category: ucSymbol),
    (First: $007F; Last: $009F; Category: ucOther),
    (First: $00A0; Last: $00A0; Category: ucSeparator),
    (First: $00A1; Last: $00A1; Category: ucPunctuation),
    (First: $00A2; Last: $00A9; Category: ucSymbol),
    (First: $00AA; Last: $00AA; Category: ucLetter),
    (First: $00AB; Last: $00AB; Category: ucPunctuation),
    (First: $00AC; Last: $00AC; Category: ucSymbol),
    (First: $00AD; Last: $00AD; Category: ucOther),
    (First: $00AE; Last: $00B1; Category: ucSymbol),
    (First: $00B2; Last: $00B3; Category: ucNumber),
    (First: $00B4; Last: $00B4; Category: ucSymbol),
    (First: $00B5; Last: $00B5; Category: ucLetter),
    (First: $00B6; Last: $00B6; Category: ucSymbol),
    (First: $00B7; Last: $00B7; Category: ucPunctuation),
    (First: $00B8; Last: $00B8; Category: ucSymbol),
    (First: $00B9; Last: $00B9; Category: ucNumber),
    (First: $00BA; Last: $00BA; Category: ucLetter),
    (First: $00BB; Last: $00BB; Category: ucPunctuation),
    (First: $00BC; Last: $00BE; Category: ucNumber),
    (First: $00BF; Last: $00BF; Category: ucPunctuation),
    (First: $00C0; Last: $00D6; Category: ucLetter),
    (First: $00D7; Last: $00D7; Category: ucSymbol),
    (First: $00D8; Last: $00F6; Category: ucLetter),
    (First: $00F7; Last: $00F7; Category: ucSymbol),
    (First: $00F8; Last: $02C1; Category: ucLetter),
    (First: $02C2; Last: $02C5; Category: ucSymbol),
    (First: $02C6; Last: $02D1; Category: ucLetter),
    (First: $02D2; Last: $02DF; Category: ucSymbol),
    (First: $02E0; Last: $02E4; Category: ucLetter),
    (First: $02E5; Last: $02ED; Category: ucSymbol),
    (First: $02EE; Last: $02EE; Category: ucLetter),
    (First: $02EF; Last: $02FF; Category: ucSymbol),
    (First: $0300; Last: $036F; Category: ucMark),
    (First: $0374; Last: $0375; Category: ucSymbol),
    (First: $037A; Last: $037D; Category: ucLetter),
    (First: $037E; Last: $037E; Category: ucPunctuation),
    (First: $0384; Last: $0385; Category: ucSymbol),
    (First: $0386; Last: $0386; Category: ucLetter),
    (First: $0387; Last: $0387; Category: ucPunctuation),
    (First: $0388; Last: $038A; Category: ucLetter),
    (First: $038C; Last: $038C; Category: ucLetter),
    (First: $038E; Last: $03A1; Category: ucLetter),
    (First: $03A3; Last: $03CE; Category: ucLetter),
    (First: $03D0; Last: $03F5; Category: ucLetter),
    (First: $03F6; Last: $03F6; Category: ucSymbol),
    (First: $03F7; Last: $0481; Category: ucLetter),
    (First: $0482; Last: $0482; Category: ucSymbol),
    (First: $0483; Last: $0486; Category: ucMark),
    (First: $0488; Last: $0489; Category: ucMark),
    (First: $048A; Last: $0513; Category: ucLetter),
    (First: $0531; Last: $0556; Category: ucLetter),
    (First: $0559; Last: $0559; Category: ucLetter),
    (First: $055A; Last: $055F; Category: ucPunctuation),
    (First: $0561; Last: $0587; Category: ucLetter),
    (First: $0589; Last: $058A; Category: ucPunctuation),
    (First: $0591; Last: $05BD; Category: ucMark),
    (First: $05BE; Last: $05BE; Category: ucPunctuation),
    (First: $05BF; Last: $05BF; Category: ucMark),
    (First: $05C0; Last: $05C0; Category: ucPunctuation),
    (First: $05C1; Last: $05C2; Category: ucMark),
    (First: $05C3; Last: $05C3; Category: ucPunctuation),
    (First: $05C4; Last: $05C5; Category: ucMark),
    (First: $05C6; Last: $05C6; Category: ucPunctuation),
    (First: $05C7; Last: $05C7; Category: ucMark),
    (First: $05D0; Last: $05EA; Category: ucLetter),
    (First: $05F0; Last: $05F2; Category: ucLetter),
    (First: $05F3; Last: $05F4; Category: ucPunctuation),
    (First: $0600; Last: $0603; Category: ucOther),
    (First: $060B; Last: $060B; Category: ucSymbol),
    (First: $060C; Last: $060D; Category: ucPunctuation),
    (First: $060E; Last: $060F; Category: ucSymbol),
    (First: $0610; Last: $0615; Category: ucMark),
    (First: $061B; Last: $061B; Category: ucPunctuation),
    (First: $061E; Last: $061F; Category: ucPunctuation),
    (First: $0621; Last: $063A; Category: ucLetter),
    (First: $0640; Last: $064A; Category: ucLetter),
    (First: $064B; Last: $065E; Category: ucMark),
    (First: $0660; Last: $0669; Category: ucNumber),
    (First: $066A; Last: $066D; Category: ucPunctuation),
    (First: $066E; Last: $066F; Category: ucLetter),
    (First: $0670; Last: $0670; Category: ucMark),
    (First: $0671; Last: $06D3; Category: ucLetter),
    (First: $06D4; Last: $06D4; Category: ucPunctuation),
    (First: $06D5; Last: $06D5; Category: ucLetter),
    (First: $06D6; Last: $06DC; Category: ucMark),
    (First: $06DD; Last: $06DD; Category: ucOther),
    (First: $06DE; Last: $06E4; Category: ucMark),
    (First: $06E5; Last: $06E6; Category: ucLetter),
    (First: $06E7; Last: $06E8; Category: ucMark),
    (First: $06E9; Last: $06E9; Category: ucSymbol),
    (First: $06EA; Last: $06ED; Category: ucMark),
    (First: $06EE; Last: $06EF; Category: ucLetter),
    (First: $06F0; Last: $06F9; Category: ucNumber),
    (First: $06FA; Last: $06FC; Category: ucLetter),
    (First: $06FD; Last: $06FE; Category: ucSymbol),
    (First: $06FF; Last: $06FF; Category: ucLetter),
    (First: $0700; Last: $070D; Category: ucPunctuation),
    (First: $070F; Last: $070F; Category: ucOther),
    (First: $0710; Last: $0710; Category: ucLetter),
    (First: $0711; Last: $0711; Category: ucMark),
    (First: $0712; Last: $072F; Category: ucLetter),
    (First: $0730; Last: $074A; Category: ucMark),
    (First: $074D; Last: $076D; Category: ucLetter),
    (First: $0780; Last: $07A5; Category: ucLetter),
    (First: $07A6; Last: $07B0; Category: ucMark),
    (First: $07B1; Last: $07B1; Category: ucLetter),
    (First: $07C0; Last: $07C9; Category: ucNumber),
    (First: $07CA; Last: $07EA; Category: ucLetter),
    (First: $07EB; Last: $07F3; Category: ucMark),
    (First: $07F4; Last: $07F5; Category: ucLetter),
    (First: $07F6; Last: $07F6; Category: ucSymbol),
    (First: $07F7; Last: $07F9; Category: ucPunctuation),
    (First: $07FA; Last: $07FA; Category: ucLetter),
    (First: $0901; Last: $0903; Category: ucMark),
    (First: $0904; Last: $0939; Category: ucLetter),
    (First: $093C; Last: $093C; Category: ucMark),
    (First: $093D; Last: $093D; Category: ucLetter),
    (First: $093E; Last: $094D; Category: ucMark),
    (First: $0950; Last: $0950; Category: ucLetter),
    (First: $0951; Last: $0954; Category: ucMark),
    (First: $0958; Last: $0961; Category: ucLetter),
    (First: $0962; Last: $0963; Category: ucMark),
    (First: $0964; Last: $0965; Category: ucPunctuation),
    (First: $0966; Last: $096F; Category: ucNumber),
    (First: $0970; Last: $0970; Category: ucPunctuation),
    (First: $097B; Last: $097F; Category: ucLetter),
    (First: $0981; Last: $0983; Category: ucMark),
    (First: $0985; Last: $098C; Category: ucLetter),
    (First: $098F; Last: $0990; Category: ucLetter),
    (First: $0993; Last: $09A8; Category: ucLetter),
    (First: $09AA; Last: $09B0; Category: ucLetter),
    (First: $09B2; Last: $09B2; Category: ucLetter),
    (First: $09B6; Last: $09B9; Category: ucLetter),
    (First: $09BC; Last: $09BC; Category: ucMark),
    (First: $09BD; Last: $09BD; Category: ucLetter),
    (First: $09BE; Last: $09C4; Category: ucMark),
    (First: $09C7; Last: $09C8; Category: ucMark),
    (First: $09CB; Last: $09CD; Category: ucMark),
    (First: $09CE; Last: $09CE; Category: ucLetter),
    (First: $09D7; Last: $09D7; Category: ucMark),
    (First: $09DC; Last: $09DD; Category: ucLetter),
    (First: $09DF; Last: $09E1; Category: ucLetter),
    (First: $09E2; Last: $09E3; Category: ucMark),
    (First: $09E6; Last: $09EF; Category: ucNumber),
    (First: $09F0; Last: $09F1; Category: ucLetter),
    (First: $09F2; Last: $09F3; Category: ucSymbol),
    (First: $09F4; Last: $09F9; Category: ucNumber),
    (First: $09FA; Last: $09FA; Category: ucSymbol),
    (First: $0A01; Last: $0A03; Category: ucMark),
    (First: $0A05; Last: $0A0A; Category: ucLetter),
    (First: $0A0F; Last: $0A10; Category: ucLetter),
    (First: $0A13; Last: $0A28; Category: ucLetter),
    (First: $0A2A; Last: $0A30; Category: ucLetter),
    (First: $0A32; Last: $0A33; Category: ucLetter),
    (First: $0A35; Last: $0A36; Category: ucLetter),
    (First: $0A38; Last: $0A39; Category: ucLetter),
    (First: $0A3C; Last: $0A3C; Category: ucMark),
    (First: $0A3E; Last: $0A42; Category: ucMark),
    (First: $0A47; Last: $0A48; Category: ucMark),
    (First: $0A4B; Last: $0A4D; Category: ucMark),
    (First: $0A59; Last: $0A5C; Category: ucLetter),
    (First: $0A5E; Last: $0A5E; Category: ucLetter),
    (First: $0A66; Last: $0A6F; Category: ucNumber),
    (First: $0A70; Last: $0A71; Category: ucMark),
    (First: $0A72; Last: $0A74; Category: ucLetter),
    (First: $0A81; Last: $0A83; Category: ucMark),
    (First: $0A85; Last: $0A8D; Category: ucLetter),
    (First: $0A8F; Last: $0A91; Category: ucLetter),
    (First: $0A93; Last: $0AA8; Category: ucLetter),
    (First: $0AAA; Last: $0AB0; Category: ucLetter),
    (First: $0AB2; Last: $0AB3; Category: ucLetter),
    (First: $0AB5; Last: $0AB9; Category: ucLetter),
    (First: $0ABC; Last: $0ABC; Category: ucMark),
    (First: $0ABD; Last: $0ABD; Category: ucLetter),
    (First: $0ABE; Last: $0AC5; Category: ucMark),
    (First: $0AC7; Last: $0AC9; Category: ucMark),
    (First: $0ACB; Last: $0ACD; Category: ucMark),
    (First: $0AD0; Last: $0AD0; Category: ucLetter),
    (First: $0AE0; Last: $0AE1; Category: ucLetter),
    (First: $0AE2; Last: $0AE3; Category: ucMark),
    (First: $0AE6; Last: $0AEF; Category: ucNumber),
    (First: $0AF1; Last: $0AF1; Category: ucSymbol),
    (First: $0B01; Last: $0B03; Category: ucMark),
    (First: $0B05; Last: $0B0C; Category: ucLetter),
    (First: $0B0F; Last: $0B10; Category: ucLetter),
    (First: $0B13; Last: $0B28; Category: ucLetter),
    (First: $0B2A; Last: $0B30; Category: ucLetter),
    (First: $0B32; Last: $0B33; Category: ucLetter),
    (First: $0B35; Last: $0B39; Category: ucLetter),
    (First: $0B3C; Last: $0B3C; Category: ucMark),
    (First: $0B3D; Last: $0B3D; Category: ucLetter),
    (First: $0B3E; Last: $0B43; Category: ucMark),
    (First: $0B47; Last: $0B48; Category: ucMark),
    (First: $0B4B; Last: $0B4D; Category: ucMark),
    (First: $0B56; Last: $0B57; Category: ucMark),
    (First: $0B5C; Last: $0B5D; Category: ucLetter),
    (First: $0B5F; Last: $0B61; Category: ucLetter),
    (First: $0B66; Last: $0B6F; Category: ucNumber),
    (First: $0B70; Last: $0B70; Category: ucSymbol),
    (First: $0B71; Last: $0B71; Category: ucLetter),
    (First: $0B82; Last: $0B82; Category: ucMark),
    (First: $0B83; Last: $0B83; Category: ucLetter),
    (First: $0B85; Last: $0B8A; Category: ucLetter),
    (First: $0B8E; Last: $0B90; Category: ucLetter),
    (First: $0B92; Last: $0B95; Category: ucLetter),
    (First: $0B99; Last: $0B9A; Category: ucLetter),
    (First: $0B9C; Last: $0B9C; Category: ucLetter),
    (First: $0B9E; Last: $0B9F; Category: ucLetter),
    (First: $0BA3; Last: $0BA4; Category: ucLetter),
    (First: $0BA8; Last: $0BAA; Category: ucLetter),
    (First: $0BAE; Last: $0BB9; Category: ucLetter),
    (First: $0BBE; Last: $0BC2; Category: ucMark),
    (First: $0BC6; Last: $0BC8; Category: ucMark),
    (First: $0BCA; Last: $0BCD; Category: ucMark),
    (First: $0BD7; Last: $0BD7; Category: ucMark),
    (First: $0BE6; Last: $0BF2; Category: ucNumber),
    (First: $0BF3; Last: $0BFA; Category: ucSymbol),
    (First: $0C01; Last: $0C03; Category: ucMark),
    (First: $0C05; Last: $0C0C; Category: ucLetter),
    (First: $0C0E; Last: $0C10; Category: ucLetter),
    (First: $0C12; Last: $0C28; Category: ucLetter),
    (First: $0C2A; Last: $0C33; Category: ucLetter),
    (First: $0C35; Last: $0C39; Category: ucLetter),
    (First: $0C3E; Last: $0C44; Category: ucMark),
    (First: $0C46; Last: $0C48; Category: ucMark),
    (First: $0C4A; Last: $0C4D; Category: ucMark),
    (First: $0C55; Last: $0C56; Category: ucMark),
    (First: $0C60; Last: $0C61; Category: ucLetter),
    (First: $0C66; Last: $0C6F; Category: ucNumber),
    (First: $0C82; Last: $0C83; Category: ucMark),
    (First: $0C85; Last: $0C8C; Category: ucLetter),
    (First: $0C8E; Last: $0C90; Category: ucLetter),
    (First: $0C92; Last: $0CA8; Category: ucLetter),
    (First: $0CAA; Last: $0CB3; Category: ucLetter),
    (First: $0CB5; Last: $0CB9; Category: ucLetter),
    (First: $0CBC; Last: $0CBC; Category: ucMark),
    (First: $0CBD; Last: $0CBD; Category: ucLetter),
    (First: $0CBE; Last: $0CC4; Category: ucMark),
    (First: $0CC6; Last: $0CC8; Category: ucMark),
    (First: $0CCA; Last: $0CCD; Category: ucMark),
    (First: $0CD5; Last: $0CD6; Category: ucMark),
    (First: $0CDE; Last: $0CDE; Category: ucLetter),
    (First: $0CE0; Last: $0CE1; Category: ucLetter),
    (First: $0CE2; Last: $0CE3; Category: ucMark),
    (First: $0CE6; Last: $0CEF; Category: ucNumber),
    (First: $0CF1; Last: $0CF2; Category: ucSymbol),
    (First: $0D02; Last: $0D03; Category: ucMark),
    (First: $0D05; Last: $0D0C; Category: ucLetter),
    (First: $0D0E; Last: $0D10; Category: ucLetter),
    (First: $0D12; Last: $0D28; Category: ucLetter),
    (First: $0D2A; Last: $0D39; Category: ucLetter),
    (First: $0D3E; Last: $0D43; Category: ucMark),
    (First: $0D46; Last: $0D48; Category: ucMark),
    (First: $0D4A; Last: $0D4D; Category: ucMark),
    (First: $0D57; Last: $0D57; Category: ucMark),
    (First: $0D60; Last: $0D61; Category: ucLetter),
    (First: $0D66; Last: $0D6F; Category: ucNumber),
    (First: $0D82; Last: $0D83; Category: ucMark),
    (First: $0D85; Last: $0D96; Category: ucLetter),
    (First: $0D9A; Last: $0DB1; Category: ucLetter),
    (First: $0DB3; Last: $0DBB; Category: ucLetter),
    (First: $0DBD; Last: $0DBD; Category: ucLetter),
    (First: $0DC0; Last: $0DC6; Category: ucLetter),
    (First: $0DCA; Last: $0DCA; Category: ucMark),
    (First: $0DCF; Last: $0DD4; Category: ucMark),
    (First: $0DD6; Last: $0DD6; Category: ucMark),
    (First: $0DD8; Last: $0DDF; Category: ucMark),
    (First: $0DF2; Last: $0DF3; Category: ucMark),
    (First: $0DF4; Last: $0DF4; Category: ucPunctuation),
    (First: $0E01; Last: $0E30; Category: ucLetter),
    (First: $0E31; Last: $0E31; Category: ucMark),
    (First: $0E32; Last: $0E33; Category: ucLetter),
    (First: $0E34; Last: $0E3A; Category: ucMark),
    (First: $0E3F; Last: $0E3F; Category: ucSymbol),
    (First: $0E40; Last: $0E46; Category: ucLetter),
    (First: $0E47; Last: $0E4E; Category: ucMark),
    (First: $0E4F; Last: $0E4F; Category: ucPunctuation),
    (First: $0E50; Last: $0E59; Category: ucNumber),
    (First: $0E5A; Last: $0E5B; Category: ucPunctuation),
    (First: $0E81; Last: $0E82; Category: ucLetter),
    (First: $0E84; Last: $0E84; Category: ucLetter),
    (First: $0E87; Last: $0E88; Category: ucLetter),
    (First: $0E8A; Last: $0E8A; Category: ucLetter),
    (First: $0E8D; Last: $0E8D; Category: ucLetter),
    (First: $0E94; Last: $0E97; Category: ucLetter),
    (First: $0E99; Last: $0E9F; Category: ucLetter),
    (First: $0EA1; Last: $0EA3; Category: ucLetter),
    (First: $0EA5; Last: $0EA5; Category: ucLetter),
    (First: $0EA7; Last: $0EA7; Category: ucLetter),
    (First: $0EAA; Last: $0EAB; Category: ucLetter),
    (First: $0EAD; Last: $0EB0; Category: ucLetter),
    (First: $0EB1; Last: $0EB1; Category: ucMark),
    (First: $0EB2; Last: $0EB3; Category: ucLetter),
    (First: $0EB4; Last: $0EB9; Category: ucMark),
    (First: $0EBB; Last: $0EBC; Category: ucMark),
    (First: $0EBD; Last: $0EBD; Category: ucLetter),
    (First: $0EC0; Last: $0EC4; Category: ucLetter),
    (First: $0EC6; Last: $0EC6; Category: ucLetter),
    (First: $0EC8; Last: $0ECD; Category: ucMark),
    (First: $0ED0; Last: $0ED9; Category: ucNumber),
    (First: $0EDC; Last: $0EDD; Category: ucLetter),
    (First: $0F00; Last: $0F00; Category: ucLetter),
    (First: $0F01; Last: $0F03; Category: ucSymbol),
    (First: $0F04; Last: $0F12; Category: ucPunctuation),
    (First: $0F13; Last: $0F17; Category: ucSymbol),
    (First: $0F18; Last: $0F19; Category: ucMark),
    (First: $0F1A; Last: $0F1F; Category: ucSymbol),
    (First: $0F20; Last: $0F33; Category: ucNumber),
    (First: $0F34; Last: $0F34; Category: ucSymbol),
    (First: $0F35; Last: $0F35; Category: ucMark),
    (First: $0F36; Last: $0F36; Category: ucSymbol),
    (First: $0F37; Last: $0F37; Category: ucMark),
    (First: $0F38; Last: $0F38; Category: ucSymbol),
    (First: $0F39; Last: $0F39; Category: ucMark),
    (First: $0F3A; Last: $0F3D; Category: ucPunctuation),
    (First: $0F3E; Last: $0F3F; Category: ucMark),
    (First: $0F40; Last: $0F47; Category: ucLetter),
    (First: $0F49; Last: $0F6A; Category: ucLetter),
    (First: $0F71; Last: $0F84; Category: ucMark),
    (First: $0F85; Last: $0F85; Category: ucPunctuation),
    (First: $0F86; Last: $0F87; Category: ucMark),
    (First: $0F88; Last: $0F8B; Category: ucLetter),
    (First: $0F90; Last: $0F97; Category: ucMark),
    (First: $0F99; Last: $0FBC; Category: ucMark),
    (First: $0FBE; Last: $0FC5; Category: ucSymbol),
    (First: $0FC6; Last: $0FC6; Category: ucMark),
    (First: $0FC7; Last: $0FCC; Category: ucSymbol),
    (First: $0FCF; Last: $0FCF; Category: ucSymbol),
    (First: $0FD0; Last: $0FD1; Category: ucPunctuation),
    (First: $1000; Last: $1021; Category: ucLetter),
    (First: $1023; Last: $1027; Category: ucLetter),
    (First: $1029; Last: $102A; Category: ucLetter),
    (First: $102C; Last: $1032; Category: ucMark),
    (First: $1036; Last: $1039; Category: ucMark),
    (First: $1040; Last: $1049; Category: ucNumber),
    (First: $104A; Last: $104F; Category: ucPunctuation),
    (First: $1050; Last: $1055; Category: ucLetter),
    (First: $1056; Last: $1059; Category: ucMark),
    (First: $10A0; Last: $10C5; Category: ucLetter),
    (First: $10D0; Last: $10FA; Category: ucLetter),
    (First: $10FB; Last: $10FB; Category: ucPunctuation),
    (First: $10FC; Last: $10FC; Category: ucLetter),
    (First: $1100; Last: $1159; Category: ucLetter),
    (First: $115F; Last: $11A2; Category: ucLetter),
    (First: $11A8; Last: $11F9; Category: ucLetter),
    (First: $1200; Last: $1248; Category: ucLetter),
    (First: $124A; Last: $124D; Category: ucLetter),
    (First: $1250; Last: $1256; Category: ucLetter),
    (First: $1258; Last: $1258; Category: ucLetter),
    (First: $125A; Last: $125D; Category: ucLetter),
    (First: $1260; Last: $1288; Category: ucLetter),
    (First: $128A; Last: $128D; Category: ucLetter),
    (First: $1290; Last: $12B0; Category: ucLetter),
    (First: $12B2; Last: $12B5; Category: ucLetter),
    (First: $12B8; Last: $12BE; Category: ucLetter),
    (First: $12C0; Last: $12C0; Category: ucLetter),
    (First: $12C2; Last: $12C5; Category: ucLetter),
    (First: $12C8; Last: $12D6; Category: ucLetter),
    (First: $12D8; Last: $1310; Category: ucLetter),
    (First: $1312; Last: $1315; Category: ucLetter),
    (First: $1318; Last: $135A; Category: ucLetter),
    (First: $135F; Last: $135F; Category: ucMark),
    (First: $1360; Last: $1360; Category: ucSymbol),
    (First: $1361; Last: $1368; Category: ucPunctuation),
    (First: $1369; Last: $137C; Category: ucNumber),
    (First: $1380; Last: $138F; Category: ucLetter),
    (First: $1390; Last: $1399; Category: ucSymbol),
    (First: $13A0; Last: $13F4; Category: ucLetter),
    (First: $1401; Last: $166C; Category: ucLetter),
    (First: $166D; Last: $166E; Category: ucPunctuation),
    (First: $166F; Last: $1676; Category: ucLetter),
    (First: $1680; Last: $1680; Category: ucSeparator),
    (First: $1681; Last: $169A; Category: ucLetter),
    (First: $169B; Last: $169C; Category: ucPunctuation),
    (First: $16A0; Last: $16EA; Category: ucLetter),
    (First: $16EB; Last: $16ED; Category: ucPunctuation),
    (First: $16EE; Last: $16F0; Category: ucNumber),
    (First: $1700; Last: $170C; Category: ucLetter),
    (First: $170E; Last: $1711; Category: ucLetter),
    (First: $1712; Last: $1714; Category: ucMark),
    (First: $1720; Last: $1731; Category: ucLetter),
    (First: $1732; Last: $1734; Category: ucMark),
    (First: $1735; Last: $1736; Category: ucPunctuation),
    (First: $1740; Last: $1751; Category: ucLetter),
    (First: $1752; Last: $1753; Category: ucMark),
    (First: $1760; Last: $176C; Category: ucLetter),
    (First: $176E; Last: $1770; Category: ucLetter),
    (First: $1772; Last: $1773; Category: ucMark),
    (First: $1780; Last: $17B3; Category: ucLetter),
    (First: $17B4; Last: $17B5; Category: ucOther),
    (First: $17B6; Last: $17D3; Category: ucMark),
    (First: $17D4; Last: $17D6; Category: ucPunctuation),
    (First: $17D7; Last: $17D7; Category: ucLetter),
    (First: $17D8; Last: $17DA; Category: ucPunctuation),
    (First: $17DB; Last: $17DB; Category: ucSymbol),
    (First: $17DC; Last: $17DC; Category: ucLetter),
    (First: $17DD; Last: $17DD; Category: ucMark),
    (First: $17E0; Last: $17E9; Category: ucNumber),
    (First: $17F0; Last: $17F9; Category: ucNumber),
    (First: $1800; Last: $180A; Category: ucPunctuation),
    (First: $180B; Last: $180D; Category: ucMark),
    (First: $180E; Last: $180E; Category: ucSeparator),
    (First: $1810; Last: $1819; Category: ucNumber),
    (First: $1820; Last: $1877; Category: ucLetter),
    (First: $1880; Last: $18A8; Category: ucLetter),
    (First: $18A9; Last: $18A9; Category: ucMark),
    (First: $1900; Last: $191C; Category: ucLetter),
    (First: $1920; Last: $192B; Category: ucMark),
    (First: $1930; Last: $193B; Category: ucMark),
    (First: $1940; Last: $1940; Category: ucSymbol),
    (First: $1944; Last: $1945; Category: ucPunctuation),
    (First: $1946; Last: $194F; Category: ucNumber),
    (First: $1950; Last: $196D; Category: ucLetter),
    (First: $1970; Last: $1974; Category: ucLetter),
    (First: $1980; Last: $19A9; Category: ucLetter),
    (First: $19B0; Last: $19C0; Category: ucMark),
    (First: $19C1; Last: $19C7; Category: ucLetter),
    (First: $19C8; Last: $19C9; Category: ucMark),
    (First: $19D0; Last: $19D9; Category: ucNumber),
    (First: $19DE; Last: $19DF; Category: ucPunctuation),
    (First: $19E0; Last: $19FF; Category: ucSymbol),
    (First: $1A00; Last: $1A16; Category: ucLetter),
    (First: $1A17; Last: $1A1B; Category: ucMark),
    (First: $1A1E; Last: $1A1F; Category: ucPunctuation),
    (First: $1B00; Last: $1B04; Category: ucMark),
    (First: $1B05; Last: $1B33; Category: ucLetter),
    (First: $1B34; Last: $1B44; Category: ucMark),
    (First: $1B45; Last: $1B4B; Category: ucLetter),
    (First: $1B50; Last: $1B59; Category: ucNumber),
    (First: $1B5A; Last: $1B60; Category: ucPunctuation),
    (First: $1B61; Last: $1B6A; Category: ucSymbol),
    (First: $1B6B; Last: $1B73; Category: ucMark),
    (First: $1B74; Last: $1B7C; Category: ucSymbol),
    (First: $1D00; Last: $1DBF; Category: ucLetter),
    (First: $1DC0; Last: $1DCA; Category: ucMark),
    (First: $1DFE; Last: $1DFF; Category: ucMark),
    (First: $1E00; Last: $1E9B; Category: ucLetter),
    (First: $1EA0; Last: $1EF9; Category: ucLetter),
    (First: $1F00; Last: $1F15; Category: ucLetter),
    (First: $1F18; Last: $1F1D; Category: ucLetter),
    (First: $1F20; Last: $1F45; Category: ucLetter),
    (First: $1F48; Last: $1F4D; Category: ucLetter),
    (First: $1F50; Last: $1F57; Category: ucLetter),
    (First: $1F59; Last: $1F59; Category: ucLetter),
    (First: $1F5B; Last: $1F5B; Category: ucLetter),
    (First: $1F5D; Last: $1F5D; Category: ucLetter),
    (First: $1F5F; Last: $1F7D; Category: ucLetter),
    (First: $1F80; Last: $1FB4; Category: ucLetter),
    (First: $1FB6; Last: $1FBC; Category: ucLetter),
    (First: $1FBD; Last: $1FBD; Category: ucSymbol),
    (First: $1FBE; Last: $1FBE; Category: ucLetter),
    (First: $1FBF; Last: $1FC1; Category: ucSymbol),
    (First: $1FC2; Last: $1FC4; Category: ucLetter),
    (First: $1FC6; Last: $1FCC; Category: ucLetter),
    (First: $1FCD; Last: $1FCF; Category: ucSymbol),
    (First: $1FD0; Last: $1FD3; Category: ucLetter),
    (First: $1FD6; Last: $1FDB; Category: ucLetter),
    (First: $1FDD; Last: $1FDF; Category: ucSymbol),
    (First: $1FE0; Last: $1FEC; Category: ucLetter),
    (First: $1FED; Last: $1FEF; Category: ucSymbol),
    (First: $1FF2; Last: $1FF4; Category: ucLetter),
    (First: $1FF6; Last: $1FFC; Category: ucLetter),
    (First: $1FFD; Last: $1FFE; Category: ucSymbol),
    (First: $2000; Last: $200A; Category: ucSeparator),
    (First: $200B; Last: $200F; Category: ucOther),
    (First: $2010; Last: $2027; Category: ucPunctuation),
    (First: $2028; Last: $2029; Category: ucSeparator),
    (First: $202A; Last: $202E; Category: ucOther),
    (First: $202F; Last: $202F; Category: ucSeparator),
    (First: $2030; Last: $2043; Category: ucPunctuation),
    (First: $2044; Last: $2044; Category: ucSymbol),
    (First: $2045; Last: $2051; Category: ucPunctuation),
    (First: $2052; Last: $2052; Category: ucSymbol),
    (First: $2053; Last: $205E; Category: ucPunctuation),
    (First: $205F; Last: $205F; Category: ucSeparator),
    (First: $2060; Last: $2063; Category: ucOther),
    (First: $206A; Last: $206F; Category: ucOther),
    (First: $2070; Last: $2070; Category: ucNumber),
    (First: $2071; Last: $2071; Category: ucLetter),
    (First: $2074; Last: $2079; Category: ucNumber),
    (First: $207A; Last: $207C; Category: ucSymbol),
    (First: $207D; Last: $207E; Category: ucPunctuation),
    (First: $207F; Last: $207F; Category: ucLetter),
    (First: $2080; Last: $2089; Category: ucNumber),
    (First: $208A; Last: $208C; Category: ucSymbol),
    (First: $208D; Last: $208E; Category: ucPunctuation),
    (First: $2090; Last: $2094; Category: ucLetter),
    (First: $20A0; Last: $20B5; Category: ucSymbol),
    (First: $20D0; Last: $20EF; Category: ucMark),
    (First: $2100; Last: $2101; Category: ucSymbol),
    (First: $2102; Last: $2102; Category: ucLetter),
    (First: $2103; Last: $2106; Category: ucSymbol),
    (First: $2107; Last: $2107; Category: ucLetter),
    (First: $2108; Last: $2109; Category: ucSymbol),
    (First: $210A; Last: $2113; Category: ucLetter),
    (First: $2114; Last: $2114; Category: ucSymbol),
    (First: $2115; Last: $2115; Category: ucLetter),
    (First: $2116; Last: $2118; Category: ucSymbol),
    (First: $2119; Last: $211D; Category: ucLetter),
    (First: $211E; Last: $2123; Category: ucSymbol),
    (First: $2124; Last: $2124; Category: ucLetter),
    (First: $2125; Last: $2125; Category: ucSymbol),
    (First: $2126; Last: $2126; Category: ucLetter),
    (First: $2127; Last: $2127; Category: ucSymbol),
    (First: $2128; Last: $2128; Category: ucLetter),
    (First: $2129; Last: $2129; Category: ucSymbol),
    (First: $212A; Last: $212D; Category: ucLetter),
    (First: $212E; Last: $212E; Category: ucSymbol),
    (First: $212F; Last: $2139; Category: ucLetter),
    (First: $213A; Last: $213B; Category: ucSymbol),
    (First: $213C; Last: $213F; Category: ucLetter),
    (First: $2140; Last: $2144; Category: ucSymbol),
    (First: $2145; Last: $2149; Category: ucLetter),
    (First: $214A; Last: $214D; Category: ucSymbol),
    (First: $214E; Last: $214E; Category: ucLetter),
    (First: $2153; Last: $2182; Category: ucNumber),
    (First: $2183; Last: $2184; Category: ucLetter),
    (First: $2190; Last: $2328; Category: ucSymbol),
    (First: $2329; Last: $232A; Category: ucPunctuation),
    (First: $232B; Last: $23E7; Category: ucSymbol),
    (First: $2400; Last: $2426; Category: ucSymbol),
    (First: $2440; Last: $244A; Category: ucSymbol),
    (First: $2460; Last: $249B; Category: ucNumber),
    (First: $249C; Last: $24E9; Category: ucSymbol),
    (First: $24EA; Last: $24FF; Category: ucNumber),
    (First: $2500; Last: $269C; Category: ucSymbol),
    (First: $26A0; Last: $26B2; Category: ucSymbol),
    (First: $2701; Last: $2704; Category: ucSymbol),
    (First: $2706; Last: $2709; Category: ucSymbol),
    (First: $270C; Last: $2727; Category: ucSymbol),
    (First: $2729; Last: $274B; Category: ucSymbol),
    (First: $274D; Last: $274D; Category: ucSymbol),
    (First: $274F; Last: $2752; Category: ucSymbol),
    (First: $2756; Last: $2756; Category: ucSymbol),
    (First: $2758; Last: $275E; Category: ucSymbol),
    (First: $2761; Last: $2767; Category: ucSymbol),
    (First: $2768; Last: $2775; Category: ucPunctuation),
    (First: $2776; Last: $2793; Category: ucNumber),
    (First: $2794; Last: $2794; Category: ucSymbol),
    (First: $2798; Last: $27AF; Category: ucSymbol),
    (First: $27B1; Last: $27BE; Category: ucSymbol),
    (First: $27C0; Last: $27C4; Category: ucSymbol),
    (First: $27C5; Last: $27C6; Category: ucPunctuation),
    (First: $27C7; Last: $27CA; Category: ucSymbol),
    (First: $27D0; Last: $27E5; Category: ucSymbol),
    (First: $27E6; Last: $27EB; Category: ucPunctuation),
    (First: $27F0; Last: $2982; Category: ucSymbol),
    (First: $2983; Last: $2998; Category: ucPunctuation),
    (First: $2999; Last: $29D7; Category: ucSymbol),
    (First: $29D8; Last: $29DB; Category: ucPunctuation),
    (First: $29DC; Last: $29FB; Category: ucSymbol),
    (First: $29FC; Last: $29FD; Category: ucPunctuation),
    (First: $29FE; Last: $2B1A; Category: ucSymbol),
    (First: $2B20; Last: $2B23; Category: ucSymbol),
    (First: $2C00; Last: $2C2E; Category: ucLetter),
    (First: $2C30; Last: $2C5E; Category: ucLetter),
    (First: $2C60; Last: $2C6C; Category: ucLetter),
    (First: $2C74; Last: $2C77; Category: ucLetter),
    (First: $2C80; Last: $2CE4; Category: ucLetter),
    (First: $2CE5; Last: $2CEA; Category: ucSymbol),
    (First: $2CF9; Last: $2CFC; Category: ucPunctuation),
    (First: $2CFD; Last: $2CFD; Category: ucNumber),
    (First: $2CFE; Last: $2CFF; Category: ucPunctuation),
    (First: $2D00; Last: $2D25; Category: ucLetter),
    (First: $2D30; Last: $2D65; Category: ucLetter),
    (First: $2D6F; Last: $2D6F; Category: ucLetter),
    (First: $2D80; Last: $2D96; Category: ucLetter),
    (First: $2DA0; Last: $2DA6; Category: ucLetter),
    (First: $2DA8; Last: $2DAE; Category: ucLetter),
    (First: $2DB0; Last: $2DB6; Category: ucLetter),
    (First: $2DB8; Last: $2DBE; Category: ucLetter),
    (First: $2DC0; Last: $2DC6; Category: ucLetter),
    (First: $2DC8; Last: $2DCE; Category: ucLetter),
    (First: $2DD0; Last: $2DD6; Category: ucLetter),
    (First: $2DD8; Last: $2DDE; Category: ucLetter),
    (First: $2E00; Last: $2E17; Category: ucPunctuation),
    (First: $2E1C; Last: $2E1D; Category: ucPunctuation),
    (First: $2E80; Last: $2E99; Category: ucSymbol),
    (First: $2E9B; Last: $2EF3; Category: ucSymbol),
    (First: $2F00; Last: $2FD5; Category: ucSymbol),
    (First: $2FF0; Last: $2FFB; Category: ucSymbol),
    (First: $3000; Last: $3000; Category: ucSeparator),
    (First: $3001; Last: $3003; Category: ucPunctuation),
    (First: $3004; Last: $3004; Category: ucSymbol),
    (First: $3005; Last: $3006; Category: ucLetter),
    (First: $3007; Last: $3007; Category: ucNumber),
    (First: $3008; Last: $3011; Category: ucPunctuation),
    (First: $3012; Last: $3013; Category: ucSymbol),
    (First: $3014; Last: $301F; Category: ucPunctuation),
    (First: $3020; Last: $3020; Category: ucSymbol),
    (First: $3021; Last: $3029; Category: ucNumber),
    (First: $302A; Last: $302F; Category: ucMark),
    (First: $3030; Last: $3030; Category: ucPunctuation),
    (First: $3031; Last: $3035; Category: ucLetter),
    (First: $3036; Last: $3037; Category: ucSymbol),
    (First: $3038; Last: $303A; Category: ucNumber),
    (First: $303B; Last: $303C; Category: ucLetter),
    (First: $303D; Last: $303D; Category: ucPunctuation),
    (First: $303E; Last: $303F; Category: ucSymbol),
    (First: $3041; Last: $3096; Category: ucLetter),
    (First: $3099; Last: $309A; Category: ucMark),
    (First: $309B; Last: $309C; Category: ucSymbol),
    (First: $309D; Last: $309F; Category: ucLetter),
    (First: $30A0; Last: $30A0; Category: ucPunctuation),
    (First: $30A1; Last: $30FA; Category: ucLetter),
    (First: $30FB; Last: $30FB; Category: ucPunctuation),
    (First: $30FC; Last: $30FF; Category: ucLetter),
    (First: $3105; Last: $312C; Category: ucLetter),
    (First: $3131; Last: $318E; Category: ucLetter),
    (First: $3190; Last: $3191; Category: ucSymbol),
    (First: $3192; Last: $3195; Category: ucNumber),
    (First: $3196; Last: $319F; Category: ucSymbol),
    (First: $31A0; Last: $31B7; Category: ucLetter),
    (First: $31C0; Last: $31CF; Category: ucSymbol),
    (First: $31F0; Last: $31FF; Category: ucLetter),
    (First: $3200; Last: $321E; Category: ucSymbol),
    (First: $3220; Last: $3229; Category: ucNumber),
    (First: $322A; Last: $3243; Category: ucSymbol),
    (First: $3250; Last: $3250; Category: ucSymbol),
    (First: $3251; Last: $325F; Category: ucNumber),
    (First: $3260; Last: $327F; Category: ucSymbol),
    (First: $3280; Last: $3289; Category: ucNumber),
    (First: $328A; Last: $32B0; Category: ucSymbol),
    (First: $32B1; Last: $32BF; Category: ucNumber),
    (First: $32C0; Last: $32FE; Category: ucSymbol),
    (First: $3300; Last: $33FF; Category: ucSymbol),
    (First: $3400; Last: $4DB5; Category: ucLetter),
    (First: $4DC0; Last: $4DFF; Category: ucSymbol),
    (First: $4E00; Last: $9FBB; Category: ucLetter),
    (First: $A000; Last: $A48C; Category: ucLetter),
    (First: $A490; Last: $A4C6; Category: ucSymbol),
    (First: $A700; Last: $A716; Category: ucSymbol),
    (First: $A717; Last: $A71A; Category: ucLetter),
    (First: $A720; Last: $A721; Category: ucSymbol),
    (First: $A800; Last: $A801; Category: ucLetter),
    (First: $A802; Last: $A802; Category: ucMark),
    (First: $A803; Last: $A805; Category: ucLetter),
    (First: $A806; Last: $A806; Category: ucMark),
    (First: $A807; Last: $A80A; Category: ucLetter),
    (First: $A80B; Last: $A80B; Category: ucMark),
    (First: $A80C; Last: $A822; Category: ucLetter),
    (First: $A823; Last: $A827; Category: ucMark),
    (First: $A828; Last: $A82B; Category: ucSymbol),
    (First: $A840; Last: $A873; Category: ucLetter),
    (First: $A874; Last: $A877; Category: ucPunctuation),
    (First: $AC00; Last: $D7A3; Category: ucLetter),
    (First: $D800; Last: $F8FF; Category: ucOther),
    (First: $F900; Last: $FA2D; Category: ucLetter),
    (First: $FA30; Last: $FA6A; Category: ucLetter),
    (First: $FA70; Last: $FAD9; Category: ucLetter),
    (First: $FB00; Last: $FB06; Category: ucLetter),
    (First: $FB13; Last: $FB17; Category: ucLetter),
    (First: $FB1D; Last: $FB1D; Category: ucLetter),
    (First: $FB1E; Last: $FB1E; Category: ucMark),
    (First: $FB1F; Last: $FB28; Category: ucLetter),
    (First: $FB29; Last: $FB29; Category: ucSymbol),
    (First: $FB2A; Last: $FB36; Category: ucLetter),
    (First: $FB38; Last: $FB3C; Category: ucLetter),
    (First: $FB3E; Last: $FB3E; Category: ucLetter),
    (First: $FB40; Last: $FB41; Category: ucLetter),
    (First: $FB43; Last: $FB44; Category: ucLetter),
    (First: $FB46; Last: $FBB1; Category: ucLetter),
    (First: $FBD3; Last: $FD3D; Category: ucLetter),
    (First: $FD3E; Last: $FD3F; Category: ucPunctuation),
    (First: $FD50; Last: $FD8F; Category: ucLetter),
    (First: $FD92; Last: $FDC7; Category: ucLetter),
    (First: $FDF0; Last: $FDFB; Category: ucLetter),
    (First: $FDFC; Last: $FDFD; Category: ucSymbol),
    (First: $FE00; Last: $FE0F; Category: ucMark),
    (First: $FE10; Last: $FE19; Category: ucPunctuation),
    (First: $FE20; Last: $FE23; Category: ucMark),
    (First: $FE30; Last: $FE52; Category: ucPunctuation),
    (First: $FE54; Last: $FE61; Category: ucPunctuation),
    (First: $FE62; Last: $FE62; Category: ucSymbol),
    (First: $FE63; Last: $FE63; Category: ucPunctuation),
    (First: $FE64; Last: $FE66; Category: ucSymbol),
    (First: $FE68; Last: $FE68; Category: ucPunctuation),
    (First: $FE69; Last: $FE69; Category: ucSymbol),
    (First: $FE6A; Last: $FE6B; Category: ucPunctuation),
    (First: $FE70; Last: $FE74; Category: ucLetter),
    (First: $FE76; Last: $FEFC; Category: ucLetter),
    (First: $FEFF; Last: $FEFF; Category: ucOther),
    (First: $FF01; Last: $FF03; Category: ucPunctuation),
    (First: $FF04; Last: $FF04; Category: ucSymbol),
    (First: $FF05; Last: $FF0A; Category: ucPunctuation),
    (First: $FF0B; Last: $FF0B; Category: ucSymbol),
    (First: $FF0C; Last: $FF0F; Category: ucPunctuation),
    (First: $FF10; Last: $FF19; Category: ucNumber),
    (First: $FF1A; Last: $FF1B; Category: ucPunctuation),
    (First: $FF1C; Last: $FF1E; Category: ucSymbol),
    (First: $FF1F; Last: $FF20; Category: ucPunctuation),
    (First: $FF21; Last: $FF3A; Category: ucLetter),
    (First: $FF3B; Last: $FF3D; Category: ucPunctuation),
    (First: $FF3E; Last: $FF3E; Category: ucSymbol),
    (First: $FF3F; Last: $FF3F; Category: ucPunctuation),
    (First: $FF40; Last: $FF40; Category: ucSymbol),
    (First: $FF41; Last: $FF5A; Category: ucLetter),
    (First: $FF5B; Last: $FF5B; Category: ucPunctuation),
    (First: $FF5C; Last: $FF5C; Category: ucSymbol),
    (First: $FF5D; Last: $FF5D; Category: ucPunctuation),
    (First: $FF5E; Last: $FF5E; Category: ucSymbol),
    (First: $FF5F; Last: $FF65; Category: ucPunctuation),
    (First: $FF66; Last: $FFBE; Category: ucLetter),
    (First: $FFC2; Last: $FFC7; Category: ucLetter),
    (First: $FFCA; Last: $FFCF; Category: ucLetter),
    (First: $FFD2; Last: $FFD7; Category: ucLetter),
    (First: $FFDA; Last: $FFDC; Category: ucLetter),
    (First: $FFE0; Last: $FFE6; Category: ucSymbol),
    (First: $FFE8; Last: $FFEE; Category: ucSymbol),
    (First: $FFF9; Last: $FFFB; Category: ucOther),
    (First: $FFFC; Last: $FFFD; Category: ucSymbol),
    (First: $10000; Last: $1000B; Category: ucLetter),
    (First: $1000D; Last: $10026; Category: ucLetter),
    (First: $10028; Last: $1003A; Category: ucLetter),
    (First: $1003C; Last: $1003D; Category: ucLetter),
    (First: $1003F; Last: $1004D; Category: ucLetter),
    (First: $10050; Last: $1005D; Category: ucLetter),
    (First: $10080; Last: $100FA; Category: ucLetter),
    (First: $10100; Last: $10101; Category: ucPunctuation),
    (First: $10102; Last: $10102; Category: ucSymbol),
    (First: $10107; Last: $10133; Category: ucNumber),
    (First: $10137; Last: $1013F; Category: ucSymbol),
    (First: $10140; Last: $10178; Category: ucNumber),
    (First: $10179; Last: $10189; Category: ucSymbol),
    (First: $1018A; Last: $1018A; Category: ucNumber),
    (First: $10300; Last: $1031E; Category: ucLetter),
    (First: $10320; Last: $10323; Category: ucNumber),
    (First: $10330; Last: $10340; Category: ucLetter),
    (First: $10341; Last: $10341; Category: ucNumber),
    (First: $10342; Last: $10349; Category: ucLetter),
    (First: $1034A; Last: $1034A; Category: ucNumber),
    (First: $10380; Last: $1039D; Category: ucLetter),
    (First: $1039F; Last: $1039F; Category: ucPunctuation),
    (First: $103A0; Last: $103C3; Category: ucLetter),
    (First: $103C8; Last: $103CF; Category: ucLetter),
    (First: $103D0; Last: $103D0; Category: ucPunctuation),
    (First: $103D1; Last: $103D5; Category: ucNumber),
    (First: $10400; Last: $1049D; Category: ucLetter),
    (First: $104A0; Last: $104A9; Category: ucNumber),
    (First: $10800; Last: $10805; Category: ucLetter),
    (First: $10808; Last: $10808; Category: ucLetter),
    (First: $1080A; Last: $10835; Category: ucLetter),
    (First: $10837; Last: $10838; Category: ucLetter),
    (First: $1083C; Last: $1083C; Category: ucLetter),
    (First: $1083F; Last: $1083F; Category: ucLetter),
    (First: $10900; Last: $10915; Category: ucLetter),
    (First: $10916; Last: $10919; Category: ucNumber),
    (First: $1091F; Last: $1091F; Category: ucPunctuation),
    (First: $10A00; Last: $10A00; Category: ucLetter),
    (First: $10A01; Last: $10A03; Category: ucMark),
    (First: $10A05; Last: $10A06; Category: ucMark),
    (First: $10A0C; Last: $10A0F; Category: ucMark),
    (First: $10A10; Last: $10A13; Category: ucLetter),
    (First: $10A15; Last: $10A17; Category: ucLetter),
    (First: $10A19; Last: $10A33; Category: ucLetter),
    (First: $10A38; Last: $10A3A; Category: ucMark),
    (First: $10A3F; Last: $10A3F; Category: ucMark),
    (First: $10A40; Last: $10A47; Category: ucNumber),
    (First: $10A50; Last: $10A58; Category: ucPunctuation),
    (First: $12000; Last: $1236E; Category: ucLetter),
    (First: $12400; Last: $12462; Category: ucNumber),
    (First: $12470; Last: $12473; Category: ucPunctuation),
    (First: $1D000; Last: $1D0F5; Category: ucSymbol),
    (First: $1D100; Last: $1D126; Category: ucSymbol),
    (First: $1D12A; Last: $1D164; Category: ucSymbol),
    (First: $1D165; Last: $1D169; Category: ucMark),
    (First: $1D16A; Last: $1D16C; Category: ucSymbol),
    (First: $1D16D; Last: $1D172; Category: ucMark),
    (First: $1D173; Last: $1D17A; Category: ucOther),
    (First: $1D17B; Last: $1D182; Category: ucMark),
    (First: $1D183; Last: $1D184; Category: ucSymbol),
    (First: $1D185; Last: $1D18B; Category: ucMark),
    (First: $1D18C; Last: $1D1A9; Category: ucSymbol),
    (First: $1D1AA; Last: $1D1AD; Category: ucMark),
    (First: $1D1AE; Last: $1D1DD; Category: ucSymbol),
    (First: $1D200; Last: $1D241; Category: ucSymbol),
    (First: $1D242; Last: $1D244; Category: ucMark),
    (First: $1D245; Last: $1D245; Category: ucSymbol),
    (First: $1D300; Last: $1D356; Category: ucSymbol),
    (First: $1D360; Last: $1D371; Category: ucNumber),
    (First: $1D400; Last: $1D454; Category: ucLetter),
    (First: $1D456; Last: $1D49C; Category: ucLetter),
    (First: $1D49E; Last: $1D49F; Category: ucLetter),
    (First: $1D4A2; Last: $1D4A2; Category: ucLetter),
    (First: $1D4A5; Last: $1D4A6; Category: ucLetter),
    (First: $1D4A9; Last: $1D4AC; Category: ucLetter),
    (First: $1D4AE; Last: $1D4B9; Category: ucLetter),
    (First: $1D4BB; Last: $1D4BB; Category: ucLetter),
    (First: $1D4BD; Last: $1D4C3; Category: ucLetter),
    (First: $1D4C5; Last: $1D505; Category: ucLetter),
    (First: $1D507; Last: $1D50A; Category: ucLetter),
    (First: $1D50D; Last: $1D514; Category: ucLetter),
    (First: $1D516; Last: $1D51C; Category: ucLetter),
    (First: $1D51E; Last: $1D539; Category: ucLetter),
    (First: $1D53B; Last: $1D53E; Category: ucLetter),
    (First: $1D540; Last: $1D544; Category: ucLetter),
    (First: $1D546; Last: $1D546; Category: ucLetter),
    (First: $1D54A; Last: $1D550; Category: ucLetter),
    (First: $1D552; Last: $1D6A5; Category: ucLetter),
    (First: $1D6A8; Last: $1D6C0; Category: ucLetter),
    (First: $1D6C1; Last: $1D6C1; Category: ucSymbol),
    (First: $1D6C2; Last: $1D6DA; Category: ucLetter),
    (First: $1D6DB; Last: $1D6DB; Category: ucSymbol),
    (First: $1D6DC; Last: $1D6FA; Category: ucLetter),
    (First: $1D6FB; Last: $1D6FB; Category: ucSymbol),
    (First: $1D6FC; Last: $1D714; Category: ucLetter),
    (First: $1D715; Last: $1D715; Category: ucSymbol),
    (First: $1D716; Last: $1D734; Category: ucLetter),
    (First: $1D735; Last: $1D735; Category: ucSymbol),
    (First: $1D736; Last: $1D74E; Category: ucLetter),
    (First: $1D74F; Last: $1D74F; Category: ucSymbol),
    (First: $1D750; Last: $1D76E; Category: ucLetter),
    (First: $1D76F; Last: $1D76F; Category: ucSymbol),
    (First: $1D770; Last: $1D788; Category: ucLetter),
    (First: $1D789; Last: $1D789; Category: ucSymbol),
    (First: $1D78A; Last: $1D7A8; Category: ucLetter),
    (First: $1D7A9; Last: $1D7A9; Category: ucSymbol),
    (First: $1D7AA; Last: $1D7C2; Category: ucLetter),
    (First: $1D7C3; Last: $1D7C3; Category: ucSymbol),
    (First: $1D7C4; Last: $1D7CB; Category: ucLetter),
    (First: $1D7CE; Last: $1D7FF; Category: ucNumber),
    (First: $20000; Last: $2A6D6; Category: ucLetter),
    (First: $2F800; Last: $2FA1D; Category: ucLetter),
    (First: $E0001; Last: $E0001; Category: ucOther),
    (First: $E0020; Last: $E007F; Category: ucOther),
    (First: $E0100; Last: $E01EF; Category: ucMark),
    (First: $F0000; Last: $FFFFD; Category: ucOther),
    (First: $100000; Last: $10FFFD; Category: ucOther)
);

function UnicodeCategory(Code: Cardinal): TUnicodeCategory;
var
  Lo, Hi, Mid: Integer;
begin
  Lo := 0;
  Hi := Length(UnicodeRanges) - 1;
  Result := ucOther;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    if Code < UnicodeRanges[Mid].First then
      Hi := Mid - 1
    else if Code > UnicodeRanges[Mid].Last then
      Lo := Mid + 1
    else
    begin
      Result := UnicodeRanges[Mid].Category;
      Break;
    end;
  end;
end;

function IsLetter(Code: Cardinal): Boolean;
begin
  Result := UnicodeCategory(Code) = ucLetter;
end;

function IsMark(Code: Cardinal): Boolean;
begin
  Result := UnicodeCategory(Code) = ucMark;
end;

function IsNumber(Code: Cardinal): Boolean;
begin
  Result := UnicodeCategory(Code) = ucNumber;
end;

function IsSeparator(Code: Cardinal): Boolean;
begin
  Result := UnicodeCategory(Code) = ucSeparator;
end;

function IsSymbol(Code: Cardinal): Boolean;
begin
  Result := UnicodeCategory(Code) = ucSymbol;
end;

function IsPunctuation(Code: Cardinal): Boolean;
begin
  Result := UnicodeCategory(Code) = ucPunctuation;
end;

function CodePointAt(const Text: UnicodeString; Pos: Integer): Cardinal;
var
  c1, c2: WideChar;
begin
  Result := 0;
  if (Pos < 1) or (Pos > Length(Text)) then
    Exit;
  c1 := WideChar(Text[Pos]);
  if (Ord(c1) >= $D800) and (Ord(c1) <= $DBFF) then
  begin
    if Pos + 1 <= Length(Text) then
    begin
      c2 := WideChar(Text[Pos + 1]);
      if (Ord(c2) >= $DC00) and (Ord(c2) <= $DFFF) then
      begin
        Result := ((Ord(c1) - $D800) shl 10) + (Ord(c2) - $DC00) + $10000;
        Exit;
      end;
    end;
    Result := Ord(c1);
  end
  else
    Result := Ord(c1);
end;

function CodePointSize(Code: Cardinal): Integer;
begin
  if Code > $FFFF then
    Result := 2
  else
    Result := 1;
end;

function CharAt(const Text: UnicodeString; Pos: Integer): UnicodeString;
var
  cp: Cardinal;
  n: Integer;
begin
  cp := CodePointAt(Text, Pos);
  n := CodePointSize(cp);
  if n = 0 then
    Result := ''
  else
    Result := Copy(Text, Pos, n);
end;

function GraphemeToUserPart(const Text: UnicodeString; Pos: Integer): UnicodeString;
var
  cp, cp2: Cardinal;
  n, p: Integer;
  cat: TUnicodeCategory;
begin
  Result := '';
  if (Pos < 1) or (Pos > Length(Text)) then
    Exit;

  cp := CodePointAt(Text, Pos);
  n := CodePointSize(cp);

  // CRLF pair: treat CR+LF as one unit
  if (cp = $000D) and (Pos + n <= Length(Text)) then
  begin
    cp2 := CodePointAt(Text, Pos + n);
    if cp2 = $000A then
      n := n + CodePointSize(cp2);
  end;

  Result := Copy(Text, Pos, n);
  p := Pos + n;

  // Consume combining marks, ZWJ sequences, and regional indicator pairs
  while p <= Length(Text) do
  begin
    cp := CodePointAt(Text, p);
    cat := UnicodeCategory(cp);

    // Combining marks (Mn, Mc, Me) always attach
    if cat = ucMark then
    begin
      n := CodePointSize(cp);
      Result := Result + Copy(Text, p, n);
      p := p + n;
      Continue;
    end;

    // ZWJ (U+200D): join next base + its marks
    if cp = $200D then
    begin
      n := CodePointSize(cp);
      p := p + n;
      if p <= Length(Text) then
      begin
        cp := CodePointAt(Text, p);
        n := CodePointSize(cp);
        Result := Result + WideChar($200D) + Copy(Text, p, n);
        p := p + n;
        Continue;
      end
      else
        Break;
    end;

    Break;
  end;
end;

function GraphemeToNextPart(const Text: UnicodeString; Pos: Integer): UnicodeString;
var
  startCat, cat: TUnicodeCategory;
  cp: Cardinal;
  n, p: Integer;
  gotBase: Boolean;
begin
  Result := '';
  if (Pos < 1) or (Pos > Length(Text)) then
    Exit;

  cp := CodePointAt(Text, Pos);
  n := CodePointSize(cp);
  startCat := UnicodeCategory(cp);

  // Leading marks: standalone mark run at start of part
  if startCat = ucMark then
  begin
    p := Pos + n;
    while p <= Length(Text) do
    begin
      cp := CodePointAt(Text, p);
      if UnicodeCategory(cp) <> ucMark then
        Break;
      p := p + CodePointSize(cp);
    end;
    Result := Copy(Text, Pos, p - Pos);
    Exit;
  end;

  // Consume base characters of the same category plus attached marks
  p := Pos;
  gotBase := False;
  while p <= Length(Text) do
  begin
    cp := CodePointAt(Text, p);
    n := CodePointSize(cp);
    cat := UnicodeCategory(cp);

    // Marks always attach to preceding base
    if cat = ucMark then
    begin
      p := p + n;
      Continue;
    end;

    if not gotBase then
    begin
      // First non-mark character determines the part's category
      gotBase := True;
      startCat := cat;
      p := p + n;
      Continue;
    end;

    // Same category as the base: continue
    if cat = startCat then
    begin
      p := p + n;
      Continue;
    end;

    // Category changed: stop here
    Break;
  end;

  Result := Copy(Text, Pos, p - Pos);
end;

type
  TucpConverter = record
    MBToWCProc: procedure(S: AnsiChar; var R: WideChar);
    WCToMBProc: procedure(S: WideChar; var R: AnsiChar);
  end;

var
  FConverter: TucpConverter;

{$IFDEF FPC}
procedure Ansi2WideMove(source: PAnsiChar; cp : TSystemCodePage; var dest: WideString; len:SizeInt);
begin
  dest := ucpAnsiToUnicode(source);
end;

procedure Wide2AnsiMove(source: PWideChar; var dest: RawByteString; cp : TSystemCodePage; len:SizeInt);
begin
  dest := ucpUnicodeToAnsi(source);
end;

procedure Ansi2UnicodeMove(source: PAnsiChar; cp : TSystemCodePage; var dest: UnicodeString; len: SizeInt);
begin
  dest := ucpAnsiToUnicode(source);
end;

procedure Unicode2AnsiMove(source: punicodechar; var dest:RawByteString; cp : TSystemCodePage; len:SizeInt);
begin
  dest := ucpUnicodeToAnsi(source);
end;
{$ENDIF}

procedure ucpInstall(MBToWCProc: Tmbtowc_proc; WCtoMBProc: Twctomb_proc{$IFDEF FPC}; Hook: Boolean{$ENDIF});
{$IFDEF FPC}
var
  Manager: TUnicodeStringManager;
{$ENDIF}
begin
  FConverter.MBToWCProc := MBToWCProc;
  FConverter.WCToMBProc := WCtoMBProc;
{$IFDEF FPC}
  if Hook then
  begin
    GetWideStringManager(Manager);
    Manager.Ansi2WideMoveProc := Ansi2WideMove;
    Manager.Wide2AnsiMoveProc := Wide2AnsiMove;
    Manager.Ansi2UnicodeMoveProc := Ansi2UnicodeMove;
    Manager.Unicode2AnsiMoveProc := Unicode2AnsiMove;
    SetWideStringManager(Manager);
  end;
{$ENDIF}
end;

function ucpAnsiToUnicode(const s: AnsiString; Proc: Tmbtowc_proc): WideString; overload;
var
  i: Integer;
  r: WideChar;
begin
  if not Assigned(Proc) then
    raise Exception.Create('AnsiToUnicode: Proc params = nil!');
  SetLength(Result, length(s));
  for i := 1 to Length(s) do
  begin
    Proc(s[i], r);
    Result[i] := r;
  end;
end;

function ucpAnsiToUnicode(const S: AnsiString): WideString; overload;
begin
  Result := ucpAnsiToUnicode(S, FConverter.MBToWCProc);
end;

function ucpUnicodeToAnsi(const S: WideString; Proc: Twctomb_proc): AnsiString; overload;
var
  i: Integer;
  r: AnsiChar;
begin
  if not Assigned(Proc) then
    raise Exception.Create('UnicodeToAnsi: Proc params = nil!');
  SetLength(Result, length(s));
  for i := 1 to Length(s) do
  begin
    Proc(s[i], r);
    Result[i] := r;
  end;
end;

function ucpUnicodeToAnsi(const S: WideString): AnsiString; overload;
begin
  Result := ucpUnicodeToAnsi(S, FConverter.WCToMBProc);
end;

initialization
  FConverter.MBToWCProc := cp1250_mbtowc;
  FConverter.WCToMBProc := cp1250_wctomb;
end.

