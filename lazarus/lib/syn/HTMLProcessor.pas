unit HTMLProcessor;
{$mode objfpc}{$H+}
{**
 *  Light PHP Edit project
 *
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  SysUtils, Graphics, Controls,
  SynEdit, Classes, SynEditTypes, SynEditHighlighter, SynHighlighterHashEntries, mnSynHighlighterMultiProc;

type
  THTMLRangeState = (rshtmlText, rshtmlAmpersand, rshtmlComment, rshtmlKeyword, rshtmlParam, rshtmlValue, rshtmlStringSQ, rshtmlStringDQ);

  { THTMLProcessor }

  THTMLProcessor = class(TSynProcessor)
  protected
    FRange: THTMLRangeState;
    FAndCode: Integer;
    function GetIdentChars: TSynIdentChars; override;
    procedure ResetRange; override;
    function GetRange: Byte; override;
    procedure SetRange(Value: Byte); override;
  public
    procedure BraceOpenProc;
    procedure IdentProc; override;
    procedure StringProc;
    procedure AmpersandProc;
    procedure TextProc;
    procedure CommentProc;
    procedure BraceCloseProc;
    procedure CRProc;
    procedure EqualProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;

    procedure Next; override;
    procedure InitIdent; override;
    procedure MakeProcTable; override;
  end;

const
  {$INCLUDE 'HTMLKeywords.inc'}

  MAX_ESCAPEAMPS = 249;

  EscapeAmps: array[0..MAX_ESCAPEAMPS - 1] of PChar = (
    ('&Alpha;'), { ?        } { greek capital alpha }
    ('&Beta;'), { ?        } { greek capital beta }
    ('&Gamma;'), { G        } { greek capital gamma }
    ('&Delta;'), { ?        } { greek capital delta }
    ('&Epsilon;'), { ?        } { greek capital epsilon }
    ('&Zeta;'), { ?        } { greek capital zeta }
    ('&Eta;'), { ?        } { greek capital eta }
    ('&Theta;'), { T        } { greek capital theta }
    ('&Iota;'), { ?        } { greek capital iota }
    ('&Kappa;'), { ?        } { greek capital kappa }
    ('&Lambda;'), { ?        } { greek capital lambda }
    ('&Mu;'), { ?        } { greek capital mu }
    ('&Nu;'), { ?        } { greek capital nu }
    ('&Xi;'), { ?        } { greek capital xi }
    ('&Omicron;'), { ?        } { greek capital omicron }
    ('&Pi;'), { ?        } { greek capital pi }
    ('&Rho;'), { ?        } { greek capital rho }
    ('&Sigma;'), { S        } { greek capital sigma }
    ('&Tau;'), { ?        } { greek capital tau }
    ('&Upsilon;'), { ?        } { greek capital upsilon }
    ('&Phi;'), { F        } { greek capital phi }
    ('&Chi;'), { ?        } { greek capital chi }
    ('&Psi;'), { ?        } { greek capital psi }
    ('&Omega;'), { O        } { greek capital omega }
    ('&alpha;'), { a        } { greek small alpha }
    ('&beta;'), { ß        } { greek small beta }
    ('&gamma;'), { ?        } { greek small gamma }
    ('&delta;'), { d        } { greek small delta }
    ('&epsilon;'), { e        } { greek small epsilon }
    ('&zeta;'), { ?        } { greek small zeta }
    ('&eta;'), { ?        } { greek small eta }
    ('&theta;'), { ?        } { greek small theta }
    ('&iota;'), { ?        } { greek small iota }
    ('&kappa;'), { ?        } { greek small kappa }
    ('&lambda;'), { ?        } { greek small lambda }
    ('&mu;'), { µ        } { greek small mu }
    ('&nu;'), { ?        } { greek small nu }
    ('&xi;'), { ?        } { greek small xi }
    ('&omicron;'), { ?        } { greek small omicron }
    ('&pi;'), { p        } { greek small pi }
    ('&rho;'), { ?        } { greek small rho }
    ('&sigmaf;'), { ?        } { greek small final sigma }
    ('&sigma;'), { s        } { greek small sigma }
    ('&tau;'), { t        } { greek small tau }
    ('&upsilon;'), { ?        } { greek small upsilon }
    ('&phi;'), { f        } { greek small phi }
    ('&chi;'), { ?        } { greek small chi }
    ('&psi;'), { ?        } { greek small psi }
    ('&omega;'), { ?        } { greek small omega }
    ('&thetasym;'), { ?        } { greek small theta symbol }
    ('&upsih;'), { ?        } { greek upsilon with hook symbol }
    ('&piv;'), { ?        } { greek pi symbol }
    ('&bull;'), { •        } { bullet }
    ('&hellip;'), { …        } { horizontal ellipsis }
    ('&prime;'), { '        } { prime }
    ('&Prime;'), { "        } { double prime }
    ('&oline;'), { ?        } { overline, = spacing overscore }
    ('&frasl;'), { /        } { fraction slash }
    ('&weierp;'), { P        } { script capital P }
    ('&image;'), { I        } { imaginary part }
    ('&real;'), { R        } { real part }
    ('&trade;'), { ™        } { trademark sign }
    ('&alefsym;'), { ?        } { first transfinite cardinal }
    ('&larr;'), { ?        } { leftwards arrow }
    ('&uarr;'), { ?        } { upwards arrow }
    ('&rarr;'), { ?        } { rightwards arrow }
    ('&darr;'), { ?        } { downwards arrow }
    ('&harr;'), { ?        } { left right arrow }
    ('&crarr;'), { ?        } { carriage return arrow }
    ('&lArr;'), { ?        } { leftwards double arrow }
    ('&uArr;'), { ?        } { upwards double arrow }
    ('&rArr;'), { ?        } { rightwards double arrow }
    ('&dArr;'), { ?        } { downwards double arrow }
    ('&hArr;'), { ?        } { left right double arrow }
    ('&forall;'), { ?        } { for all }
    ('&part;'), { ?        } { partial differential }
    ('&exist;'), { ?        } { there exists }
    ('&empty;'), { Ø        } { empty set }
    ('&nabla;'), { ?        } { backward difference }
    ('&isin;'), { ?        } { element of }
    ('&notin;'), { ?        } { not an element of }
    ('&ni;'), { ?        } { contains as member }
    ('&prod;'), { ?        } { n-ary product }
    ('&sum;'), { ?        } { n-ary sumation }
    ('&minus;'), { -        } { minus sign }
    ('&lowast;'), { *        } { asterisk operator }
    ('&radic;'), { v        } { square root }
    ('&prop;'), { ?        } { proportional to }
    ('&infin;'), { 8        } { infinity }
    ('&ang;'), { ?        } { angle }
    ('&and;'), { ?        } { logical and }
    ('&or;'), { ?        } { logical or }
    ('&cap;'), { n        } { intersection }
    ('&cup;'), { ?        } { union }
    ('&int;'), { ?        } { integral }
    ('&there4;'), { ?        } { therefore }
    ('&sim;'), { ~        } { similar to = tilde operator }
    ('&cong;'), { ?        } { approximately equal to }
    ('&asymp;'), { ˜        } { almost euqal to }
    ('&ne;'), { ?        } { not equal to }
    ('&equiv;'), { =        } { identical to }
    ('&le;'), { =        } { less-than or equal to }
    ('&ge;'), { =        } { greater-than or equal to }
    ('&sub;'), { ?        } { subset of }
    ('&sup;'), { ?        } { superset of }
    ('&nsub;'), { ?        } { not a subset of }
    ('&sube;'), { ?        } { subset of or equal to }
    ('&supe;'), { ?        } { superset of or equal to }
    ('&oplus;'), { ?        } { circled plus }
    ('&otimes;'), { ?        } { circled times }
    ('&perp;'), { ?        } { orthogonal to = perpendicular }
    ('&sdot;'), { ·        } { dot operator }
    ('&lceil;'), { ?        } { left ceiling }
    ('&rceil;'), { ?        } { right ceiling }
    ('&lfloor;'), { ?        } { left floor }
    ('&rfloor;'), { ?        } { right floor }
    ('&lang;'), { <        } { left-pointing angle bracket }
    ('&rang;'), { >        } { right-pointing angle bracket }
    ('&loz;'), { ?        } { lozenge }
    ('&spades;'), { ?        } { black spade suit }
    ('&clubs;'), { ?        } { black club suit }
    ('&hearts;'), { ?        } { black heart suit }
    ('&diams;'), { ?        } { black diamond suit }
    ('&lsquo;'), { ‘        } { left single quote  }
    ('&rsquo;'), { ’        } { right single quote }
    ('&sbquo;'), { ‚        } { single low-9 quote }
    ('&ldquo;'), { “        } { left double quote }
    ('&rdquo;'), { ”        } { right double quote }
    ('&bdquo;'), { „        } { double low-9 quote }
    ('&dagger;'), { †        } { dagger }
    ('&Dagger;'), { ‡        } { double dagger }
    ('&permil;'), { ‰        } { per mill sign }
    ('&lsaquo;'), { ‹        } { single left-pointing angle quote }
    ('&rsaquo;'), { ›        } { single right-pointing angle quote }
    ('&quot;'), { &#034; " } { double quotation mark }
    ('&amp;'), { &#038; & } { ampersand }
    ('&lt;'), { &#060; < } { less-than sign }
    ('&gt;'), { >        } { greater-than sign }
    ('&ndash;'), { &#150; – } { en dash }
    ('&mdash;'), { &#151; — } { em dash }
    ('&nbsp;'), { &#160;   } { nonbreaking space }
    ('&thinsp;'), {          } { thin space }
    ('&ensp;'), {          } { en space }
    ('&emsp;'), {          } { em space }
    ('&iexcl;'), { &#161; ! } { inverted exclamation }
    ('&cent;'), { &#162; c } { cent sign }
    ('&pound;'), { &#163; L } { pound sterling }
    ('&curren;'), { &#164; ¤ } { general currency sign }
    ('&yen;'), { &#165; Y } { yen sign }
    ('&brvbar;'), { &#166; ¦ } { broken vertical bar }
    ('&brkbar;'), { &#166; ¦ } { broken vertical bar }
    ('&sect;'), { &#167; § } { section sign }
    ('&uml;'), { &#168; ¨ } { umlaut }
    ('&die;'), { &#168; ¨ } { umlaut }
    ('&copy;'), { &#169; © } { copyright }
    ('&ordf;'), { &#170; a } { feminine ordinal }
    ('&laquo;'), { &#171; « } { left angle quote }
    ('&not;'), { &#172; ¬ } { not sign }
    ('&shy;'), { &#173; ­ } { soft hyphen }
    ('&reg;'), { &#174; ® } { registered trademark }
    ('&macr;'), { &#175; — } { macron accent }
    ('&hibar;'), { &#175; — } { macron accent }
    ('&deg;'), { &#176; ° } { degree sign }
    ('&plusmn;'), { &#177; ± } { plus or minus }
    ('&sup2;'), { &#178; 2 } { superscript two }
    ('&sup3;'), { &#179; 3 } { superscript three }
    ('&acute;'), { &#180; ´ } { acute accent }
    ('&micro;'), { &#181; µ } { micro sign }
    ('&para;'), { &#182; ¶ } { paragraph sign }
    ('&middot;'), { &#183; · } { middle dot }
    ('&cedil;'), { &#184; ¸ } { cedilla }
    ('&sup1;'), { &#185; 1 } { superscript one }
    ('&ordm;'), { &#186; o } { masculine ordinal }
    ('&raquo;'), { &#187; » } { right angle quote }
    ('&frac14;'), { &#188; 1 } { one-fourth }
    ('&frac12;'), { &#189; 1 } { one-half }
    ('&frac34;'), { &#190; 3 } { three-fourths }
    ('&iquest;'), { &#191; ? } { inverted question mark }
    ('&Agrave;'), { &#192; A } { uppercase A, grave accent }
    ('&Aacute;'), { &#193; Á } { uppercase A, acute accent }
    ('&Acirc;'), { &#194; Â } { uppercase A, circumflex accent }
    ('&Atilde;'), { &#195; A } { uppercase A, tilde }
    ('&Auml;'), { &#196; Ä } { uppercase A, umlaut }
    ('&Aring;'), { &#197; A } { uppercase A, ring }
    ('&AElig;'), { &#198; A } { uppercase AE }
    ('&Ccedil;'), { &#199; Ç } { uppercase C, cedilla }
    ('&Egrave;'), { &#200; E } { uppercase E, grave accent }
    ('&Eacute;'), { &#201; É } { uppercase E, acute accent }
    ('&Ecirc;'), { &#202; E } { uppercase E, circumflex accent }
    ('&Euml;'), { &#203; Ë } { uppercase E, umlaut }
    ('&Igrave;'), { &#204; I } { uppercase I, grave accent }
    ('&Iacute;'), { &#205; Í } { uppercase I, acute accent }
    ('&Icirc;'), { &#206; Î } { uppercase I, circumflex accent }
    ('&Iuml;'), { &#207; I } { uppercase I, umlaut }
    ('&ETH;'), { &#208; ? } { uppercase Eth, Icelandic }
    ('&Ntilde;'), { &#209; N } { uppercase N, tilde }
    ('&Ograve;'), { &#210; O } { uppercase O, grave accent }
    ('&Oacute;'), { &#211; Ó } { uppercase O, acute accent }
    ('&Ocirc;'), { &#212; Ô } { uppercase O, circumflex accent }
    ('&Otilde;'), { &#213; O } { uppercase O, tilde }
    ('&Ouml;'), { &#214; Ö } { uppercase O, umlaut }
    ('&times;'), { &#215; × } { multiplication sign }
    ('&Oslash;'), { &#216; O } { uppercase O, slash }
    ('&Ugrave;'), { &#217; U } { uppercase U, grave accent }
    ('&Uacute;'), { &#218; Ú } { uppercase U, acute accent }
    ('&Ucirc;'), { &#219; U } { uppercase U, circumflex accent }
    ('&Uuml;'), { &#220; Ü } { uppercase U, umlaut }
    ('&Yacute;'), { &#221; Ý } { uppercase Y, acute accent }
    ('&THORN;'), { &#222; ? } { uppercase THORN, Icelandic }
    ('&szlig;'), { &#223; ß } { lowercase sharps, German }
    ('&agrave;'), { &#224; à } { lowercase a, grave accent }
    ('&aacute;'), { &#225; á } { lowercase a, acute accent }
    ('&acirc;'), { &#226; â } { lowercase a, circumflex accent }
    ('&atilde;'), { &#227; ã } { lowercase a, tilde }
    ('&auml;'), { &#228; ä } { lowercase a, umlaut }
    ('&aring;'), { &#229; å } { lowercase a, ring }
    ('&aelig;'), { &#230; a } { lowercase ae }
    ('&ccedil;'), { &#231; ç } { lowercase c, cedilla }
    ('&egrave;'), { &#232; e } { lowercase e, grave accent }
    ('&eacute;'), { &#233; é } { lowercase e, acute accent }
    ('&ecirc;'), { &#234; ê } { lowercase e, circumflex accent }
    ('&euml;'), { &#235; ë } { lowercase e, umlaut }
    ('&igrave;'), { &#236; i } { lowercase i, grave accent }
    ('&iacute;'), { &#237; í } { lowercase i, acute accent }
    ('&icirc;'), { &#238; î } { lowercase i, circumflex accent }
    ('&iuml;'), { &#239; i } { lowercase i, umlaut }
    ('&eth;'), { &#240; ? } { lowercase eth, Icelandic }
    ('&ntilde;'), { &#241; ñ } { lowercase n, tilde }
    ('&ograve;'), { &#242; o } { lowercase o, grave accent }
    ('&oacute;'), { &#243; ó } { lowercase o, acute accent }
    ('&ocirc;'), { &#244; ô } { lowercase o, circumflex accent }
    ('&otilde;'), { &#245; o } { lowercase o, tilde }
    ('&ouml;'), { &#246; ö } { lowercase o, umlaut }
    ('&divide;'), { &#247; ÷ } { division sign }
    ('&oslash;'), { &#248; o } { lowercase o, slash }
    ('&ugrave;'), { &#249; u } { lowercase u, grave accent }
    ('&uacute;'), { &#250; ú } { lowercase u, acute accent }
    ('&ucirc;'), { &#251; u } { lowercase u, circumflex accent }
    ('&uuml;'), { &#252; ü } { lowercase u, umlaut }
    ('&yacute;'), { &#253; ý } { lowercase y, acute accent }
    ('&thorn;'), { &#254; ? } { lowercase thorn, Icelandic }
    ('&yuml;'), { &#255; y } { lowercase y, umlaut }
    ('&euro;'), { €        } { euro sign }
    ('&OElig;'), { Œ        } { capital ligature OE }
    ('&oelig;'), { œ        } { small ligature oe }
    ('&scaron;'), { š        } { small S with caron }
    ('&Scaron;'), { Š        } { capital S with caron }
    ('&fnof;'), { ƒ        } { function }
    ('&circ;') { ˆ        } { circumflex accent }
    );

implementation

{ THTMLProcessor }

procedure THTMLProcessor.MakeProcTable;
var
  I: char;
begin
  for I := #0 to #255 do
  begin
    case I of
      #0: ProcTable[I] := @NullProc;
      #10: ProcTable[I] := @LFProc;
      #13: ProcTable[I] := @CRProc;
      #1..#9, #11, #12, #14..#32: ProcTable[I] := @SpaceProc;
      '&': ProcTable[I] := @AmpersandProc;
      '"', '''': ProcTable[I] := @StringProc;
      '<': ProcTable[I] := @BraceOpenProc;
      '>': ProcTable[I] := @BraceCloseProc;
      '=': ProcTable[I] := @EqualProc;
    else
      ProcTable[I] := @IdentProc;
    end;
  end;
end;

procedure THTMLProcessor.Next;
begin
  Parent.fTokenPos := Parent.Run;
  case fRange of
    rshtmlText:
      TextProc;
    rshtmlComment:
      CommentProc;
    rshtmlStringSQ, rshtmlStringDQ:
      if Parent.fLine[Parent.Run] in [#0, #10, #13] then
        ProcTable[Parent.fLine[Parent.Run]]
      else
        StringProc;
  else
    ProcTable[Parent.FLine[Parent.Run]];
  end;
end;

procedure THTMLProcessor.AmpersandProc;
begin
  if fRange <> rshtmlAmpersand then
  begin
    if fRange = rshtmlKeyword then
    begin
      Inc(Parent.Run);
      fRange := rshtmlText;
      Parent.fTokenID := tkKeyword;
    end
    else
      IdentProc;
    Exit;
  end;

  case fAndCode of
    Low(EscapeAmps)..High(EscapeAmps):
      begin
        Parent.fTokenID := tkVariable;
        Inc(Parent.Run, StrLen(EscapeAmps[fAndCode]));
      end;
  else
    begin
      if (Parent.FLine[Parent.Run + 1] = '#') then
      begin
        fAndCode := -1;
        inc(Parent.Run, 2);
        if Parent.FLine[Parent.Run] in ['X', 'x'] then
        begin
          inc(Parent.Run);
          while (Parent.FLine[Parent.Run] in ['0'..'9', 'A'..'F', 'a'..'f']) do
            inc(Parent.Run);
        end
        else
          while (Parent.FLine[Parent.Run] in ['0'..'9']) do
            inc(Parent.Run);
        if (Parent.FLine[Parent.Run] = ';') then
        begin
          inc(Parent.Run);
          Parent.FTokenID := tkVariable;
        end
        else
          Parent.FTokenID := tkUnknown;
      end;
    end;
  end;
  fAndCode := -1;
  fRange := rshtmlText;
end;

procedure THTMLProcessor.BraceCloseProc;
begin
  fRange := rshtmlText;
  Parent.fTokenId := tkSymbol;
  Inc(Parent.Run);
end;

procedure THTMLProcessor.BraceOpenProc;
var
  aProcessor: string;
begin
  Inc(Parent.Run);
  aProcessor := '';
  if (Parent.FLine[Parent.Run] = '?') then
  begin
    Inc(Parent.Run);
    while Parent.FLine[Parent.Run] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-'] do
    begin
      aProcessor := aProcessor + Parent.FLine[Parent.Run];
      Inc(Parent.Run);
    end;
    Parent.fTokenID := tkProcessor;
    Parent.Processors.Switch(aProcessor);
  end
  else if (Parent.FLine[Parent.Run] = '!') and (Parent.FLine[Parent.Run + 1] = '-') and (Parent.FLine[Parent.Run + 2] = '-') then
  begin
    fRange := rshtmlComment;
    Parent.fTokenID := tkComment;
    Inc(Parent.Run, 3);
  end
  else if (Parent.FLine[Parent.Run] = '/') then
  begin
    fRange := rshtmlKeyword;
    Parent.fTokenID := tkSymbol;
    Inc(Parent.Run);
  end
  else
  begin
    fRange := rshtmlKeyword;
    Parent.fTokenID := tkSymbol;
  end;
end;

procedure THTMLProcessor.CommentProc;
begin
  Parent.fTokenID := tkComment;
  if (Parent.FLine[Parent.Run] in [#0, #10, #13]) then
    ProcTable[Parent.FLine[Parent.Run]]
  else
  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if (Parent.FLine[Parent.Run] = '>') and (Parent.FLine[Parent.Run - 1] = '-') and (Parent.FLine[Parent.Run - 2] = '-') then
    begin
      fRange := rshtmlText;
      Inc(Parent.Run);
      break;
    end;
    Inc(Parent.Run);
  end;
end;

procedure THTMLProcessor.CRProc;
begin
  Parent.fTokenID := tkSpace;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] = #10 then
    Inc(Parent.Run);
end;

procedure THTMLProcessor.EqualProc;
begin
  fRange := rshtmlValue;
  Parent.fTokenID := tkSymbol;
  Inc(Parent.Run);
end;

function THTMLProcessor.GetRange: Byte;
begin
  Result := Byte(FRange);
end;

procedure THTMLProcessor.IdentProc;
begin
  case FRange of
    rshtmlKeyword:
      begin
        FRange := rshtmlParam;
        inherited;
      end;
    rshtmlValue:
      begin
        FRange := rshtmlParam;
        Parent.FTokenID := tkString;
        repeat
          Inc(Parent.Run);
        until (Parent.FLine[Parent.Run] in [#0..#32, '>']);
      end;
  else
    begin
      Parent.FTokenID := ScanIdent(Parent.FLine + Parent.Run);//check here maybe no need to inc(run)
      //Inc(Parent.Run);
      while not (Parent.FLine[Parent.Run] in [#0..#32, '=', '"', '>']) do
        Inc(Parent.Run);
    end;
  end;
end;

procedure THTMLProcessor.LFProc;
begin
  Parent.fTokenID := tkSpace;
  Inc(Parent.Run);
end;

procedure THTMLProcessor.NullProc;
begin
  Parent.fTokenID := tkNull;
end;

procedure THTMLProcessor.ResetRange;
begin
  inherited;
  FRange := rshtmlText;
end;

procedure THTMLProcessor.SetRange(Value: Byte);
begin
  FRange := THTMLRangeState(Value);
end;

procedure THTMLProcessor.SpaceProc;
begin
  Inc(Parent.Run);
  Parent.fTokenID := tkSpace;
  while Parent.FLine[Parent.Run] <= #32 do
  begin
    if Parent.FLine[Parent.Run] in [#0, #9, #10, #13] then
      break;
    Inc(Parent.Run);
  end;
end;

procedure THTMLProcessor.StringProc;
var
  iOpenChar: Char;
begin
  case fRange of
    rshtmlStringSQ:
      begin
        iOpenChar := '''';
        Parent.fTokenID := tkString;
      end;
    rshtmlStringDQ:
      begin
        iOpenChar := '"';
        Parent.fTokenID := tkString;
      end;
    rshtmlValue:
    begin
      iOpenChar := Parent.FLine[Parent.Run];
      if iOpenChar = '"' then
        fRange := rshtmlStringDQ
      else
        fRange := rshtmlStringSQ;
      Parent.fTokenID := tkString;
      Inc(Parent.Run); { jumps over the opening char }
    end
  else
    begin
      iOpenChar := Parent.FLine[Parent.Run];
      IdentProc;
      Inc(Parent.Run); { jumps over the opening char }
      Exit;
    end;
  end;

  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if Parent.FLine[Parent.Run] = iOpenChar then
    begin
      Inc(Parent.Run); { jumps over the closing char }
      if fRange in [rshtmlStringDQ, rshtmlStringSQ] then
        fRange := rshtmlParam
      else
        fRange := rshtmlText;
      break;
    end;
    Inc(Parent.Run);
  end;
end;

procedure THTMLProcessor.TextProc;
const
  StopSet = [#0..#31, '<', '&'];
var
  i: Integer;
begin
  if Parent.FLine[Parent.Run] in (StopSet - ['&']) then
  begin
    ProcTable[Parent.FLine[Parent.Run]];
  end
  else
  begin
    Parent.fTokenID := tkUnknown;
    while True do
    begin
      while not (Parent.FLine[Parent.Run] in StopSet) do
        Inc(Parent.Run);

      if (Parent.FLine[Parent.Run] = '&') then
      begin
        if (Parent.FLine[Parent.Run + 1] = '#') then
        begin
          fAndCode := -1;
          i := Parent.Run;
          inc(Parent.Run, 2);
          if Parent.FLine[Parent.Run] in ['X', 'x'] then
          begin
            inc(Parent.Run);
            while (Parent.FLine[Parent.Run] in ['0'..'9', 'A'..'F', 'a'..'f']) do
              inc(Parent.Run);
          end
          else
            while (Parent.FLine[Parent.Run] in ['0'..'9']) do
              inc(Parent.Run);
          if (Parent.FLine[Parent.Run] = ';') then
          begin
            inc(Parent.Run);
            Parent.Run := i;
            fRange := rshtmlAmpersand;
          end;
          BREAK;
        end
        else
        begin
          for i := Low(EscapeAmps) to High(EscapeAmps) do
          begin
            if (StrLComp((Parent.FLine + Parent.Run), PChar(EscapeAmps[i]), StrLen(EscapeAmps[i])) = 0) then
            begin
              fAndCode := i;
              fRange := rshtmlAmpersand;
              Exit;
            end;
          end;
        end;

        Inc(Parent.Run);
      end
      else
      begin
        Break;
      end;
    end;
  end;
end;

procedure THTMLProcessor.InitIdent;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sHTMLKeywords, TSynValidStringChars, @DoAddKeyword);
  FRange := rshtmlText;
  FAndCode := -1;
end;

function THTMLProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars + ['&', ';'];
end;

end.

