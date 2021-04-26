unit minibidi;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey
 *}
{$IFDEF FPC}
{$MODE objfpc}
{$ELSE}
{$ENDIF}
{$H+}

(************************************************************************
* Ported from minibidi.c
* Original header
* ------------
* Description:
* ------------
* This is an implemention of Unicode's Bidirectional Algorithm
* (known as UAX #9).
*
* http://www.unicode.org/reports/tr9/
*
* Author: Ahmad Khalifa
*
************************************************************************)
{
* Ported with improvements to Pascal by: Zaher Dirkey, zaher at gmail.com
* License under MIT license
*}
{***********************************************************************

http://cvs.arabeyes.org/viewcvs/projects/adawat/minibidi
http://svn.tartarus.org/sgt/putty/minibidi.c?r1=8097&view=log
///http://portaputty.googlecode.com/svn-history/r6/trunk/minibidi.c

http://www.unicode.org/Public/PROGRAMS/BidiReferenceCpp/

http://unicode.org/cldr/utility/bidi.jsp

***********************************************************************}

{
  Bugs
* 'arabic words #13#10'
  when Start is Default or RightToLeft, converted to:
  '#13#10 words arabic'
   the problem the control chars moved to the first
* search for '- 1]' for Types[i - 1], there is no -1 element, it is give range check error 

test ET chars
}

interface

uses
  Classes, SysUtils;

type
  TBidiParagraph = (bdpDefault, bdpLeftToRight, bdpRightToLeft);
  TBidiNumbers = (bdnContext, bdnLatin, bdnArabic); //TODO or Latin =Arabic, and Arabic =Hindi, but for me it names Arabic1, Arabic2 it is not Hindi, need to extended mirror
  TBidiOptions = set of (bdoApplyShape, bdoReorderCombining);
  TBidiLigatures = (bdlComplex, bdlSimple, bdlNone);

function BidiString(const Text: UnicodeString; Options: TBidiOptions = [bdoApplyShape]; Numbers: TBidiNumbers = bdnContext; Start: TBidiParagraph = bdpDefault; Ligatures: TBidiLigatures = bdlComplex): UnicodeString;

function BidiText(Line: PWideChar; Count: Integer; Options: TBidiOptions = [bdoApplyShape]; Numbers: TBidiNumbers = bdnContext; Start: TBidiParagraph = bdpDefault; Ligatures: TBidiLigatures = bdlComplex): Integer;

implementation

const
{$IFDEF FPC}
{$ELSE}
  cMAX = MaxInt div sizeof(Integer) - 1;
{$ENDIF}
  MAX_STACK = 60;

  LMASK = $3F; { Embedding Level mask }
  OMASK = $C0; { Override mask }
  OISL = $80; { Override is L }
  OISR = $40; { Override is R }

type
    { Shaping Types }

  TShapeType =
    (
    stSL, { Left-Joining, doesnt exist in U+0600 - U+06FF }
    stSR, { Right-Joining, ie has Isolated, Final }
    stSD, { Dual-Joining, ie has Isolated, Final, Initial, Medial }
    stSU, { Non-Joining }
    stSC { Join-Causing, like U+0640 (TATWEEL) }
    );

  { character Types }
  TCharacterType =
    (
    { Strong Char Types }
    ctL, { Left-to-Right char }
    ctLRE, { Left-to-Right Embedding }
    ctLRO, { Left-to-Right Override }
    ctR, { Right-to-Left char }
    ctAL, { Right-to-Left Arabic char }
    ctRLE, { Right-to-Left Embedding }
    ctRLO, { Right-to-Left Override }
   { Weak Char Types }
    ctPDF, { Pop Directional Format }
    ctEN, { European Number }
    ctES, { European Number Separator }
    ctET, { European Number Terminator }
    ctAN, { Arabic Number }
    ctCS, { Common Number Separator }
    ctNSM, { Non Spacing Mark }
    ctBN, { Boundary Neutral }
    { Neutral Char Types }
    ctB, { Paragraph Separator }
    ctS, { Segment Separator }
    ctWS, { Whitespace }
    ctON { Other Neutrals }
    );

{$IFDEF FPC}
  PCharacterType = ^TCharacterType;
{$ELSE}
  TCharacterTypes = array[0..cMAX] of TCharacterType;
  PCharacterType = ^TCharacterTypes;
{$ENDIF}

//  PShapeType = ^TShapeType;

  TLevel = Integer;
{$IFDEF FPC}
  PLevel = ^TLevel;
{$ELSE}
  TLevels = array[0..cMAX] of TLevel;
  PLevel = ^TLevels;
{$ENDIF}

{$I minibidi.inc}

function BidiString(const Text: UnicodeString; Options: TBidiOptions; Numbers: TBidiNumbers; Start: TBidiParagraph; Ligatures: TBidiLigatures): UnicodeString;
var
  l: Integer;
begin
  Result := UnicodeString(Text); //for new string can be changed safe by BidiText
  l := BidiText(PWideChar(Result), Length(Result), Options, Numbers, Start, Ligatures);
  SetLength(Result, l);
end;

{
from here minibidi.c
}

{ Returns the first odd/even value greater than x }

function even(x: Integer): Boolean;
begin
  Result := not Odd(x);
end;

function LeastGreaterOdd(x: Integer): Integer;
begin
  if odd(x) then
    Result := (x + 2)
  else
    Result := (x + 1);
end;

function LeastGreaterEven(x: Integer): Integer;
begin
  if odd(x) then
    Result := (x + 1)
  else
    Result := (x + 2);
end;

{ Shaping Helpers }

function ShapeType(xh: WideChar): TShapeType;
begin
  if ((xh >= SHAPE_FIRST) and (xh <= SHAPE_LAST)) then
    Result := ShapeTypes[ord(xh) - ord(SHAPE_FIRST)].ST
  else
    Result := stSU;
end;

function SISOLATED(xh: WideChar): WideChar;
begin
  Result := ShapeTypes[Ord(xh) - ord(SHAPE_FIRST)].CH;
end;

function SFINAL(xh: WideChar): WideChar;
begin
  Result := WideChar(Ord(xh) + 1);
end;

function SINITIAL(xh: WideChar): WideChar;
begin
  Result := WideChar(Ord(xh) + 2);
end;

function SMEDIAL(xh: WideChar): WideChar;
begin
  Result := WideChar(Ord(xh) + 3);
end;

{ function GetType
* Return the type of char using CharLookup array
}

function GetType(ch: WideChar): TCharacterType;
var
  i, j, k: Integer;
begin
  i := -1;
  j := Length(CharLookup);
  Result := ctON;
  while (j - i > 1) do
  begin
    k := (i + j) div 2;
    if (ch < CharLookup[k].Fr) then
      j := k
    else if (ch > CharLookup[k].Lt) then
      i := k
    else
    begin
      Result := CharLookup[k].CT;
      break;
    end;
  end;
    {*
     * If we reach here, the character was not in any of the
     * intervals listed in the lookup table. This means we return
     * ON (`Other Neutrals'). This is the appropriate code for any
     * character genuinely not listed in the Unicode table, and
     * also the table above has deliberately left out any
     * characters _explicitly_ listed as ON (to save space!).
     *}
end;

{ Finds the index of a run with level equals tlevel }

function FindIndexOfRun(const Levels: PLevel; Start, Count: Integer; Level: TLevel): Integer;
var
  i: Integer;
begin
  Result := Count;
  for i := Start to Count - 1 do
  begin
    if (Level <= Levels[i]) then
    begin
      Result := i;
      break;
    end;
  end;
end;

{*
 * Flips the text buffer, according to max level, and
 * all higher Levels
 *
 * Input:
 * Line: text buffer, on which to apply flipping
 * Levels: resolved Levels buffer
 * Max: the maximum level found in this Line (should be unsigned char)
 * Count: Line size in wchar_t
 *}

procedure FlipThisRun(Line: PWideChar; Levels: PLevel; Max, Count: Integer);
var
  i, j, k: Integer;
  Level: TLevel;
  temp: WideChar;
begin
  j := 0;
  i := 0;
  while (i < Count) and (j < Count) do
  begin
     { find the start of the run of level=max }
    Level := Max;
    i := FindIndexOfRun(Levels, i, Count, Max);
    j := i;
     { find the end of the run }
    while ((i < Count) and (Level <= Levels[i])) do
      Inc(i);

    k := i - 1;
    while (k > j) do
    begin
      temp := Line[k];
      Line[k] := Line[j];
      Line[j] := temp;
      Dec(k);
      Inc(j);
    end;
  end;
end;

function GetParagraphLevel(Line: PWideChar; Count: Integer): TLevel;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    if (GetType(Line[i]) = ctR) or (GetType(Line[i]) = ctAL) then
    begin
      Result := 1;
      break;
    end
    else if (GetType(Line[i]) = ctL) then
    begin
      Result := 0;
      break;
    end;
  end;
end;

{*
 * Returns character type of ch, by calling RLE table lookup
 * function
 *}

function getCAPRtl(ch: WideChar): TCharacterType;
{ CAPRtl Charset }
const
  cTypesFromChar: array[0..127] of TCharacterType =
  (
    ctON, ctON, ctON, ctON, ctL, ctR, ctON, ctON, ctON, ctON, ctON, ctON, ctON, ctB, ctRLO, ctRLE, { 00-0f }
    ctLRO, ctLRE, ctPDF, ctWS, ctON, ctON, ctON, ctON, ctON, ctON, ctON, ctON, ctON, ctON, ctON, ctON, { 10-1f }
    ctWS, ctON, ctON, ctON, ctET, ctON, ctON, ctON, ctON, ctON, ctON, ctET, ctCS, ctON, ctES, ctES, { 20-2f }
    ctEN, ctEN, ctEN, ctEN, ctEN, ctEN, ctAN, ctAN, ctAN, ctAN, ctLRE, ctRLE, ctRLO, ctPDF, ctLRO, ctON, { 30-3f }
//  ctEN, ctEN, ctEN, ctEN, ctEN, ctEN, ctAN, ctAN, ctAN, ctAN, ctCS, ctON, ctON, ctON, ctON, ctON, { 30-3f }
    ctR, ctAL, ctAL, ctAL, ctAL, ctAL, ctAL, ctR, ctR, ctR, ctR, ctR, ctR, ctR, ctR, ctR, { 40-4f }
    ctR, ctR, ctR, ctR, ctR, ctR, ctR, ctR, ctR, ctR, ctR, ctON, ctB, ctON, ctON, ctON, { 50-5f }
    ctNSM, ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctL, { 60-6f }
    ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctL, ctON, ctS, ctON, ctON, ctON { 70-7f }
    );

begin
  if (ch < #$007F) then
    Result := cTypesFromChar[ord(ch)]
  else
    Result := ctR;
end;

{
 * takes a pointer to a character that is checked for
 * having a mirror glyph, and replaced on the spot
}

procedure DoMirror(var ch: WideChar);
var
  i, j, k: Integer;
begin
  i := -1;
  j := Length(MirrorLookup);
  while (j - i > 1) do
  begin
    k := (i + j) div 2;
    if (ch < MirrorLookup[k].Idx) then
      j := k
    else if (ch > MirrorLookup[k].Idx) then
      i := k
    else if (ch = MirrorLookup[k].Idx) then
    begin
      ch := MirrorLookup[k].Mr;
      break;
    end;
  end;
end;

{* The Main shaping function, and the only one to be used
 * by the outside world.
 *
 * Line: buffer to apply shaping to. this must be passed by doBidi() first
 * ToLine: output buffer for the shaped data
 * from: start bidi at this index
 * Count: number of characters in Line
 *}

function DoShape(Line: PWideChar; var ToLine: PWideChar; From: Integer; Count: Integer; Ligatures: TBidiLigatures): Integer;
var
  i, j: Integer;
  ligFlag: Boolean;
  prevTemp, nextTemp: TShapeType;
  nWC: WideChar;//Next wide char
begin
  ligFlag := False;
  prevTemp := stSU;
  nextTemp := stSU;
  i := From;
  while i < Count do
  begin
  { Get Previous and next Characters type }
    j := i;
    Dec(j);
    while (j >= 0) do
    begin
      if (GetType(Line[j]) <> ctNSM) then
      begin
        prevTemp := ShapeType(Line[j]);
        break;
      end;
      Dec(j);
    end;

    j := i;
    Inc(j);
    while (j < Count) do
    begin
      if (GetType(Line[j]) <> ctNSM) then
      begin
        nextTemp := ShapeType(Line[j]);
        break;
      end
      else if (j = Count - 1) then
      begin
        nextTemp := stSU;
        break;
      end;
      Inc(j);
    end;

    case (ShapeType(Line[i])) of
      stSC, stSU:
        ToLine[i] := Line[i];
      stSR:
        if (prevTemp = stSD) or (prevTemp = stSC) then
          ToLine[i] := SFINAL(SISOLATED(Line[i]))
        else
          ToLine[i] := SISOLATED(Line[i]);
      stSD:
        begin
        { Make Ligatures }
          if (Line[i] = #$0644) then //{LAM}
          begin
            nWC := #0; //TODO check if ctNSM not found
            j := i;
            while (j < Count) do
            begin
              Inc(j);
              if (GetType(Line[j]) <> ctNSM) then
              begin
                nWC := Line[j];
                break;
              end;
            end;

            case (nWC) of
              #$0622: //{ALEF WITH MADDA ABOVE}
                begin
                  ligFlag := True;
                  if (prevTemp = stSD) or (prevTemp = stSC) then
                    ToLine[i] := #$FEF6
                  else
                    ToLine[i] := #$FEF5;
                end;
              #$623: //{ALEF WITH HAMZA ABOVE}
                begin
                  ligFlag := True;
                  if (prevTemp = stSD) or (prevTemp = stSC) then
                    ToLine[i] := #$FEF8
                  else
                    ToLine[i] := #$FEF7;
                end;
              #$625: {ALEF WITH HAMZA BELOW}
                begin
                  ligFlag := True;
                  if (prevTemp = stSD) or (prevTemp = stSC) then
                    ToLine[i] := #$FEFA
                  else
                    ToLine[i] := #$FEF9;
                end;
              #$627: //{ALEF}
                begin
                  ligFlag := True;
                  if (prevTemp = stSD) or (prevTemp = stSC) then
                    ToLine[i] := #$FEFC
                  else
                    ToLine[i] := #$FEFB;
                end;
            end;
          end; //end of {if (Line[i] = #$0644) then}

          if ligFlag then
          begin
            if Ligatures = bdlSimple then
              ToLine[j] := #$20
            else
              ToLine[j] := #$0; //dead char for delete
            i := j;
            ligFlag := False;
          end
          else if ((prevTemp = stSD) or (prevTemp = stSC)) then
          begin
            if (nextTemp = stSR) or (nextTemp = stSD) or (nextTemp = stSC) then
              ToLine[i] := SMEDIAL(SISOLATED(Line[i]))
            else
              ToLine[i] := SFINAL(SISOLATED(Line[i]));
          end
          else
          begin
            if (nextTemp = stSR) or (nextTemp = stSD) or (nextTemp = stSC) then
              ToLine[i] := SINITIAL(SISOLATED(Line[i]))
            else
              ToLine[i] := SISOLATED(Line[i]);
          end;
        end;
    end; //case ShapeType
    nextTemp := stSU;
    Inc(i);
  end;
  Result := i;
end;

{
  Rule (X1), (X2), (X3), (X4), (X5), (X6), (X7), (X8), (X9)
  returns the length of the string without explicit marks
}

function DoTypes(Line: PWideChar; ParagraphLevel: TLevel; Types: PCharacterType; Levels: PLevel; Count: Integer; fX: Boolean): Integer;
var
  tempType: TCharacterType;
  CurrentEmbedding: Integer;
  CurrentOverride: TCharacterType;
  LevelStack: array[0..MAX_STACK + 2] of TLevel;
  OverrideStack: array[0..MAX_STACK + 2] of TCharacterType;
  StackTop: Integer;
  i, j: Integer;
begin
  CurrentEmbedding := ParagraphLevel;
  CurrentOverride := ctON;
  StackTop := 0;
  if fX then
  begin
    j := 0;
    for i := 0 to Count - 1 do
    begin
      Line[j] := Line[i];
      tempType := GetType(Line[i]);
      case tempType of
        ctRLE:
          if (stackTop < MAX_STACK) then
          begin
            LevelStack[stackTop] := CurrentEmbedding;
            OverrideStack[stackTop] := currentOverride;
            Inc(StackTop);
            CurrentEmbedding := LeastGreaterOdd(currentEmbedding);
            CurrentOverride := ctON;
          end;
        ctLRE:
          if (stackTop < MAX_STACK) then
          begin
            LevelStack[stackTop] := CurrentEmbedding;
            OverrideStack[stackTop] := CurrentOverride;
            Inc(StackTop);
            CurrentEmbedding := LeastGreaterEven(CurrentEmbedding);
            CurrentOverride := ctON;
          end;
        ctRLO:
          if (stackTop < MAX_STACK) then
          begin
            LevelStack[stackTop] := CurrentEmbedding;
            overrideStack[stackTop] := CurrentOverride;
            Inc(StackTop);
            CurrentEmbedding := leastGreaterOdd(CurrentEmbedding);
            CurrentOverride := ctR;
          end;
        ctLRO:
          if (stackTop <= MAX_STACK) then
          begin
            LevelStack[stackTop] := CurrentEmbedding;
            OverrideStack[stackTop] := CurrentOverride;
            Inc(StackTop);
            CurrentEmbedding := LeastGreaterEven(CurrentEmbedding);
            CurrentOverride := ctL;
          end;
        ctPDF:
          if (stackTop > 0) then
          begin
            CurrentEmbedding := LevelStack[StackTop - 1];
            CurrentOverride := OverrideStack[StackTop - 1];
            Dec(StackTop);
          end;
       { Whitespace is treated as neutral for now }
        ctWS, ctB, ctS:
          begin
            Levels[j] := CurrentEmbedding;
            tempType := ctON;
            if (currentOverride <> ctON) then
              tempType := CurrentOverride;
            Types[j] := TempType;
            Inc(j);
          end;
      else
        begin
          Levels[j] := CurrentEmbedding;
          if (currentOverride <> ctON) then
            tempType := CurrentOverride;
          Types[j] := tempType;
          Inc(j);
        end;
      end;
    end;
  end
  else
  begin
    j := Count;
    for i := 0 to Count - 1 do
    begin
      tempType := GetType(Line[i]);
      case tempType of
        ctWS, ctB, ctS:
          begin
            Levels[i] := CurrentEmbedding;
            tempType := ctON;
            if (CurrentOverride <> ctON) then
              tempType := CurrentOverride;
          end;
      else
        begin
          Levels[i] := CurrentEmbedding;
          if (CurrentOverride <> ctON) then
            tempType := CurrentOverride;
        end;
      end;
      Types[i] := tempType;
    end;
  end;
  Result := j;
end;

{ Rule (W3) }

procedure DoALtoR(Types: PCharacterType; Count: integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if (Types[i] = ctAL) then
      Types[i] := ctR;
  end;
end;

{
 * The Main Bidi Function, and the only function that should
 * be used by the outside world.
 *
 * Line: a buffer of size Count containing text to apply
 * the Bidirectional algorithm to.
 }

function BidiText(Line: PWideChar; Count: Integer; Options: TBidiOptions; Numbers: TBidiNumbers; Start: TBidiParagraph; Ligatures: TBidiLigatures): Integer;
var
  Types: PCharacterType;
  Levels: PLevel;
  ParagraphLevel: TLevel;
  tempType, tempTypeSec: TCharacterType;
  tempLevel: TLevel;
  temp: WideChar;
  i, j: Integer;
  it: Integer;
  fX, fAL, fET, fNSM: Boolean;
  ShapeTo: PWideChar;
  NewCount: Integer;
begin
//  Result := 0;

  fX := False;
  fAL := False;
  fET := False;
  fNSM := False;
  for i := 0 to Count - 1 do
  begin
    case GetType(Line[i]) of
      ctAL, ctR:
        fAL := True;
      ctLRE, ctLRO, ctRLE, ctRLO, ctPDF, ctBN:
        fX := True;
      ctET:
        fET := True;
      ctNSM:
        fNSM := True;
    end
  end;

  if (not fAL) and (not fX) then
  begin
    Result := Count;
    exit; //nothing todo
  end;

   { Initialize Types, Levels }
  Types := AllocMem(SizeOf(TCharacterType) * Count);
  Levels := AllocMem(SizeOf(TLevel) * Count);
  try

     { Rule (P1)  NOT IMPLEMENTED
      * P1. Split the text into separate paragraphs. A paragraph separator is
      * kept with the previous paragraph. Within each paragraph, apply all the
      * other rules of this algorithm.
      }

     { Rule (P2), (P3)
      * P2. In each paragraph, find the first character of type L, AL, or R.
      * P3. If a character is found in P2 and it is of type AL or R, then set
      * the paragraph embedding level to one; otherwise, set it to zero.
      }
    case Start of
      bdpLeftToRight:
        ParagraphLevel := 0;
      bdpRightToLeft:
        ParagraphLevel := 1;
      else
        ParagraphLevel := GetParagraphLevel(Line, Count);
    end;

     { Rule (X1), (X2), (X3), (X4), (X5), (X6), (X7), (X8), (X9)
      * X1. Begin by setting the current embedding level to the paragraph
   *     embedding level. Set the directional override status to neutral.
      * X2. With each RLE, compute the least greater odd embedding level.
      * X3. With each LRE, compute the least greater even embedding level.
      * X4. With each RLO, compute the least greater odd embedding level.
      * X5. With each LRO, compute the least greater even embedding level.
      * X6. For all Types besides RLE, LRE, RLO, LRO, and PDF:
      *		a. Set the level of the current character to the current
      *		    embedding level.
      *		b.  Whenever the directional override status is not neutral,
      *               reset the current character type to the directional
      *               override status.
      * X7. With each PDF, determine the matching embedding or override code.
      * If there was a valid matching code, restore (pop) the last
      * remembered (pushed) embedding level and directional override.
      * X8. All explicit directional embeddings and overrides are completely
      * terminated at the end of each paragraph. Paragraph separators are not
      * included in the embedding. (Useless here) NOT IMPLEMENTED
      * X9. Remove all RLE, LRE, RLO, LRO, PDF, and BN codes.
      * Here, they're converted to BN.
      }

    NewCount := DoTypes(Line, ParagraphLevel, Types, Levels, Count, fX);
    if NewCount < Count then
      Count := NewCount;

     { Rule (W1)
      * W1. Examine each non-spacing mark (NSM) in the level run, and change
      * the type of the NSM to the type of the previous character. If the NSM
      * is at the start of the level run, it will get the type of sor.
      }

    if (fNSM) then
    begin
      if (Types[0] = ctNSM) then
        Types[0] := TCharacterType(ParagraphLevel); //TODO: wrong assign

      for i := 1 to Count - 1 do //*
      begin
        if (Types[i] = ctNSM) then
          Types[i] := Types[i - 1];
      { Is this a safe assumption?
       * I assumed the previous, IS a character.
       }
      end
    end;

     { Rule (W2)
      * W2. Search backwards from each instance of a European number until the
      * first strong type (R, L, AL, or sor) is found.  If an AL is found,
      * change the type of the European number to Arabic number.
      }
    for i := 0 to Count - 1 do
    begin
      if (Types[i] = ctEN) then
      begin
        tempLevel := Levels[i]; //zaher tempType -> tempLevel
        j := i;
        Dec(j);
        while (j >= 0) and (Levels[j] = tempLevel) do
        begin
          if (Types[j] = ctAL) then
          begin
            Types[i] := ctAN;
            break;
          end
          else if (Types[j] = ctR) or (Types[j] = ctL) then
          begin
            break;
          end;
          Dec(j);
        end
      end
    end;

     { Rule (W3)
      * W3. Change all ALs to R.
      *
      * Optimization: on Rule Xn, we might set a flag on AL type
      * to prevent this loop in L R lines only...
      }
    doALtoR(Types, Count);

     { Rule (W4)
      * W4. A single European separator between two European numbers changes
      * to a European number. A single common separator between two numbers
      * of the same type changes to that type.
      }

    //TODO: what if is the last char and after european number?
    for i := 1 to Count - 2 do //1..count-2 for use [i - 1] [i + 1]
    begin
      if (Types[i] = ctES) then
      begin
        if (Types[i - 1] = ctEN) and (Types[i + 1] = ctEN) then
          Types[i] := ctEN;
      end else if (Types[i] = ctCS) then
      begin
        if (Types[i - 1] = ctEN) and (Types[i + 1] = ctEN) then
          Types[i] := ctEN
        else if (Types[i - 1] = ctAN) and (Types[i + 1] = ctAN) then
          Types[i] := ctAN;
      end
    end;

     { Rule (W5)
      * W5. A sequence of European terminators adjacent to European numbers
      * changes to all European numbers.
      *
      * Optimization: lots here... else ifs need rearrangement
      }
    if (fET) then
    begin
      for i := 0 to Count - 1 do
      begin
        if (Types[i] = ctET) then
        begin
          if (i > 0) and (Types[i - 1] = ctEN) then
          begin
            Types[i] := ctEN;
            //continue;//do we need it?
          end
          else if (i < Count - 2) and (Types[i + 1] = ctEN) then
          begin
            Types[i] := ctEN;
            //continue;//do we need it?
          end
          else if (i < Count - 2) and (Types[i + 1] = ctET) then
          begin
            j := i + 1;
            while (j < Count) and (Types[j] = ctET) do
              Inc(j);
            if (j < Count) and (Types[j] = ctEN) then
              Types[i] := ctEN;
          end
        end
      end
    end;

     { Rule (W6)
      * W6. Otherwise, separators and terminators change to Other Neutral:
      }
    for i := 0 to Count - 1 do
    begin
      case Types[i] of
        ctES, ctET, ctCS:
          Types[i] := ctON;
      end
    end;

     { Rule (W7)
      * W7. Search backwards from each instance of a European number until
      * the first strong type (R, L, or sor) is found. If an L is found,
      * then change the type of the European number to L.
      }
   //TODO
    for i := 0 to Count - 1 do
    begin
      if (Types[i] = ctEN) then
      begin
        tempLevel := Levels[i];
        j := i;
        Dec(j);
        while (j >= 0) and (Levels[j] = tempLevel) do
        begin
          if (Types[j] = ctL) then
          begin
            Types[i] := ctL;
            break;
          end
          else
            if (Types[j] = ctR) or (Types[j] = ctAL) then
              break;
          Dec(j);
        end
      end
    end;

     { Rule (N1)
      * N1. A sequence of neutrals takes the direction of the surrounding
      * strong text if the text on both sides has the same direction. European
      * and Arabic numbers are treated as though they were R.
      }

    i := 0;
    while i < Count - 1 do
    begin
      if (Types[i] = ctON) then
      begin
        if i = 0 then
          tempType := ctR
        else if (Types[i - 1] = ctR) or (Types[i - 1] = ctEN) or (Types[i - 1] = ctAN) then//TODO: bug '- 1]'
          tempType := ctR
        else
          tempType := ctL;

        tempTypeSec := ctL;
        
        j := i;
        while (j < Count) do
        begin
          tempTypeSec := Types[j];
          if (tempTypeSec = ctON) then
            Inc(j)
          else
            break;
        end;

        if (j = Count) then
        begin
          if odd(ParagraphLevel) then
            tempTypeSec := ctR
          else
            tempTypeSec := ctL;
        end;

        if (((tempTypeSec = ctL) or (tempTypeSec = ctLRE)) and (tempType = ctL)) or
          (((tempTypeSec = ctR) or (tempTypeSec = ctEN) or (tempTypeSec = ctAN)) and (tempType = ctR)) then
        begin
          while (i < j) do
          begin
            Types[i] := tempType;
            inc(i);
          end
        end
        else
          i := j;
      end;
      inc(i);
    end;

  { Rule (N2)
  * N2. Any remaining neutrals take the embedding direction.
  }

    for i := 0 to Count - 1 do
    begin
      if (Types[i] = ctON) then
      begin
        if even(Levels[i]) then
          Types[i] := ctL
        else
          Types[i] := ctR;
      end
    end;

     { Rule (I1)
      * I1. For all characters with an even (left-to-right) embedding
      * direction, those of type R go up one level and those of type AN or
      * EN go up two Levels.
      }
    for i := 0 to Count - 1 do
    begin
      if even(Levels[i]) then
      begin
        if (Types[i] = ctR) then
          Levels[i] := Levels[i] + 1
        else if ((Types[i] = ctAN) or (Types[i] = ctEN)) then
          Levels[i] := Levels[i] + 2;
      end else
      begin
        if ((Types[i] = ctL) or
          (Types[i] = ctEN) or
          (Types[i] = ctAN)) then
          Levels[i] := Levels[i] + 1;
      end
    end;

     { Rule (I2)
      * I2. For all characters with an odd (right-to-left) embedding direction,
      * those of type L, EN or AN go up one level.
      }

    for i := 0 to Count - 1 do
    begin
      if odd(Levels[i]) then
      begin
        if (Types[i] = ctL) or (Types[i] = ctEN) or (Types[i] = ctAN) then
          Levels[i] := Levels[i] + 1;
      end
    end;

     { Rule (L1)
      * L1. On each Line, reset the embedding level of the following characters
      * to the paragraph embedding level:
      *		(1)segment separators, (2)paragraph separators,
      *           (3)any sequence of whitespace characters preceding
      *           a segment separator or paragraph separator,
      *           (4)and any sequence of white space characters
      *           at the end of the Line.
      * The Types of characters used here are the original Types, not those
      * modified by the previous phase.
      }

    j := Count - 1;
    while (j > 0) and (GetType(Line[j]) = ctWS) do
      Dec(j);

    if (j < Count - 1) then
    begin
      for j := j + 1 to Count - 1 do
        Levels[j] := ParagraphLevel;
    end;

    for i := 0 to Count - 1 do
    begin
      tempType := GetType(Line[i]);
      if (tempType = ctWS) then
      begin
        j := i;
        Inc(j);
        while ((j < Count) and ((tempType = ctWS) or (tempType = ctRLE))) do
        begin
          tempType := GetType(Line[j]);
          Inc(j);
        end;

        if (GetType(Line[j]) = ctB) or (GetType(Line[j]) = ctS) then
        begin
          for j := j - 1 downto i do
          begin
            Levels[j] := ParagraphLevel;
          end
        end
      end else if (tempType = ctB) or (tempType = ctS) then
        Levels[i] := ParagraphLevel;
    end;

     { Rule (L4)
      * L4. A character that possesses the mirrored property as specified by
      * Section 4.7, Mirrored, must be depicted by a mirrored glyph if the
      * resolved directionality of that character is R.
      }
     { Note: this is implemented before L2 for efficiency }

    for i := 0 to Count - 1 do
    begin
      if odd(Levels[i]) then
      begin
        temp := Line[i];
        DoMirror(temp);
        Line[i] := temp;
      end;;
    end;

     { Rule (L3)
      * L3. Combining marks applied to a right-to-left base character will at
      * this point precede their base character. If the rendering engine
      * expects them to follow the base characters in the final display
      * process, then the ordering of the marks and the base character must
      * be reversed.
      * Combining marks are reordered to the right of each character on an
      * odd level.
      }

    if (fNSM and (bdoReorderCombining in Options)) then
    begin
      i := 0;
      while i < Count do
      begin
        if (GetType(Line[i]) = ctNSM) and odd(Levels[i]) then
        begin
          j := i;
          Inc(j);
          while ((j < Count) and (GetType(Line[j]) = ctNSM)) do
            Inc(j);
          dec(j);
          dec(i);
          it := j;
          while j > i do
          begin
            temp := Line[i];
            Line[i] := Line[j];
            Line[j] := temp;
            Inc(i);
            Dec(j);
          end;
          i := it + 1;
        end;
        Inc(i);
      end
    end;

  { Shaping
  * Shaping is Applied to each run of Levels separately....
  }

    if (bdoApplyShape in Options) then
    begin
      GetMem(ShapeTo, SizeOf(WideChar) * Count);
      try
        for i := 0 to Count - 1 do
          ShapeTo[i] := Line[i];

        j := 0;
        i := 0;
        while (j < Count) do
        begin
          if (GetType(Line[j]) = ctAL) then
          begin
            if (j < Count) and (j >= i) then
            begin
              tempLevel := Levels[j];
              i := j;
              while (i < Count) and (Levels[i] = tempLevel) do
                Inc(i);
              DoShape(Line, ShapeTo, j, i, Ligatures);
              j := i;
  //            tempLevel := Levels[j]; ????
            end
          end;
          Inc(j);
        end;

        i := 0;
        j := 0;
        NewCount := Count;
        while i < Count do
        begin
          if (Ligatures = bdlComplex) and (ShapeTo[i] = #$0) then //remove deleted char
            NewCount := NewCount - 1
          else
          begin
            Levels[j] := Levels[i];
            Line[j] := ShapeTo[i];
            j := j + 1;
          end;
          i := i + 1;
        end;
        Count := NewCount;
      finally
        Freemem(ShapeTo);
      end;
    end;

  { Rule (L2)
  * L2. From the highest level found in the text to the lowest odd level on
  * each Line, including intermediate Levels not actually present in the
  * text, reverse any contiguous sequence of characters that are at that
  * level or higher
  }

  { we flip the character string and leave the level array }
    i := 0;
    tempLevel := Levels[0];
    while (i < Count) do
    begin
      if (Levels[i] > tempLevel) then
        tempLevel := Levels[i];
      Inc(i);
    end;

    while (tempLevel > 0) do { loop from highest level to the least odd, }
    begin { which i assume is 1 }
      FlipThisRun(Line, Levels, tempLevel, Count);
      Dec(tempLevel);
    end;

  {* Rule (L3) NOT IMPLEMENTED
   * L3. Combining marks applied to a right-to-left base character will at
   * this point precede their base character. If the rendering engine
   * expects them to follow the base characters in the final display
   * process, then the ordering of the marks and the base character must
   * be reversed.
   *}

  finally
    Freemem(Types);
    Freemem(Levels);
  end;
  Line[Count] := #0;
  Result := Count;
end;

{

Samples (Lorem Ipsum)

http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=ArabicFonts
http://people.w3.org/rishida/scripts/samples/arabic.php

عندما يريد العالم أن ‪يتكلّم ‬ ، فهو يتحدّث بلغة يونيكود. تسجّل الآن لحضور المؤتمر الدولي العاشر ليونيكود (Unicode Conference)، الذي سيعقد في 10-12 آذار 1997 بمدينة مَايِنْتْس، ألمانيا. و سيجمع المؤتمر بين خبراء من كافة قطاعات الصناعة على الشبكة العالمية انترنيت ويونيكود، حيث ستتم، على الصعيدين الدولي والمحلي على حد سواء مناقشة سبل استخدام يونكود في النظم القائمة وفيما يخص التطبيقات الحاسوبية، الخطوط، تصميم النصوص والحوسبة متعددة اللغات.

}

end.
