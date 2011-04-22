unit SynHighlighterApache;
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
  Graphics,
  SynEditTypes,
  SynEditHighlighter, SynHighlighterHashEntries,
  Classes;

type
  TtkTokenKind = (tkComment, tkText, tkSection, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TProcTableProc = procedure of object;

type

  { TSynApacheSyn }

  TSynApacheSyn = class(TSynCustomHighlighter)
  private
    FLine: PChar;
    FLineNumber: Integer;
    FProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    FTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FTextAttri: TSynHighlighterAttributes;
    FSectionAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    procedure SectionOpenProc;
    procedure KeyProc;
    procedure CRProc;
    procedure EqualProc;
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure CommentProc;
    procedure SpaceProc;
    procedure StringProc;  // ""
    procedure StringProc1; // ''
    procedure MakeMethodTables;
  protected
    {General Stuff}
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: String; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    //procedure DoAddKeyword(AKeyword: string; AKind: integer);
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;   
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber:Integer); override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property TextAttri   : TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property SectionAttri: TSynHighlighterAttributes read fSectionAttri
      write fSectionAttri;
    property KeyAttri    : TSynHighlighterAttributes read fKeyAttri
      write fKeyAttri;
    property NumberAttri : TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri  : TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri : TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri : TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

  PHashTable = ^THashTable;
  THashTable = array[Char] of Integer;

const
  sApacheKeywords =
    'IfModule,'+
    'Include,'+
    'ServerRoot';

implementation

uses
  SynEditStrConst;

procedure TSynApacheSyn.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      #0      : fProcTable[i] := @NullProc;
      #10 {LF}: fProcTable[i] := @LFProc;
      #13 {CR}: fProcTable[i] := @CRProc;
      #34 {"} : fProcTable[i] := @StringProc;
      #39 {'} : fProcTable[i] := @StringProc1;
      '0'..'9': fProcTable[i] := @NumberProc;
      '#' {#} : fProcTable[i] := @CommentProc;
      #61 {=} : fProcTable[i] := @EqualProc;
      #91 {[} : fProcTable[i] := @SectionOpenProc;
      #1..#9, #11, #12, #14..#32: FProcTable[i] := @SpaceProc;
    else
      fProcTable[i] := @TextProc;
    end;
end;

constructor TSynApacheSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri            := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style      := [fsItalic];
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);
  fTextAttri               := TSynHighlighterAttributes.Create(SYNS_AttrText);
  AddAttribute(fTextAttri);
  fSectionAttri            := TSynHighlighterAttributes.Create(SYNS_AttrSection);
  fSectionAttri.Style      := [fsBold];
  AddAttribute(fSectionAttri);
  fKeyAttri                := TSynHighlighterAttributes.Create(SYNS_AttrKey);
  AddAttribute(fKeyAttri);
  fNumberAttri             := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri              := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri             := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fSymbolAttri             := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(@DefHighlightChange);

  fDefaultFilter := SYNS_FilterINI;
  //EnumerateKeywords(Ord(tkKeyword), sApacheKeywords, TSynValidStringChars, DoAddKeyword);
  MakeMethodTables;
end; { Create }

procedure TSynApacheSyn.SetLine(const NewValue: String; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynApacheSyn.SectionOpenProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if Run > 0 then
  begin
    fTokenID := tkText;
    inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a Section
  fTokenID := tkSection;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      ']': begin inc(Run); break end;
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynApacheSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynApacheSyn.EqualProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynApacheSyn.KeyProc;
begin
  fTokenID := tkKey;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      '=': break;
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynApacheSyn.TextProc;
begin
  if Run = 0 then
    KeyProc
  else begin
    fTokenID := tkText;
    inc(Run);
    while FLine[Run] <> #0 do
      if FLine[Run] in ['a'..'z', 'A'..'Z', '0'..'9'] then
        inc(Run)
      else break;
  end;
end;

procedure TSynApacheSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynApacheSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynApacheSyn.NumberProc;
begin
  if Run = 0 then
    KeyProc
  else begin
    inc(Run);
    fTokenID := tkNumber;
    while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do inc(Run);
    if FLine[Run] in ['a'..'z','A'..'Z'] then TextProc;
  end;
end;

// ;
procedure TSynApacheSyn.CommentProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if Run > 0 then
  begin
    fTokenID := tkText;
    inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a comment
  fTokenID := tkComment;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynApacheSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

// ""
procedure TSynApacheSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

// ''
procedure TSynApacheSyn.StringProc1;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynApacheSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynApacheSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynApacheSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynApacheSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynApacheSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynApacheSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynApacheSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkText   : Result := fTextAttri;
    tkSection: Result := fSectionAttri;
    tkKey    : Result := fKeyAttri;
    tkNumber : Result := fNumberAttri;
    tkSpace  : Result := fSpaceAttri;
    tkString : Result := fStringAttri;
    tkSymbol : Result := fSymbolAttri;
    tkUnknown: Result := fTextAttri;
    else Result := nil;
  end;
end;

function TSynApacheSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynApacheSyn.GetTokenPos: Integer;
begin
 Result := fTokenPos;
end;

function TSynApacheSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynApacheSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterINI;
end;

class function TSynApacheSyn.GetLanguageName: string;
begin
  Result := 'Apache';
end;

{procedure TSynApacheSyn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  FKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;}

function TSynApacheSyn.GetSampleSource: String;
begin
  Result := '# Syntax highlighting'#13#10+
            '<Section>'#13#10+
            'DocumentRoot "d:/httpd"'+#13#10+
            'Listen 80'#13#10+
            '</Section>'#13#10;
end;

initialization
  RegisterPlaceableHighlighter(TSynApacheSyn);
end.
