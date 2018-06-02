unit mnSynHighlighterConfig;
{$mode objfpc}{$H+}
{**
 *  MiniLib project
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
  Classes, Graphics,
  SynEditTypes, SynEditHighlighter, mnSynUtils;


type
  TtkTokenKind = (tkComment, tkText, tkSection, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  { TSynINISyn }

  TSynConfigSyn = class(TSynCustomHighlighter)
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
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: String; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
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
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property TextAttri   : TSynHighlighterAttributes read FTextAttri write FTextAttri;
    property SectionAttri: TSynHighlighterAttributes read FSectionAttri write FSectionAttri;
    property KeyAttri    : TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri : TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri  : TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri : TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri : TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

procedure TSynConfigSyn.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      #0      : FProcTable[i] := @NullProc;
      #10 {LF}: FProcTable[i] := @LFProc;
      #13 {CR}: FProcTable[i] := @CRProc;
      #1..#9, #11, #12, #14..#32: FProcTable[i] := @SpaceProc;
      '"'  : FProcTable[i] := @StringProc;
      '''' : FProcTable[i] := @StringProc1;
      '#' : FProcTable[i] := @CommentProc;
      ';' : FProcTable[i] := @CommentProc;
      '=' : FProcTable[i] := @EqualProc;
      '[' : FProcTable[i] := @SectionOpenProc;
      '0'..'9': FProcTable[i] := @NumberProc;
    else
      FProcTable[i] := @TextProc;
    end;
end;

constructor TSynConfigSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clGreen;
  AddAttribute(FCommentAttri);
  FTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText);
  AddAttribute(FTextAttri);
  FSectionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSection);
  FSectionAttri.Style := [fsBold];
  AddAttribute(FSectionAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrKey);
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(@DefHighlightChange);

  FDefaultFilter := SYNS_FilterINI;
  MakeMethodTables;
end;

procedure TSynConfigSyn.SetLine(const NewValue: String; LineNumber:Integer);
begin
  inherited;
  FLine := PChar(NewValue);
  Run := 0;
  FLineNumber := LineNumber;
  Next;
end;

procedure TSynConfigSyn.SectionOpenProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if Run > 0 then
  begin
    FTokenID := tkText;
    inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a Section
  FTokenID := tkSection;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      ']': begin inc(Run); break end;
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynConfigSyn.CRProc;
begin
  FTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynConfigSyn.EqualProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynConfigSyn.KeyProc;
begin
  FTokenID := tkKey;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      ' ': break;
      '=': break;
      #10: break;
      #13: break;
    else
        inc(Run);
    end;
end;

procedure TSynConfigSyn.TextProc;
begin
  if Run = 0 then
    KeyProc
  else begin
    FTokenID := tkText;
    inc(Run);
    while FLine[Run] <> #0 do
      if FLine[Run] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
        inc(Run)
      else break;
  end;
end;

procedure TSynConfigSyn.LFProc;
begin
  FTokenID := tkSpace;
  inc(Run);
end;

procedure TSynConfigSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynConfigSyn.NumberProc;
begin
  if Run = 0 then
    KeyProc
  else begin
    inc(Run);
    FTokenID := tkNumber;
    while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do inc(Run);
    if FLine[Run] in ['a'..'z','A'..'Z'] then TextProc;
  end;
end;

// ;
procedure TSynConfigSyn.CommentProc;
begin
  // if it is not column 0 mark as tkText and get out of here
  if Run > 0 then
  begin
    FTokenID := tkText;
    inc(Run);
    Exit;
  end;

  // this is column 0 ok it is a comment
  FTokenID := tkComment;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TSynConfigSyn.SpaceProc;
begin
  inc(Run);
  FTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do
    inc(Run);
end;

// ""
procedure TSynConfigSyn.StringProc;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #34;

  if FLine[Run] <> #0 then
    inc(Run);
end;

// ''
procedure TSynConfigSyn.StringProc1;
begin
  FTokenID := tkString;
  if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = #39;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynConfigSyn.Next;
begin
  FTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynConfigSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynConfigSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynConfigSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

procedure TSynConfigSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynConfigSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynConfigSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment: Result := FCommentAttri;
    tkText   : Result := FTextAttri;
    tkSection: Result := FSectionAttri;
    tkKey    : Result := FKeyAttri;
    tkNumber : Result := FNumberAttri;
    tkSpace  : Result := FSpaceAttri;
    tkString : Result := FStringAttri;
    tkSymbol : Result := FSymbolAttri;
    tkUnknown: Result := FTextAttri;
    else Result := nil;
  end;
end;

function TSynConfigSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynConfigSyn.GetTokenPos: Integer;
begin
 Result := FTokenPos;
end;

function TSynConfigSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynConfigSyn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterINI;
end;

class function TSynConfigSyn.GetLanguageName: string;
begin
  Result := 'INI';
end;

function TSynConfigSyn.GetSampleSource: String;
begin
  Result := '# Syntax highlighting'#13#10+
            '[Options]'#13#10+
            'Visible = true'+#13#10+
            'Protected = false'+#13#10+
            'Port = 80'#13#10+
            'Host = "server"'#13#10+
            ';End of INI'#13#10;
end;

initialization
  RegisterPlaceableHighlighter(TSynConfigSyn);
end.
