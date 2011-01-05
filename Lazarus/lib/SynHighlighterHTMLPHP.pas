unit SynHighlighterHTMLPHP;
{$mode delphi}
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
{
  http://flatdev.republika.pl/php-functions-lastest.zip
}
interface

uses
  SysUtils, Windows, Messages, Graphics, Registry, Controls,
  SynEdit, SynEditTextBuffer, Contnrs, Classes, SynEditTypes, SynEditHighlighter, SynHighlighterHashEntries;

type
  TtkTokenKind = (tkUnknown, tkNull, tkSpace, tkComment, tkIdentifier, tkSymbol, tkNumber,
    tkString, tkText, tkValue, tkHTML, tkKeyword, tkFunction, tkVariable, tkSQL, tkProcessor);

  THTMLRangeState = (rshtmlText, rshtmlAmpersand, rshtmlComment, rshtmlKeyword, rshtmlParam,
    rshtmlValue, rshtmlStringSQ, rshtmlStringDQ);

  TPHPRangeState = (rsphpUnknown, rsphpComment, rsphpStringDQ, rsphpStringSQ, rsphpVarExpansion);

  TProcTableProc = procedure of object;

  PIdentifierTable = ^TIdentifierTable;
  TIdentifierTable = array[Char] of ByteBool;

  PHashTable = ^THashTable;
  THashTable = array[Char] of Integer;

  TSynHTMLPHPSyn = class;

  TSynProcessor = class(TObject)
  private
    FKeywords: TSynHashEntryList;
    FName: string;
    FIndex: Integer;
    FParent: TSynHTMLPHPSyn;
    function KeyComp(const aKey: string): Boolean;
  protected
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    function GetIdentChars: TSynIdentChars; virtual;
    procedure ResetRange; virtual;
    function GetRange: Byte; virtual;
    procedure SetRange(Value: Byte); virtual;
    function KeyHash(ToHash: PChar): Integer; virtual;
    function IdentKind(MayBe: PChar): TtkTokenKind; virtual;
  public
    FStringLen: Integer;
    FToIdent: PChar;
    Identifiers: TIdentifierTable;
    HashTable: THashTable;
    ProcTable: array[#0..#255] of TProcTableProc;
    constructor Create(AParent: TSynHTMLPHPSyn; Name: string); virtual;
    destructor Destroy; override;
    procedure Next; virtual;
    procedure InitIdent; virtual;
    procedure MakeIdentTable; virtual;
    procedure MakeMethodTables; virtual;
    property Parent: TSynHTMLPHPSyn read FParent;
    property Name: string read FName write FName;
    property Index: Integer read FIndex;
  end;

  TSynProcessors = class(TObjectList)
  private
    FCurrent: TSynProcessor;
    FMainProcessor: string;
    FDefaultProcessor: string;
    FPlainProcessor: string;
    function GetItem(Index: Integer): TSynProcessor;
    procedure SetItem(Index: Integer; const Value: TSynProcessor);
    function GetMain: TSynProcessor;
    procedure SetCurrent(Value: TSynProcessor);
  public
    function Add(AProcessor: TSynProcessor): Integer;
    function Find(const Name: string): TSynProcessor;
    function IndexOf(const Name: string): Integer;
    procedure Switch(const Name: string); overload;
    procedure Switch(Index: Integer); overload;
    property Current: TSynProcessor read FCurrent;
    property Main: TSynProcessor read GetMain;
    property MainProcessor: string read FMainProcessor write FMainProcessor;
    property DefaultProcessor: string read FDefaultProcessor write FDefaultProcessor;
    property PlainProcessor: string read FPlainProcessor write FPlainProcessor;
    property Items[Index: Integer]: TSynProcessor read GetItem write SetItem; default;
  end;

//Unkown Processor
  TPlainProcessor = class(TSynProcessor)
  public
    procedure NullProc;
    procedure LFProc;
    procedure CRProc;
    procedure Next; override;
  end;

//SynEdit

  { TSynHTMLPHPSyn }

  TSynHTMLPHPSyn = class(TSynCustomHighlighter)
  private
    fCommentAttri: TSynHighlighterAttributes;
    fValueAttri: TSynHighlighterAttributes;
    FFunctionAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fHtmlAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fKeywordAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    FBracketsAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    fProcessorAttri: TSynHighlighterAttributes;
    procedure InitIdent;
    procedure MakeMethodTables;
    procedure MakeIdentTable;
  protected
    FProcessors: TSynProcessors;
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    property Processors: TSynProcessors read FProcessors;
    class function GetLanguageName: string; override;
  public
    Run: LongInt;
    FLineNumber: Integer;
    FTokenPos: Integer;
    FLine: PChar;
    FTokenID: TtkTokenKind;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEOL: Boolean; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;
    procedure Next; override;
    procedure SetLine(const NewValue: string; LineNumber: Integer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property ValueAttri: TSynHighlighterAttributes read fValueAttri write fValueAttri;
    property FunctionAttri: TSynHighlighterAttributes read FFunctionAttri write FFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property HtmlAttri: TSynHighlighterAttributes read fHtmlAttri write fHtmlAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri write fTextAttri;
    property KeywordAttri: TSynHighlighterAttributes read fKeywordAttri write fKeywordAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property BracketsAttri: TSynHighlighterAttributes read FBracketsAttri write FBracketsAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri write fVariableAttri;
    property ProcessorAttri: TSynHighlighterAttributes read fProcessorAttri write fProcessorAttri;
  end;

const
{$INCLUDE 'PHPKeywords.inc'}

  SYNS_FilterHTMLPHP = 'PHP Files (*.php;*.php3;*.phtml;*.inc)|*.php;*.php3;*.phtml;*.inc';
  SYNS_LangHTMLPHP = 'HTML/PHP';

//range mix Main processor as byte and Current processor as byte and index Byte
function RangeToProcessor(Range: Pointer): Byte;
function MixRange(Index, Main, Current: Byte): Cardinal;
procedure SplitRange(Range: Cardinal; var Index, Main, Current: Byte);

function GetWordAtRowColEx(SynEdit: TCustomSynEdit; XY: TPoint; IdentChars: TSynIdentChars; Select:Boolean): string;
function GetHighlighterAttriAtRowColEx2(SynEdit: TCustomSynEdit; const XY: TPoint;
  var Token: string; var TokenType, Start: Integer;
  var Attri: TSynHighlighterAttributes;
  var Range: Pointer): boolean;

implementation

uses
  SynEditStrConst, PHPProcessor, HTMLProcessor, VarUtils;

function RangeToProcessor(Range: Pointer): Byte;
begin
  Result := Integer(Range) and $FF;
end;

function MixRange(Index, Main, Current: Byte): Cardinal;
begin
  Result := Index or Main shl 8 or Current shl 16;
end;

procedure SplitRange(Range: Cardinal; var Index, Main, Current: Byte);
begin
  Index := Range and $FF;
  Main := Range shr 8 and $FF;
  Current := Range shr 16 and $FF;
end;

function GetHighlighterAttriAtRowColEx2(SynEdit: TCustomSynEdit; const XY: TPoint;
  var Token: string; var TokenType, Start: Integer;
  var Attri: TSynHighlighterAttributes;
  var Range: Pointer): boolean;
var
  PosX, PosY: integer;
  Line: string;
  aToken: string;
begin
  with SynEdit do
  begin
    TokenType := 0;
    Token := '';
    Attri := nil;
    Result := FALSE;
    PosY := XY.Y - 1;
    if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then
    begin
      Line := Lines[PosY];
      if PosY = 0 then
        Highlighter.ResetRange
      else
        Highlighter.SetRange(TSynEditStringList(Lines).Ranges[Pointer(PosY - 1)]);
      Highlighter.SetLine(Line, PosY);
      PosX := XY.X;
      Range := Highlighter.GetRange;
      if PosX > 0 then
        while not Highlighter.GetEol do
        begin
          Start := Highlighter.GetTokenPos + 1;
          aToken := Highlighter.GetToken;
          Range := Highlighter.GetRange;
          if (PosX >= Start) and (PosX < Start + Length(aToken)) then
          begin
            Attri := Highlighter.GetTokenAttribute;
            TokenType := Highlighter.GetTokenKind;
            Token := aToken;
            Result := TRUE;
            exit;
          end;
          Highlighter.Next;
        end;
    end;
  end;
end;

function GetWordAtRowColEx(SynEdit: TCustomSynEdit; XY: TPoint; IdentChars: TSynIdentChars; Select:Boolean): string;
var
  Line: string;
  Len, Stop: integer;
begin
  Result := '';
  if (XY.Y >= 1) and (XY.Y <= SynEdit.Lines.Count) then
  begin
    Line := SynEdit.Lines[XY.Y - 1];
    Len := Length(Line);
    if Len <> 0 then
    begin
      if (XY.X > 1) and (XY.X <= Len + 1) and not (Line[XY.X] in IdentChars) then
        XY.X :=XY.X - 1;
      if (XY.X >= 1) and (XY.X <= Len + 1) and (Line[XY.X] in IdentChars) then
      begin
        Stop := XY.X;
        while (Stop <= Len) and (Line[Stop] in IdentChars) do
          Inc(Stop);
        while (XY.X > 1) and (Line[XY.X - 1] in IdentChars) do
          Dec(XY.X);
        if Stop > XY.X then
        begin
          Result := Copy(Line, XY.X, Stop - XY.X);
          if Select then
          begin
            SynEdit.CaretXY := XY;
            SynEdit.SelEnd := XY.x + Length(Result);
          end;
        end;
      end;
    end;
  end;
end;

function TSynProcessor.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
end;

function TSynProcessor.KeyComp(const aKey: string): Boolean;
var
  i: integer;
  pKey1, pKey2: PChar;
begin
  pKey1 := fToIdent;
  // Note: fStringLen is always > 0 !
  pKey2 := pointer(aKey);
  for i := 1 to fStringLen do
  begin
    if HashTable[pKey1^] <> HashTable[pKey2^] then
    begin
      Result := FALSE;
      exit;
    end;
    Inc(pKey1);
    Inc(pKey2);
  end;
  Result := True;
end;

function TSynProcessor.IdentKind(MayBe: PChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[KeyHash(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if KeyComp(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

procedure TSynProcessor.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  FKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

procedure TSynHTMLPHPSyn.MakeMethodTables;
var
  i: Integer;
begin
  for i := 0 to Processors.Count - 1 do
    Processors[i].MakeMethodTables;
end;

procedure TSynHTMLPHPSyn.MakeIdentTable;
var
  i: Integer;
begin
  for i := 0 to Processors.Count - 1 do
    Processors[i].MakeIdentTable;
end;

constructor TSynHTMLPHPSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProcessors := TSynProcessors.Create;
  FProcessors.Add(THTMLProcessor.Create(Self, 'html'));
  FProcessors.Add(TPHPProcessor.Create(Self, 'php'));
  FProcessors.Add(TPlainProcessor.Create(Self, 'plain'));

  FProcessors.MainProcessor := 'html';
  FProcessors.DefaultProcessor := 'php';
  FProcessors.PlainProcessor := 'plain';
  FProcessors.Switch(FProcessors.FMainProcessor);

  MakeIdentTable;

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  fSpaceAttri.Foreground := clSilver;
  AddAttribute(fSpaceAttri);

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground := $000069D2;
  AddAttribute(fCommentAttri);

  fValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue);
  fValueAttri.Style := [fsBold];
  fValueAttri.Foreground := $00985A89;
  AddAttribute(fValueAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  fIdentifierAttri.Foreground := $00A35949;
  AddAttribute(fIdentifierAttri);

  FFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction);
  FFunctionAttri.Style := [fsBold];
  FFunctionAttri.Foreground := $00926221;
  AddAttribute(FFunctionAttri);

  fHtmlAttri := TSynHighlighterAttributes.Create('HTML');
  fHtmlAttri.Style := [fsBold];
  fHtmlAttri.Foreground := $00AD655A;
  AddAttribute(fHtmlAttri);

  fTextAttri := TSynHighlighterAttributes.Create('Text');
  AddAttribute(fTextAttri);

  fKeywordAttri := TSynHighlighterAttributes.Create('Keyword');
  fKeywordAttri.Foreground := clGreen;
  AddAttribute(fKeywordAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := $00006F00;
  fNumberAttri.Style := [fsBold];
  AddAttribute(fNumberAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  StringAttri.Foreground := $002F2FC6;
  AddAttribute(StringAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(FSymbolAttri);

  FBracketsAttri := TSynHighlighterAttributes.Create('Brackets');
  FBracketsAttri.Background := $00E1D3BD;
  AddAttribute(FBracketsAttri);

  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable);
  fVariableAttri.Style := [fsBold];
  AddAttribute(fVariableAttri);

  fProcessorAttri := TSynHighlighterAttributes.Create('Processor');
  fProcessorAttri.Style := [fsBold];
  AddAttribute(fProcessorAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterHTMLPHP;
end;

destructor TSynHTMLPHPSyn.Destroy;
begin
  FProcessors.Free;
  inherited;
end;

procedure TSynHTMLPHPSyn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  FLine := PChar(NewValue);
  FLineNumber := LineNumber;
  Run := 0;
  Next;
end;

function TSynHTMLPHPSyn.IsKeyword(const AKeyword: string): boolean;
var
  tk: TtkTokenKind;
begin
  tk := Processors.Current.IdentKind(PChar(AKeyword));
  Result := tk in [tkFunction, tkKeyword, tkHTML];
end;

procedure TSynHTMLPHPSyn.Next;
begin
  Processors.Current.Next;
end;

function TSynHTMLPHPSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeywordAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynHTMLPHPSyn.GetEOL: Boolean;
begin
  Result := FTokenID = tkNull;
end;

function TSynHTMLPHPSyn.GetRange: Pointer;
begin
  Result := Pointer(MixRange(Processors.Current.Index, Processors.Main.GetRange, Processors.Current.GetRange));
end;

function TSynHTMLPHPSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

procedure TSynHTMLPHPSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynHTMLPHPSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynHTMLPHPSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkValue: Result := fValueAttri;
    tkFunction: Result := FFunctionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkHtml: Result := fHtmlAttri;
    tkKeyword: Result := fKeywordAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkText: Result := FTextAttri;
    tkVariable: Result := fVariableAttri;
    tkProcessor: Result := fProcessorAttri;
    tkUnknown: Result := fTextAttri;
  else
    Result := nil;
  end;
end;

function TSynHTMLPHPSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynHTMLPHPSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

procedure TSynHTMLPHPSyn.ResetRange;
var
  i: Integer;
begin
  for i := 0 to Processors.Count - 1 do
    Processors[i].ResetRange;
  Processors.Switch(Processors.MainProcessor);
end;

procedure TSynHTMLPHPSyn.SetRange(Value: Pointer);
var
  aIndex, aMain, aCurrent: Byte;
  i:Integer;
begin
  inherited;
  SplitRange(Cardinal(Value), aIndex, aMain, aCurrent);
  Processors.Switch(aIndex);
  Processors.Main.SetRange(aMain);
  if aIndex = 0 then
  begin
    for i := 1 to Processors.Count -1 do
      Processors[i].ResetRange;
  end
  else
    Processors.Current.SetRange(aCurrent);
end;

function TSynHTMLPHPSyn.GetIdentChars: TSynIdentChars;
begin
//  Result := TSynValidStringChars + ['&', '#', ';', '$'];
  Result := TSynValidStringChars + ['&', '#', '$'];
end;

class function TSynHTMLPHPSyn.GetLanguageName: string;
begin
  Result := SYNS_LangHTMLPHP;
end;

function TSynHTMLPHPSyn.GetSampleSource: string;
begin
  Result := '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'#13#10 +
    '<html dir="ltr">'#13#10 +
    '  <body class="normal">'#13#10 +
    '  HTML and PHP syntax editor'#13#10 +
    '<?php'#13#10 +
    '// Syntax highlighting'#13#10 +
    '  function printNumber()'#13#10 +
    '  {'#13#10 +
    '    $number = 1234;'#13#10 +
    '    print "The number is $number";'#13#10 +
    '    for ($i = 0; $i <= $number; $i++)'#13#10 +
    '    {'#13#10 +
    '      $x++;'#13#10 +
    '      $x--;'#13#10 +
    '      $x += 1.0;'#13#10 +
    '    }'#13#10 +
    ' }'#13#10 +
    '?>'#13#10 +
    '  </body>'#13#10 +
    '</html>'#13#10;
end;

{ TSynProcessor }

procedure TSynProcessor.MakeIdentTable;
begin

end;

procedure TSynProcessor.MakeMethodTables;
begin

end;

procedure TSynProcessor.Next;
begin
end;

constructor TSynProcessor.Create(AParent: TSynHTMLPHPSyn; Name: string);
begin
  inherited Create;
  FName := Name;
  FParent := AParent;
  FKeywords := TSynHashEntryList.Create;
end;

destructor TSynProcessor.Destroy;
begin
  FKeywords.Free;
  inherited;
end;

procedure TSynProcessor.ResetRange;
begin
end;

function TSynProcessor.GetRange: Byte;
begin
  Result := 0;
end;

procedure TSynProcessor.SetRange(Value: Byte);
begin
end;

procedure TSynProcessor.InitIdent;
begin
end;

function TSynProcessor.GetIdentChars: TSynIdentChars;
begin
  Result := [#33..#255];
end;

{ TPlainProcessor }

procedure TPlainProcessor.CRProc;
begin
  Parent.FTokenID := tkSpace;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] = #10 then
    Inc(Parent.Run);
end;

procedure TPlainProcessor.LFProc;
begin
  Parent.FTokenID := tkSpace;
  inc(Parent.Run);
end;

procedure TPlainProcessor.Next;
begin
  Parent.FTokenPos := Parent.Run;
  case Parent.FLine[Parent.Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      Parent.FTokenID := tkUnknown;
      repeat
        if (Parent.FLine[Parent.Run] = '?') and (Parent.FLine[Parent.Run + 1] = '>') then
        begin
          Parent.Processors.Switch(Parent.Processors.MainProcessor);
          Inc(Parent.Run, 2);
          break;
        end;
        Inc(Parent.Run);
      until Parent.FLine[Parent.Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TPlainProcessor.NullProc;
begin
  Parent.FTokenID := tkNull;
end;

{ TSynProcessors }

function TSynProcessors.Add(AProcessor: TSynProcessor): Integer;
begin
  AProcessor.FIndex := inherited Add(AProcessor);
  Result := AProcessor.Index;
end;

function TSynProcessors.Find(const Name: string): TSynProcessor;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TSynProcessors.GetItem(Index: Integer): TSynProcessor;
begin
  Result := inherited Items[Index] as TSynProcessor;
end;

function TSynProcessors.GetMain: TSynProcessor;
begin
  Result := Items[0];
end;

function TSynProcessors.IndexOf(const Name: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := Items[i].Index;
      break;
    end;
  end;
end;

procedure TSynProcessors.SetCurrent(Value: TSynProcessor);
begin
  if FCurrent <> Value then
  begin
    FCurrent := Value;
  end;
end;

procedure TSynProcessors.SetItem(Index: Integer; const Value: TSynProcessor);
begin
  inherited Items[Index] := Value;
end;

procedure TSynProcessors.Switch(const Name: string);
var
  aProcessor: TSynProcessor;
begin
  aProcessor := Find(Name);
  if Name = '' then
    aProcessor := Find(DefaultProcessor); //default, the first processor
  if aProcessor = nil then
    aProcessor := Find(PlainProcessor); //unkown, the last processor
  if aProcessor = nil then
    raise Exception.Create('Fail to switch to processor');
  SetCurrent(aProcessor);
end;

procedure TSynProcessors.Switch(Index: Integer);
begin
  SetCurrent(Items[Index]);
end;

procedure TSynHTMLPHPSyn.InitIdent;
var
  i: Integer;
begin
  for i := 0 to Processors.Count - 1 do
    Processors[i].InitIdent;
end;

end.

