unit mnSynHighlighterMultiProc;
{$mode objfpc}{$H+}
{**
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
  Classes, Contnrs, SysUtils, Controls, Graphics,
  SynEdit, SynEditTypes, SynEditHighlighter;

type
  TtkTokenKind = (tkUnknown, tkNull, tkSpace, tkComment, tkDocument, tkIdentifier, tkKeyword, tkType, tkFunction, tkSymbol, tkNumber,
    tkString, tkText, tkVariable, tkProcessor);

  //Common range used for some syntax
  TCommonRangeState = (rscUnknown, rscComment, rscSpecialComment, rscDocument, rscStringSQ, rscStringDQ, rscStringBQ {BackQuote}, rscStringSpecial);
  TCommonRangeStates = set of TCommonRangeState;

  TProcTableProc = procedure of object;
  TProcTable = array[AnsiChar] of TProcTableProc;

  PIdentifierTable = ^TIdentifierTable;
  TIdentifierTable = array[AnsiChar] of bytebool;

  TSynMultiProcSyn = class;

  { TTokenObject }

  TTokenObject = class(TObject)
    Keyword: string;
    Kind: TtkTokenKind;
  public
    constructor Create(AKeyword: string; AKind: TtkTokenKind);
  end;

  { TSynProcessor }

  TSynProcessor = class(TObject)
  private
    FKeywords: TFPObjectHashTable;
    FName: string;
    FIndex: integer;
    FParent: TSynMultiProcSyn;
  protected
    StringCEscaped: TCommonRangeStates;
    StringMultiLine: TCommonRangeStates;
    function GetIdentChars: TSynIdentChars; virtual;
    procedure ResetRange; virtual;
    function GetRange: Byte; virtual;
    procedure SetRange(Value: Byte); virtual;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; virtual;

    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    function ScanIdent(const Identifier: string): TtkTokenKind; //this will move Line.Run and bring Identifire kind
    function ScanMatch(const MatchWith: string): Boolean; //this will move Line.Run if matched
    procedure Created; virtual;
  public
    IdentTable: TIdentifierTable;
    ProcTable: TProcTable;
    constructor Create(AParent: TSynMultiProcSyn; AName: string); virtual;
    destructor Destroy; override;
    procedure Next; virtual;
    procedure SetLine(const NewValue: string; LineNumber: integer); virtual;
    procedure Prepare; virtual;
    procedure MakeProcTable; virtual;

    procedure IdentProc; virtual;

    property Parent: TSynMultiProcSyn read FParent;
    property Name: string read FName write FName;
    property Index: integer read FIndex;
  end;

  { TCommonSynProcessor }

  TCommonSynProcessor = class(TSynProcessor)
  private
    FRange: TCommonRangeState;
  protected
    //LastRange: Bad Idea but let us try
    LastRange: TCommonRangeState;
    CloseComment: string;
    CloseSpecialComment: string;
  public
    procedure Created; override;
    procedure ResetRange; override;
    function GetRange: Byte; override;
    procedure SetRange(Value: Byte); override;
    procedure SetRange(Value: TCommonRangeState); overload;

    property Range: TCommonRangeState read FRange;
    procedure SetLine(const NewValue: string; LineNumber: integer); override;

    //Common procs
    procedure InternalCommentProc; //   /* */
    procedure InternalSpecialCommentProc; //    /+ +/

    procedure WordProc; //Identifire started with char like #define
    procedure SLCommentProc; //Single Line Comment //comment or #comment depend on who started
    procedure CommentProc; //Mutli line comment
    procedure SpecialCommentProc;
    procedure DocumentProc;

    procedure StringProc;
    procedure StringSQProc;
    procedure StringDQProc;
    procedure StringBQProc;

    procedure UnknownProc;
    procedure NullProc;
    procedure CRProc;
    procedure LFProc;
    procedure SpaceProc;

    procedure SymbolProc;
    procedure ControlProc;
    procedure NumberProc;

    procedure MakeProcTable; override;
  end;

  TSynProcessors = class(TObjectList)
  private
    FCurrent: TSynProcessor;
    FMainProcessor: string;
    FDefaultProcessor: string;
    function GetItem(Index: integer): TSynProcessor;
    procedure SetItem(Index: integer; const Value: TSynProcessor);
    function GetMain: TSynProcessor;
    procedure SetCurrent(Value: TSynProcessor);
  public
    function Add(AProcessor: TSynProcessor): integer;
    function Find(const Name: string): TSynProcessor;
    function IndexOf(const Name: string): integer;
    procedure Switch(const Name: string); overload;
    procedure Switch(Index: integer); overload;
    property Current: TSynProcessor read FCurrent;
    property Main: TSynProcessor read GetMain;
    property MainProcessor: string read FMainProcessor write FMainProcessor;
    property DefaultProcessor: string read FDefaultProcessor write FDefaultProcessor;
    property Items[Index: integer]: TSynProcessor read GetItem write SetItem; default;
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

  { TSynMultiProcSyn }

  TSynMultiProcSyn = class(TSynCustomHighlighter)
  private
    FCommentAttri: TSynHighlighterAttributes;
    FDocumentAttri: TSynHighlighterAttributes;
    FTypeAttri: TSynHighlighterAttributes;
    FFunctionAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FTextAttri: TSynHighlighterAttributes;
    FKeywordAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FWhitespaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    FProcessorAttri: TSynHighlighterAttributes;

    procedure Prepare;
  protected
    FProcessors: TSynProcessors;
    procedure InitProcessors; virtual;
    function GetIdentChars: TSynIdentChars; override; final;
    function GetSampleSource: string; override;
  public
    class function GetLanguageName: string; override;
    property Processors: TSynProcessors read FProcessors;
  public
    Run: longint;
    FLineNumber: integer;
    FTokenPos: integer;
    FLine: PChar;
    FTokenID: TtkTokenKind;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEOL: boolean; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    function GetEndOfLineAttribute: TSynHighlighterAttributes; override;
    procedure ResetRange; override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;
    procedure Next; override;
    procedure SetLine(const NewValue: string; LineNumber: integer); override;
  published
    property WhitespaceAttri: TSynHighlighterAttributes read FWhitespaceAttri write FWhitespaceAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property DocumentAttri: TSynHighlighterAttributes read FDocumentAttri write FDocumentAttri;
    property KeywordAttri: TSynHighlighterAttributes read FKeywordAttri write FKeywordAttri;
    property TypeAttri: TSynHighlighterAttributes read FTypeAttri write FTypeAttri;
    property FunctionAttri: TSynHighlighterAttributes read FFunctionAttri write FFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property TextAttri: TSynHighlighterAttributes read FTextAttri write FTextAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri write FVariableAttri;
    property ProcessorAttri: TSynHighlighterAttributes read fProcessorAttri write fProcessorAttri;
  end;

const
  SYNS_LangMultiProc = 'MultiProc';
  //SYNS_FilterMultiProc = 'HTML/PHP Files (*.php;*.html;*.phtml;*.inc)|*.php;*.html;*.phtml;*.inc';

//range mix Main processor as byte and Current processor as byte and index Byte
function RangeToProcessor(Range: Pointer): Byte;
function MixRange(Index, Main, Current: Byte): Pointer;
procedure SplitRange(Range: Pointer; out Index, Main, Current: Byte);

implementation

uses
  SynEditStrConst;

function RangeToProcessor(Range: Pointer): Byte;
begin
  Result := PtrUInt(Range) and $FF;
end;

function MixRange(Index, Main, Current: byte): Pointer;
begin
  {$PUSH}{$HINTS OFF}
  Result := Pointer(PtrUInt(Index or Main shl 8 or Current shl 16));
  {$POP}
end;

procedure SplitRange(Range: Pointer; out Index, Main, Current: byte);
var
  r: PtrUInt;
begin
  {$PUSH}{$HINTS OFF}
  r := Integer(Range);
  {$POP}
  Index := r and $FF;
  Main := r shr 8 and $FF;
  Current := r shr 16 and $FF;
end;

{ TTokenObject }

constructor TTokenObject.Create(AKeyword: string; AKind: TtkTokenKind);
begin
  inherited Create;
  Keyword := AKeyword;
  Kind := AKind;
end;

function TSynProcessor.ScanIdent(const Identifier: string): TtkTokenKind;
var
  Entry: TTokenObject;
  IdentChars: TSynIdentChars;
  AKeyword: string;
  i: Integer;
begin
  IdentChars := GetIdentChars;
  i := 1;
  while Identifier[i] in IdentChars do
    Inc(i);
  AKeyword := Copy(Identifier, 1, i - 1);
  inc(Parent.Run, i - 1);
  Entry := FKeywords.Items[AKeyword] as TTokenObject;
  if Entry <> nil then
    Result := Entry.Kind
  else
    Result := tkIdentifier;
end;

function TSynProcessor.ScanMatch(const MatchWith: string): Boolean;
var
  i, c: integer;
begin
  Result := False;
  i := Parent.Run;
  c := 1;
  repeat
    if (Parent.FLine[i] <> MatchWith[c]) then
        break;
    if c = Length(MatchWith) then
    begin
      Result := True;
      break;
    end;
    Inc(c);
    Inc(i);
  until (Parent.FLine[i] in [#0, #10, #13]);

  if Result then
    Inc(Parent.Run, Length(MatchWith));
end;

procedure TSynProcessor.Created;
begin

end;

function TSynProcessor.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  Result := nil;
end;

procedure TSynProcessor.DoAddKeyword(AKeyword: string; AKind: integer);
begin
  FKeywords.Add(AKeyword, TTokenObject.Create(AKeyword, TtkTokenKind(AKind)));
end;

{ TCommonSynProcessor }

function TCommonSynProcessor.GetRange: Byte;
begin
  Result := Byte(Range);
end;

procedure TCommonSynProcessor.Created;
begin
  inherited Created;
  CloseComment := '*/';
  CloseSpecialComment := '+/';//for D but u can change it
end;

procedure TCommonSynProcessor.ResetRange;
begin
  inherited;
  SetRange(rscUnknown);
  LastRange := rscUnknown;
end;

procedure TCommonSynProcessor.SetRange(Value: Byte);
begin
  SetRange(TCommonRangeState(Value));
end;

procedure TCommonSynProcessor.SetRange(Value: TCommonRangeState);
begin
  if FRange <> Value then
    LastRange := FRange;
  FRange := Value;
end;

procedure TCommonSynProcessor.SetLine(const NewValue: string; LineNumber: integer);
begin
  inherited;
  LastRange := rscUnknown;
end;

procedure TCommonSynProcessor.InternalCommentProc;
begin
  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if ScanMatch(CloseComment) then
    begin
      SetRange(rscUnKnown);
      break;
    end;
    Inc(Parent.Run);
  end;
end;

procedure TCommonSynProcessor.InternalSpecialCommentProc;
begin
  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if ScanMatch(CloseSpecialComment) then
    begin
      SetRange(rscUnknown);
      break;
    end;
    Inc(Parent.Run);
  end;
end;

procedure TCommonSynProcessor.WordProc;
begin
  Inc(Parent.Run);
  repeat
    Inc(Parent.Run);
  until Parent.FLine[Parent.Run] in [#0, #10, #13, ' '];
end;

procedure TCommonSynProcessor.SLCommentProc;
begin
  Parent.FTokenID := tkComment;
  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    Inc(Parent.Run);
  end
end;

procedure TCommonSynProcessor.CommentProc;
begin
  Parent.FTokenID := tkComment;
  SetRange(rscComment);
  InternalCommentProc;
end;

procedure TCommonSynProcessor.SpecialCommentProc;
begin
  Parent.FTokenID := tkComment;
  SetRange(rscSpecialComment);
  InternalSpecialCommentProc;
end;

procedure TCommonSynProcessor.DocumentProc;
begin
  Parent.FTokenID := tkDocument;
  SetRange(rscDocument);
  InternalCommentProc;
end;

procedure TCommonSynProcessor.StringProc;

  function IsEscaped: boolean;
  var
    iFirstSlashPos: integer;
  begin
    if (Range in StringCEscaped) then
    begin
      iFirstSlashPos := Parent.Run - 1;
      while (iFirstSlashPos > 0) and (Parent.FLine[iFirstSlashPos] = '\') do
        Dec(iFirstSlashPos);
      Result := (Parent.Run - iFirstSlashPos + 1) mod 2 <> 0;
    end;
  end;

var
  iCloseChar: char;
begin
  Parent.FTokenID := tkString;
  case Range of
    rscStringSQ: iCloseChar := '''';
    rscStringDQ: iCloseChar := '"';
    rscStringBQ: iCloseChar := '`';
  end;

  while not (Parent.FLine[Parent.Run] in [#0, #10, #13]) do
  begin
    if (Parent.FLine[Parent.Run] = iCloseChar) and (not IsEscaped) then
    begin
      SetRange(rscUnKnown);
      inc(Parent.Run);
      break;
    end;
    Inc(Parent.Run);
  end;
end;

procedure TCommonSynProcessor.StringDQProc;
begin
  SetRange(rscStringDQ);
  Inc(Parent.Run);
  StringProc;
end;

procedure TCommonSynProcessor.StringBQProc;
begin
  SetRange(rscStringBQ);
  Inc(Parent.Run);
  StringProc;
end;

procedure TCommonSynProcessor.UnknownProc;
begin
  inc(Parent.Run);
  Parent.FTokenID := tkUnknown;
end;

procedure TCommonSynProcessor.NullProc;
begin
  Parent.FTokenID := tkNull;
end;

procedure TCommonSynProcessor.CRProc;
begin
  Parent.FTokenID := tkSpace;
  Inc(Parent.Run);
  if Parent.FLine[Parent.Run] = #10 then
    Inc(Parent.Run);
end;

procedure TCommonSynProcessor.LFProc;
begin
  Parent.FTokenID := tkSpace;
  inc(Parent.Run);
end;

procedure TCommonSynProcessor.SpaceProc;
begin
  Parent.FTokenID := tkSpace;
  repeat
    Inc(Parent.Run);
  until (Parent.FLine[Parent.Run] > #32) or (Parent.FLine[Parent.Run] in [#0, #10, #13]);
end;

procedure TCommonSynProcessor.SymbolProc;
begin
  Inc(Parent.Run);
  Parent.FTokenID := tkSymbol;
end;

procedure TCommonSynProcessor.ControlProc;
begin
  Inc(Parent.Run);
  Parent.FTokenID := tkSymbol;
end;

procedure TCommonSynProcessor.NumberProc;
begin
  inc(Parent.Run);
  Parent.FTokenID := tkNumber;
  while Parent.FLine[Parent.Run] in ['0'..'9', '.', 'A'..'Z', 'a'..'z'] do //C format hex
  begin
    case Parent.FLine[Parent.Run] of
      '.':
        if Parent.FLine[Parent.Run + 1] = '.' then
          break;
    end;
    inc(Parent.Run);
  end;
end;

procedure TCommonSynProcessor.MakeProcTable;
var
  c: ansichar;
begin
  inherited;
  ProcTable[#0] := @NullProc;
  ProcTable[#10] := @LFProc;
  ProcTable[#13] := @CRProc;

  for c in [#1..#9, #11, #12, #14..#32] do
    ProcTable[c] := @SpaceProc;

  for c in ['-','=', '|', '+', '&','$','^', '%', '*', '!', '#'] do
    ProcTable[c] := @SymbolProc;

  for c in ['{', '}', '.', ',', ';', '(', ')', '[', ']', '~'] do
    ProcTable[c] := @ControlProc;
end;

procedure TCommonSynProcessor.StringSQProc;
begin
  SetRange(rscStringSQ);
  Inc(Parent.Run);
  StringProc;
end;

procedure TSynMultiProcSyn.InitProcessors;
begin
end;

constructor TSynMultiProcSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FProcessors := TSynProcessors.Create;
  InitProcessors;

  //FProcessors.Add(TPlainProcessor.Create(Self, ''));

  FProcessors.Switch(FProcessors.MainProcessor);

  FWhitespaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrWhitespace);
  FWhitespaceAttri.Foreground := clBlack;
  AddAttribute(FWhitespaceAttri);

  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  FCommentAttri.Foreground := $000069D2;
  AddAttribute(FCommentAttri);

  FDocumentAttri := TSynHighlighterAttributes.Create('Document');
  FDocumentAttri.Foreground := $000069D2;
  FDocumentAttri.Style := [fsBold];
  AddAttribute(FDocumentAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  FIdentifierAttri.Foreground := $00A35949;
  AddAttribute(fIdentifierAttri);

  FFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction);
  FFunctionAttri.Style := [fsBold];
  FFunctionAttri.Foreground := $00926221;
  AddAttribute(FFunctionAttri);

  FTypeAttri := TSynHighlighterAttributes.Create('Type');
  FTypeAttri.Style := [];
  FTypeAttri.Foreground := $00926221;
  AddAttribute(FTypeAttri);

  FTextAttri := TSynHighlighterAttributes.Create('Text');
  AddAttribute(fTextAttri);

  FKeywordAttri := TSynHighlighterAttributes.Create('Keyword');
  FKeywordAttri.Foreground := clGreen;
  AddAttribute(fKeywordAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  FNumberAttri.Foreground := $00006F00;
  FNumberAttri.Style := [fsBold];
  AddAttribute(fNumberAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  StringAttri.Foreground := $002F2FC6;
  AddAttribute(StringAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(FSymbolAttri);

  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable);
  FVariableAttri.Style := [fsBold];
  AddAttribute(fVariableAttri);

  FProcessorAttri := TSynHighlighterAttributes.Create('Processor');
  FProcessorAttri.Style := [fsBold];
  FProcessorAttri.Foreground := $0000006C;
  AddAttribute(FProcessorAttri);

  SetAttributesOnChange(@DefHighlightChange);
  Prepare;
  //FDefaultFilter := SYNS_FilterMultiProc;
end;

destructor TSynMultiProcSyn.Destroy;
begin
  FProcessors.Free;
  inherited;
end;

procedure TSynMultiProcSyn.SetLine(const NewValue: string; LineNumber: integer);
begin
  inherited;
  FLine := PChar(NewValue);
  FLineNumber := LineNumber;
  Run := 0;
  Processors.Current.SetLine(NewValue, LineNumber);
  Next;
end;

function TSynMultiProcSyn.IsKeyword(const AKeyword: string): boolean;
begin
  Result := (Processors.Current.FKeywords.Items[AKeyword] as TTokenObject).Kind in [tkKeyword, tkFunction];
end;

procedure TSynMultiProcSyn.Next;
begin
  Processors.Current.Next;
end;

function TSynMultiProcSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeywordAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FWhitespaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
    else
      Result := nil;
  end;
end;

function TSynMultiProcSyn.GetEOL: boolean;
begin
  Result := FTokenID = tkNull;
end;

function TSynMultiProcSyn.GetRange: Pointer;
begin
  Result := Pointer(MixRange(Processors.Current.Index, Processors.Main.GetRange, Processors.Current.GetRange));
end;

function TSynMultiProcSyn.GetToken: string;
var
  Len: longint;
begin
  Result := '';
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

procedure TSynMultiProcSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynMultiProcSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynMultiProcSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkDocument: Result := FDocumentAttri;
    tkFunction: Result := FFunctionAttri;
    tkType: Result := FTypeAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkText: Result := FTextAttri;
    tkKeyword: Result := FKeywordAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FWhitespaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkVariable: Result := FVariableAttri;
    tkProcessor: Result := FProcessorAttri;
    tkUnknown: Result := FWhitespaceAttri;
    else
      Result := nil;
  end;
end;

function TSynMultiProcSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynMultiProcSyn.GetTokenPos: integer;
begin
  Result := FTokenPos;
end;

procedure TSynMultiProcSyn.ResetRange;
var
  i: integer;
begin
  for i := 0 to Processors.Count - 1 do
    Processors[i].ResetRange;
  Processors.Switch(Processors.MainProcessor);
end;

procedure TSynMultiProcSyn.SetRange(Value: Pointer);
var
  aIndex, aMain, aCurrent: byte;
  i: integer;
begin
  inherited;
  SplitRange(Value, aIndex, aMain, aCurrent);
  Processors.Switch(aIndex);
  Processors.Main.SetRange(aMain);
  if aIndex = 0 then
  begin
    for i := 1 to Processors.Count - 1 do
      Processors[i].ResetRange;
  end
  else
    Processors.Current.SetRange(aCurrent);
end;

function TSynMultiProcSyn.GetEndOfLineAttribute: TSynHighlighterAttributes;
begin
  Result := Processors.Current.GetEndOfLineAttribute;
end;

function TSynMultiProcSyn.GetIdentChars: TSynIdentChars;
begin
  Result := Processors.Current.GetIdentChars;
end;

class function TSynMultiProcSyn.GetLanguageName: string;
begin
  Result := SYNS_LangMultiProc;
end;

function TSynMultiProcSyn.GetSampleSource: string;
begin
  Result := '';
end;

{ TSynProcessor }

procedure TSynProcessor.MakeProcTable;
begin
end;

procedure TSynProcessor.IdentProc;
begin
  Parent.FTokenID := ScanIdent(Parent.FLine + Parent.Run);
end;

procedure TSynProcessor.Next;
begin
end;

procedure TSynProcessor.SetLine(const NewValue: string; LineNumber: integer);
begin
end;

constructor TSynProcessor.Create(AParent: TSynMultiProcSyn; AName: string);
begin
  inherited Create;
  FName := AName;
  FParent := AParent;
  FKeywords := TFPObjectHashTable.Create;
  StringCEscaped := [rscStringSQ, rscStringDQ, rscStringBQ];
  StringMultiLine := [];
  Created;
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

procedure TSynProcessor.Prepare;
var
  c: char;
  chars: TSynIdentChars;
begin
  chars := GetIdentChars;
  for c in chars do
    IdentTable[c] := True;
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
  Inc(Parent.Run);
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

function TSynProcessors.Add(AProcessor: TSynProcessor): integer;
begin
  AProcessor.FIndex := inherited Add(AProcessor);
  Result := AProcessor.Index;
end;

function TSynProcessors.Find(const Name: string): TSynProcessor;
var
  i: integer;
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

function TSynProcessors.GetItem(Index: integer): TSynProcessor;
begin
  Result := inherited Items[Index] as TSynProcessor;
end;

function TSynProcessors.GetMain: TSynProcessor;
begin
  Result := Items[0];
end;

function TSynProcessors.IndexOf(const Name: string): integer;
var
  i: integer;
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

procedure TSynProcessors.SetItem(Index: integer; const Value: TSynProcessor);
begin
  inherited Items[Index] := Value;
end;

procedure TSynProcessors.Switch(const Name: string);
var
  aProcessor: TSynProcessor;
begin
  aProcessor := Find(Name);
  if aProcessor = nil then
    aProcessor := Find(''); //unkown, the last processor //We need it when write strange name <?bla
  if aProcessor = nil then
    raise Exception.Create('Fail to switch to processor');
  SetCurrent(aProcessor);
end;

procedure TSynProcessors.Switch(Index: integer);
begin
  SetCurrent(Items[Index]);
end;

procedure TSynMultiProcSyn.Prepare;
var
  i: integer;
begin
  for i := 0 to Processors.Count - 1 do
    Processors[i].Prepare;

  for i := 0 to Processors.Count - 1 do
    Processors[i].MakeProcTable;
end;

end.
