unit SynHighlighterSARD;
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
  SysUtils, Classes, Graphics,
  SynEditTypes, SynEditHighlighter, SynHighlighterHashEntries, SynUtils;

type
  TtkTokenKind = (tkNull, {TODO: tkPreprocessor, tkEmbedComment}tkComment, tkObject, tkIdentifier, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsEmbedComment, rsComment, rsPreprocessor, rsSQString, rsDQString);

  { TSynSARDSyn }

  TSynSARDSyn = class(TSynCustomHighlighter)
  private
    Run: LongInt;
    FProcTable: array[#0..#255] of TProcTableProc;
    FRange: TRangeState;
    FLine: PChar;
    FStringLen: Integer;
    FTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FObjectAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    procedure StringProc(QuoteString: Char);
    procedure SQStringProc;
    procedure DQStringProc;
    procedure CommentProc;
    procedure SLCommentProc;
    procedure EmbedCommentProc;
    procedure PreprocessorProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure SlashProc;
    procedure BlockOpenProc;
    procedure SpaceProc;
    procedure SymbolProc;
    procedure SymbolAssignProc;
    procedure MakeProcTables;
    function IsIdentifiers(C: Char): Boolean;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(const NewValue: string; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property ObjectAttri: TSynHighlighterAttributes read FObjectAttri write FObjectAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
  end;

const

  // Objects
  SARDObjects =
    'string,str,integer,int,float,color,datetime'+
    'avg,cast,count,max,min,sum,upper'+
    'abs,'+
    'cos,cot,'+
    'exp,floor,hash,ln,log,lpad,'+
    'mod,pi,power,'+
    'rand,replace,reverse,round,rpad,'+
    'sign,sin,sinh,sqrt,tan,tanh,trunc';

  // types
  //SARDTypes = 'integer,float,string,boolean,color,datetime';

type

  { TSARDSyn }

  TSARDSyn = class(TSynPersistent)
   private
   protected
     function GetDefaultKind: Integer; override;
   public
     function IdentKind(MayBe: PChar; out L: Integer): TtkTokenKind; overload;
     function IdentKind(MayBe: PChar): TtkTokenKind; overload;
     constructor Create; override;
   end;

function SARDSyn: TSARDSyn;

implementation

uses
  SynEditStrConst;

var
  FSARDSyn: TSARDSyn = nil;

function SARDSyn: TSARDSyn;
begin
  if FSARDSyn = nil then
    FSARDSyn := TSARDSyn.Create;
  Result := FSARDSyn;
end;

{ TSARDSyn }

function TSARDSyn.GetDefaultKind: Integer;
begin
  Result := ord(tkIdentifier);
end;

function TSARDSyn.IdentKind(MayBe: PChar; out L: Integer): TtkTokenKind;
begin
  Result := TtkTokenKind(GetIdentKind(MayBe, L));
end;

function TSARDSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  L: Integer;
begin
  Result := TtkTokenKind(GetIdentKind(MayBe, L));
end;

constructor TSARDSyn.Create;
begin
  inherited;
  EnumerateKeywords(Ord(tkObject), SARDObjects, GetIdentChars, @DoAddKeyword);
end;

procedure TSynSARDSyn.MakeProcTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: FProcTable[I] := @NullProc;
      #10: FProcTable[I] := @LFProc;
      #13: FProcTable[I] := @CRProc;
      '=': FProcTable[I] := @EqualProc;
      '>': FProcTable[I] := @GreaterProc;
      '<': FProcTable[I] := @LowerProc;
      '/': FProcTable[I] := @SlashProc;
      '{': FProcTable[I] := @BlockOpenProc;
      '|': FProcTable[I] := @OrSymbolProc;
      '+': FProcTable[I] := @PlusProc;
      '''': FProcTable[I] := @SQStringProc;
      '"': FProcTable[I] := @DQStringProc;
      '0'..'9':
        FProcTable[I] := @NumberProc;
      #1..#9, #11, #12, #14..#32:
        FProcTable[I] := @SpaceProc;
      '^', '%', '*', '!':
        FProcTable[I] := @SymbolAssignProc;
      '}', ':', '.', ',', ';', '?', '(', ')', '[', ']', '~', '&':
        FProcTable[I] := @SymbolProc;
    else
      FProcTable[I] := @IdentProc;
    end;
end;

constructor TSynSARDSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  FCommentAttri.Foreground := clMaroon;
  FCommentAttri.Style := [];
  AddAttribute(FCommentAttri);
  FObjectAttri := TSynHighlighterAttributes.Create('Object');
  FObjectAttri.Style := [fsBold];
  FObjectAttri.Foreground := $00C56A31;
  AddAttribute(FObjectAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  FIdentifierAttri.Foreground := clBlack;
  AddAttribute(FIdentifierAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(@DefHighlightChange);
  FDefaultFilter := 'SARD Files (*.sard)|*.sard';
  FRange := rsUnknown;
  MakeProcTables;
end;

destructor TSynSARDSyn.Destroy;
begin
  inherited;
end;

procedure TSynSARDSyn.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TSynSARDSyn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  inherited;
  FLine := PChar(NewValue);
  Run := 0;
  Next;
end;

procedure TSynSARDSyn.StringProc(QuoteString: Char);
begin
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkString;
      repeat
        if (FLine[Run] = QuoteString) then
        begin
          FRange := rsUnknown;
          Inc(Run);
          break;
        end;
        Inc(Run);
      until FLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TSynSARDSyn.SQStringProc;
begin
  FRange := rsSQString;
  Inc(Run);
  StringProc('''');
end;

procedure TSynSARDSyn.DQStringProc;
begin
  FRange := rsDQString;
  Inc(Run);
  StringProc('"');
end;

procedure TSynSARDSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynSARDSyn.EqualProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynSARDSyn.GreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynSARDSyn.IdentProc;
begin
  FTokenID := SARDSyn.IdentKind((FLine + Run));
  inc(Run, FStringLen);
  while not (FLine[Run] in [#0, #10, #13]) and IsIdentifiers(FLine[Run]) do
    Inc(Run);
end;

procedure TSynSARDSyn.LFProc;
begin
  FTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSARDSyn.LowerProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  case FLine[Run] of
    '=': Inc(Run);
    '<':
      begin
        Inc(Run);
        if FLine[Run] = '=' then
          Inc(Run);
      end;
  end;
end;

procedure TSynSARDSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynSARDSyn.NumberProc;
begin
  inc(Run);
  FTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', '-'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then
          break;
    end;
    inc(Run);
  end;
end;

procedure TSynSARDSyn.OrSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '|'] then
    Inc(Run);
end;

procedure TSynSARDSyn.PlusProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '+'] then
    Inc(Run);
end;

procedure TSynSARDSyn.SlashProc;
begin
  Inc(Run);
  case FLine[Run] of
    '/':
      begin
        SLCommentProc;
      end;
    '*':
      begin
        FRange := rsComment;
        FTokenID := tkComment;
        CommentProc;
      end;
  else
    FTokenID := tkSymbol;
  end;
end;

procedure TSynSARDSyn.BlockOpenProc;
begin
  Inc(Run);
  case FLine[Run] of
    '?':
      begin
        FRange := rsPreprocessor;
        FTokenID := tkComment;
        Inc(Run);
        PreprocessorProc;
      end;
    '*':
      begin
        FRange := rsEmbedComment;
        FTokenID := tkComment;
        Inc(Run);
        EmbedCommentProc;
      end;
  else
    FTokenID := tkSymbol;
  end;
end;

procedure TSynSARDSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until (FLine[Run] > #32) or (FLine[Run] in [#0, #10, #13]);
end;

procedure TSynSARDSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynSARDSyn.SymbolAssignProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then
    Inc(Run);
end;

function TSynSARDSyn.IsIdentifiers(C: Char): Boolean;
begin
  Result := not (C in [' ', #13, #10, #0, '{', '}', ':', ',', ';', '?', '(', ')', '[', ']',
          '~', '^', '%', '*', '!', '=', '>', '<', '-', '|', '+', '/', '&',  '''', '"']);
end;

procedure TSynSARDSyn.CommentProc;
begin
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if (FLine[Run] = '*') and (FLine[Run + 1] = '/') then
        begin
          FRange := rsUnknown;
          Inc(Run, 2);
          break;
        end;
        Inc(Run);
      until FLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TSynSARDSyn.SLCommentProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until FLine[Run] in [#0, #10, #13];
end;

procedure TSynSARDSyn.EmbedCommentProc;
begin
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if (FLine[Run] = '*') and (FLine[Run + 1] = '}') then
        begin
          FRange := rsUnknown;
          Inc(Run, 2);
          break;
        end;
        Inc(Run);
      until FLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TSynSARDSyn.PreprocessorProc;
begin
  case FLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      FTokenID := tkComment;
      repeat
        if (FLine[Run] = '?') and (FLine[Run + 1] = '}') then
        begin
          FRange := rsUnknown;
          Inc(Run, 2);
          break;
        end;
        Inc(Run);
      until FLine[Run] in [#0, #10, #13];
    end;
  end;
end;

function TSynSARDSyn.IsKeyword(const AKeyword: string): boolean;
var
  tk: TtkTokenKind;
begin
  tk := SARDSyn.IdentKind(PChar(AKeyword));
  Result := tk in [tkObject];
end;

procedure TSynSARDSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsPreprocessor:
      PreprocessorProc;
    rsComment:
      CommentProc;
    rsEmbedComment:
      EmbedCommentProc;
    rsSQString:
      StringProc('''');
    rsDQString:
      StringProc('"');
  else
    FProcTable[FLine[Run]];
  end;
end;

function TSynSARDSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_KEYWORD: Result := FObjectAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSARDSyn.GetEol: Boolean;
begin
  Result := FTokenID = tkNull;
end;

function TSynSARDSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynSARDSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - FTokenPos;
  Setstring(Result, (FLine + FTokenPos), Len);
end;

procedure TSynSARDSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: Integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynSARDSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynSARDSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkObject: Result := FObjectAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynSARDSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynSARDSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

procedure TSynSARDSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynSARDSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynSARDSyn.GetIdentChars: TSynIdentChars;
begin
  Result := SARDSyn.GetIdentChars
end;

class function TSynSARDSyn.GetLanguageName: string;
begin
  Result := 'SARD script';
end;

function TSynSARDSyn.GetSampleSource: string;
begin
  Result := '';
end;

initialization
  RegisterPlaceableHighlighter(TSynSARDSyn);
end.

