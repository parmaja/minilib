unit SynHighlighterFirebird;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics,
  SynEditTypes, SynEditHighlighter, SynHighlighterHashEntries;

type
  TtkTokenKind = (tkComment, tkDatatype, tkObject, tkException,
    tkFunction, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown, tkVariable);

  TRangeState = (rsUnknown, rsComment, rsString);

  TProcTableProc = procedure of object;

type
  PIdentifierTable = ^TIdentifierTable;
  TIdentifierTable = array[Char] of ByteBool;

  PHashTable = ^THashTable;
  THashTable = array[Char] of Integer;

type

  TSynFirebirdSyn = class(TSynCustomHighlighter)
  private
    FRange: TRangeState;
    FLine: PChar;
    FLineNumber: Integer;
    FProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    FStringLen: Integer;
    FToIdent: PChar;
    FTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FKeywords: TSynHashEntryList;
    FCommentAttri: TSynHighlighterAttributes;
    FDataTypeAttri: TSynHighlighterAttributes;
    FObjectAttri: TSynHighlighterAttributes;
    FExceptionAttri: TSynHighlighterAttributes;
    FFunctionAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    procedure AndSymbolProc;
    procedure StringProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SymbolProc;
    procedure SymbolAssignProc;
    procedure VariableProc;
    procedure ObjectProc;
    procedure UnknownProc;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AnsiCProc;
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
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
    property DataTypeAttri: TSynHighlighterAttributes read FDataTypeAttri write FDataTypeAttri;
    property ObjectAttri: TSynHighlighterAttributes read FObjectAttri write FObjectAttri;
    property ExceptionAttri: TSynHighlighterAttributes read FExceptionAttri write FExceptionAttri;
    property FunctionAttri: TSynHighlighterAttributes read FFunctionAttri write FFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read FVariableAttri write FVariableAttri;
  end;


const
//---Firebird 6----------------------------------------------------------------

  // functions
  FirebirdFunctions =
    'AVG,CAST,COUNT,GEN_ID,MAX,MIN,SUM,UPPER'+
    'ABS,ACOS,ASCII_CHAR,ASCII_VAL,ASIN,ATAN,ATAN2,'+
    'BIN_AND,BIN_OR,BIN_SHL,BIN_SHR,BIN_XOR,'+
    'CEIL,COS,COSH,COT,DATEADD,DATEDIFF,DECODE,'+
    'EXP,FLOOR,GEN_UUID,HASH,LN,LOG,LOG10,LPAD,'+
    'MAXVALUE,MINVALUE,MOD,OVERLAY,PI,POWER,'+
    'RAND,REPLACE,REVERSE,ROUND,RPAD,'+
    'SIGN,SIN,SINH,SQRT,TAN,TANH,TRUNC,'+
    'UUID_TO_CHAR,CHAR_TO_UUID';

  // keywords
  FirebirdKeywords: string =
    'ACTIVE,ADD,AFTER,ALL,ALTER,AND,ANY,AS,ASC,ASCENDING,AT,AUTO,AUTONOMOUS,AUTODDL,'+
    'BASED,BASENAME,BASE_NAME,BEFORE,BEGIN,BETWEEN,BLOBEDIT,BLOCK,BREAK,BUFFER,BY,' +
    'CACHE,CASE,CHARACTER_LENGTH,CHAR_LENGTH,CHECK,' +
    'CHECK_POINT_LEN,CHECK_POINT_LENGTH,COALESCE,COLLATE,COLLATION,COLUMN,COMMIT,' +
    'COMMITED,COMPILETIME,COMPUTED,CLOSE,CONDITIONAL,CONNECT,CONSTRAINT,' +
    'CONTAINING,CONTINUE,CREATE,CURRENT,CURRENT_DATE,CURRENT_TIME,' +
    'CURRENT_CONNECTION,CURRENT_TIMESTAMP,CURRENT_TRANSACTION,CURSOR,' +
    'DATABASE,DAY,DB_KEY,DEBUG,DEC,DECLARE,DEFAULT,DELETE,DELETING,DESC,DESCENDING,' +
    'DESCRIBE,DESCRIPTOR,DISCONNECT,DISTINCT,DO,DOMAIN,DROP,ECHO,EDIT,ELSE,END,' +
    'ENTRY_POINT,ESCAPE,EVENT,EXCEPTION,EXECUTE,EXISTS,EXIT,EXTERN,EXTERNAL,EXTRACT,' +
    'FETCH,FILE,FILTER,FIRST,FOR,FOREIGN,FOUND,FREE_IT,FROM,FULL,FUNCTION,' +
    'GDSCODE,GENERATOR,GLOBAL,GOTO,GRANT,GROUP,GROUP_COMMIT_WAIT,GROUP_COMMIT_WAIT_TIME,' +
    'HAVING,HELP,HOUR,'+
    'IF,IIF,IMMEDIATE,IN,INACTIVE,INDEX,INDICATOR,INIT,INNER,INPUT,INPUT_TYPE,INSENSITIVE,' +
    'INSERT,INSERTING,INT,INTO,IS,ISOLATION,ISQL,'+
    'JOIN,'+
    'KEY,LAST,LIST,'+
    'LC_MESSAGES,LC_TYPE,LEAVE,LEFT,LENGTH,LEV,LEVEL,LIKE,' +
    'LOGFILE,LOG_BUFFER_SIZE,LOG_BUF_SIZE,LONG,LOCK,MANUAL,' +
    'MATCHING,MAXIMUM,MAXIMUM_SEGMENT,MAX_SEGMENT,MERGE,MESSAGE,MINIMUM,MINUTE,MODULE_NAME,MONTH,' +
    'NAMES,NATIONAL,NATURAL,NCHAR,NEW,NEXT,NO,NOAUTO,NOT,NULL,NULLIF' +
    'NUM_LOG_BUFFS,NUM_LOG_BUFFERS,'+
    'OCTET_LENGTH,OF,OLD,ON,ONLY,OPEN,OPTION,OR,' +
    'ORDER,OUTER,OUTPUT,OUTPUT_TYPE,OVERFLOW,'+
    'PAGE,PAGELENGTH,PAGES,PAGE_SIZE,' +
    'PARAMETER,PASSWORD,PLAN,POSITION,POST_EVENT,PREPARE,PROCEDURE,' +
    'PROTECTED,PRIMARY,PRIVILEGES,PUBLIC,QUIT,RAW_PARTITIONS,READ,REAL,' +
    'RECORD_VERSION,RECREATE,REFERENCES,RELEASE,RESERV,RESERVING,RETAIN,RETURN,' +
    'RETURNING,RETURNING_VALUES,RETURNS,RESTART,REVOKE,RIGHT,ROLLBACK,ROW_COUNT,ROWS,RUNTIME,'+
    'SAVEPOINT,SCHEMA,SECOND,' +
    'SEGMENT,SELECT,SET,SEQUENCE,SHADOW,SHARED,SHELL,SHOW,SIMILAR,SINGULAR,SIZE,SNAPSHOT,SOME,' +
    'SORT,SKIP,SQL,SQLCODE,SQLERROR,SQLWARNING,STABILITY,STARTING,STARTS,' +
    'SENSITIVE,STATEMENT,STATIC,STATISTICS,SUB_TYPE,SUSPEND,SUBSTRING,'+
    'TABLE,TERMINATOR,THEN,TO,TRANSACTION,TRANSLATE,TRANSLATION,TRIGGER,TRIM,TYPE,' +
    'UNCOMMITTED,UNION,UNIQUE,UNICODE,UPDATE,UPDATING,USER,USING,UTF8,'+
    'VALUE,VALUES,VARIABLE,VARYING,VERSION,VIEW,' +
    'WAIT,WEEKDAY,WHEN,WHENEVER,WHERE,WHILE,WITH,WORK,WRITE,'+
    'YEAR,YEARDAY';

  // types
  FirebirdTypes = 'BIGINT,BLOB,CHAR,CHARACTER,DATE,DECIMAL,DOUBLE,FLOAT,INT64,INTEGER,' +
    'NUMERIC,PRECISION,SMALLINT,TIME,TIMESTAMP,VARCHAR';

  ISQLKeywords = 'TERM';

implementation

uses
  SynEditStrConst;

const
  SYNS_AttrObjects = 'Objects';

var
  Identifiers: TIdentifierTable;
  mHashTable: THashTable;

procedure MakeIdentTable;
var
  c: char;
begin
  FillChar(Identifiers, SizeOf(Identifiers), 0);
  for c := 'a' to 'z' do
    Identifiers[c] := True;
  for c := 'A' to 'Z' do
    Identifiers[c] := True;
  for c := '0' to '9' do
    Identifiers[c] := True;
  Identifiers['_'] := True;
  Identifiers[':'] := True;
  Identifiers['"'] := True;

  FillChar(mHashTable, SizeOf(mHashTable), 0);
  mHashTable['_'] := 1;
  for c := 'a' to 'z' do
    mHashTable[c] := 2 + Ord(c) - Ord('a');
  for c := 'A' to 'Z' do
    mHashTable[c] := 2 + Ord(c) - Ord('A');
  mHashTable[':'] := mHashTable['Z'] + 1;
  mHashTable['"'] := mHashTable['Z'] + 1;
end;

function TSynFirebirdSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while Identifiers[ToHash^] do
  begin
{$IFOPT Q-}
    Result := 2 * Result + mHashTable[ToHash^];
{$ELSE}
    Result := (2 * Result + mHashTable[ToHash^]) and $FFFFFF;
{$ENDIF}
    inc(ToHash);
  end;
  Result := Result and $FF; // 255
  FStringLen := ToHash - FToIdent;
end;

function TSynFirebirdSyn.KeyComp(const aKey: string): Boolean;
var
  i: integer;
  pKey1, pKey2: PChar;
begin
  pKey1 := FToIdent;
  // Note: FStringLen is always > 0 !
  pKey2 := pointer(aKey);
  for i := 1 to FStringLen do
  begin
    if mHashTable[pKey1^] <> mHashTable[pKey2^] then
    begin
      Result := FALSE;
      exit;
    end;
    Inc(pKey1);
    Inc(pKey2);
  end;
  Result := True;
end;

function TSynFirebirdSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  FToIdent := MayBe;
  Entry := FKeywords[KeyHash(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      break
    else if Entry.KeywordLen = FStringLen then
      if KeyComp(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

procedure TSynFirebirdSyn.MakeMethodTables;
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
      '-': FProcTable[I] := @MinusProc;
      '|': FProcTable[I] := @OrSymbolProc;
      '+': FProcTable[I] := @PlusProc;
      '/': FProcTable[I] := @SlashProc;
      '&': FProcTable[I] := @AndSymbolProc;
      #39: FProcTable[I] := @StringProc;
      '"': FProcTable[I] := @ObjectProc;
      ':': FProcTable[I] := @VariableProc;
      'A'..'Z', 'a'..'z', '_':
        FProcTable[I] := @IdentProc;
      '0'..'9':
        FProcTable[I] := @NumberProc;
      #1..#9, #11, #12, #14..#32:
        FProcTable[I] := @SpaceProc;
      '^', '%', '*', '!':
        FProcTable[I] := @SymbolAssignProc;
      '{', '}', '.', ',', ';', '?', '(', ')', '[', ']', '~':
        FProcTable[I] := @SymbolProc;
    else
      FProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynFirebirdSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeywords := TSynHashEntryList.Create;
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  FCommentAttri.Foreground := clMaroon;
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FDataTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType);
  FDataTypeAttri.Style := [fsBold];
  FDataTypeAttri.Foreground := $00C56A31;
  AddAttribute(FDataTypeAttri);
  FObjectAttri := TSynHighlighterAttributes.Create(SYNS_AttrObjects);
  FObjectAttri.Style := [fsBold];
  FObjectAttri.Foreground := clGreen;
  AddAttribute(FObjectAttri);
  FFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction);
  FFunctionAttri.Style := [fsBold];
  FFunctionAttri.Foreground := $00C56A31;
  AddAttribute(FFunctionAttri);
  FExceptionAttri := TSynHighlighterAttributes.Create(SYNS_AttrException);
  FExceptionAttri.Style := [fsItalic];
  AddAttribute(FExceptionAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  FIdentifierAttri.Foreground := clBlack;
  AddAttribute(FIdentifierAttri);
  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  FKeyAttri.Style := [fsBold];
  FKeyAttri.Foreground := $00C56A31;
  AddAttribute(FKeyAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(FSymbolAttri);
  FVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable);
  AddAttribute(FVariableAttri);
  SetAttributesOnChange(@DefHighlightChange);
  EnumerateKeywords(Ord(tkDatatype), FirebirdTypes, IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), FirebirdFunctions, IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), FirebirdKeywords, IdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), ISQLKeywords, IdentChars, @DoAddKeyword);
  MakeMethodTables;
  FDefaultFilter := SYNS_FilterSQL;
  FRange := rsUnknown;
end;

destructor TSynFirebirdSyn.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynFirebirdSyn.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TSynFirebirdSyn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  inherited;
  FLine := PChar(NewValue);
  Run := 0;
  FLineNumber := LineNumber;
  Next;
end;

procedure TSynFirebirdSyn.AndSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '&'] then
    Inc(Run);
end;

procedure TSynFirebirdSyn.StringProc;
begin
  if FLine[Run] = #0 then
    NullProc
  else
  begin
    FTokenID := tkString;
    if (Run > 0) or (FRange <> rsString) or (FLine[Run] <> #39) then
    begin
      FRange := rsString;
      repeat
        Inc(Run);
      until FLine[Run] in [#0, #10, #13, #39];
    end;
    if FLine[Run] = #39 then
    begin
      Inc(Run);
      FRange := rsUnknown;
    end;
  end;
end;

procedure TSynFirebirdSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynFirebirdSyn.EqualProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynFirebirdSyn.GreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynFirebirdSyn.IdentProc;
begin
  FTokenID := IdentKind((FLine + Run));
  inc(Run, FStringLen);
  if FTokenID = tkComment then
  begin
    while not (FLine[Run] in [#0, #10, #13]) do
      Inc(Run);
  end
  else
    while Identifiers[FLine[Run]] do
      inc(Run);
end;

procedure TSynFirebirdSyn.LFProc;
begin
  FTokenID := tkSpace;
  inc(Run);
end;

procedure TSynFirebirdSyn.LowerProc;
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

procedure TSynFirebirdSyn.MinusProc;
begin
  Inc(Run);
  if FLine[Run] = '-' then
  begin
    FTokenID := tkComment;
    repeat
      Inc(Run);
    until FLine[Run] in [#0, #10, #13];
  end
  else
    FTokenID := tkSymbol;
end;

procedure TSynFirebirdSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynFirebirdSyn.NumberProc;
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

procedure TSynFirebirdSyn.OrSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '|'] then
    Inc(Run);
end;

procedure TSynFirebirdSyn.PlusProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '+'] then
    Inc(Run);
end;

procedure TSynFirebirdSyn.SlashProc;
begin
  Inc(Run);
  case FLine[Run] of
    '*':
      begin
        FRange := rsComment;
        FTokenID := tkComment;
        repeat
          Inc(Run);
          if (FLine[Run] = '*') and (FLine[Run + 1] = '/') then
          begin
            FRange := rsUnknown;
            Inc(Run, 2);
            break;
          end;
        until FLine[Run] in [#0, #10, #13];
      end;
    '=':
      begin
        Inc(Run);
        FTokenID := tkSymbol;
      end;
  else
    FTokenID := tkSymbol;
  end;
end;

procedure TSynFirebirdSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until (FLine[Run] > #32) or (FLine[Run] in [#0, #10, #13]);
end;

procedure TSynFirebirdSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynFirebirdSyn.SymbolAssignProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then
    Inc(Run);
end;

procedure TSynFirebirdSyn.VariableProc;
var
  i: integer;
begin
  if (FLine[Run] = ':') then
  begin
    FTokenID := tkVariable;
    i := Run;
    repeat
      Inc(i);
    until not (Identifiers[FLine[i]]);
    Run := i;
  end;
end;

procedure TSynFirebirdSyn.UnknownProc;
begin
  inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynFirebirdSyn.AnsiCProc;
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

function TSynFirebirdSyn.IsKeyword(const AKeyword: string): boolean;
var
  tk: TtkTokenKind;
begin
  tk := IdentKind(PChar(AKeyword));
  Result := tk in [tkDatatype, tkException, tkFunction, tkKey, tkObject];
end;

procedure TSynFirebirdSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsComment:
      AnsiCProc;
    rsString:
      StringProc;
  else
    FProcTable[FLine[Run]];
  end;
end;

function TSynFirebirdSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynFirebirdSyn.GetEOL: Boolean;
begin
  Result := FTokenID = tkNull;
end;

function TSynFirebirdSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynFirebirdSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - FTokenPos;
  Setstring(Result, (FLine + FTokenPos), Len);
end;

procedure TSynFirebirdSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynFirebirdSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynFirebirdSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkDatatype: Result := FDataTypeAttri;
    tkObject: Result := FObjectAttri;
    tkException: Result := FExceptionAttri;
    tkFunction: Result := FFunctionAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkVariable: Result := FVariableAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynFirebirdSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynFirebirdSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

procedure TSynFirebirdSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynFirebirdSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynFirebirdSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TSynFirebirdSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSQL;
end;

procedure TSynFirebirdSyn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  FKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TSynFirebirdSyn.GetSampleSource: string;
begin
  Result := '';
end;

procedure TSynFirebirdSyn.ObjectProc;
begin
  FTokenID := tkObject;
  Inc(Run);
  while not (FLine[Run] in [#0, #10, #13]) do
  begin
    if FLine[Run] = '"' then
    begin
      Inc(Run);
      break;
    end;
    Inc(Run);
  end;
end;

initialization
  MakeIdentTable;
  RegisterPlaceableHighlighter(TSynFirebirdSyn);
end.

