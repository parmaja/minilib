unit SynHighlighterSqlite;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode delphi}{$H+}

interface

uses
{$ifndef FPC}
  Windows, Messages,
{$endif}
  SysUtils, Graphics, Controls,
  Classes, SynEditTypes, SynEditHighlighter, SynHighlighterHashEntries;

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

  { TSynSqliteSyn }

  TSynSqliteSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fKeywords: TSynHashEntryList;
    fCommentAttri: TSynHighlighterAttributes;
    fDataTypeAttri: TSynHighlighterAttributes;
    fObjectAttri: TSynHighlighterAttributes;
    fExceptionAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
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
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    {.$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    {.$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property DataTypeAttri: TSynHighlighterAttributes read fDataTypeAttri write fDataTypeAttri;
    property ObjectAttri: TSynHighlighterAttributes read fObjectAttri write fObjectAttri;
    property ExceptionAttri: TSynHighlighterAttributes read fExceptionAttri write fExceptionAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri write fFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri write fVariableAttri;
  end;


const
//---Sqlite 6----------------------------------------------------------------

  // functions
  SqliteFunctions =
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
  SqliteKeywords: string =
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
  SqliteTypes = 'BIGINT,BLOB,CHAR,CHARACTER,DATE,DECIMAL,DOUBLE,FLOAT,INT64,INTEGER,' +
    'NUMERIC,PRECISION,SMALLINT,TIME,TIMESTAMP,VARCHAR';

  ISQLKeywords = 'TERM';

implementation

uses
  SynEditStrConst;

resourcestring
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

function TSynSqliteSyn.KeyHash(ToHash: PChar): Integer;
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
  fStringLen := ToHash - fToIdent;
end;

function TSynSqliteSyn.KeyComp(const aKey: string): Boolean;
var
  i: integer;
  pKey1, pKey2: PChar;
begin
  pKey1 := fToIdent;
  // Note: fStringLen is always > 0 !
  pKey2 := pointer(aKey);
  for i := 1 to fStringLen do
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

function TSynSqliteSyn.IdentKind(MayBe: PChar): TtkTokenKind;
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

procedure TSynSqliteSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      '=': fProcTable[I] := EqualProc;
      '>': fProcTable[I] := GreaterProc;
      '<': fProcTable[I] := LowerProc;
      '-': fProcTable[I] := MinusProc;
      '|': fProcTable[I] := OrSymbolProc;
      '+': fProcTable[I] := PlusProc;
      '/': fProcTable[I] := SlashProc;
      '&': fProcTable[I] := AndSymbolProc;
      #39: fProcTable[I] := StringProc;
      '"': fProcTable[I] := ObjectProc;
      ':': fProcTable[I] := VariableProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := IdentProc;
      '0'..'9':
        fProcTable[I] := NumberProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := SpaceProc;
      '^', '%', '*', '!':
        fProcTable[I] := SymbolAssignProc;
      '{', '}', '.', ',', ';', '?', '(', ')', '[', ']', '~':
        fProcTable[I] := SymbolProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynSqliteSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fKeywords := TSynHashEntryList.Create;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fDataTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType);
  fDataTypeAttri.Style := [fsBold];
  AddAttribute(fDataTypeAttri);
  fObjectAttri := TSynHighlighterAttributes.Create(SYNS_AttrObjects);
  fObjectAttri.Style := [fsBold];
  AddAttribute(fObjectAttri);
  fExceptionAttri := TSynHighlighterAttributes.Create(SYNS_AttrException);
  fExceptionAttri.Style := [fsItalic];
  AddAttribute(fExceptionAttri);
  fFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction);
  fFunctionAttri.Style := [fsBold];
  AddAttribute(fFunctionAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(FSymbolAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable);
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  EnumerateKeywords(Ord(tkDatatype), SqliteTypes, IdentChars, DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), SqliteFunctions, IdentChars, DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), SqliteKeywords, IdentChars, DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), ISQLKeywords, IdentChars, DoAddKeyword);
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterSQL;
  fRange := rsUnknown;
end;

destructor TSynSqliteSyn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TSynSqliteSyn.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TSynSqliteSyn.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynSqliteSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '&'] then
    Inc(Run);
end;

procedure TSynSqliteSyn.StringProc;
begin
  if fLine[Run] = #0 then
    NullProc
  else
  begin
    fTokenID := tkString;
    if (Run > 0) or (fRange <> rsString) or (fLine[Run] <> #39) then
    begin
      fRange := rsString;
      repeat
        Inc(Run);
      until fLine[Run] in [#0, #10, #13, #39];
    end;
    if fLine[Run] = #39 then
    begin
      Inc(Run);
      fRange := rsUnknown;
    end;
  end;
end;

procedure TSynSqliteSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynSqliteSyn.EqualProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynSqliteSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynSqliteSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  if fTokenID = tkComment then
  begin
    while not (fLine[Run] in [#0, #10, #13]) do
      Inc(Run);
  end
  else
    while Identifiers[fLine[Run]] do
      inc(Run);
end;

procedure TSynSqliteSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSqliteSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  case fLine[Run] of
    '=': Inc(Run);
    '<':
      begin
        Inc(Run);
        if fLine[Run] = '=' then
          Inc(Run);
      end;
  end;
end;

procedure TSynSqliteSyn.MinusProc;
begin
  Inc(Run);
  if fLine[Run] = '-' then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynSqliteSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynSqliteSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
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

procedure TSynSqliteSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '|'] then
    Inc(Run);
end;

procedure TSynSqliteSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '+'] then
    Inc(Run);
end;

procedure TSynSqliteSyn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '*':
      begin
        fRange := rsComment;
        fTokenID := tkComment;
        repeat
          Inc(Run);
          if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
          begin
            fRange := rsUnknown;
            Inc(Run, 2);
            break;
          end;
        until fLine[Run] in [#0, #10, #13];
      end;
    '=':
      begin
        Inc(Run);
        fTokenID := tkSymbol;
      end;
  else
    fTokenID := tkSymbol;
  end;
end;

procedure TSynSqliteSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TSynSqliteSyn.SymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSqliteSyn.SymbolAssignProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '=' then
    Inc(Run);
end;

procedure TSynSqliteSyn.VariableProc;
var
  i: integer;
begin
  if (fLine[Run] = ':') then
  begin
    fTokenID := tkVariable;
    i := Run;
    repeat
      Inc(i);
    until not (Identifiers[fLine[i]]);
    Run := i;
  end;
end;

procedure TSynSqliteSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynSqliteSyn.AnsiCProc;
begin
  case fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkComment;
      repeat
        if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
        begin
          fRange := rsUnknown;
          Inc(Run, 2);
          break;
        end;
        Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end;

function TSynSqliteSyn.IsKeyword(const AKeyword: string): boolean;
var
  tk: TtkTokenKind;
begin
  tk := IdentKind(PChar(AKeyword));
  Result := tk in [tkDatatype, tkException, tkFunction, tkKey, tkObject];
end;

procedure TSynSqliteSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment:
      AnsiCProc;
    rsString:
      StringProc;
  else
    fProcTable[fLine[Run]];
  end;
end;

function TSynSqliteSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSqliteSyn.GetEOL: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynSqliteSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynSqliteSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

{.$IFDEF SYN_LAZARUS}
procedure TSynSqliteSyn.GetTokenEx(out TokenStart: PChar; out
  TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;
{.$ENDIF}

function TSynSqliteSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynSqliteSyn.GetTokenAttribute: TSynHighlighterAttributes;
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

function TSynSqliteSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynSqliteSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynSqliteSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynSqliteSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynSqliteSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TSynSqliteSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSQL;
end;

procedure TSynSqliteSyn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TSynSqliteSyn.GetSampleSource: string;
begin
  Result := '';
end;

procedure TSynSqliteSyn.ObjectProc;
begin
  fTokenID := tkObject;
  Inc(Run);
  while not (fLine[Run] in [#0, #10, #13]) do
  begin
    if fLine[Run] = '"' then
    begin
      Inc(Run);
      break;
    end;
    Inc(Run);
  end;
end;

initialization
  MakeIdentTable;
end.

