unit SynHighlighterSQLite;
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

{$H+}

interface

uses
  SysUtils, Controls, Graphics,
  Classes, SynEditTypes, SynEditHighlighter, SynHighlighterHashEntries, SynUtils;

type
  TtkTokenKind = (tkComment, tkDatatype, tkObject,
    tkFunction, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkVariable, tkUnknown);

  TRangeState = (rsUnknown, rsComment, rsString);

  { TSynSqliteSyn }

  TSynSqliteSyn = class(TSynCustomHighlighter)
  private
    Run: LongInt;
    FProcTable: array[AnsiChar] of TProcTableProc;
    FRange: TRangeState;
    FLine: PChar;
    FTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FDataTypeAttri: TSynHighlighterAttributes;
    FObjectAttri: TSynHighlighterAttributes;
    FFunctionAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    FVariableAttri: TSynHighlighterAttributes;
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
    procedure CommentProc;

    procedure MakeProcTables;
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
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
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
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property DataTypeAttri: TSynHighlighterAttributes read fDataTypeAttri write fDataTypeAttri;
    property ObjectAttri: TSynHighlighterAttributes read fObjectAttri write fObjectAttri;
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
  // keywords
  SqliteKeywords: string =
    'abort,add,after,all,alter,analyze,and,as,asc,attach,autoincrement,'+
    'before,begin,between,by,cascade,case,cast,check,collate,column,commit,'+
    'conflict,constraint,create,cross,current_date,current_time,current_timestamp,'+
    'database,default,deferrable,deferred,delete,desc,detach,distinct,drop,each,'+
    'else,end,escape,except,exclusive,exists,explain,fail,for,foreign,from,full,'+
    'glob,group,having,if,ignore,immediate,in,index,indexed,initially,inner,insert,'+
    'instead,intersect,into,is,isnull,join,key,left,like,limit,match,natural,not,'+
    'notnull,null,of,offset,on,or,order,outer,plan,pragma,primary,query,raise,'+
    'references,regexp,reindex,release,rename,replace,restrict,right,rollback,'+
    'row,savepoint,select,set,table,temp,temporary,then,to,transaction,trigger,'+
    'union,unique,update,using,vacuum,values,view,virtual,when,where';

  // functions
  SqliteFunctions =
    'avg,count,group_concat,max,min,sum,total,'+
    'abs,changes,coalesce,ifnull,hex,last_insert_rowid,length,'+
    'load_extension,lower,ltrim,nullif,quote,random,randomblob,round,rtrim,'+
    'soundex,sqlite_version,substr,total_changes,trim,typeof,upper,zeroblob,'+
    'date,time,datetime,julianday,strftime';

  // types
  SqliteTypes = 'blob,char,character,decimal,double,float,integer,' +
    'numeric,precision,smallint,timestamp,varchar';

  type

    { TSQLiteSyn }

   TSQLiteSyn = class(TSynPersistent)
    private
    protected
      procedure MakeIdentifiers; override;
      procedure MakeHashes; override;
      function GetDefaultKind: Integer; override;
    public
      function IdentKind(MayBe: PChar; out L: Integer): TtkTokenKind; overload;
      function IdentKind(MayBe: PChar): TtkTokenKind; overload;
      constructor Create; override;
    end;

function SQLiteSyn: TSQLiteSyn;

implementation

uses
  SynEditStrConst;

const
  SYNS_AttrObjects = 'Objects';

var
  FSQLiteSyn: TSQLiteSyn = nil;

function SQLiteSyn: TSQLiteSyn;
begin
  if FSQLiteSyn = nil then
    FSQLiteSyn := TSQLiteSyn.Create;
  Result := FSQLiteSyn;
end;

{ TSQLiteSyn }

procedure TSQLiteSyn.MakeIdentifiers;
begin
  inherited;
  Identifiers[':'] := True;
  Identifiers['"'] := True;
end;

procedure TSQLiteSyn.MakeHashes;
begin
  inherited;
  HashTable[':'] := HashTable['Z'] + 1;
  HashTable['"'] := HashTable['Z'] + 2;
end;

function TSQLiteSyn.GetDefaultKind: Integer;
begin
  Result := Ord(tkIdentifier);
end;

function TSQLiteSyn.IdentKind(MayBe: PChar; out L: Integer): TtkTokenKind;
begin
  Result := TtkTokenKind(GetIdentKind(MayBe, L));
end;

function TSQLiteSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  L: Integer;
begin
  Result := TtkTokenKind(GetIdentKind(MayBe, L));
end;

constructor TSQLiteSyn.Create;
begin
  inherited;
  EnumerateKeywords(Ord(tkDatatype), SqliteTypes, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), SqliteFunctions, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), SqliteKeywords, TSynValidStringChars, @DoAddKeyword);
end;


procedure TSynSqliteSyn.MakeProcTables;
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

constructor TSynSqliteSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  FCommentAttri.Style := [fsBold];
  FCommentAttri.Foreground := clMaroon;
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
  FVariableAttri.Style := [fsBold];
  FVariableAttri.Foreground := clBlack;
  AddAttribute(FVariableAttri);
  SetAttributesOnChange(@DefHighlightChange);
  FDefaultFilter := SYNS_FilterSQL;
  FRange := rsUnknown;
  MakeProcTables;
end;

destructor TSynSqliteSyn.Destroy;
begin
  inherited;
end;

procedure TSynSqliteSyn.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TSynSqliteSyn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  inherited;
  FLine := PChar(NewValue);
  Run := 0;
  Next;
end;

procedure TSynSqliteSyn.AndSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '&'] then
    Inc(Run);
end;

procedure TSynSqliteSyn.StringProc;
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

procedure TSynSqliteSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynSqliteSyn.EqualProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynSqliteSyn.GreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynSqliteSyn.IdentProc;
var
  L: Integer;
begin
  FTokenID := SqliteSyn.IdentKind((FLine + Run), L);
  inc(Run, L);
  if FTokenID = tkComment then
  begin
    while not (FLine[Run] in [#0, #10, #13]) do
      Inc(Run);
  end
  else
    while SQLiteSyn.Identifiers[FLine[Run]] do
      inc(Run);
end;

procedure TSynSqliteSyn.LFProc;
begin
  FTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSqliteSyn.LowerProc;
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

procedure TSynSqliteSyn.MinusProc;
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

procedure TSynSqliteSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynSqliteSyn.NumberProc;
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

procedure TSynSqliteSyn.OrSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '|'] then
    Inc(Run);
end;

procedure TSynSqliteSyn.PlusProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '+'] then
    Inc(Run);
end;

procedure TSynSqliteSyn.SlashProc;
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

procedure TSynSqliteSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until (FLine[Run] > #32) or (FLine[Run] in [#0, #10, #13]);
end;

procedure TSynSqliteSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynSqliteSyn.SymbolAssignProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then
    Inc(Run);
end;

procedure TSynSqliteSyn.VariableProc;
var
  i: integer;
begin
  if (FLine[Run] = ':') then
  begin
    FTokenID := tkVariable;
    i := Run;
    repeat
      Inc(i);
    until not (SQLiteSyn.Identifiers[FLine[i]]);
    Run := i;
  end;
end;

procedure TSynSqliteSyn.UnknownProc;
begin
  inc(Run);
  while (FLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((FLine[Run]<>#0) and (fProcTable[FLine[Run]] = @UnknownProc)) do inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynSqliteSyn.CommentProc;
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

function TSynSqliteSyn.IsKeyword(const AKeyword: string): boolean;
var
  tk: TtkTokenKind;
begin
  tk := SQLiteSyn.IdentKind(PChar(AKeyword));
  Result := tk in [tkDatatype, tkFunction, tkKey, tkObject];
end;

procedure TSynSqliteSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsComment:
      CommentProc;
    rsString:
      StringProc;
  else
    FProcTable[FLine[Run]]();
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
  Result := FTokenID = tkNull;
end;

function TSynSqliteSyn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynSqliteSyn.GetToken: string;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

procedure TSynSqliteSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynSqliteSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynSqliteSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkDatatype: Result := FDataTypeAttri;
    tkObject: Result := FObjectAttri;
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
  Result := Ord(FTokenID);
end;

function TSynSqliteSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

procedure TSynSqliteSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynSqliteSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynSqliteSyn.GetIdentChars: TSynIdentChars;
begin
  Result := SQLiteSyn.GetIdentChars
end;

class function TSynSqliteSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSQL;
end;

function TSynSqliteSyn.GetSampleSource: string;
begin
  Result := 'select * from Employees';
end;

procedure TSynSqliteSyn.ObjectProc;
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
  RegisterPlaceableHighlighter(TSynSqliteSyn);
end.

