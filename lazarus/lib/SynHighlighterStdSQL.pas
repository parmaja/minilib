unit SynHighlighterStdSQL;
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
  SysUtils, Controls, Graphics,
  Classes, SynEditTypes, SynEditHighlighter, SynHighlighterHashEntries, SynUtils;

type
  TtkTokenKind = (tkComment, tkDatatype, tkObject,
    tkFunction, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkVariable, tkUnknown);

  TRangeState = (rsUnknown, rsComment, rsSQString, rsDQString);

  { TSynStdSQLSyn }

  TSynStdSQLSyn = class(TSynCustomHighlighter)
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
    procedure SQStringProc;
    procedure DQStringProc;
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
  StdSQLKeywords: string =
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
  StdSQLFunctions =
    'avg,count,group_concat,max,min,sum,total,'+
    'abs,changes,coalesce,ifnull,hex,last_insert_rowid,length,'+
    'load_extension,lower,ltrim,nullif,quote,random,randomblob,round,rtrim,'+
    'soundex,StdSQL_version,substr,total_changes,trim,typeof,upper,zeroblob,'+
    'date,time,datetime,julianday,strftime';

  // types
  StdSQLTypes = 'blob,char,character,decimal,double,float,integer,' +
    'numeric,precision,smallint,timestamp,varchar';

  type

    { TStdSQLSyn }

   TStdSQLSyn = class(TSynPersistent)
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

function StdSQLSyn: TStdSQLSyn;

implementation

uses
  SynEditStrConst;

const
  SYNS_AttrObjects = 'Objects';

var
  FStdSQLSyn: TStdSQLSyn = nil;

function StdSQLSyn: TStdSQLSyn;
begin
  if FStdSQLSyn = nil then
    FStdSQLSyn := TStdSQLSyn.Create;
  Result := FStdSQLSyn;
end;

{ TStdSQLSyn }

procedure TStdSQLSyn.MakeIdentifiers;
begin
  inherited;
  Identifiers[':'] := True;
  Identifiers['"'] := True;
end;

procedure TStdSQLSyn.MakeHashes;
begin
  inherited;
  HashTable[':'] := HashTable['Z'] + 1;
  HashTable['"'] := HashTable['Z'] + 2;
end;

function TStdSQLSyn.GetDefaultKind: Integer;
begin
  Result := Ord(tkIdentifier);
end;

function TStdSQLSyn.IdentKind(MayBe: PChar; out L: Integer): TtkTokenKind;
begin
  Result := TtkTokenKind(GetIdentKind(MayBe, L));
end;

function TStdSQLSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  L: Integer;
begin
  Result := TtkTokenKind(GetIdentKind(MayBe, L));
end;

constructor TStdSQLSyn.Create;
begin
  inherited;
  EnumerateKeywords(Ord(tkDatatype), StdSQLTypes, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), StdSQLFunctions, TSynValidStringChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), StdSQLKeywords, TSynValidStringChars, @DoAddKeyword);
end;


procedure TSynStdSQLSyn.MakeProcTables;
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
      #39: FProcTable[I] := @SQStringProc;
      '"': FProcTable[I] := @DQStringProc;
      ':': FProcTable[I] := @VariableProc;
      '?': FProcTable[I] := @VariableProc;
      'A'..'Z', 'a'..'z', '_':
        FProcTable[I] := @IdentProc;
      '0'..'9':
        FProcTable[I] := @NumberProc;
      #1..#9, #11, #12, #14..#32:
        FProcTable[I] := @SpaceProc;
      '^', '%', '*', '!':
        FProcTable[I] := @SymbolAssignProc;
      '{', '}', '.', ',', ';', '(', ')', '[', ']', '~':
        FProcTable[I] := @SymbolProc;
    else
      FProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynStdSQLSyn.Create(AOwner: TComponent);
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

procedure TSynStdSQLSyn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  inherited;
  FLine := PChar(NewValue);
  Run := 0;
  Next;
end;

procedure TSynStdSQLSyn.AndSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '&'] then
    Inc(Run);
end;

procedure TSynStdSQLSyn.SQStringProc;
begin
  if FLine[Run] = #0 then
    NullProc
  else
  begin
    FTokenID := tkString;
    if (Run > 0) or (FRange <> rsSQString) or (FLine[Run] <> #39) then
    begin
      FRange := rsSQString;
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

procedure TSynStdSQLSyn.DQStringProc;
begin
  if FLine[Run] = #0 then
    NullProc
  else
  begin
    FTokenID := tkString;
    if (Run > 0) or (FRange <> rsDQString) or (FLine[Run] <> '"') then
    begin
      FRange := rsDQString;
      repeat
        Inc(Run);
      until FLine[Run] in [#0, #10, #13, '"'];
    end;
    if FLine[Run] = '"' then
    begin
      Inc(Run);
      FRange := rsUnknown;
    end;
  end;
end;

procedure TSynStdSQLSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynStdSQLSyn.EqualProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynStdSQLSyn.GreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynStdSQLSyn.IdentProc;
var
  L: Integer;
begin
  FTokenID := StdSQLSyn.IdentKind((FLine + Run), L);
  inc(Run, L);
  if FTokenID = tkComment then
  begin
    while not (FLine[Run] in [#0, #10, #13]) do
      Inc(Run);
  end
  else
    while StdSQLSyn.Identifiers[FLine[Run]] do
      inc(Run);
end;

procedure TSynStdSQLSyn.LFProc;
begin
  FTokenID := tkSpace;
  inc(Run);
end;

procedure TSynStdSQLSyn.LowerProc;
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

procedure TSynStdSQLSyn.MinusProc;
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

procedure TSynStdSQLSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynStdSQLSyn.NumberProc;
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

procedure TSynStdSQLSyn.OrSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '|'] then
    Inc(Run);
end;

procedure TSynStdSQLSyn.PlusProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '+'] then
    Inc(Run);
end;

procedure TSynStdSQLSyn.SlashProc;
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

procedure TSynStdSQLSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until (FLine[Run] > #32) or (FLine[Run] in [#0, #10, #13]);
end;

procedure TSynStdSQLSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynStdSQLSyn.SymbolAssignProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then
    Inc(Run);
end;

procedure TSynStdSQLSyn.VariableProc;
var
  i: integer;
begin
  if (FLine[Run] = ':') or (FLine[Run] = '?') then
  begin
    FTokenID := tkVariable;
    i := Run;
    repeat
      Inc(i);
    until not (StdSQLSyn.Identifiers[FLine[i]]);
    Run := i;
  end;
end;

procedure TSynStdSQLSyn.UnknownProc;
begin
  inc(Run);
  while (FLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((FLine[Run]<>#0) and (fProcTable[FLine[Run]] = @UnknownProc)) do inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynStdSQLSyn.CommentProc;
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

function TSynStdSQLSyn.IsKeyword(const AKeyword: string): boolean;
var
  tk: TtkTokenKind;
begin
  tk := StdSQLSyn.IdentKind(PChar(AKeyword));
  Result := tk in [tkDatatype, tkFunction, tkKey, tkObject];
end;

procedure TSynStdSQLSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsComment:
      CommentProc;
    rsSQString:
      SQStringProc;
    rsDQString:
      DQStringProc;
  else
    FProcTable[FLine[Run]]();
  end;
end;

function TSynStdSQLSyn.GetDefaultAttribute(Index: integer):
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

function TSynStdSQLSyn.GetEol: Boolean;
begin
  Result := FTokenID = tkNull;
end;

function TSynStdSQLSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrUInt(FRange));
end;

function TSynStdSQLSyn.GetToken: string;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - FTokenPos;
  SetString(Result, (FLine + FTokenPos), Len);
end;

procedure TSynStdSQLSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynStdSQLSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynStdSQLSyn.GetTokenAttribute: TSynHighlighterAttributes;
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

function TSynStdSQLSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynStdSQLSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

procedure TSynStdSQLSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynStdSQLSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(PtrUInt(Value));
end;

function TSynStdSQLSyn.GetIdentChars: TSynIdentChars;
begin
  Result := StdSQLSyn.GetIdentChars
end;

class function TSynStdSQLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSQL;
end;

function TSynStdSQLSyn.GetSampleSource: string;
begin
  Result := '/* SQL Example*/'#13#10 +
    #13#10 +
    'create table employees ('#13#10 +
    '        id int not null,'#13#10 +
    '        name char(30) not null,'#13#10 +
    '        primary key (id),'#13#10 +
    '        index name (name));'#13#10 +
    #13#10 +
    '// Single line comment'#13#10+
    'select name from employees'#13#10+
    'where id=?id and name="Unkown"'#13#10;
end;

initialization
  RegisterPlaceableHighlighter(TSynStdSQLSyn);
finalization
  FreeAndNil(FStdSQLSyn);
end.

