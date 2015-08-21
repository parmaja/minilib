unit SynHighlighterFirebird;
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
  SysUtils, Classes, Graphics,
  SynEditTypes, SynEditHighlighter, SynHighlighterHashEntries, SynUtils;

type
  TtkTokenKind = (tkComment, tkDatatype, tkObject, tkException,
    tkFunction, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkUnknown, tkVariable);

  TRangeState = (rsUnknown, rsComment, rsString);

  TSynFirebirdSyn = class(TSynCustomHighlighter)
  private
    Run: LongInt;
    FProcTable: array[#0..#255] of TProcTableProc;
    FRange: TRangeState;
    FLine: PChar;
    FStringLen: Integer;
    FTokenPos: Integer;
    FTokenID: TtkTokenKind;
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

  // functions
  FirebirdFunctions =
    'avg,cast,count,gen_id,max,min,sum,upper'+
    'abs,acos,ascii_char,ascii_val,asin,atan,atan2,'+
    'bin_and,bin_or,bin_shl,bin_shr,bin_xor,'+
    'ceil,cos,cosh,cot,dateadd,datediff,decode,'+
    'exp,floor,gen_uuid,hash,ln,log,log10,lpad,'+
    'maxvalue,minvalue,mod,overlay,pi,power,'+
    'rand,replace,reverse,round,rpad,'+
    'sign,sin,sinh,sqrt,tan,tanh,trunc,'+
    'uuid_to_char,char_to_uuid';

  // keywords
  FirebirdKeywords: string =
    'active,add,after,all,alter,and,any,as,asc,ascending,at,auto,autonomous,autoddl,'+
    'based,basename,base_name,before,begin,between,blobedit,block,break,buffer,by,' +
    'cache,case,character_length,char_length,check,' +
    'check_point_len,check_point_length,coalesce,collate,collation,column,commit,' +
    'commited,compiletime,computed,close,conditional,connect,constraint,' +
    'containing,continue,create,current,current_date,current_time,' +
    'current_connection,current_timestamp,current_transaction,cursor,' +
    'database,day,db_key,debug,dec,declare,default,delete,deleting,desc,descending,' +
    'describe,descriptor,disconnect,distinct,do,domain,drop,echo,edit,else,end,' +
    'entry_point,escape,event,exception,execute,exists,exit,extern,external,extract,' +
    'fetch,file,filter,first,for,foreign,found,free_it,from,full,function,' +
    'gdscode,generator,global,goto,grant,group,group_commit_wait,group_commit_wait_time,' +
    'having,help,hour,'+
    'if,iif,immediate,in,inactive,index,indicator,init,inner,input,input_type,insensitive,' +
    'insert,inserting,int,into,is,isolation,isql,'+
    'join,'+
    'key,last,list,'+
    'lc_messages,lc_type,leave,left,length,lev,level,like,' +
    'logfile,log_buffer_size,log_buf_size,long,lock,manual,' +
    'matching,maximum,maximum_segment,max_segment,merge,message,minimum,minute,module_name,month,' +
    'names,national,natural,nchar,new,next,no,noauto,not,null,nullif' +
    'num_log_buffs,num_log_buffers,'+
    'octet_length,of,old,on,only,open,option,or,' +
    'order,outer,output,output_type,overflow,'+
    'page,pagelength,pages,page_size,' +
    'parameter,password,plan,position,post_event,prepare,procedure,' +
    'protected,primary,privileges,public,quit,raw_partitions,read,real,' +
    'record_version,recreate,references,release,reserv,reserving,retain,return,' +
    'returning,returning_values,returns,restart,revoke,right,rollback,row_count,rows,runtime,'+
    'savepoint,schema,second,' +
    'segment,select,set,sequence,shadow,shared,shell,show,similar,singular,size,snapshot,some,' +
    'sort,skip,sql,sqlcode,sqlerror,sqlwarning,stability,starting,starts,' +
    'sensitive,statement,static,statistics,sub_type,suspend,substring,'+
    'table,terminator,then,to,transaction,translate,translation,trigger,trim,type,' +
    'uncommitted,union,unique,unicode,update,updating,user,using,utf8,'+
    'value,values,variable,varying,version,view,' +
    'wait,weekday,when,whenever,where,while,with,work,write,'+
    'year,yearday';

  // types
  FirebirdTypes = 'bigint,blob,char,character,date,decimal,double,float,int64,integer,' +
    'numeric,precision,smallint,time,timestamp,varchar';

  ISQLKeywords = 'TERM';

type

  { TFirebirdSyn }

  TFirebirdSyn = class(TSynPersistent)
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

function FirebirdSyn: TFirebirdSyn;

implementation

uses
  SynEditStrConst;

const
  SYNS_AttrObjects = 'Objects';

var
  FFirebirdSyn: TFirebirdSyn = nil;

function FirebirdSyn: TFirebirdSyn;
begin
  if FFirebirdSyn = nil then
    FFirebirdSyn := TFirebirdSyn.Create;
  Result := FFirebirdSyn;
end;

{ TFirebirdSyn }

procedure TFirebirdSyn.MakeIdentifiers;
begin
  inherited;
  Identifiers[':'] := True;
  Identifiers['"'] := True;
end;

procedure TFirebirdSyn.MakeHashes;
begin
  inherited;
  HashTable[':'] := HashTable['Z'] + 1;
  HashTable['"'] := HashTable['Z'] + 2;
end;

function TFirebirdSyn.GetDefaultKind: Integer;
begin
  Result := ord(tkIdentifier);
end;

function TFirebirdSyn.IdentKind(MayBe: PChar; out L: Integer): TtkTokenKind;
begin
  Result := TtkTokenKind(GetIdentKind(MayBe, L));
end;

function TFirebirdSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  L: Integer;
begin
  Result := TtkTokenKind(GetIdentKind(MayBe, L));
end;

constructor TFirebirdSyn.Create;
begin
  inherited;
  EnumerateKeywords(Ord(tkDatatype), FirebirdTypes, GetIdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkFunction), FirebirdFunctions, GetIdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), FirebirdKeywords, GetIdentChars, @DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), ISQLKeywords, GetIdentChars, @DoAddKeyword);
end;

procedure TSynFirebirdSyn.MakeProcTables;
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
  FVariableAttri.Style := [fsBold];
  FVariableAttri.Foreground := clBlack;
  AddAttribute(FVariableAttri);
  SetAttributesOnChange(@DefHighlightChange);
  FDefaultFilter := SYNS_FilterSQL;
  FRange := rsUnknown;
  MakeProcTables;
end;

destructor TSynFirebirdSyn.Destroy;
begin
  inherited;
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
  FTokenID := FirebirdSyn.IdentKind((FLine + Run));
  inc(Run, FStringLen);
  if FTokenID = tkComment then
  begin
    while not (FLine[Run] in [#0, #10, #13]) do
      Inc(Run);
  end
  else
    while FirebirdSyn.Identifiers[FLine[Run]] do
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
    until not (FirebirdSyn.Identifiers[FLine[i]]);
    Run := i;
  end;
end;

procedure TSynFirebirdSyn.UnknownProc;
begin
  inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynFirebirdSyn.CommentProc;
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
  tk := FirebirdSyn.IdentKind(PChar(AKeyword));
  Result := tk in [tkDatatype, tkException, tkFunction, tkKey, tkObject];
end;

procedure TSynFirebirdSyn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsComment:
      CommentProc;
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
  Result := FirebirdSyn.GetIdentChars
end;

class function TSynFirebirdSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSQL;
end;

function TSynFirebirdSyn.GetSampleSource: string;
begin
  Result := 'select EMP_NO, "EMP_NAME" from EMPLOYEE' + LineEnding + 'where EMP_NO=?EMP_NO';
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
  RegisterPlaceableHighlighter(TSynFirebirdSyn);
end.

