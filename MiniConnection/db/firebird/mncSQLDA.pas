{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

(**

SQLDA

|-INFO-|-TXSQLVAR[0]-|-TXSQLVAR[1]-|-TXSQLVAR[2]-|-TXSQLVAR[3]-|

TXSQLVAR

|-INFO-|-sqllen-|-sqlind-|-sqldata-|

*)

unit mncSQLDA;

interface

uses
  SysUtils, Classes, Variants,
  mncFBHeader, mncFBTypes, mncFBUtils, mncFBErrors, mncFBStrings, mncFBClient, mncFBBlob;

type
  TFBSQLDA = class;

  TSQLVAR = class(TObject)
  private
    FXSQLVAR: PXSQLVAR;
    function GetSqlDef: Short;
  protected
    function GetSQLVAR: PXSQLVAR;
    procedure SetSQLVAR(const Value: PXSQLVAR);
    function GetAliasName: string;
    function GetOwnName: string;
    function GetRelName: string;
    function GetSqlData: PAnsiChar;
    function GetSqlInd: PShort;
    function GetSqlLen: Short;
    function GetSqlName: string;
    function GetSqlPrecision: Short;
    function GetSqlScale: Short;
    function GetSqlSubtype: Short;
    function GetSqlType: Short;
    procedure SetAliasName(const Value: string);
    procedure SetOwnName(const Value: string);
    procedure SetRelName(const Value: string);
    procedure SetSqlName(const Value: string);
    procedure SetSqlData(const Value: PAnsiChar);
    procedure SetSqlInd(const Value: PShort);
    procedure SetSqlLen(const Value: Short);
    procedure SetSqlPrecision(const Value: Short);
    procedure SetSqlScale(const Value: Short);
    procedure SetSqlSubtype(const Value: Short);
    procedure SetSqlType(const Value: Short);
  public
    constructor Create;
    procedure SetDataSize(oldsize, newsize: Integer);
    procedure SetIndSize(oldsize, newsize: Integer);
    property XSQLVar: PXSQLVAR read GetSQLVAR write SetSQLVAR;

    property SqlType: Short read GetSqlType write SetSqlType;
    property SqlDef: Short read GetSqlDef;
    property SqlScale: Short read GetSqlScale write SetSqlScale;
    property SqlPrecision: Short read GetSqlPrecision write SetSqlPrecision;
    property SqlSubtype: Short read GetSqlSubtype write SetSqlSubtype;
    property SqlLen: Short read GetSqlLen write SetSqlLen;
    property SqlData: PAnsiChar read GetSqlData write SetSqlData;
    property SqlInd: PShort read GetSqlInd write SetSqlInd;

    property SqlName: string read GetSqlName write SetSqlName;
    property RelName: string read GetRelName write SetRelName;
    property OwnName: string read GetOwnName write SetOwnName;
    property AliasName: string read GetAliasName write SetAliasName;
  end;

  TFBSQLVARList = class;

  { TFBSQLVAR }

  TFBSQLVAR = class(TObject, IStreamPersist)
  private
    FClones: TFBSQLVARList;
    FCloned: Boolean;
    function GetXSQLVAR: PXSQLVAR;
  private
    FDBHandle: PISC_DB_HANDLE;
    FTRHandle: PISC_TR_HANDLE;

    FIndex: Integer;
    FModified: Boolean;
    FName: string;
    FSQLVAR: TSQLVAR;
    FMaxLen: Short; { length of data buffer }

    function GetAsCurrency: Currency;
    function GetAsInt64: Int64;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsFloat: Double;
    function GetAsLong: Long;
    function GetAsPointer: Pointer;
    function GetAsQuad: TISC_QUAD;
    function GetAsShort: Short;
    function GetAsString: string;
    function GetAsVariant: Variant;
    function GetIsNull: Boolean;
    function GetIsNullable: Boolean;
    function GetSize: Integer;
    function GetSQLType: Integer;
    procedure SetAsCurrency(Value: Currency);
    procedure SetAsInt64(Value: Int64);
    procedure SetAsDate(Value: TDateTime);
    procedure SetAsTime(Value: TDateTime);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsDouble(Value: Double);
    procedure SetAsFloat(Value: Double);
    procedure SetAsLong(Value: Long);
    procedure SetAsPointer(Value: Pointer);
    procedure SetAsQuad(Value: TISC_QUAD);
    procedure SetAsShort(Value: Short);
    procedure SetAsString(Value: string);
    procedure SetAsVariant(Value: Variant);
    procedure SetIsNull(Value: Boolean);
    procedure SetIsNullable(Value: Boolean);
    procedure SetAsStrip(const Value: string);
    function GetAsStrip: string;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetSQLVAR(const Value: TSQLVAR);
    function GetAsGUID: TGUID;
    procedure SetAsGUID(const Value: TGUID);
    procedure SetModified(const Value: Boolean);
    procedure SetName(const Value: string);
    function GetAsHex: string;
    procedure SetAsHex(const Value: string);
    function GetAsText: string;
    procedure SetAsText(const Value: string);
    procedure Clone(FBCSQLVAR: TFBSQLVAR);
    procedure AddClone(FBCSQLVAR: TFBSQLVAR);
    procedure ApplyClones;
    procedure SetAsNullString(const Value: string);
    property Clones: TFBSQLVARList read FClones; //this property can be nil if there is no clones
    function Call(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
  public
    constructor Create(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE);
    destructor Destroy; override;
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure Assign(Source: TFBSQLVAR);
    procedure SetBuffer(Buffer: Pointer; Size: Integer); //zaher
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromIStream(Stream: IStreamPersist);
    procedure SaveToFile(const FileName: string);
    function CreateReadBlobSteam: TFBBlobStream;
    function CreateWriteBlobSteam: TFBBlobStream;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToIStream(Stream: IStreamPersist);
    procedure Clear;
    property Cloned: Boolean read FCloned;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsID: Int64 read GetAsInt64 write SetAsInt64; //More flixable names
    property AsInteger: Integer read GetAsLong write SetAsLong;
    property AsLong: Long read GetAsLong write SetAsLong;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
    property AsShort: Short read GetAsShort write SetAsShort;
    property AsString: string read GetAsString write SetAsString;
    property AsNullString: string read GetAsString write SetAsNullString;
    property AsHex: string read GetAsHex write SetAsHex;
    property AsText: string read GetAsText write SetAsText; //binary blob not text convert to hex
    property AsTrimString: string read GetAsStrip write SetAsStrip;
    property AsStrip: string read GetAsStrip write SetAsStrip;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AsGUID: TGUID read GetAsGUID write SetAsGUID;

    property SQLVar: TSQLVAR read FSQLVAR write SetSQLVAR;
    property XSQLVAR: PXSQLVAR read GetXSQLVAR;
    property Data: TSQLVAR read FSQLVAR write FSQLVAR;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsNullable: Boolean read GetIsNullable write SetIsNullable;
    property Index: Integer read FIndex;
    property Modified: Boolean read FModified write SetModified;
    property Name: string read FName write SetName;
    property Size: Integer read GetSize;
    property SQLType: Integer read GetSQLType;
    property Value: Variant read GetAsVariant write SetAsVariant;
  end;

  { TFBSQLDA }
  //List of TFBSQLVAR and buffer of params and fields

  TFBSQLDA = class(TObject)
  private
    function GetItem(Index: Integer): TFBSQLVAR;
    procedure SetItem(Index: Integer; const Value: TFBSQLVAR);
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    function GetField(Index: string): TFBSQLVAR;
  protected
    FDBHandle: PISC_DB_HANDLE;
    FTRHandle: PISC_TR_HANDLE;
    FData: PXSQLDA;
    FList: TList;
    function GetModified: Boolean;
    function GetNames: string;
    function GetRecordSize: Integer;
    function GetXSQLDA: PXSQLDA;
    procedure FindClones;
    procedure ApplyClones;
  public
    CaseSensitive: Boolean;{TODO}
    constructor Create(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE);
    destructor Destroy; override;
    procedure Clear;
    procedure Clean;
    procedure Initialize;
    function IsExists(Idx: string): Boolean;
    function Clone(Source: TFBSQLDA): TFBSQLVARList;
    function GetSQLVARByName(Idx: string): TFBSQLVAR;
    function ByName(Idx: string): TFBSQLVAR;
    property AsXSQLDA: PXSQLDA read GetXSQLDA;
    property Modified: Boolean read GetModified;
    property Names: string read GetNames;
    property RecordSize: Integer read GetRecordSize;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TFBSQLVAR read GetItem write SetItem; default;
    property Field[Index: string]: TFBSQLVAR read GetField;
    property Data: PXSQLDA read FData;
  end;

  TFBDSQLTypes = (SQLUnknown, SQLSelect, SQLInsert, SQLUpdate, SQLDelete,
    SQLDDL, SQLGetSegment, SQLPutSegment,
    SQLExecProcedure, SQLStartTransaction, SQLCommit, SQLRollback,
    SQLSelectForUpdate, SQLSetGenerator, SQLSavePoint);

  { TFBSQLVARList }

  TFBSQLVARList = class(TList)
  private
    function GetItem(Index: Integer): TFBSQLVAR;
    procedure SetItem(Index: Integer; const Value: TFBSQLVAR);
  public
    function Add(Item: TFBSQLVAR): Integer;
    procedure Assign(Source: TFBSQLVARList);
    property Items[Index: Integer]: TFBSQLVAR read GetItem write SetItem; default;
  end;

implementation

{ TFBSQLVAR }

constructor TFBSQLVAR.Create(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE);
begin
  inherited Create;
  FDBHandle:= DBHandle;
  FTRHandle:= TRHandle;
  FSQLVAR := TSQLVAR.Create;
end;

function TFBSQLVAR.CreateReadBlobSteam: TFBBlobStream;
begin
  Result := TFBBlobStream.Create(FDBHandle ,FTRHandle);
  try
    Result.Mode := bmRead;
    Result.BlobID := AsQuad;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TFBSQLVAR.CreateWriteBlobSteam: TFBBlobStream;
begin
  Result := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    Result.Mode := bmWrite;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TFBSQLVAR.Assign(Source: TFBSQLVAR);
var
  szBuff: PAnsiChar;
  s_bhandle, d_bhandle: TISC_BLOB_HANDLE;
  bSourceBlob, bDestBlob: Boolean;
  iSegs, iMaxSeg, iSize, oldSize: Long;
  iBlobType: Short;
  StatusVector: TStatusVector;
begin
  szBuff := nil;
  bSourceBlob := True;
  bDestBlob := True;
  s_bhandle := nil;
  d_bhandle := nil;
  iSize := 0;
  try
    if (Source.IsNull) then
    begin
      IsNull := True;
    end
    else if (FSQLVAR.SqlDef = SQL_ARRAY) or (Source.FSQLVAR.SqlDef = SQL_ARRAY) then
      { arrays not supported }
    else if (FSQLVAR.sqlDef <> SQL_BLOB) and (Source.FSQLVAR.SqlDef <> SQL_BLOB) then
    begin
      Value := Source.Value;
    end
    else
    begin
      if (Source.FSQLVAR.SqlDef <> SQL_BLOB) then
      begin
        szBuff := nil;
        FBAlloc(szBuff, 0, Source.FSQLVAR.sqllen);
        if (Source.FSQLVAR.SqlDef = SQL_TEXT) or (Source.FSQLVAR.SqlDef = SQL_VARYING) then
        begin
          iSize := FBClient.isc_vax_integer(Source.FSQLVAR.sqldata, 2);
          Move(Source.FSQLVAR.sqldata[2], szBuff[0], iSize)
        end
        else
        begin
          iSize := Source.FSQLVAR.sqllen;
          Move(Source.FSQLVAR.sqldata[0], szBuff[0], iSize);
        end;
        bSourceBlob := False;
      end
      else if (FSQLVAR.SqlDef <> SQL_BLOB) then
        bDestBlob := False;

      if bSourceBlob then
      begin
        { read the blob }
        Call(FBClient.isc_open_blob2(@StatusVector, FDBHandle,
          FTRHandle, @s_bhandle, PISC_QUAD(Source.FSQLVAR.sqldata),
          0, nil), StatusVector, True);
        try
          FBGetBlobInfo(@s_bhandle, iSegs, iMaxSeg, iSize, iBlobType);
          szBuff := nil;
          FBAlloc(szBuff, 0, iSize);
          FBReadBlob(@s_bhandle, szBuff, iSize);
        finally
          Call(FBClient.isc_close_blob(@StatusVector, @s_bhandle), StatusVector, True);
        end;
      end;

      if bDestBlob then
      begin
        { write the blob }
        Call(FBClient.isc_create_blob2(@StatusVector, FDBHandle,
          FTRHandle, @d_bhandle, PISC_QUAD(FSQLVAR.sqldata),
          0, nil), StatusVector, True);
        try
          FBWriteBlob(@d_bhandle, szBuff, iSize);
          IsNull := false;
        finally
          Call(FBClient.isc_close_blob(@StatusVector, @d_bhandle), StatusVector, True);
        end;
      end
      else
      begin
        { just copy the buffer }
        FSQLVAR.sqltype := SQL_TEXT;
        oldSize := FSQLVAR.sqllen;
        if iSize  > FMaxLen then
          FSQLVAR.sqllen := FMaxLen
        else
          FSQLVAR.sqllen := iSize;
        FSQLVAR.SetDataSize(oldSize, FSQLVAR.sqllen + 1);
        Move(szBuff[0], FSQLVAR.sqldata[0], FSQLVAR.sqllen);
      end;
    end;
  finally
    FreeMem(szBuff);
  end;
end;

function TFBSQLVAR.GetAsCurrency: Currency;
begin
  Result := 0;
  if not IsNull then
    case FSQLVAR.SqlDef of
      SQL_TEXT, SQL_VARYING:
        begin
          try
            Result := StrToCurr(AsString);
          except
            on E: Exception do
              FBRaiseError(fbceInvalidDataConversion, [nil]);
          end;
        end;
      SQL_SHORT:
        Result := FBScaleCurrency(Int64(PShort(FSQLVAR.sqldata)^), FSQLVAR.sqlscale);
      SQL_LONG:
        Result := FBScaleCurrency(Int64(PLong(FSQLVAR.sqldata)^), FSQLVAR.sqlscale);
      SQL_INT64:
        Result := FBScaleCurrency(PInt64(FSQLVAR.sqldata)^, FSQLVAR.sqlscale);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := GetAsDouble;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TFBSQLVAR.GetAsInt64: Int64;
begin
  Result := 0;
  if not IsNull then
    case FSQLVAR.SqlDef of
      SQL_TEXT, SQL_VARYING:
        begin
          try
            Result := StrToInt64(AsString);
          except
            on E: Exception do
              FBRaiseError(fbceInvalidDataConversion, [nil]);
          end;
        end;
      SQL_SHORT:
        Result := FBScaleInt64(Int64(PShort(FSQLVAR.sqldata)^),
          FSQLVAR.sqlscale);
      SQL_LONG:
        Result := FBScaleInt64(Int64(PLong(FSQLVAR.sqldata)^),
          FSQLVAR.sqlscale);
      SQL_INT64:
        Result := FBScaleInt64(PInt64(FSQLVAR.sqldata)^,
          FSQLVAR.sqlscale);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := Trunc(AsDouble);
      SQL_BOOLEAN:
        case PShort(FSQLVAR.sqldata)^ of
          ISC_TRUE: Result := 1;
          ISC_FALSE: Result := 0;
        end;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TFBSQLVAR.GetAsDateTime: TDateTime;
var
  tm_date: TCTimeStructure;
begin
  Result := 0;
  if not IsNull then
    case FSQLVAR.SqlDef of
      SQL_TEXT, SQL_VARYING:
        begin
          try    
            Result := StrToDate(AsString);
          except
            on E: EConvertError do
              FBRaiseError(fbceInvalidDataConversion, [nil]);
          end;
        end;
      SQL_TYPE_DATE:
        begin
          FBClient.isc_decode_sql_date(PISC_DATE(FSQLVAR.sqldata), @tm_date);
          try
            Result := EncodeDate(Word(tm_date.tm_year + 1900), Word(tm_date.tm_mon + 1),
              Word(tm_date.tm_mday));
          except
            on E: EConvertError do
            begin
              FBRaiseError(fbceInvalidDataConversion, [nil]);
            end;
          end;
        end;
      SQL_TYPE_TIME:
        begin
          FBClient.isc_decode_sql_time(PISC_TIME(FSQLVAR.sqldata), @tm_date);
          try
            Result := EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
              Word(tm_date.tm_sec), 0)
          except
            on E: EConvertError do
            begin
              FBRaiseError(fbceInvalidDataConversion, [nil]);
            end;
          end;
        end;
      SQL_TIMESTAMP:
        begin
          FBClient.isc_decode_date(PISC_QUAD(FSQLVAR.sqldata), @tm_date);
          try
            Result := EncodeDate(Word(tm_date.tm_year + 1900), Word(tm_date.tm_mon + 1),
              Word(tm_date.tm_mday));
            if Result >= 0 then
              Result := Result + EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
                Word(tm_date.tm_sec), 0)
            else
              Result := Result - EncodeTime(Word(tm_date.tm_hour), Word(tm_date.tm_min),
                Word(tm_date.tm_sec), 0)
          except
            on E: EConvertError do
            begin
              FBRaiseError(fbceInvalidDataConversion, [nil]);
            end;
          end;
        end;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TFBSQLVAR.GetAsDouble: Double;
begin
  Result := 0;
  if not IsNull then
  begin
    case FSQLVAR.SqlDef of
      SQL_TEXT, SQL_VARYING:
        begin
          try
            Result := StrToFloat(AsString);
          except
            on E: Exception do
              FBRaiseError(fbceInvalidDataConversion, [nil]);
          end;
        end;
      SQL_SHORT:
        Result := FBScaleDouble(Int64(PShort(FSQLVAR.sqldata)^), FSQLVAR.sqlscale);
      SQL_LONG:
        Result := FBScaleDouble(Int64(PLong(FSQLVAR.sqldata)^), FSQLVAR.sqlscale);
      SQL_INT64:
        Result := FBScaleDouble(PInt64(FSQLVAR.sqldata)^, FSQLVAR.sqlscale);
      SQL_FLOAT:
        Result := PFloat(FSQLVAR.sqldata)^;
      SQL_DOUBLE, SQL_D_FLOAT:
        Result := PDouble(FSQLVAR.sqldata)^;
      SQL_BOOLEAN:
        case PShort(FSQLVAR.sqldata)^ of
          ISC_TRUE: Result := 1;
          ISC_FALSE: Result := 0;
        end;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
  end;
end;

function TFBSQLVAR.GetAsFloat: Double;
begin
  Result := 0;
  try
    Result := AsDouble;
  except
    on E: SysUtils.EOverflow do
      FBRaiseError(fbceInvalidDataConversion, [nil])
    else
      raise; 
  end;
end;

function TFBSQLVAR.GetAsLong: Long;
begin
  Result := 0;
  if not IsNull then
    case FSQLVAR.SqlDef of
      SQL_TEXT, SQL_VARYING:
        begin
          try
            Result := StrToInt(AsString);
          except
            on E: Exception do
              FBRaiseError(fbceInvalidDataConversion, [nil]);
          end;
        end;
      SQL_SHORT:
        Result := Trunc(FBScaleDouble(Int64(PShort(FSQLVAR.sqldata)^),
          FSQLVAR.sqlscale));
      SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP, SQL_LONG:
        Result := Trunc(FBScaleDouble(Int64(PLong(FSQLVAR.sqldata)^),
          FSQLVAR.sqlscale));
      SQL_INT64:
        Result := Trunc(FBScaleDouble(PInt64(FSQLVAR.sqldata)^, FSQLVAR.sqlscale));
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := Trunc(AsDouble);
      SQL_BOOLEAN:
        case PShort(FSQLVAR.sqldata)^ of
          ISC_TRUE: Result := 1;
          ISC_FALSE: Result := 0;
        end;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TFBSQLVAR.GetAsPointer: Pointer;
begin
  if not IsNull then
    Result := FSQLVAR.sqldata
  else
    Result := nil;
end;

function TFBSQLVAR.GetAsQuad: TISC_QUAD;
begin
  Result.gds_quad_high := 0;
  Result.gds_quad_low := 0;
  if not IsNull then
    case FSQLVAR.SqlDef of
      SQL_BLOB, SQL_ARRAY, SQL_QUAD:
        Result := PISC_QUAD(FSQLVAR.sqldata)^;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TFBSQLVAR.GetAsShort: Short;
begin
  Result := 0;
  try
    Result := AsLong;
  except
    on E: Exception do
      FBRaiseError(fbceInvalidDataConversion, [nil]);
  end;
end;

function TFBSQLVAR.GetAsString: string;
var
  sz: PAnsiChar;
  str_len: Integer;
  ss: TStringStream;
begin
  Result := '';
  { Check null, if so return a default string }
  if not IsNull then
    case FSQLVAR.SqlDef of
      SQL_ARRAY:
        Result := '(Array)';
      SQL_BLOB:
        begin
          ss := TStringStream.Create('');
          try
            SaveToStream(ss);
            Result := ss.DataString;
          finally
            ss.Free;
          end;
        end;
      SQL_TEXT, SQL_VARYING:
        begin
          sz := FSQLVAR.sqldata;
          if (FSQLVAR.SqlDef = SQL_TEXT) then
            str_len := FSQLVAR.sqllen
          else
          begin
            str_len := FBClient.isc_vax_integer(FSQLVAR.sqldata, 2);
            Inc(sz, 2);
          end;
          SetString(Result, sz, str_len);
        end;
      SQL_TYPE_DATE:
        Result := DateToStr(AsDateTime);
      SQL_TYPE_TIME:
        Result := TimeToStr(AsDateTime);
      SQL_TIMESTAMP:
        Result := DateTimeToStr(AsDateTime);
      SQL_SHORT, SQL_LONG:
        if FSQLVAR.sqlscale = 0 then
          Result := IntToStr(AsLong)
        else if FSQLVAR.sqlscale >= (-4) then
          Result := CurrToStr(AsCurrency)
        else
          Result := FloatToStr(AsDouble);
      SQL_INT64:
        if FSQLVAR.sqlscale = 0 then
          Result := IntToStr(AsInt64)
        else if FSQLVAR.sqlscale >= (-4) then
          Result := CurrToStr(AsCurrency)
        else
          Result := FloatToStr(AsDouble);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := FloatToStr(AsDouble);
      SQL_BOOLEAN:
        if AsBoolean then
          Result := 'True'
        else
          Result := 'False';
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TFBSQLVAR.GetAsVariant: Variant;
begin
  if IsNull then
    Result := NULL
  { Check null, if so return a default string }
  else
    case FSQLVAR.SqlDef of
      SQL_ARRAY:
        Result := '(Array)';
      SQL_BLOB:
        begin
          if FSQLVAR.SqlSubtype = 1 then
            Result := AsString
          else
            Result := '(Blob)';
        end;
      SQL_TEXT, SQL_VARYING:
        Result := AsString;
      SQL_TIMESTAMP, SQL_TYPE_DATE, SQL_TYPE_TIME:
        Result := AsDateTime;
      SQL_SHORT, SQL_LONG:
        if FSQLVAR.sqlscale = 0 then
          Result := AsLong
        else if FSQLVAR.sqlscale >= (-4) then
          Result := AsCurrency
        else
          Result := AsDouble;
      SQL_INT64:
        if FSQLVAR.sqlscale = 0 then
          Result := AsINT64
        else if FSQLVAR.sqlscale >= (-4) then
          Result := AsCurrency
        else
          Result := AsDouble;
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := AsDouble;
      SQL_BOOLEAN:
        Result := AsBoolean;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TFBSQLVAR.GetIsNull: Boolean;
begin
  Result := IsNullable and (FSQLVAR.sqlind^ = -1);
end;

function TFBSQLVAR.GetIsNullable: Boolean;
begin
  Result := (FSQLVAR.sqltype and 1 = 1);
end;

procedure TFBSQLVAR.LoadFromFile(const FileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TFBSQLVAR.LoadFromStream(Stream: TStream);
var
  bs: TFBBlobStream;
begin
  bs := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    bs.Mode := bmWrite;
//    Stream.Seek(0, soFromBeginning);//not all stream support seek
    bs.LoadFromStream(Stream);
    bs.Finalize;
    AsQuad := bs.BlobID;
  finally
    bs.Free;
  end;
end;

procedure TFBSQLVAR.SaveToFile(const FileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TFBSQLVAR.SaveToStream(Stream: TStream);
var
  bs: TFBBlobStream;
begin
  bs := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    bs.Mode := bmRead;
    bs.BlobID := AsQuad;
    bs.SaveToStream(Stream);
  finally
    bs.Free;
  end;
end;

function TFBSQLVAR.GetSize: Integer;
begin
  Result := FSQLVAR.sqllen;
end;

function TFBSQLVAR.GetSQLType: Integer;
begin
  Result := FSQLVAR.SqlDef;
end;

procedure TFBSQLVAR.SetAsCurrency(Value: Currency);
begin
  if IsNullable then
    IsNull := False;
  FSQLVAR.sqltype := SQL_INT64 or (FSQLVAR.sqltype and 1);
  FSQLVAR.sqlscale := -4;
  FSQLVAR.sqllen := SizeOf(Int64);
  FSQLVAR.SetDataSize(0, FSQLVAR.sqllen);
  PCurrency(FSQLVAR.sqldata)^ := Value;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsInt64(Value: Int64);
begin
  if IsNullable then
    IsNull := False;
  FSQLVAR.sqltype := SQL_INT64 or (FSQLVAR.sqltype and 1);
  FSQLVAR.sqlscale := 0;
  FSQLVAR.sqllen := SizeOf(Int64);
  FSQLVAR.SetDataSize(0, FSQLVAR.sqllen);
  PInt64(FSQLVAR.sqldata)^ := Value;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsDate(Value: TDateTime);
var
  tm_date: TCTimeStructure;
  Yr, Mn, Dy: Word;
begin
  if IsNullable then
    IsNull := False;
  FSQLVAR.sqltype := SQL_TYPE_DATE or (FSQLVAR.sqltype and 1);
  DecodeDate(Value, Yr, Mn, Dy);
  with tm_date do
  begin
    tm_sec := 0;
    tm_min := 0;
    tm_hour := 0;
    tm_mday := Dy;
    tm_mon := Mn - 1;
    tm_year := Yr - 1900;
  end;
  FSQLVAR.sqllen := SizeOf(ISC_DATE);
  FSQLVAR.SetDataSize(0, FSQLVAR.sqllen);
  FBClient.isc_encode_sql_date(@tm_date, PISC_DATE(FSQLVAR.sqldata));
  Modified := True;
end;

procedure TFBSQLVAR.SetAsTime(Value: TDateTime);
var
  tm_date: TCTimeStructure;
  Hr, Mt, S, Ms: Word;
begin
  if IsNullable then
    IsNull := False;
  FSQLVAR.sqltype := SQL_TYPE_TIME or (FSQLVAR.sqltype and 1);
  DecodeTime(Value, Hr, Mt, S, Ms);
  with tm_date do
  begin
    tm_sec := S;
    tm_min := Mt;
    tm_hour := Hr;
    tm_mday := 0;
    tm_mon := 0;
    tm_year := 0;
  end;
  FSQLVAR.sqllen := SizeOf(ISC_TIME);
  FSQLVAR.SetDataSize(0, FSQLVAR.sqllen);
  FBClient.isc_encode_sql_time(@tm_date, PISC_TIME(FSQLVAR.sqldata));
  Modified := True;
end;

procedure TFBSQLVAR.SetAsDateTime(Value: TDateTime);
var
  tm_date: TCTimeStructure;
  Yr, Mn, Dy, Hr, Mt, S, Ms: Word;
begin
  if IsNullable then
    IsNull := False;
  FSQLVAR.sqltype := SQL_TIMESTAMP or (FSQLVAR.sqltype and 1);
  DecodeDate(Value, Yr, Mn, Dy);
  DecodeTime(Value, Hr, Mt, S, Ms);
  with tm_date do
  begin
    tm_sec := S;
    tm_min := Mt;
    tm_hour := Hr;
    tm_mday := Dy;
    tm_mon := Mn - 1;
    tm_year := Yr - 1900;
  end;
  FSQLVAR.sqllen := SizeOf(TISC_QUAD);
  FSQLVAR.SetDataSize(0, FSQLVAR.sqllen);
  FBClient.isc_encode_date(@tm_date, PISC_QUAD(FSQLVAR.sqldata));
  Modified := True;
end;

procedure TFBSQLVAR.SetAsDouble(Value: Double);
begin
  if IsNullable then
    IsNull := False;
  FSQLVAR.sqltype := SQL_DOUBLE or (FSQLVAR.sqltype and 1);
  FSQLVAR.sqllen := SizeOf(Double);
  FSQLVAR.sqlscale := 0;
  FSQLVAR.SetDataSize(0, FSQLVAR.sqllen);
  PDouble(FSQLVAR.sqldata)^ := Value;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsFloat(Value: Double);
begin
  if IsNullable then
    IsNull := False;
  FSQLVAR.sqltype := SQL_FLOAT or (FSQLVAR.sqltype and 1);
  FSQLVAR.sqllen := SizeOf(Float);
  FSQLVAR.sqlscale := 0;
  FSQLVAR.SetDataSize(0, FSQLVAR.sqllen);
  PSingle(FSQLVAR.sqldata)^ := Value;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsLong(Value: Long);
begin
  if IsNullable then
    IsNull := False;
  FSQLVAR.sqltype := SQL_LONG or (FSQLVAR.sqltype and 1);
  FSQLVAR.sqllen := SizeOf(Long);
  FSQLVAR.sqlscale := 0;
  FSQLVAR.SetDataSize(0, FSQLVAR.sqllen);
  PLong(FSQLVAR.sqldata)^ := Value;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsNullString(const Value: string);
begin
  if Value = '' then
    Clear
  else
    AsString := Value;
end;

procedure TFBSQLVAR.SetAsPointer(Value: Pointer);
begin
  if IsNullable and (Value = nil) then
    IsNull := True
  else
  begin
    IsNull := False;
    FSQLVAR.sqltype := SQL_TEXT or (FSQLVAR.sqltype and 1);
    Move(Value^, FSQLVAR.sqldata^, FSQLVAR.sqllen);
    Modified := True;
  end;
end;

procedure TFBSQLVAR.SetAsQuad(Value: TISC_QUAD);
begin
  if IsNullable then
    IsNull := False;
  if (FSQLVAR.SqlDef <> SQL_BLOB) and
    (FSQLVAR.SqlDef <> SQL_ARRAY) then
    FBRaiseError(fbceInvalidDataConversion, [nil]);
  FSQLVAR.sqllen := SizeOf(TISC_QUAD);
  FSQLVAR.SetDataSize(0, FSQLVAR.sqllen);
  PISC_QUAD(FSQLVAR.sqldata)^ := Value;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsShort(Value: Short);
begin
  if IsNullable then
    IsNull := False;
  FSQLVAR.sqltype := SQL_SHORT or (FSQLVAR.sqltype and 1);
  FSQLVAR.sqllen := SizeOf(Short);
  FSQLVAR.sqlscale := 0;
  FSQLVAR.SetDataSize(0, FSQLVAR.sqllen);
  PShort(FSQLVAR.sqldata)^ := Value;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsString(Value: string);
var
  stype: Integer;
  ss: TStringStream;

  procedure SetStringValue;
  begin
    if (FSQLVAR.sqlname = 'DB_KEY') or
      (FSQLVAR.sqlname = 'RDB$DB_KEY') then
      Move(Value[1], FSQLVAR.sqldata^, FSQLVAR.sqllen)
    else
    begin
      FSQLVAR.sqltype := SQL_TEXT or (FSQLVAR.sqltype and 1);
      if (FMaxLen > 0) and (Length(Value) > FMaxLen) then
        Value := Copy(Value, 1, FMaxLen);
      FSQLVAR.sqllen := Length(Value);
      FSQLVAR.SetDataSize(0, FSQLVAR.sqllen + 1);
      if (Length(Value) > 0) then
        Move(Value[1], FSQLVAR.sqldata^, FSQLVAR.sqllen);
    end;
    Modified := True;
  end;
begin
  if IsNullable then
    IsNull := False;
  stype := FSQLVAR.SqlDef;
  if (stype = SQL_TEXT) or (stype = SQL_VARYING) then
    SetStringValue
  else
  begin
    if (stype = SQL_BLOB) then
    begin
      if Value = '' then
        IsNull := True
      else
      begin
        ss := TStringStream.Create(Value);
        try
          LoadFromStream(ss);
        finally
          ss.Free;
        end;
      end;
    end
    else if Value = '' then
      IsNull := True
    else if (stype = SQL_TIMESTAMP) or (stype = SQL_TYPE_DATE) or
      (stype = SQL_TYPE_TIME) then
      SetAsDateTime(StrToDateTime(Value))
    else
      SetStringValue;
  end;
end;

procedure TFBSQLVAR.SetAsVariant(Value: Variant);
begin
  if VarIsNull(Value) then
    IsNull := True
  else
    case VarType(Value) of
      varEmpty, varNull:
        IsNull := True;
      varSmallint, varInteger, varByte, varShortInt, varWord, varLongWord:
        AsLong := Value;
      varSingle, varDouble:
        AsDouble := Value;
      varCurrency:
        AsCurrency := Value;
      varBoolean:
        if Value then
          AsBoolean := true
        else
          AsBoolean := false;
      varDate:
        AsDateTime := Value;
      varOleStr, varString:
        AsString := Value;
      varArray:
        FBRaiseError(fbceNotSupported, [nil]);
      varByRef, varDispatch, varError, varUnknown, varVariant:
        FBRaiseError(fbceNotPermitted, [nil]);
      varInt64:
        AsInt64 := Value;
    else
      FBRaiseError(fbceNotSupported, [nil]);
    end;
end;

procedure TFBSQLVAR.SetIsNull(Value: Boolean);
begin
  if Value then
  begin
    if not IsNullable then
      IsNullable := True;
    if Assigned(FSQLVAR.sqlind) then
      FSQLVAR.sqlind^ := -1;
    Modified := True;
  end
  else if ((not Value) and IsNullable) then
  begin
    if Assigned(FSQLVAR.sqlind) then
      FSQLVAR.sqlind^ := 0;
    Modified := True;
  end;
end;

procedure TFBSQLVAR.SetIsNullable(Value: Boolean);
begin
  if (Value <> IsNullable) then
  begin
    if Value then
    begin
      FSQLVAR.sqltype := FSQLVAR.sqltype or 1;
      FSQLVAR.SetIndSize(0, SizeOf(Short));
    end
    else
    begin
      FSQLVAR.sqltype := FSQLVAR.SqlDef;
      FSQLVAR.SetIndSize(0, 0);
    end;
  end;
end;

procedure TFBSQLVAR.Clear;
begin
  IsNull := True;
end;

procedure TFBSQLVAR.SetAsStrip(const Value: string);
begin
  if Value = '' then
    Clear
  else
    SetAsString(TrimRight(Value));
end;

function TFBSQLVAR.GetAsStrip: string;
begin
  Result := TrimRight(GetAsString);
end;

function TFBSQLVAR.GetAsBoolean: Boolean;
begin
  Result := false;
  if not IsNull then
    case FSQLVAR.SqlDef of
      SQL_INT64: Result := PInt64(FSQLVAR.sqldata)^ <> ISC_FALSE;
      SQL_LONG: Result := PLong(FSQLVAR.sqldata)^ <> ISC_FALSE;
      SQL_SHORT, SQL_BOOLEAN:
        Result := PShort(FSQLVAR.sqldata)^ <> ISC_FALSE
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

procedure TFBSQLVAR.SetAsBoolean(const Value: Boolean);
begin
  if IsNullable then
    IsNull := False;
  if Value then
    PShort(FSQLVAR.sqldata)^ := ISC_TRUE
  else
    PShort(FSQLVAR.sqldata)^ := ISC_FALSE;
end;

procedure TFBSQLVAR.SetSQLVAR(const Value: TSQLVAR);
var
  local_sqlind: PShort;
  local_sqldata: PAnsiChar;
  local_sqllen: Integer;
begin
  local_sqlind := FSQLVAR.sqlind;
  local_sqldata := FSQLVAR.sqldata;
  move(TSQLVAR(Value).FXSQLVAR^, TSQLVAR(FSQLVAR).FXSQLVAR^, sizeof(TXSQLVAR));
  FSQLVAR.sqlind := local_sqlind;
  FSQLVAR.sqldata := local_sqldata;
  if (Value.sqltype and 1 = 1) then
  begin
    if (FSQLVAR.sqlind = nil) then
      FSQLVAR.SetIndSize(0, SizeOf(Short));
    FSQLVAR.sqlind^ := Value.sqlind^;
  end
  else if (FSQLVAR.sqlind <> nil) then
    FSQLVAR.SetIndSize(0, 0);
  if ((FSQLVAR.SqlDef) = SQL_VARYING) then
    local_sqllen := FSQLVAR.sqllen + 2
  else
    local_sqllen := FSQLVAR.sqllen;
  FSQLVAR.sqlscale := Value.sqlscale;
  FSQLVAR.SetDataSize(0, local_sqllen);
  Move(Value.sqldata[0], FSQLVAR.sqldata[0], local_sqllen);
  Modified := True;
end;

destructor TFBSQLVAR.Destroy;
begin
  FreeMem(FSQLVAR.sqldata);
  FreeMem(FSQLVAR.sqlind);
  FreeAndNil(FSQLVAR);
  FreeAndNil(FClones);
  inherited;
end;

function TFBSQLVAR.Call(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := FBCall(ErrCode, StatusVector, RaiseError);
end;

procedure TFBSQLVAR.SetModified(const Value: Boolean);
begin
  FCloned := False;
  FModified := Value;
end;

procedure TFBSQLVAR.SetName(const Value: string);
begin
  FName := Value
end;

function TFBSQLVAR.GetAsHex: string;
var
  s: string;
begin
  s := GetAsString;
  SetLength(Result, Length(s) * 2);
  BinToHex(PAnsiChar(s), @Result[1], Length(s));
end;

procedure TFBSQLVAR.SetAsHex(const Value: string);
var
  s: string;
begin
  SetLength(s, Length(Value) div 2);
  HexToBin(PAnsiChar(Value), @s[1], Length(s));
  AsString := s;
end;

function TFBSQLVAR.GetAsText: string;
begin
  if (SqlVar.SqlDef = SQL_BLOB) and (SqlVar.SqlSubtype <> 1) then
    Result := AsHex
  else
    Result := AsString;
end;

procedure TFBSQLVAR.SetAsText(const Value: string);
begin
  if (SqlVar.SqlDef = SQL_BLOB) and (SqlVar.SqlSubtype <> 1) then
    AsHex := Value
  else
    AsString := Value;
end;

procedure TFBSQLVAR.SetBuffer(Buffer: Pointer; Size: Integer);
var
  sz: PAnsiChar;
  len: Integer;
begin
  sz := FSQLVAR.sqldata;
  if (FSQLVAR.SqlDef = SQL_TEXT) then
    len := FSQLVAR.sqllen
  else
  begin
    len := FBClient.isc_vax_integer(FSQLVAR.sqldata, 2);
    Inc(sz, 2);
  end;
  if (Size <> 0) and (len > Size) then
    len := Size;
  Move(sz^, Buffer^, len);
end;

function TFBSQLVAR.GetAsGUID: TGUID;
begin

end;

procedure TFBSQLVAR.SetAsGUID(const Value: TGUID);
begin

end;

procedure TFBSQLVAR.AddClone(FBCSQLVAR: TFBSQLVAR);
begin
  if FClones = nil then
    FClones := TFBSQLVARList.Create;
  FBCSQLVAR.FCloned := True;
  Clones.Add(FBCSQLVAR);
end;

procedure TFBSQLVAR.ApplyClones;
var
  i: Integer;
begin
  if (Clones <> nil) and (not Cloned) then
    for i := 0 to Clones.Count - 1 do
    begin
      Clones[i].Clone(Self);
    end;
end;

procedure TFBSQLVAR.Clone(FBCSQLVAR: TFBSQLVAR);
begin
  Value := FBCSQLVAR.Value;
end;

function TFBSQLVAR.GetXSQLVAR: PXSQLVAR;
begin
  Result := FSQLVar.XSQLVar;
end;

procedure TFBSQLVAR.SaveToIStream(Stream: IStreamPersist);
var
  bs: TFBBlobStream;
begin
  bs := TFBBlobStream.Create(FDBHandle ,FTRHandle);
  try
    bs.Mode := bmRead;
    bs.BlobID := AsQuad;
    Stream.LoadFromStream(bs);
  finally
    bs.Free;
  end;
end;

procedure TFBSQLVAR.LoadFromIStream(Stream: IStreamPersist);
var
  bs: TFBBlobStream;
begin
  bs := TFBBlobStream.Create(FDBHandle ,FTRHandle);
  try
    bs.Mode := bmWrite;
    Stream.SaveToStream(bs);
    bs.Finalize;
    AsQuad := bs.BlobID;
  finally
    bs.Free;
  end;
end;

function TFBSQLVAR._AddRef: Integer;
begin
  Result := -1;
end;

function TFBSQLVAR._Release: Integer;
begin
  Result := -1
end;

function TFBSQLVAR.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{ TFBSQLDA }

constructor TFBSQLDA.Create(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE);
begin
  inherited Create;
  FDBHandle:= DBHandle;
  FTRHandle:= TRHandle;
  FList := TList.Create;
  FBAlloc(FData, 0, XSQLDA_LENGTH(0));
  FData.version := SQLDA_VERSION1;
end;

destructor TFBSQLDA.Destroy;
begin
  Clear;
  if FData <> nil then
  begin
    FreeMem(FData);
    FData := nil;
  end;
  FList.Free;
  inherited;
end;

function TFBSQLDA.GetModified: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if Items[i].Modified then
    begin
      Result := True;
      break;
    end;
end;

function TFBSQLDA.GetNames: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if Result <> '' then
      Result := Result + sLineFeed;
    Result := Result + Items[i].FName;
  end;
end;

function TFBSQLDA.GetRecordSize: Integer;
begin
  Result := SizeOf(TFBSQLDA) + XSQLDA_LENGTH(Count);
end;

function TFBSQLDA.GetXSQLDA: PXSQLDA;
begin
  Result := FData;
end;

function TFBSQLDA.ByName(Idx: string): TFBSQLVAR;
begin
  Result := GetSQLVARByName(Idx);
  if Result = nil then
    FBRaiseError(fbceFieldNotFound, [Idx]);
end;

function TFBSQLDA.GetSQLVARByName(Idx: string): TFBSQLVAR;
var
  i: Integer;
begin
  Result := nil;
  if CaseSensitive then
  begin
    for i := 0 to Count - 1 do
    begin
      if (Items[i].FName = Idx) then
      begin
        Result := Items[i];
        break;
      end;
    end;
  end
  else
  begin
    Idx := UpperCase(FBDequoteName(Idx));
    for i := 0 to Count - 1 do
    begin
      if (UpperCase(Items[i].FName) = Idx) then
      begin
        Result := Items[i];
        break;
      end;
    end;
  end;
end;

procedure TFBSQLDA.Initialize;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    with Items[i].Data do
    begin
      if Items[i].Name = '' then
      begin
        if AliasName = '' then
          AliasName := 'F_' + IntToStr(i);
        Items[i].Name := FBDequoteName(aliasname);
      end;

      if (SqlDef = SQL_TEXT) or
        (SqlDef = SQL_VARYING) then
        Items[i].FMaxLen := sqllen
      else
        Items[i].FMaxLen := 0;

      if FXSQLVAR^.sqldata = nil then
        case SqlDef of
          SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP,
            SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT,
            SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT, SQL_BOOLEAN:
            begin
              if (sqllen = 0) then
              { Make sure you get a valid pointer anyway
               select '' from foo }
                SetDataSize(0, 1)
              else
                SetDataSize(0, sqllen)
            end;
          SQL_VARYING:
            begin
              SetDataSize(0, sqllen + 2);
            end;
        else
          FBRaiseError(fbceUnknownSQLDataType, [SqlDef])
        end;
      if (sqltype and 1 = 1) then
        SetIndSize(0, SizeOf(Short))
      else if (sqlind <> nil) then
        SetIndSize(0, 0);
    end;
  end;
end;

function TFBSQLDA.IsExists(Idx: string): Boolean;
begin
  Result := GetSQLVARByName(Idx) <> nil;
end;

procedure TFBSQLDA.SetCount(const Value: Integer);
var
  i: Integer;
  XSQLVar_Size: Integer;
  p: Pointer;
  OldCount: Integer;
begin
  OldCount := Count;
  if Value <> 0 then
  begin
    if Value <> Count then
    begin
      if Value < Count then
      begin
        for i := Value to Count - 1 do
        begin
          Items[i].Free;
        end;
      end;
      FBAlloc(FData, XSQLDA_LENGTH(Count), XSQLDA_LENGTH(Value));
    end;
    FList.Count := Value;
    FData.version := SQLDA_VERSION1;
    XSQLVar_Size := sizeof(TXSQLVAR);
    p := @FData^.sqlvar[0];
    for i := 0 to Count - 1 do
    begin
      if i >= OldCount then
      begin
        Items[i] := TFBSQLVAR.Create(FDBHandle, FTRHandle);
        Items[i].FIndex := i;
        Items[i].FSQLVAR.XSqlVar := p;
        Items[i].Clear;
      end
      else
        Items[i].FSQLVAR.XSqlVar := p;
      p := Pointer(PAnsiChar(p) + XSQLVar_Size);
    end;
    if Count > 0 then
    begin
      FData^.sqln := Value;
      FData^.sqld := Value;
    end;
  end;
end;

function TFBSQLDA.GetItem(Index: Integer): TFBSQLVAR;
begin
  Result := TFBSQLVAR(FList[Index]);
end;

procedure TFBSQLDA.SetItem(Index: Integer; const Value: TFBSQLVAR);
begin
  FList[Index] := Value;
end;

procedure TFBSQLDA.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Free;
  end;
  FList.Clear;
  inherited;
end;

function TFBSQLDA.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TFBSQLDA.GetField(Index: string): TFBSQLVAR;
begin
  Result := ByName(Index);
end;

procedure TFBSQLDA.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Clear;
  end;
end;

function TFBSQLDA.Clone(Source: TFBSQLDA): TFBSQLVARList;
var
  i: Integer;
  aVAR: TFBSQLVAR;
begin
  Result := TFBSQLVARList.Create;
  for i := 0 to Count - 1 do
  begin
    aVAR := Source.ByName(Items[i].Name);
    if aVAR <> nil then
    begin
      Result.Add(Items[i]);
    end;
  end;
end;

procedure TFBSQLDA.FindClones;
var
  i, j: Integer;
  aItem: TFBSQLVAR;
begin
  for i := 0 to Count - 1 do
  begin
    aItem := Items[i];
    FreeAndNil(aItem.FClones);
    if not aItem.Cloned then
      for j := i + 1 to Count - 1 do
      begin
        if (aItem.Name = Items[j].Name) then
          aItem.AddClone(Items[j]);
      end;
  end;
end;

procedure TFBSQLDA.ApplyClones;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].ApplyClones;
end;

function TSQLVAR.GetAliasName: string;
begin
  Result := '';
  SetString(Result, FXSQLVAR.aliasname, FXSQLVAR.aliasname_length);
end;

function TSQLVAR.GetOwnName: string;
begin
  Result := '';
  SetString(Result, FXSQLVAR.ownname, FXSQLVAR.ownname_length);
end;

function TSQLVAR.GetRelName: string;
begin
  Result := '';
  SetString(Result, FXSQLVAR.relname, FXSQLVAR.relname_length);
end;

function TSQLVAR.GetSqlData: PAnsiChar;
begin
  Result := FXSQLVAR.sqldata;
end;

function TSQLVAR.GetSqlInd: PShort;
begin
  Result := FXSQLVAR.sqlind;
end;

function TSQLVAR.GetSqlLen: Short;
begin
  Result := FXSQLVAR.sqllen;
end;

function TSQLVAR.GetSqlName: string;
begin
  Result := '';
  SetString(Result, FXSQLVAR.sqlname, FXSQLVAR.sqlname_length);
end;

function TSQLVAR.GetSqlPrecision: Short;
begin
  case sqltype and not 1 of
    SQL_SHORT:
      Result := 4;
    SQL_LONG:
      Result := 9;
    SQL_INT64:
      Result := 18;
  else
    Result := 0;
  end;
end;

function TSQLVAR.GetSqlScale: Short;
begin
  Result := FXSQLVAR.sqlscale;
end;

function TSQLVAR.GetSqlSubtype: Short;
begin
  Result := FXSQLVAR.sqlsubtype;
end;

function TSQLVAR.GetSqlType: Short;
begin
  Result := FXSQLVAR.sqltype;
end;

function TSQLVAR.GetSQLVAR: PXSQLVAR;
begin
  Result := FXSQLVAR;
end;

procedure TSQLVAR.SetAliasName(const Value: string);
begin
  StrPCopy(FXSQLVAR^.aliasname, Value);
  FXSQLVAR^.aliasname_length := Length(Value);
end;

procedure TSQLVAR.SetDataSize(oldsize, newsize: Integer);
begin
  FBAlloc(FXSQLVAR^.sqldata, oldsize, newsize);
end;

procedure TSQLVAR.SetIndSize(oldsize, newsize: Integer);
begin
  FBAlloc(FXSQLVAR^.sqlind, oldsize, newsize);
end;

procedure TSQLVAR.SetOwnName(const Value: string);
begin
  StrPCopy(FXSQLVAR^.ownname, Value);
  FXSQLVAR^.ownname_length := Length(Value);
end;

procedure TSQLVAR.SetRelName(const Value: string);
begin
  StrPCopy(FXSQLVAR^.relname, Value);
  FXSQLVAR^.relname_length := Length(Value);
end;

procedure TSQLVAR.SetSqlData(const Value: PAnsiChar);
begin
  FXSQLVAR.sqldata := Value;
end;

procedure TSQLVAR.SetSqlInd(const Value: PShort);
begin
  FXSQLVAR.sqlInd := Value
end;

procedure TSQLVAR.SetSqlLen(const Value: Short);
begin
  FXSQLVAR.sqlLen := Value
end;

procedure TSQLVAR.SetSqlName(const Value: string);
begin
  StrPCopy(FXSQLVAR^.sqlname, Value);
  FXSQLVAR^.sqlname_length := Length(Value);
end;

procedure TSQLVAR.SetSqlPrecision(const Value: Short);
begin
  FBRaiseError(fbceNotSupported, []);
end;

procedure TSQLVAR.SetSqlScale(const Value: Short);
begin
  FXSQLVAR.sqlscale := Value
end;

procedure TSQLVAR.SetSqlSubtype(const Value: Short);
begin
  FXSQLVAR.sqlsubtype := Value
end;

procedure TSQLVAR.SetSqlType(const Value: Short);
begin
  FXSQLVAR.sqltype := Value
end;

procedure TSQLVAR.SetSQLVAR(const Value: PXSQLVAR);
begin
  FXSQLVAR := Value;
end;

{ TSQLVAR }

constructor TSQLVAR.Create;
begin
  inherited;
end;

function TSQLVAR.GetSqlDef: Short;
begin
  Result := SqlType and (not 1);
end;

{ TFBSQLVARList }

function TFBSQLVARList.Add(Item: TFBSQLVAR): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TFBSQLVARList.Assign(Source: TFBSQLVARList);
var
  i: Integer;
begin
  i := 0;
  while (i < Count) and (i < Source.Count) do
  begin
    Items[i].Assign(Source[i]);
  end;
end;

function TFBSQLVARList.GetItem(Index: Integer): TFBSQLVAR;
begin
  Result := inherited Items[Index];
end;

procedure TFBSQLVARList.SetItem(Index: Integer; const Value: TFBSQLVAR);
begin
  inherited Items[Index] := Value;
end;

end.

