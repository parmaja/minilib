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
  mnFields,
  mncConnections,
  mncFBHeader, mncFBTypes, mncFBUtils, mncFBErrors, mncFBStrings, mncFBClient, mncFBBlob;

type

  TFBDSQLTypes = (SQLUnknown, SQLSelect, SQLInsert, SQLUpdate, SQLDelete,
    SQLDDL, SQLGetSegment, SQLPutSegment,
    SQLExecProcedure, SQLStartTransaction, SQLCommit, SQLRollback,
    SQLSelectForUpdate, SQLSetSequence, SQLSavePoint);

  TFBSQLDA = class;

  TFBSQLVAR = class(TmnCustomField) //Object to store pointer of Fields elements
  private
    FIndex: Integer;
    FModified: Boolean;
    FName: string;
    FMaxLen: Short; { length of data buffer }
    FXSQLVAR: PXSQLVAR;
    function GetSqlDef: Short;
  protected
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
    procedure SetAliasName(const AValue: string);
    procedure SetOwnName(const AValue: string);
    procedure SetRelName(const AValue: string);
    procedure SetSqlName(const AValue: string);
    procedure SetSqlData(const AValue: PAnsiChar);
    procedure SetSqlInd(const AValue: PShort);
    procedure SetSqlLen(const AValue: Short);
    procedure SetSqlPrecision(const AValue: Short);
    procedure SetSqlScale(const AValue: Short);
    procedure SetSqlSubtype(const AValue: Short);
    procedure SetSqlType(const AValue: Short);
  public
    procedure SetDataSize(oldsize, newsize: Integer);
    procedure SetIndSize(oldsize, newsize: Integer);
    property XSQLVar: PXSQLVAR read FXSQLVar write FXSQLVar;

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


  protected
    function GetAsCurrency: Currency; override;
    function GetAsInt64: Int64; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsDouble: Double; override;
    function GetAsFloat: Double;
    function GetAsLong: Long;
    function GetAsPointer: Pointer;
    function GetAsQuad: TISC_QUAD;
    function GetAsShort: Short;
    function GetAsString: string; override;
    function GetAsVariant: Variant;
    function GetIsNull: Boolean; override;
    function GetIsNullable: Boolean;

    function GetSize: Integer;
    procedure SetAsCurrency(const AValue: Currency); override;
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsDate(const AValue: TDateTime); override;
    procedure SetAsTime(const AValue: TDateTime); override;
    procedure SetAsDateTime(const AValue: TDateTime); override;
    procedure SetAsDouble(const AValue: Double); override;
    procedure SetAsFloat(const AValue: Double);
    procedure SetAsLong(const AValue: Long);
    procedure SetAsPointer(const AValue: Pointer);
    procedure SetAsQuad(const AValue: TISC_QUAD);
    procedure SetAsShort(const AValue: Short);
    procedure SetAsString(const AValue: string); override;

    procedure SetAsVariant(const AValue: Variant);
    procedure SetIsNull(const AValue: Boolean); override;
    procedure SetIsNullable(const AValue: Boolean);
    procedure SetAsStrip(const AValue: string);
    function GetAsStrip: string;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetName(const AValue: string);
    procedure SetSQLVAR(const AValue: TFBSQLVAR);
    function GetAsGUID: TGUID;
    procedure SetAsGUID(const AValue: TGUID);
    procedure SetModified(const AValue: Boolean);
    function GetAsText: string;
    procedure SetAsText(const AValue: string);
    procedure SetAsNullString(const AValue: string);
    function Call(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
  public
    destructor Destroy; override;

    procedure Assign(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE; Source: TFBSQLVAR);
    procedure SetBuffer(Buffer: Pointer; Size: Integer); //zaher
    {procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromIStream(Stream: IStreamPersist);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToIStream(Stream: IStreamPersist);}
    function CreateReadBlobSteam(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE): TFBBlobStream;
    function CreateWriteBlobSteam(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE): TFBBlobStream;
    procedure Clear; override;

    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsFloat: Double read GetAsFloat write SetAsFloat;

    property AsID: Int64 read GetAsInt64 write SetAsInt64; //More flixable names
    property AsLong: Long read GetAsLong write SetAsLong;
    property AsPointer: Pointer read GetAsPointer write SetAsPointer;
    property AsQuad: TISC_QUAD read GetAsQuad write SetAsQuad;
    property AsShort: Short read GetAsShort write SetAsShort;
    property AsText: string read GetAsText write SetAsText; //binary blob not text blob will convert to hex
    property AsStrip: string read GetAsStrip write SetAsStrip;
    property AsGUID: TGUID read GetAsGUID write SetAsGUID;

    property IsNullable: Boolean read GetIsNullable write SetIsNullable;
    property Index: Integer read FIndex;
    property Modified: Boolean read FModified write SetModified;
    property Size: Integer read GetSize;
    property Name: string read FName write SetName; deprecated;
  end;


  TFBSQLField = class(TmncField)
  end;

  TFBSQLParam = class(TmncParam)
  end;

  { TFBSQLFields }

  TFBSQLFields = class(TmncFields)
  private
    function GetItem(Index: Integer): TFBSQLField;
    procedure SetItem(Index: Integer; const AValue: TFBSQLField);
  protected
    constructor Create(vColumns: TmncColumns); override;
    destructor Destroy; override;
  end;

  { TFBSQLParams }

  TFBSQLParams = class(TmncParams)
  private
    function GetItem(Index: Integer): TFBSQLParam;
    procedure SetItem(Index: Integer; const AValue: TFBSQLParam);
  protected
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TFBSQLDA }

  TFBSQLDA = class(TmnFields)
  private
    function GetItem(Index: Integer): TFBSQLVAR;
    procedure SetItem(Index: Integer; const AValue: TFBSQLVAR);
    function GetField(Index: string): TFBSQLVAR;
    //procedure ChangeCount(const AValue: Integer);
  protected
    FData: PXSQLDA;
    function GetModified: Boolean;
    function GetNames: string;
    function GetRecordSize: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize;
    property Modified: Boolean read GetModified;
    property Names: string read GetNames;
    property RecordSize: Integer read GetRecordSize;
    property Field[Index: string]: TFBSQLVAR read GetField; default;
    property Items[Index: Integer]: TFBSQLVAR read GetItem write SetItem;
    property Data: PXSQLDA read FData;
  end;

implementation

{ TFieldHelper }


{ TFBSQLParams }

function TFBSQLParams.GetItem(Index: Integer): TFBSQLParam;
begin
  Result := (inherited Items[Index]) as TFBSQLParam;
end;

procedure TFBSQLParams.SetItem(Index: Integer; const AValue: TFBSQLParam);
begin
  (inherited Items[Index]) := AValue;
end;

constructor TFBSQLParams.Create;
begin
  inherited;
end;

destructor TFBSQLParams.Destroy;
begin
  inherited Destroy;
end;

{ TFBSQLFields }

function TFBSQLFields.GetItem(Index: Integer): TFBSQLField;
begin
  Result := (inherited Items[Index]) as TFBSQLField;
end;

procedure TFBSQLFields.SetItem(Index: Integer; const AValue: TFBSQLField);
begin
  (inherited Items[Index]) := AValue;
end;

constructor TFBSQLFields.Create(vColumns: TmncColumns);
begin
  inherited;
end;

destructor TFBSQLFields.Destroy;
begin
  inherited Destroy;
end;

{ TFBSQLVAR }

function TFBSQLVAR.CreateReadBlobSteam(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE): TFBBlobStream;
begin
  Result := TFBBlobStream.Create(DBHandle, TRHandle);
  try
    Result.Mode := bmRead;
    Result.BlobID := AsQuad;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TFBSQLVAR.CreateWriteBlobSteam(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE): TFBBlobStream;
begin
  Result := TFBBlobStream.Create(DBHandle, TRHandle);
  try
    Result.Mode := bmWrite;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TFBSQLVAR.Assign(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE; Source: TFBSQLVAR);
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
      Clear
    else if (Self.SqlDef = SQL_ARRAY) or (Source.SqlDef = SQL_ARRAY) then
      { arrays not supported }
    else if (Self.sqlDef <> SQL_BLOB) and (Source.SqlDef <> SQL_BLOB) then
    begin
      Value := Source.Value;
    end
    else
    begin
      if (Source.SqlDef <> SQL_BLOB) then
      begin
        szBuff := nil;
        FBAlloc(szBuff, 0, Source.sqllen);
        if (Source.SqlDef = SQL_TEXT) or (Source.SqlDef = SQL_VARYING) then
        begin
          iSize := FBClient.isc_vax_integer(Source.sqldata, 2);
          Move(Source.sqldata[2], szBuff[0], iSize)
        end
        else
        begin
          iSize := Source.sqllen;
          Move(Source.sqldata[0], szBuff[0], iSize);
        end;
        bSourceBlob := False;
      end
      else if (Self.SqlDef <> SQL_BLOB) then
        bDestBlob := False;

      if bSourceBlob then
      begin
        { read the blob }
        Call(FBClient.isc_open_blob2(@StatusVector, DBHandle, TRHandle, @s_bhandle, PISC_QUAD(Source.sqldata),
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
        Call(FBClient.isc_create_blob2(@StatusVector, DBHandle, TRHandle, @d_bhandle, PISC_QUAD(Self.sqldata),
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
        Self.sqltype := SQL_TEXT;
        oldSize := Self.sqllen;
        if iSize  > FMaxLen then
          Self.sqllen := FMaxLen
        else
          Self.sqllen := iSize;
        Self.SetDataSize(oldSize, Self.sqllen + 1);
        Move(szBuff[0], Self.sqldata[0], Self.sqllen);
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
    case Self.SqlDef of
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
        Result := FBScaleCurrency(Int64(PShort(Self.sqldata)^), Self.sqlscale);
      SQL_LONG:
        Result := FBScaleCurrency(Int64(PLong(Self.sqldata)^), Self.sqlscale);
      SQL_INT64:
        Result := FBScaleCurrency(PInt64(Self.sqldata)^, Self.sqlscale);
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
    case Self.SqlDef of
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
        Result := FBScaleInt64(Int64(PShort(Self.sqldata)^),
          Self.sqlscale);
      SQL_LONG:
        Result := FBScaleInt64(Int64(PLong(Self.sqldata)^),
          Self.sqlscale);
      SQL_INT64:
        Result := FBScaleInt64(PInt64(Self.sqldata)^,
          Self.sqlscale);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := Trunc(AsDouble);
      SQL_BOOLEAN:
        case PShort(Self.sqldata)^ of
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
    case Self.SqlDef of
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
          FBClient.isc_decode_sql_date(PISC_DATE(Self.sqldata), @tm_date);
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
          FBClient.isc_decode_sql_time(PISC_TIME(Self.sqldata), @tm_date);
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
          FBClient.isc_decode_date(PISC_QUAD(Self.sqldata), @tm_date);
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
    case Self.SqlDef of
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
        Result := FBScaleDouble(Int64(PShort(Self.sqldata)^), Self.sqlscale);
      SQL_LONG:
        Result := FBScaleDouble(Int64(PLong(Self.sqldata)^), Self.sqlscale);
      SQL_INT64:
        Result := FBScaleDouble(PInt64(Self.sqldata)^, Self.sqlscale);
      SQL_FLOAT:
        Result := PFloat(Self.sqldata)^;
      SQL_DOUBLE, SQL_D_FLOAT:
        Result := PDouble(Self.sqldata)^;
      SQL_BOOLEAN:
        case PShort(Self.sqldata)^ of
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
    case Self.SqlDef of
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
        Result := Trunc(FBScaleDouble(Int64(PShort(Self.sqldata)^),
          Self.sqlscale));
      SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP, SQL_LONG:
        Result := Trunc(FBScaleDouble(Int64(PLong(Self.sqldata)^),
          Self.sqlscale));
      SQL_INT64:
        Result := Trunc(FBScaleDouble(PInt64(Self.sqldata)^, Self.sqlscale));
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := Trunc(AsDouble);
      SQL_BOOLEAN:
        case PShort(Self.sqldata)^ of
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
    Result := Self.sqldata
  else
    Result := nil;
end;

function TFBSQLVAR.GetAsQuad: TISC_QUAD;
begin
  Result.gds_quad_high := 0;
  Result.gds_quad_low := 0;
  if not IsNull then
    case Self.SqlDef of
      SQL_BLOB, SQL_ARRAY, SQL_QUAD:
        Result := PISC_QUAD(Self.sqldata)^;
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
    case Self.SqlDef of
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
          sz := Self.sqldata;
          if (Self.SqlDef = SQL_TEXT) then
            str_len := Self.sqllen
          else
          begin
            str_len := FBClient.isc_vax_integer(Self.sqldata, 2);
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
        if Self.sqlscale = 0 then
          Result := IntToStr(AsLong)
        else if Self.sqlscale >= (-4) then
          Result := CurrToStr(AsCurrency)
        else
          Result := FloatToStr(AsDouble);
      SQL_INT64:
        if Self.sqlscale = 0 then
          Result := IntToStr(AsInt64)
        else if Self.sqlscale >= (-4) then
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
    case Self.SqlDef of
      SQL_ARRAY:
        Result := '(Array)';
      SQL_BLOB:
        begin
          if Self.SqlSubtype = 1 then
            Result := AsString
          else
            Result := '(Blob)';
        end;
      SQL_TEXT, SQL_VARYING:
        Result := AsString;
      SQL_TIMESTAMP, SQL_TYPE_DATE, SQL_TYPE_TIME:
        Result := AsDateTime;
      SQL_SHORT, SQL_LONG:
        if Self.sqlscale = 0 then
          Result := AsLong
        else if Self.sqlscale >= (-4) then
          Result := AsCurrency
        else
          Result := AsDouble;
      SQL_INT64:
        if Self.sqlscale = 0 then
          Result := AsINT64
        else if Self.sqlscale >= (-4) then
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
  Result := IsNullable and (Self.sqlind^ = -1);
end;

function TFBSQLVAR.GetIsNullable: Boolean;
begin
  Result := (Self.sqltype and 1 = 1);
end;

{procedure TFBSQLVAR.SaveToIStream(Stream: IStreamPersist);
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
end;}

function TFBSQLVAR.GetSize: Integer;
begin
  Result := Self.sqllen;
end;

procedure TFBSQLVAR.SetAsCurrency(const AValue: Currency);
begin
  if IsNullable then
    IsNull := False;
  Self.sqltype := SQL_INT64 or (Self.sqltype and 1);
  Self.sqlscale := -4;
  Self.sqllen := SizeOf(Int64);
  Self.SetDataSize(0, Self.sqllen);
  PCurrency(Self.sqldata)^ := AValue;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsInt64(const AValue: Int64);
begin
  if IsNullable then
    IsNull := False;
  Self.sqltype := SQL_INT64 or (Self.sqltype and 1);
  Self.sqlscale := 0;
  Self.sqllen := SizeOf(Int64);
  Self.SetDataSize(0, Self.sqllen);
  PInt64(Self.sqldata)^ := AValue;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsDate(const AValue: TDateTime);
var
  tm_date: TCTimeStructure;
  Yr, Mn, Dy: Word;
begin
  if IsNullable then
    IsNull := False;
  Self.sqltype := SQL_TYPE_DATE or (Self.sqltype and 1);
  DecodeDate(AValue, Yr, Mn, Dy);
  with tm_date do
  begin
    tm_sec := 0;
    tm_min := 0;
    tm_hour := 0;
    tm_mday := Dy;
    tm_mon := Mn - 1;
    tm_year := Yr - 1900;
  end;
  Self.sqllen := SizeOf(ISC_DATE);
  Self.SetDataSize(0, Self.sqllen);
  FBClient.isc_encode_sql_date(@tm_date, PISC_DATE(Self.sqldata));
  Modified := True;
end;

procedure TFBSQLVAR.SetAsTime(const AValue: TDateTime);
var
  tm_date: TCTimeStructure;
  Hr, Mt, S, Ms: Word;
begin
  if IsNullable then
    IsNull := False;
  Self.sqltype := SQL_TYPE_TIME or (Self.sqltype and 1);
  DecodeTime(AValue, Hr, Mt, S, Ms);
  with tm_date do
  begin
    tm_sec := S;
    tm_min := Mt;
    tm_hour := Hr;
    tm_mday := 0;
    tm_mon := 0;
    tm_year := 0;
  end;
  Self.sqllen := SizeOf(ISC_TIME);
  Self.SetDataSize(0, Self.sqllen);
  FBClient.isc_encode_sql_time(@tm_date, PISC_TIME(Self.sqldata));
  Modified := True;
end;

procedure TFBSQLVAR.SetAsDateTime(const AValue: TDateTime);
var
  tm_date: TCTimeStructure;
  Yr, Mn, Dy, Hr, Mt, S, Ms: Word;
begin
  if IsNullable then
    IsNull := False;
  Self.sqltype := SQL_TIMESTAMP or (Self.sqltype and 1);
  DecodeDate(AValue, Yr, Mn, Dy);
  DecodeTime(AValue, Hr, Mt, S, Ms);
  with tm_date do
  begin
    tm_sec := S;
    tm_min := Mt;
    tm_hour := Hr;
    tm_mday := Dy;
    tm_mon := Mn - 1;
    tm_year := Yr - 1900;
  end;
  Self.sqllen := SizeOf(TISC_QUAD);
  Self.SetDataSize(0, Self.sqllen);
  FBClient.isc_encode_date(@tm_date, PISC_QUAD(Self.sqldata));
  Modified := True;
end;

procedure TFBSQLVAR.SetAsDouble(const AValue: Double);
begin
  if IsNullable then
    IsNull := False;
  Self.sqltype := SQL_DOUBLE or (Self.sqltype and 1);
  Self.sqllen := SizeOf(Double);
  Self.sqlscale := 0;
  Self.SetDataSize(0, Self.sqllen);
  PDouble(Self.sqldata)^ := AValue;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsFloat(const AValue: Double);
begin
  if IsNullable then
    IsNull := False;
  Self.sqltype := SQL_FLOAT or (Self.sqltype and 1);
  Self.sqllen := SizeOf(Float);
  Self.sqlscale := 0;
  Self.SetDataSize(0, Self.sqllen);
  PSingle(Self.sqldata)^ := AValue;//TODO why Single
  Modified := True;
end;

procedure TFBSQLVAR.SetAsLong(const AValue: Long);
begin
  if IsNullable then
    IsNull := False;
  Self.sqltype := SQL_LONG or (Self.sqltype and 1);
  Self.sqllen := SizeOf(Long);
  Self.sqlscale := 0;
  Self.SetDataSize(0, Self.sqllen);
  PLong(Self.sqldata)^ := AValue;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsNullString(const AValue: string);
begin
  if AValue = '' then
    Clear
  else
    AsString := AValue;
end;

procedure TFBSQLVAR.SetAsPointer(const AValue: Pointer);
begin
  if IsNullable and (AValue = nil) then
    IsNull := True
  else
  begin
    IsNull := False;
    Self.sqltype := SQL_TEXT or (Self.sqltype and 1);
    Move(AValue^, Self.sqldata^, Self.sqllen);
    Modified := True;
  end;
end;

procedure TFBSQLVAR.SetAsQuad(const AValue: TISC_QUAD);
begin
  if IsNullable then
    IsNull := False;
  if (Self.SqlDef <> SQL_BLOB) and
    (Self.SqlDef <> SQL_ARRAY) then
    FBRaiseError(fbceInvalidDataConversion, [nil]);
  Self.sqllen := SizeOf(TISC_QUAD);
  Self.SetDataSize(0, Self.sqllen);
  PISC_QUAD(Self.sqldata)^ := AValue;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsShort(const AValue: Short);
begin
  if IsNullable then
    IsNull := False;
  Self.sqltype := SQL_SHORT or (Self.sqltype and 1);
  Self.sqllen := SizeOf(Short);
  Self.sqlscale := 0;
  Self.SetDataSize(0, Self.sqllen);
  PShort(Self.sqldata)^ := AValue;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsString(const AValue: string);
var
  stype: Integer;
  ss: TStringStream;

  procedure SetStringValue;
  var
    l: Integer;
  begin
    if (Self.sqlname = 'DB_KEY') or (Self.sqlname = 'RDB$DB_KEY') then
      Move(AValue[1], Self.sqldata^, Self.sqllen)
    else
    begin
      Self.sqltype := SQL_TEXT or (Self.sqltype and 1);
      l := Length(AValue);
      if (FMaxLen > 0) and (l > FMaxLen) then
        l := FMaxLen;
      Self.sqllen := l;
      Self.SetDataSize(0, Self.sqllen + 1);
      if (Length(AValue) > 0) then
        Move(AValue[1], Self.sqldata^, Self.sqllen);
    end;
    Modified := True;
  end;
begin
  if IsNullable then
    IsNull := False;
  stype := Self.SqlDef;
  if (stype = SQL_TEXT) or (stype = SQL_VARYING) then
    SetStringValue
  else
  begin
    if (stype = SQL_BLOB) then
    begin
      if AValue = '' then
        IsNull := True
      else
      begin
        ss := TStringStream.Create(AValue);
        try
          LoadFromStream(ss);
        finally
          ss.Free;
        end;
      end;
    end
    else if (AValue = '') then
      IsNull := True
    else if (stype = SQL_TIMESTAMP) or (stype = SQL_TYPE_DATE) or (stype = SQL_TYPE_TIME) then
      SetAsDateTime(StrToDateTime(AValue))
    else
      SetStringValue;
  end;
end;

procedure TFBSQLVAR.SetAsVariant(const AValue: Variant);
begin
  if VarIsNull(AValue) then
    IsNull := True
  else
    case VarType(AValue) of
      varEmpty, varNull:
        IsNull := True;
      varSmallint, varInteger, varByte, varShortInt, varWord, varLongWord:
        AsLong := AValue;
      varSingle, varDouble:
        AsDouble := AValue;
      varCurrency:
        AsCurrency := AValue;
      varBoolean:
        if AValue then
          AsBoolean := true
        else
          AsBoolean := false;
      varDate:
        AsDateTime := AValue;
      varOleStr, varString:
        AsString := AValue;
      varArray:
        FBRaiseError(fbceNotSupported, [nil]);
      varByRef, varDispatch, varError, varUnknown, varVariant:
        FBRaiseError(fbceNotPermitted, [nil]);
      varInt64:
        AsInt64 := AValue;
    else
      FBRaiseError(fbceNotSupported, [nil]);
    end;
end;

procedure TFBSQLVAR.SetIsNull(const AValue: Boolean);
begin
  if AValue then
  begin
    inherited; //call the clear
  end
  else if ((not AValue) and IsNullable) then
  begin
    if Assigned(Self.sqlind) then
      Self.sqlind^ := 0;
    Modified := True;
  end;
end;

procedure TFBSQLVAR.SetIsNullable(const AValue: Boolean);
begin
  if (AValue <> IsNullable) then
  begin
    if AValue then
    begin
      Self.sqltype := Self.sqltype or 1;
      Self.SetIndSize(0, SizeOf(Short));
    end
    else
    begin
      Self.sqltype := Self.SqlDef;
      Self.SetIndSize(0, 0);
    end;
  end;
end;

procedure TFBSQLVAR.Clear;
begin
  if not IsNullable then
    IsNullable := True;
  if Assigned(Self.sqlind) then
    Self.sqlind^ := -1;
  Modified := True;
end;

procedure TFBSQLVAR.SetAsStrip(const AValue: string);
begin
  if AValue = '' then
    Clear
  else
    SetAsString(TrimRight(AValue));
end;

function TFBSQLVAR.GetAsStrip: string;
begin
  Result := TrimRight(GetAsString);
end;

function TFBSQLVAR.GetAsBoolean: Boolean;
begin
  Result := false;
  if not IsNull then
    case Self.SqlDef of
      SQL_INT64: Result := PInt64(Self.sqldata)^ <> ISC_FALSE;
      SQL_LONG: Result := PLong(Self.sqldata)^ <> ISC_FALSE;
      SQL_SHORT, SQL_BOOLEAN:
        Result := PShort(Self.sqldata)^ <> ISC_FALSE
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

procedure TFBSQLVAR.SetAsBoolean(const AValue: Boolean);
begin
  if IsNullable then
    IsNull := False;
  if AValue then
    PShort(Self.sqldata)^ := ISC_TRUE
  else
    PShort(Self.sqldata)^ := ISC_FALSE;
end;

procedure TFBSQLVAR.SetName(const AValue: string);
begin
  if FName =AValue then exit;
    FName :=AValue;
end;

procedure TFBSQLVAR.SetSQLVAR(const AValue: TFBSQLVAR);
var
  local_sqlind: PShort;
  local_sqldata: PAnsiChar;
  local_sqllen: Integer;
begin
  local_sqlind := Self.sqlind;
  local_sqldata := Self.sqldata;
  move(TFBSQLVAR(AValue).FXSQLVAR^, TFBSQLVAR(Self).FXSQLVAR^, sizeof(TXSQLVAR));
  Self.sqlind := local_sqlind;
  Self.sqldata := local_sqldata;
  if (AValue.sqltype and 1 = 1) then
  begin
    if (Self.sqlind = nil) then
      Self.SetIndSize(0, SizeOf(Short));
    Self.sqlind^ := AValue.sqlind^;
  end
  else if (Self.sqlind <> nil) then
    Self.SetIndSize(0, 0);
  if ((Self.SqlDef) = SQL_VARYING) then
    local_sqllen := Self.sqllen + 2
  else
    local_sqllen := Self.sqllen;
  Self.sqlscale := AValue.sqlscale;
  Self.SetDataSize(0, local_sqllen);
  Move(AValue.sqldata[0], Self.sqldata[0], local_sqllen);
  Modified := True;
end;

destructor TFBSQLVAR.Destroy;
begin
  FreeMem(Self.sqldata);
  FreeMem(Self.sqlind);
  FreeAndNil(Self);
  inherited;
end;

function TFBSQLVAR.Call(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := FBCall(ErrCode, StatusVector, RaiseError);
end;

procedure TFBSQLVAR.SetModified(const AValue: Boolean);
begin
  FModified := AValue;
end;

function TFBSQLVAR.GetAsText: string;
begin
  if (Self.SqlDef = SQL_BLOB) and (Self.SqlSubtype <> 1) then
    Result := AsHex
  else
    Result := AsString;
end;

procedure TFBSQLVAR.SetAsText(const AValue: string);
begin
  if (Self.SqlDef = SQL_BLOB) and (Self.SqlSubtype <> 1) then
    AsHex := AValue
  else
    AsString := AValue;
end;

procedure TFBSQLVAR.SetBuffer(Buffer: Pointer; Size: Integer);
var
  sz: PAnsiChar;
  len: Integer;
begin
  sz := Self.sqldata;
  if (Self.SqlDef = SQL_TEXT) then
    len := Self.sqllen
  else
  begin
    len := FBClient.isc_vax_integer(Self.sqldata, 2);
    Inc(sz, 2);
  end;
  if (Size <> 0) and (len > Size) then
    len := Size;
  Move(sz^, Buffer^, len);
end;

function TFBSQLVAR.GetAsGUID: TGUID;
begin

end;

procedure TFBSQLVAR.SetAsGUID(const AValue: TGUID);
begin

end;

{ TFBSQLDA }

constructor TFBSQLDA.Create;
begin
  inherited;
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

procedure TFBSQLDA.Initialize;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    with Items[i] do
    begin
      if Items[i].Name = '' then
      begin
        if AliasName = '' then
          AliasName := 'F_' + IntToStr(i);
        Items[i].Name := FBDequoteName(aliasname);
      end;

      if (SqlDef = SQL_TEXT) or (SqlDef = SQL_VARYING) then
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
              { Make sure you get a valid pointer, select '' from table }
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

{procedure TFBSQLDA.ChangeCount(const AValue: Integer);
var
  i: Integer;
  XSQLVar_Size: Integer;
  p: Pointer;
  OldCount: Integer;
begin
  OldCount := Count;
  if AValue <> 0 then
  begin
    if AValue <> Count then
    begin
      if AValue < Count then
      begin
        for i := AValue to Count - 1 do
        begin
          Items[i].Free;
        end;
      end;
      FBAlloc(FData, XSQLDA_LENGTH(Count), XSQLDA_LENGTH(AValue));
    end;
    ChangeCount(AValue);
    FData.version := SQLDA_VERSION1;
    XSQLVar_Size := sizeof(TXSQLVAR);
    p := @FData^.sqlvar[0];
    for i := 0 to Count - 1 do
    begin
      if i >= OldCount then
      begin
        Items[i] := TFBSQLVAR.Create(Columns);
        Items[i].FIndex := i;
        Items[i].Self.XSqlVar := p;
        Items[i].Clear;
      end
      else
        Items[i].Self.XSqlVar := p;
      p := Pointer(PAnsiChar(p) + XSQLVar_Size);
    end;
    if Count > 0 then
    begin
      FData^.sqln := AValue;
      FData^.sqld := AValue;
    end;
  end;
end;}

function TFBSQLDA.GetItem(Index: Integer): TFBSQLVAR;
begin
  Result := (inherited Items[Index]) as TFBSQLVAR;
end;

procedure TFBSQLDA.SetItem(Index: Integer; const AValue: TFBSQLVAR);
begin
  (inherited Items[Index]) := AValue;
end;


function TFBSQLDA.GetField(Index: string): TFBSQLVAR;
begin
  //Result := (inherited Field[Index]) as TFBSQLVAR;
end;

function TFBSQLVAR.GetAliasName: string;
begin
  Result := '';
  SetString(Result, FXSQLVAR.aliasname, FXSQLVAR.aliasname_length);
end;

function TFBSQLVAR.GetOwnName: string;
begin
  Result := '';
  SetString(Result, FXSQLVAR.ownname, FXSQLVAR.ownname_length);
end;

function TFBSQLVAR.GetRelName: string;
begin
  Result := '';
  SetString(Result, FXSQLVAR.relname, FXSQLVAR.relname_length);
end;

function TFBSQLVAR.GetSqlData: PAnsiChar;
begin
  Result := FXSQLVAR.sqldata;
end;

function TFBSQLVAR.GetSqlInd: PShort;
begin
  Result := FXSQLVAR.sqlind;
end;

function TFBSQLVAR.GetSqlLen: Short;
begin
  Result := FXSQLVAR.sqllen;
end;

function TFBSQLVAR.GetSqlName: string;
begin
  Result := '';
  SetString(Result, FXSQLVAR.sqlname, FXSQLVAR.sqlname_length);
end;

function TFBSQLVAR.GetSqlPrecision: Short;
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

function TFBSQLVAR.GetSqlScale: Short;
begin
  Result := FXSQLVAR.sqlscale;
end;

function TFBSQLVAR.GetSqlSubtype: Short;
begin
  Result := FXSQLVAR.sqlsubtype;
end;

function TFBSQLVAR.GetSqlType: Short;
begin
  Result := FXSQLVAR.sqltype;
end;

procedure TFBSQLVAR.SetAliasName(const AValue: string);
begin
  StrPCopy(FXSQLVAR^.aliasname, AValue);
  FXSQLVAR^.aliasname_length := Length(AValue);
end;

procedure TFBSQLVAR.SetDataSize(oldsize, newsize: Integer);
begin
  FBAlloc(FXSQLVAR^.sqldata, oldsize, newsize);
end;

procedure TFBSQLVAR.SetIndSize(oldsize, newsize: Integer);
begin
  FBAlloc(FXSQLVAR^.sqlind, oldsize, newsize);
end;

procedure TFBSQLVAR.SetOwnName(const AValue: string);
begin
  StrPCopy(FXSQLVAR^.ownname, AValue);
  FXSQLVAR^.ownname_length := Length(AValue);
end;

procedure TFBSQLVAR.SetRelName(const AValue: string);
begin
  StrPCopy(FXSQLVAR^.relname, AValue);
  FXSQLVAR^.relname_length := Length(AValue);
end;

procedure TFBSQLVAR.SetSqlData(const AValue: PAnsiChar);
begin
  FXSQLVAR.sqldata := AValue;
end;

procedure TFBSQLVAR.SetSqlInd(const AValue: PShort);
begin
  FXSQLVAR.sqlInd := AValue
end;

procedure TFBSQLVAR.SetSqlLen(const AValue: Short);
begin
  FXSQLVAR.sqlLen := AValue
end;

procedure TFBSQLVAR.SetSqlName(const AValue: string);
begin
  StrPCopy(FXSQLVAR^.sqlname, AValue);
  FXSQLVAR^.sqlname_length := Length(AValue);
end;

procedure TFBSQLVAR.SetSqlPrecision(const AValue: Short);
begin
  FBRaiseError(fbceNotSupported, []);
end;

procedure TFBSQLVAR.SetSqlScale(const AValue: Short);
begin
  FXSQLVAR.sqlscale := AValue
end;

procedure TFBSQLVAR.SetSqlSubtype(const AValue: Short);
begin
  FXSQLVAR.sqlsubtype := AValue
end;

procedure TFBSQLVAR.SetSqlType(const AValue: Short);
begin
  FXSQLVAR.sqltype := AValue
end;

function TFBSQLVAR.GetSqlDef: Short;
begin
  Result := SqlType and (not 1);
end;

end.

