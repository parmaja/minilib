{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$M+}
{$H+}

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
  mncFBHeader, mncFBTypes, mncFBUtils, mncFBErrors, mncFBClient, mncFBBlob;

type

  TFBDSQLTypes = (SQLUnknown, SQLSelect, SQLInsert, SQLUpdate, SQLDelete,
    SQLDDL, SQLGetSegment, SQLPutSegment,
    SQLExecProcedure, SQLStartTransaction, SQLCommit, SQLRollback,
    SQLSelectForUpdate, SQLSetSequence, SQLSavePoint);

  { TmncSQLVAR }

  TmncSQLVAR = class(TObject)
  private
    FXSQLVAR: PXSQLVAR;
    FIgnored: Boolean;
    FModified: Boolean;
    FMaxLen: Short;
    function GetSqlDef: Short;
  protected
    function GetSQLVAR: PXSQLVAR;
    procedure SetSQLVAR(const AValue: PXSQLVAR);
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
    procedure UpdateData(OldSize, NewSize: Integer);
    procedure UpdateSQLInd;
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
    property Ignored: Boolean read FIgnored write FIgnored; //used to manual Ignored blob fields
  private
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
    procedure SetAsCurrency(AValue: Currency);
    procedure SetAsInt64(AValue: Int64);
    procedure SetAsDate(AValue: TDateTime);
    procedure SetAsLong(AValue: Integer);
    procedure SetAsTime(AValue: TDateTime);
    procedure SetAsDateTime(AValue: TDateTime);
    procedure SetAsDouble(AValue: Double);
    procedure SetAsFloat(AValue: Double);
    procedure SetAsPointer(AValue: Pointer);
    procedure SetAsQuad(AValue: TISC_QUAD);
    procedure SetAsShort(AValue: Short);
    procedure SetAsString(AValue: string);
    procedure SetAsVariant(AValue: Variant);
    procedure SetIsNull(AValue: Boolean);
    procedure SetIsNullable(AValue: Boolean);
    procedure SetAsStrip(const AValue: string);
    function GetAsStrip: string;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const AValue: Boolean);
    function GetAsGUID: TGUID;
    procedure SetAsGUID(const AValue: TGUID);
    procedure SetModified(const AValue: Boolean);
    function GetAsHex: string;
    procedure SetAsHex(const AValue: string);
    function GetAsText: string;
    procedure SetAsText(const AValue: string);
    procedure SetAsNullString(const AValue: string);
    function GetAsChar: Char;
    procedure SetAsChar(const AValue: Char);
  protected
    FDBHandle: PISC_DB_HANDLE;
    FTRHandle: PISC_TR_HANDLE;
    procedure CheckHandles;
  public
    constructor Create;
    destructor Destroy; override;
    { IInterface }
    function QueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF MSWINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF MSWINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF MSWINDOWS}cdecl{$ELSE}stdcall{$ENDIF};

    procedure Attach(vDBHandle: PISC_DB_HANDLE; vTRHandle: PISC_TR_HANDLE);
    procedure Detach;
    procedure Assign(Source: TmncSQLVAR);
    procedure Prepare;
    procedure Clear;
    procedure SetBuffer(Buffer: Pointer; Size: Integer); //TODO check if used
    procedure CopySQLVAR(const AValue: TmncSQLVAR);

    function CreateReadBlobSteam: TFBBlobStream;
    function CreateWriteBlobSteam: TFBBlobStream;
    procedure LoadFromIStream(Stream: IStreamPersist);
    procedure SaveToIStream(Stream: IStreamPersist);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property Modified: Boolean read FModified write SetModified;
    property Size: Integer read GetSize;
    property MaxLen: Short read FMaxLen write FMaxLen;

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
    property AsChar: Char read GetAsChar write SetAsChar;
    property AsNullString: string read GetAsString write SetAsNullString;
    property AsHex: string read GetAsHex write SetAsHex;
    property AsText: string read GetAsText write SetAsText; //binary blob not text will convert to hex
    property AsTrimString: string read GetAsStrip write SetAsStrip;
    property AsStrip: string read GetAsStrip write SetAsStrip;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AValue: Variant read GetAsVariant write SetAsVariant;
    property AsGUID: TGUID read GetAsGUID write SetAsGUID;

    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsNullable: Boolean read GetIsNullable write SetIsNullable;

  end;

procedure FreeSQLDA(var Data: PXSQLDA; Clean: Boolean = True);
procedure InitSQLDA(var Data: PXSQLDA; New: Integer; Clean: Boolean = True);

implementation

procedure InitSQLDA(var Data: PXSQLDA; New: Integer; Clean: Boolean = True);
var
  old: Integer;
var
  p: PXSQLVAR;
  i: Integer;
begin
  if Data = nil then
    old := 0
  else
    old := Data^.sqln;


  if Clean and (new < old) then
  begin
    p := @Data^.sqlvar[new];
    for i := new to old - 1 do
    begin
      FBFree(p^.sqldata);
      FBFree(p^.sqlind);
      p := Pointer(PAnsiChar(p) + XSQLVar_Size);
    end;
  end;

  FBAlloc(Data, XSQLDA_LENGTH(old), XSQLDA_LENGTH(new));
  Data^.version := SQLDA_VERSION1;
  Data^.sqln := New;
end;

procedure FreeSQLDA(var Data: PXSQLDA; Clean: Boolean = True);
var
  p: PXSQLVAR;
  i: Integer;
begin
  if Data <> nil then
  begin
    if Clean then
    begin
      p := @Data^.sqlvar[0];
      for i := 0 to Data.sqln - 1 do
      begin
        FBFree(p^.sqldata);
        FBFree(p^.sqlind);
        p := Pointer(PAnsiChar(p) + XSQLVar_Size);
      end;
    end;
    FBFree(Data);
  end;
end;

{ TmncSQLVAR }

function TmncSQLVAR.GetAliasName: string;
begin
  Result := '';
  SetString(Result, FXSQLVAR^.aliasname, FXSQLVAR^.aliasname_length);
end;

function TmncSQLVAR.GetOwnName: string;
begin
  Result := '';
  SetString(Result, FXSQLVAR^.ownname, FXSQLVAR^.ownname_length);
end;

function TmncSQLVAR.GetRelName: string;
begin
  Result := '';
  SetString(Result, FXSQLVAR^.relname, FXSQLVAR^.relname_length);
end;

function TmncSQLVAR.GetSqlData: PAnsiChar;
begin
  Result := FXSQLVAR^.sqldata;
end;

function TmncSQLVAR.GetSqlInd: PShort;
begin
  Result := FXSQLVAR^.sqlind;
end;

function TmncSQLVAR.GetSqlLen: Short;
begin
  Result := FXSQLVAR^.sqllen;
end;

function TmncSQLVAR.GetSqlName: string;
begin
  Result := '';
  SetString(Result, FXSQLVAR^.sqlname, FXSQLVAR^.sqlname_length);
end;

function TmncSQLVAR.GetSqlPrecision: Short;
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

function TmncSQLVAR.GetSqlScale: Short;
begin
  Result := FXSQLVAR^.sqlscale;
end;

function TmncSQLVAR.GetSqlSubtype: Short;
begin
  Result := FXSQLVAR^.sqlsubtype;
end;

function TmncSQLVAR.GetSqlType: Short;
begin
  Result := FXSQLVAR^.sqltype;
end;

function TmncSQLVAR.GetSqlDef: Short;
begin
  Result := SqlType and (not 1);
end;

function TmncSQLVAR.GetSQLVAR: PXSQLVAR;
begin
  Result := FXSQLVAR;
end;

procedure TmncSQLVAR.SetAliasName(const AValue: string);
begin
  StrPCopy(FXSQLVAR^.aliasname, AValue);
  FXSQLVAR^.aliasname_length := Length(AValue);
end;

procedure TmncSQLVAR.UpdateData(OldSize, NewSize: Integer);
begin
  if NewSize = 0 then
    FBFree(FXSQLVAR^.sqldata)
  else
    FBAlloc(FXSQLVAR^.sqldata, OldSize, NewSize);
end;

procedure TmncSQLVAR.UpdateSQLInd;
begin
  if IsNullable then
  begin
    if not Assigned(FXSQLVAR^.sqlind) then
      FBAlloc(FXSQLVAR^.sqlind, 0, SizeOf(Short))
  end
  else if Assigned(FXSQLVAR^.sqlind) then
    FBFree(FXSQLVAR^.sqlind);
end;

procedure TmncSQLVAR.SetOwnName(const AValue: string);
begin
  StrPCopy(FXSQLVAR^.ownname, AValue);
  FXSQLVAR^.ownname_length := Length(AValue);
end;

procedure TmncSQLVAR.SetRelName(const AValue: string);
begin
  StrPCopy(FXSQLVAR^.relname, AValue);
  FXSQLVAR^.relname_length := Length(AValue);
end;

procedure TmncSQLVAR.SetSqlData(const AValue: PAnsiChar);
begin
  FXSQLVAR^.sqldata := AValue;
end;

procedure TmncSQLVAR.SetSqlInd(const AValue: PShort);
begin
  FXSQLVAR^.sqlInd := AValue
end;

procedure TmncSQLVAR.SetSqlLen(const AValue: Short);
begin
  FXSQLVAR^.sqlLen := AValue
end;

procedure TmncSQLVAR.SetSqlName(const AValue: string);
begin
  StrPCopy(FXSQLVAR^.sqlname, AValue);
  FXSQLVAR^.sqlname_length := Length(AValue);
end;

procedure TmncSQLVAR.SetSqlPrecision(const AValue: Short);
begin
  FBRaiseError(fbceNotSupported, []);
end;

procedure TmncSQLVAR.SetSqlScale(const AValue: Short);
begin
  FXSQLVAR^.sqlscale := AValue
end;

procedure TmncSQLVAR.SetSqlSubtype(const AValue: Short);
begin
  FXSQLVAR^.sqlsubtype := AValue
end;

procedure TmncSQLVAR.SetSqlType(const AValue: Short);
begin
  FXSQLVAR^.sqltype := AValue
end;

procedure TmncSQLVAR.SetSQLVAR(const AValue: PXSQLVAR);
begin
  FXSQLVAR := AValue;
  //TODO Prepare
end;

{ TmncSQLVAR }

function TmncSQLVAR.CreateReadBlobSteam: TFBBlobStream;
begin
  Result := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    Result.Mode := bmRead;
    Result.BlobID := AsQuad;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TmncSQLVAR.CreateWriteBlobSteam: TFBBlobStream;
begin
  Result := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    Result.Mode := bmWrite;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TmncSQLVAR.LoadFromStream(Stream: TStream);
var
  bs: TFBBlobStream;
begin
  CheckHandles;
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

procedure TmncSQLVAR.SaveToFile(const FileName: string);
var
  fs: TFileStream;
begin
  CheckHandles;
  fs := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TmncSQLVAR.SaveToStream(Stream: TStream);
var
  bs: TFBBlobStream;
begin
  CheckHandles;
  bs := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    bs.Mode := bmRead;
    bs.BlobID := AsQuad;
    bs.SaveToStream(Stream);
  finally
    bs.Free;
  end;
end;

procedure TmncSQLVAR.SaveToIStream(Stream: IStreamPersist);
var
  bs: TFBBlobStream;
begin
  CheckHandles;
  bs := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    bs.Mode := bmRead;
    bs.BlobID := AsQuad;
    Stream.LoadFromStream(bs);
  finally
    bs.Free;
  end;
end;

procedure TmncSQLVAR.LoadFromIStream(Stream: IStreamPersist);
var
  bs: TFBBlobStream;
begin
  CheckHandles;
  bs := TFBBlobStream.Create(FDBHandle, FTRHandle);
  try
    bs.Mode := bmWrite;
    Stream.SaveToStream(bs);
    bs.Finalize;
    AsQuad := bs.BlobID;
  finally
    bs.Free;
  end;
end;


procedure TmncSQLVAR.LoadFromFile(const FileName: string);
var
  fs: TFileStream;
begin
  CheckHandles;
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TmncSQLVAR.Assign(Source: TmncSQLVAR);
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
    else if (SqlDef = SQL_ARRAY) or (Source.SqlDef = SQL_ARRAY) then
      { arrays not supported }
    else if (sqlDef <> SQL_BLOB) and (Source.SqlDef <> SQL_BLOB) then
    begin
      AValue := Source.AValue;
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
      else if (SqlDef <> SQL_BLOB) then
        bDestBlob := False;

      if bSourceBlob then
      begin
        { read the blob }
        FBCall(FBClient.isc_open_blob2(@StatusVector, @FDBHandle, @FTRHandle, @s_bhandle, PISC_QUAD(Source.sqldata),
          0, nil), StatusVector, True);
        try
          FBGetBlobInfo(@s_bhandle, iSegs, iMaxSeg, iSize, iBlobType);
          szBuff := nil;
          FBAlloc(szBuff, 0, iSize);
          FBReadBlob(@s_bhandle, szBuff, iSize);
        finally
          FBCall(FBClient.isc_close_blob(@StatusVector, @s_bhandle), StatusVector, True);
        end;
      end;

      if bDestBlob then
      begin
        { write the blob }
        FBCall(FBClient.isc_create_blob2(@StatusVector, @FDBHandle, @FTRHandle, @d_bhandle, PISC_QUAD(sqldata),
          0, nil), StatusVector, True);
        try
          FBWriteBlob(@d_bhandle, szBuff, iSize);
          IsNull := false;
        finally
          FBCall(FBClient.isc_close_blob(@StatusVector, @d_bhandle), StatusVector, True);
        end;
      end
      else
      begin
        { just copy the buffer }
        sqltype := SQL_TEXT;
        oldSize := sqllen;
        if iSize  > FMaxLen then
          sqllen := FMaxLen
        else
          sqllen := iSize;
        UpdateData(OldSize, sqllen + 1);
        Move(szBuff[0], sqldata[0], sqllen);
      end;
    end;
  finally
    FreeMem(szBuff);
  end;
end;

procedure TmncSQLVAR.Prepare;
begin
  {if Items[i].Name = '' then
  begin
    if AliasName = '' then
      AliasName := 'F_' + IntToStr(i);
    Items[i].Name := FBDequoteName(aliasname);
  end;}

  if (SqlDef = SQL_VARYING) or (SqlDef = SQL_TEXT) then
    FMaxLen := sqllen
  else
    FMaxLen := 0;

  if FXSQLVAR^.sqldata = nil then
    case SqlDef of
      SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP,
        SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT,
        SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT, SQL_BOOLEAN:
        begin
          if (sqllen = 0) then
          { Make sure you get a valid pointer anyway
           select '' from foo }
            UpdateData(0, 1)
          else
            UpdateData(0, sqllen)
        end;
      SQL_VARYING:
        begin
          UpdateData(0, sqllen + 2);
        end;
    else
      FBRaiseError(fbceUnknownSQLDataType, [SqlDef])
    end;
  UpdateSQLInd;
end;

function TmncSQLVAR.GetAsChar: Char;
var
  s: string;
begin
  s := AsString;
  if Length(s)>0 then
    Result := s[1]
  else
    Result := #0;
end;

function TmncSQLVAR.GetAsCurrency: Currency;
begin
  Result := 0;
  if not IsNull then
    case SqlDef of
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
        Result := FBScaleCurrency(Int64(PShort(sqldata)^), sqlscale);
      SQL_LONG:
        Result := FBScaleCurrency(Int64(PLong(sqldata)^), sqlscale);
      SQL_INT64:
        Result := FBScaleCurrency(PInt64(sqldata)^, sqlscale);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := GetAsDouble;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TmncSQLVAR.GetAsInt64: Int64;
begin
  Result := 0;
  if not IsNull then
    case SqlDef of
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
        Result := FBScaleInt64(Int64(PShort(sqldata)^),
          sqlscale);
      SQL_LONG:
        Result := FBScaleInt64(Int64(PLong(sqldata)^),
          sqlscale);
      SQL_INT64:
        Result := FBScaleInt64(PInt64(sqldata)^,
          sqlscale);
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := Trunc(AsDouble);
      SQL_BOOLEAN:
        case PShort(sqldata)^ of
          ISC_TRUE: Result := 1;
          ISC_FALSE: Result := 0;
        end;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TmncSQLVAR.GetAsDateTime: TDateTime;
var
  tm_date: TCTimeStructure;
begin
  Result := 0;
  if not IsNull then
    case SqlDef of
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
          FBClient.isc_decode_sql_date(PISC_DATE(sqldata), @tm_date);
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
          FBClient.isc_decode_sql_time(PISC_TIME(sqldata), @tm_date);
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
          FBClient.isc_decode_date(PISC_QUAD(sqldata), @tm_date);
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

function TmncSQLVAR.GetAsDouble: Double;
begin
  Result := 0;
  if not IsNull then
  begin
    case SqlDef of
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
        Result := FBScaleDouble(Int64(PShort(sqldata)^), sqlscale);
      SQL_LONG:
        Result := FBScaleDouble(Int64(PLong(sqldata)^), sqlscale);
      SQL_INT64:
        Result := FBScaleDouble(PInt64(sqldata)^, sqlscale);
      SQL_FLOAT:
        Result := PFloat(sqldata)^;
      SQL_DOUBLE, SQL_D_FLOAT:
        Result := PDouble(sqldata)^;
      SQL_BOOLEAN:
        case PShort(sqldata)^ of
          ISC_TRUE: Result := 1;
          ISC_FALSE: Result := 0;
        end;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
  end;
end;

function TmncSQLVAR.GetAsFloat: Double;
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

function TmncSQLVAR.GetAsLong: Long;
begin
  Result := 0;
  if not IsNull then
    case SqlDef of
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
        Result := Trunc(FBScaleDouble(Int64(PShort(sqldata)^),
          sqlscale));
      SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TIMESTAMP, SQL_LONG:
        Result := Trunc(FBScaleDouble(Int64(PLong(sqldata)^),
          sqlscale));
      SQL_INT64:
        Result := Trunc(FBScaleDouble(PInt64(sqldata)^, sqlscale));
      SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
        Result := Trunc(AsDouble);
      SQL_BOOLEAN:
        case PShort(sqldata)^ of
          ISC_TRUE: Result := 1;
          ISC_FALSE: Result := 0;
        end;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TmncSQLVAR.GetAsPointer: Pointer;
begin
  if not IsNull then
    Result := sqldata
  else
    Result := nil;
end;

function TmncSQLVAR.GetAsQuad: TISC_QUAD;
begin
  Result.gds_quad_high := 0;
  Result.gds_quad_low := 0;
  if not IsNull then
    case SqlDef of
      SQL_BLOB, SQL_ARRAY, SQL_QUAD:
        Result := PISC_QUAD(sqldata)^;
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

function TmncSQLVAR.GetAsShort: Short;
begin
  Result := 0;
  try
    Result := AsLong;
  except
    on E: Exception do
      FBRaiseError(fbceInvalidDataConversion, [nil]);
  end;
end;

function TmncSQLVAR.GetAsString: string;
var
  sz: Pointer;
  str_len: Integer;
  ss: TStringStream;
begin
  Result := '';
  { Check null, if so return a default string }
  if not IsNull then
    case SqlDef of
      SQL_ARRAY:
        Result := '[Array]';
      SQL_BLOB:
        begin
          ss := TStringStream.Create('');
          try
            SaveToStream(ss);//TODO not work witout handles
            Result := ss.DataString;
          finally
            ss.Free;
          end;
        end;
      SQL_TEXT, SQL_VARYING:
        begin
          sz := sqldata;
          if (SqlDef = SQL_TEXT) then
            str_len := sqllen
          else
          begin
            str_len := FBClient.isc_vax_integer(sqldata, 2);
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
        if sqlscale = 0 then
          Result := IntToStr(AsLong)
        else if sqlscale >= (-4) then
          Result := CurrToStr(AsCurrency)
        else
          Result := FloatToStr(AsDouble);
      SQL_INT64:
        if sqlscale = 0 then
          Result := IntToStr(AsInt64)
        else if sqlscale >= (-4) then
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

function TmncSQLVAR.GetAsVariant: Variant;
begin
  if IsNull then
    Result := NULL
  { Check null, if so return a default string }
  else
    case SqlDef of
      SQL_ARRAY:
        Result := '[Array]';
      SQL_BLOB:
        begin
          if SqlSubtype = 1 then
            Result := AsString
          else
            Result := '[Blob]';
        end;
      SQL_TEXT, SQL_VARYING:
        Result := AsString;
      SQL_TIMESTAMP, SQL_TYPE_DATE, SQL_TYPE_TIME:
        Result := AsDateTime;
      SQL_SHORT, SQL_LONG:
        if sqlscale = 0 then
          Result := AsLong
        else if sqlscale >= (-4) then
          Result := AsCurrency
        else
          Result := AsDouble;
      SQL_INT64:
        if sqlscale = 0 then
          Result := AsINT64
        else if sqlscale >= (-4) then
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

function TmncSQLVAR.GetIsNull: Boolean;
begin
  Result := IsNullable and Assigned(sqlind) and (sqlind^ = -1);
end;

function TmncSQLVAR.GetIsNullable: Boolean;
begin
  Result := (sqltype and 1 = 1);
end;

function TmncSQLVAR.GetSize: Integer;
begin
  Result := sqllen;
end;

procedure TmncSQLVAR.SetAsChar(const AValue: Char);
begin
  AsString := AValue;
end;

constructor TmncSQLVAR.Create;
begin
  inherited;
end;

procedure TmncSQLVAR.CheckHandles;
begin
  if (FDBHandle = nil) or (FTRHandle = nil) then
    raise EFBClientError.Create('Handles not opened');
end;

procedure TmncSQLVAR.Detach;
begin
  FDBHandle := nil;
  FTRHandle := nil;
end;

procedure TmncSQLVAR.Attach(vDBHandle: PISC_DB_HANDLE; vTRHandle: PISC_TR_HANDLE);
begin
  FDBHandle := vDBHandle;
  FTRHandle := vTRHandle;
end;

procedure TmncSQLVAR.SetAsCurrency(AValue: Currency);
begin
  if IsNullable then
    IsNull := False;
  sqltype := SQL_INT64 or (sqltype and 1);
  sqlscale := -4;
  sqllen := SizeOf(Int64);
  UpdateData(0, sqllen);
  PCurrency(sqldata)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsInt64(AValue: Int64);
begin
  if IsNullable then
    IsNull := False;
  sqltype := SQL_INT64 or (sqltype and 1);
  sqlscale := 0;
  sqllen := SizeOf(Int64);
  UpdateData(0, sqllen);
  PInt64(sqldata)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsDate(AValue: TDateTime);
var
  tm_date: TCTimeStructure;
  Yr, Mn, Dy: Word;
begin
  if IsNullable then
    IsNull := False;
  sqltype := SQL_TYPE_DATE or (sqltype and 1);
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
  sqllen := SizeOf(ISC_DATE);
  UpdateData(0, sqllen);
  FBClient.isc_encode_sql_date(@tm_date, PISC_DATE(sqldata));
  Modified := True;
end;

procedure TmncSQLVAR.SetAsLong(AValue: Integer);
begin
  if IsNullable then
    IsNull := False;
  sqltype := SQL_LONG or (sqltype and 1);
  sqllen := SizeOf(Long);
  sqlscale := 0;
  UpdateData(0, sqllen);
  PLong(sqldata)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsTime(AValue: TDateTime);
var
  tm_date: TCTimeStructure;
  Hr, Mt, S, Ms: Word;
begin
  if IsNullable then
    IsNull := False;
  sqltype := SQL_TYPE_TIME or (sqltype and 1);
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
  sqllen := SizeOf(ISC_TIME);
  UpdateData(0, sqllen);
  FBClient.isc_encode_sql_time(@tm_date, PISC_TIME(sqldata));
  Modified := True;
end;

procedure TmncSQLVAR.SetAsDateTime(AValue: TDateTime);
var
  tm_date: TCTimeStructure;
  Yr, Mn, Dy, Hr, Mt, S, Ms: Word;
begin
  if IsNullable then
    IsNull := False;
  sqltype := SQL_TIMESTAMP or (sqltype and 1);
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
  sqllen := SizeOf(TISC_QUAD);
  UpdateData(0, sqllen);
  FBClient.isc_encode_date(@tm_date, PISC_QUAD(sqldata));
  Modified := True;
end;

procedure TmncSQLVAR.SetAsDouble(AValue: Double);
begin
  if IsNullable then
    IsNull := False;
  sqltype := SQL_DOUBLE or (sqltype and 1);
  sqllen := SizeOf(Double);
  sqlscale := 0;
  UpdateData(0, sqllen);
  PDouble(sqldata)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsFloat(AValue: Double);
begin
  if IsNullable then
    IsNull := False;
  sqltype := SQL_FLOAT or (sqltype and 1);
  sqllen := SizeOf(Float);
  sqlscale := 0;
  UpdateData(0, sqllen);
  PSingle(sqldata)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsNullString(const AValue: string);
begin
  if AValue = '' then
    Clear
  else
    AsString := AValue;
end;

procedure TmncSQLVAR.SetAsPointer(AValue: Pointer);
begin
  if IsNullable and (AValue = nil) then
    IsNull := True
  else
  begin
    IsNull := False;
    sqltype := SQL_TEXT or (sqltype and 1);
    Move(AValue^, sqldata^, sqllen);
    Modified := True;
  end;
end;

procedure TmncSQLVAR.SetAsQuad(AValue: TISC_QUAD);
begin
  if IsNullable then
    IsNull := False;
  if (SqlDef <> SQL_BLOB) and
    (SqlDef <> SQL_ARRAY) then
    FBRaiseError(fbceInvalidDataConversion, [nil]);
  sqllen := SizeOf(TISC_QUAD);
  UpdateData(0, sqllen);
  PISC_QUAD(sqldata)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsShort(AValue: Short);
begin
  if IsNullable then
    IsNull := False;
  sqltype := SQL_SHORT or (sqltype and 1);
  sqllen := SizeOf(Short);
  sqlscale := 0;
  UpdateData(0, sqllen);
  PShort(sqldata)^ := AValue;
  Modified := True;
end;

procedure TmncSQLVAR.SetAsString(AValue: string);
var
  stype: Integer;
  ss: TStringStream;

  procedure SetStringValue;
  begin
    if (sqlname = 'DB_KEY') or
      (sqlname = 'RDB$DB_KEY') then
      Move(AValue[1], sqldata^, sqllen)
    else
    begin
      sqltype := SQL_TEXT or (sqltype and 1);
      if (FMaxLen > 0) and (Length(AValue) > FMaxLen) then
        AValue := Copy(AValue, 1, FMaxLen);
      sqllen := Length(AValue);
      UpdateData(0, sqllen + 1);
      if (Length(AValue) > 0) then
        Move(AValue[1], sqldata^, sqllen);
    end;
    Modified := True;
  end;
begin
  if IsNullable then
    IsNull := False;
  stype := SqlDef;
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
          LoadFromStream(ss);//TODO not work without handles
        finally
          ss.Free;
        end;
      end;
    end
    else if AValue = '' then
      IsNull := True
    else if (stype = SQL_TIMESTAMP) or (stype = SQL_TYPE_DATE) or
      (stype = SQL_TYPE_TIME) then
      SetAsDateTime(StrToDateTime(AValue))
    else
      SetStringValue;
  end;
end;

procedure TmncSQLVAR.SetAsVariant(AValue: Variant);
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

procedure TmncSQLVAR.SetIsNull(AValue: Boolean);
begin
  if AValue then
  begin
    if not IsNullable then
      IsNullable := True;
    if Assigned(sqlind) then
      sqlind^ := -1;
    Modified := True;
  end
  else if ((not AValue) and IsNullable) then
  begin
    if Assigned(sqlind) then
      sqlind^ := 0;
    Modified := True;
  end;
end;

procedure TmncSQLVAR.SetIsNullable(AValue: Boolean);
begin
  if (AValue <> IsNullable) then
  begin
    if AValue then
    begin
      sqltype := sqltype or 1;
      UpdateSQLInd;
    end
    else
    begin
      sqltype := SqlDef;
      UpdateSQLInd;
    end;
  end;
end;

procedure TmncSQLVAR.Clear;
begin
  IsNull := True;
end;

procedure TmncSQLVAR.SetAsStrip(const AValue: string);
begin
  if AValue = '' then
    Clear
  else
    SetAsString(TrimRight(AValue));
end;

function TmncSQLVAR.GetAsStrip: string;
begin
  Result := TrimRight(GetAsString);
end;

function TmncSQLVAR.GetAsBoolean: Boolean;
begin
  Result := false;
  if not IsNull then
    case SqlDef of
      SQL_INT64: Result := PInt64(sqldata)^ <> ISC_FALSE;
      SQL_LONG: Result := PLong(sqldata)^ <> ISC_FALSE;
      SQL_SHORT, SQL_BOOLEAN:
        Result := PShort(sqldata)^ <> ISC_FALSE
    else
      FBRaiseError(fbceInvalidDataConversion, [nil]);
    end;
end;

procedure TmncSQLVAR.SetAsBoolean(const AValue: Boolean);
begin
  if IsNullable then
    IsNull := False;
  if AValue then
    PShort(sqldata)^ := ISC_TRUE
  else
    PShort(sqldata)^ := ISC_FALSE;
end;

procedure TmncSQLVAR.CopySQLVAR(const AValue: TmncSQLVAR);
var
  local_sqlind: PShort;
  local_sqldata: PAnsiChar;
  local_sqllen: Integer;
begin
  local_sqlind := sqlind;
  local_sqldata := sqldata;
  move(AValue.FXSQLVAR^, FXSQLVAR^, sizeof(TXSQLVAR));
  //Now make new value
  sqlind := local_sqlind;
  sqldata := local_sqldata;
  if (AValue.sqltype and 1 = 1) then
  begin
    if (sqlind = nil) then
      FBAlloc(FXSQLVAR.sqlind, 0, SizeOf(Short));
    sqlind^ := AValue.sqlind^;
  end
  else if (sqlind <> nil) then
    FBFree(FXSQLVAR.sqlind);
  if ((SqlDef) = SQL_VARYING) then
    local_sqllen := sqllen + 2
  else
    local_sqllen := sqllen;
  sqlscale := AValue.sqlscale;
  UpdateData(0, local_sqllen);
  Move(AValue.sqldata[0], sqldata[0], local_sqllen);
  Modified := True;
end;

destructor TmncSQLVAR.Destroy;
begin
  inherited;
end;

procedure TmncSQLVAR.SetModified(const AValue: Boolean);
begin
  FModified := AValue;
end;

function TmncSQLVAR.GetAsHex: string;
var
  s: string;
begin
  s := GetAsString;
  SetLength(Result, Length(s) * 2);
  BinToHex(PChar(s), @Result[1], Length(s));
end;

procedure TmncSQLVAR.SetAsHex(const AValue: string);
var
  s: string;
begin
  SetLength(s, Length(AValue) div 2);
  HexToBin(PChar(AValue), @s[1], Length(s));
  AsString := s;
end;

function TmncSQLVAR.GetAsText: string;
begin
  if (SqlDef = SQL_BLOB) and (SqlSubtype <> 1) then
    Result := AsHex
  else
    Result := AsString;
end;

procedure TmncSQLVAR.SetAsText(const AValue: string);
begin
  if (SqlDef = SQL_BLOB) and (SqlSubtype <> 1) then
    AsHex := AValue
  else
    AsString := AValue;
end;

procedure TmncSQLVAR.SetBuffer(Buffer: Pointer; Size: Integer);
var
  sz: PAnsiChar;
  len: Integer;
begin
  sz := sqldata;
  if (SqlDef = SQL_TEXT) then
    len := sqllen
  else
  begin
    len := FBClient.isc_vax_integer(sqldata, 2);
    Inc(sz, 2);
  end;
  if (Size <> 0) and (len > Size) then
    len := Size;
  Move(sz^, Buffer^, len);
end;

function TmncSQLVAR.GetAsGUID: TGUID;
begin
end;

procedure TmncSQLVAR.SetAsGUID(const AValue: TGUID);
begin
end;

function TmncSQLVAR._AddRef: longint; stdcall;
begin
  Result := -1;
end;

function TmncSQLVAR._Release: longint; stdcall;
begin
  Result := -1
end;

function TmncSQLVAR.QueryInterface({$IFDEF FPC}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

end.
