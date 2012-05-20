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

unit mncFBBlob;

interface

uses
  SysUtils,
  Classes, mncFBHeader, mncFBTypes, mncFBErrors, mncFBClient;

const
  DefaultBlobSegmentSize = 16 * 1024;

type
  TBlobStreamMode = (bmRead, bmWrite, bmReadWrite);

  { TFBBlobStream }

  TFBBlobStream = class(TStream)
  private
    //FBase: TFBBase;
    FBlobInitialized: Boolean;
    FBlobID: TISC_QUAD;
    FBlobMaxSegmentSize:Long;
    FBlobNumSegments:Long;
    FBlobSize: Long;
    FBlobType: Short; { 0 = segmented, 1 = streamed }
    FBuffer: PChar;
    FHandle: TISC_BLOB_HANDLE;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    FPosition: Long;
  protected
    FDBHandle: PISC_DB_HANDLE;
    FTRHandle: PISC_TR_HANDLE;

    procedure CloseBlob;
    procedure CreateBlob;
    procedure EnsureBlobInitialized;
    procedure GetBlobInfo;
    procedure OpenBlob;
    procedure SetBlobID(Value: TISC_QUAD);
    procedure SetMode(Value: TBlobStreamMode);
  public
    constructor Create(DBHandle: PISC_DB_HANDLE; TRHandle: PISC_TR_HANDLE);
    destructor Destroy; override;
    function Call(ErrCode: ISC_STATUS; StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
    procedure Cancel;
    procedure CheckReadable;
    procedure CheckWritable;
    procedure Finalize;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromStream(Stream: TStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    procedure SaveToFile(Filename: string);
    procedure SaveToStream(Stream: TStream);
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SetSize(NewSize: Long); override;
    procedure Truncate;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Handle: TISC_BLOB_HANDLE read FHandle;
    property BlobID: TISC_QUAD read FBlobID write SetBlobID;
    property BlobMaxSegmentSize: Long read FBlobMaxSegmentSize;
    property BlobNumSegments: Long read FBlobNumSegments;
    property BlobSize: Long read FBlobSize;
    property BlobType: Short read FBlobType;
    property Mode: TBlobStreamMode read FMode write SetMode;
    property Modified: Boolean read FModified;
  end;

function getb(p: PBSTREAM): Char;
function putb(x: Char; p: PBSTREAM): Int;
function putbx(x: Char; p: PBSTREAM): Int;

procedure FBGetBlobInfo(hBlobHandle: PISC_BLOB_HANDLE; out NumSegments, MaxSegmentSize, TotalSize: Long; out BlobType: Short);
procedure FBReadBlob(hBlobHandle: PISC_BLOB_HANDLE; Buffer: PChar; BlobSize: Long);
procedure FBWriteBlob(hBlobHandle: PISC_BLOB_HANDLE; Buffer: PChar; BlobSize: Long);

function FBGetBlob(DBHandle: TISC_DB_HANDLE; TRHandle: TISC_TR_HANDLE; BlobID: PISC_QUAD): PChar;

implementation

uses
  mncFBUtils;

procedure FBGetBlobInfo(hBlobHandle: PISC_BLOB_HANDLE; out NumSegments, MaxSegmentSize, TotalSize: Long; out BlobType: Short);
var
  items: array[0..3] of Char;
  results: array[0..99] of Char;
  i, item_length: Integer;
  item: Integer;
  StatusVector: TStatusVector;
begin
  items[0] := Char(isc_info_blob_num_segments);
  items[1] := Char(isc_info_blob_max_segment);
  items[2] := Char(isc_info_blob_total_length);
  items[3] := Char(isc_info_blob_type);

  if FBClient.isc_blob_info(@StatusVector, hBlobHandle, 4, @items[0], SizeOf(results), @results[0]) > 0 then
    FBRaiseError(StatusVector);

  i := 0;
  while (i < SizeOf(results)) and (results[i] <> Char(isc_info_end)) do
  begin
    item := Integer(results[i]);
    Inc(i);
    item_length := FBClient.isc_vax_integer(@results[i], 2);
    Inc(i, 2);
    case item of
      isc_info_blob_num_segments: NumSegments := FBClient.isc_vax_integer(@results[i], item_length);
      isc_info_blob_max_segment: MaxSegmentSize := FBClient.isc_vax_integer(@results[i], item_length);
      isc_info_blob_total_length: TotalSize := FBClient.isc_vax_integer(@results[i], item_length);
      isc_info_blob_type: BlobType := FBClient.isc_vax_integer(@results[i], item_length);
    end;
    Inc(i, item_length);
  end;
end;

procedure FBReadBlob(hBlobHandle: PISC_BLOB_HANDLE; Buffer: PChar; BlobSize: Long);
var
  CurPos: Long;
  BytesRead, SegLen: UShort;
  LocalBuffer: PChar;
  StatusVector: TStatusVector;
begin
  CurPos := 0;
  LocalBuffer := Buffer;
  SegLen := UShort(DefaultBlobSegmentSize*2);
  while (CurPos < BlobSize) do
  begin
    if (CurPos + SegLen > BlobSize) then
      SegLen := BlobSize - CurPos;
    if not ((FBClient.isc_get_segment(@StatusVector, hBlobHandle, @BytesRead, SegLen, LocalBuffer) = 0) or (StatusVector[1] = isc_segment)) then
      FBRaiseError(StatusVector);
    Inc(LocalBuffer, BytesRead);
    Inc(CurPos, BytesRead);
    BytesRead := 0;
  end;
end;

procedure FBWriteBlob(hBlobHandle: PISC_BLOB_HANDLE; Buffer: PChar;
  BlobSize: Long);
var
  StatusVector: TStatusVector;
  CurPos, SegLen: Long;
begin
  CurPos := 0;
  SegLen := DefaultBlobSegmentSize;
  while (CurPos < BlobSize) do
  begin
    if (CurPos + SegLen > BlobSize) then
      SegLen := BlobSize - CurPos;
    if FBClient.isc_put_segment(@StatusVector, hBlobHandle, SegLen, PChar(@Buffer[CurPos])) > 0 then
      FBRaiseError(StatusVector);
    Inc(CurPos, SegLen);
  end;
end;

function FBGetBlob(DBHandle: TISC_DB_HANDLE; TRHandle: TISC_TR_HANDLE; BlobID: PISC_QUAD): PChar;
const
  cDefaultSize = 1024;
var
  bStream: PBSTREAM;
  aPos, aSize: Integer;
  p: PChar;
  s: string;
begin
  Result := nil;
  aPos := 0;
  aSize := 0;
  with FBClient do
  begin
    s := 'R';
    bStream := Bopen(BlobID, DBHandle, TRHandle, @s[1]);
    try
      p := nil;
      while bStream.bstr_cnt>0 do
      begin
        if aPos>=aSize then
        begin
          aSize := aSize + cDefaultSize;
          ReallocMem(Result, aSize);
          p := Result;
          Inc(p, aPos);
        end;
        p^ := getb(bStream);
        Inc(p);
        Inc(aPos);
      end;
      ReallocMem(Result, aPos);
    finally
      Bclose(bStream);  
    end;
  end;
end;

{ TFBBlobStream }

constructor TFBBlobStream.Create;
begin
  inherited Create;
  FDBHandle := DBHandle;
  FTRHandle := TRHandle;
  FBuffer := nil;
  FBlobSize := 0;
end;

destructor TFBBlobStream.Destroy;
var
  StatusVector: TStatusVector;
begin
  if (FHandle <> nil) and (Call(FBClient.isc_close_blob(@StatusVector, @FHandle), StatusVector, False) > 0) then
    FBRaiseError(StatusVector);
  SetSize(0);
  inherited;
end;

function TFBBlobStream.Call(ErrCode: ISC_STATUS; StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := FBCall(ErrCode, StatusVector, RaiseError);
end;

procedure TFBBlobStream.CheckReadable;
begin
  if FMode = bmWrite then
    FBRaiseError(fbceBlobCannotBeRead, [nil]);
end;

procedure TFBBlobStream.CheckWritable;
begin
  if FMode = bmRead then
    FBRaiseError(fbceBlobCannotBeWritten, [nil]);
end;

procedure TFBBlobStream.CloseBlob;
var
  StatusVector: TStatusVector;
begin
  Finalize;
  if (FHandle <> nil) and
    (Call(FBClient.isc_close_blob(@StatusVector, @FHandle), StatusVector, False) > 0) then
    FBRaiseError(StatusVector);
end;

procedure TFBBlobStream.CreateBlob;
begin
  CheckWritable;
  FBlobID.gds_quad_high := 0;
  FBlobID.gds_quad_low := 0;
  Truncate;
end;

procedure TFBBlobStream.EnsureBlobInitialized;
begin
  if not FBlobInitialized then
  begin
    case FMode of
      bmWrite: CreateBlob;
      bmReadWrite:
      begin
        if (FBlobID.gds_quad_high = 0) and (FBlobID.gds_quad_low = 0) then
          CreateBlob
        else
          OpenBlob;
      end;
      else
        OpenBlob;
    end;
    FBlobInitialized := True;
  end;
end;

procedure TFBBlobStream.Finalize;
var
  StatusVector: TStatusVector;
begin
  if FBlobInitialized and (FMode <> bmRead) then
  begin
    { need to start writing to a blob, create one }
    Call(FBClient.isc_create_blob2(@StatusVector, FDBHandle, FTRHandle, @FHandle, @FBlobID, 0, nil), StatusVector, True);
    FBWriteBlob(@FHandle, FBuffer, FBlobSize);
    Call(FBClient.isc_close_blob(@StatusVector, @FHandle), StatusVector, True);
    FModified := False;
  end;
end;

procedure TFBBlobStream.GetBlobInfo;
var
  iBlobSize: Long;
begin
  FBGetBlobInfo(@FHandle, FBlobNumSegments, FBlobMaxSegmentSize, iBlobSize, FBlobType);
  SetSize(iBlobSize);
end;

procedure TFBBlobStream.LoadFromFile(Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TFBBlobStream.LoadFromStream(Stream: TStream);
begin
  CheckWritable;
  EnsureBlobInitialized;
  Stream.Position := 0;
  SetSize(Stream.Size);
  if FBlobSize <> 0 then
    Stream.ReadBuffer(FBuffer^, FBlobSize);
  FModified := True;
end;

procedure TFBBlobStream.OpenBlob;
var
  StatusVector: TStatusVector;
begin
  CheckReadable;
  Call(FBClient.isc_open_blob2(@StatusVector, FDBHandle, FTRHandle, @FHandle, @FBlobID, 0, nil), StatusVector, True);
  try
    GetBlobInfo;
    SetSize(FBlobSize);
    FBReadBlob(@FHandle, FBuffer, FBlobSize);
  except
    Call(FBClient.isc_close_blob(@StatusVector, @FHandle), StatusVector, False);
    raise;
  end;
  Call(FBClient.isc_close_blob(@StatusVector, @FHandle), StatusVector, True);
end;

function TFBBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  CheckReadable;
  EnsureBlobInitialized;
  if (Count <= 0) then
  begin
    result := 0;
    exit;
  end;
  if (FPosition + Count > FBlobSize) then
    result := FBlobSize - FPosition
  else
    result := Count;
  Move(FBuffer[FPosition], Buffer, result);
  Inc(FPosition, Result);
end;

procedure TFBBlobStream.SaveToFile(Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TFBBlobStream.SaveToStream(Stream: TStream);
begin
  CheckReadable;
  EnsureBlobInitialized;
  if FBlobSize <> 0 then
  begin
    Seek(0, soFromBeginning);
    Stream.WriteBuffer(FBuffer^, FBlobSize);
  end;
end;

function TFBBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  EnsureBlobInitialized;
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: Inc(FPosition, Offset);
    soFromEnd: FPosition := FBlobSize + Offset; //ask: error must be FBlobSize - Offset
  end;
  result := FPosition;
end;

procedure TFBBlobStream.SetBlobID(Value: TISC_QUAD);
begin
  FBlobID := Value;
  FBlobInitialized := False;
end;

procedure TFBBlobStream.SetMode(Value: TBlobStreamMode);
begin
  FMode := Value;
  FBlobInitialized := False;
end;

procedure TFBBlobStream.SetSize(NewSize: Long);
begin
  if (NewSize <> FBlobSize) then
  begin
    if NewSize=0 then
    begin
      FreeMem(FBuffer);
      FBuffer := nil;
    end
    else if Assigned(FBuffer) then
      ReallocMem(FBuffer, NewSize)
    else
      GetMem(FBuffer, NewSize);
    FBlobSize := NewSize;
  end;
end;
{
procedure TFBBlobStream.SetTransaction(Value: TFBTransaction);
begin
  FBase.Transaction := Value;
  FBlobInitialized := False;
end;
}
procedure TFBBlobStream.Truncate;
begin
  SetSize(0);
end;

function TFBBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  CheckWritable;
  EnsureBlobInitialized;
  Result := Count;
  if Count <= 0 then
    exit;
  if (FPosition + Count > FBlobSize) then
    SetSize(FPosition + Count);
  Move(Buffer, FBuffer[FPosition], Count);
  Inc(FPosition, Count);
  FModified := True;
end;

procedure TFBBlobStream.Cancel;
begin
  if (not FBlobInitialized) or (FMode = bmRead) then
    exit;
  if FModified then
    OpenBlob;
  FModified := False;
end;

function getb(p: PBSTREAM): Char;
(*  The C-macro reads like this:
   getb(p)	(--(p)->bstr_cnt >= 0 ? *(p)->bstr_ptr++ & 0377: BLOB_get (p)) *)
begin
  Dec(p^.bstr_cnt);
  if (p^.bstr_cnt >= 0) then
  begin
    result := Char(Int(p^.bstr_ptr^) and Int(0377));
    Inc(p^.bstr_ptr);
  end
  else
    result := Char(FBClient.BLOB_get(p));
end;

function putb(x: Char; p: PBSTREAM): Int;
(*  The C-macro reads like this:
   putb(x,p) ((x == '\n' || (!(--(p)->bstr_cnt))) ?      // then
     BLOB_put (x,p) :                                    // else
     ((int) (*(p)->bstr_ptr++ = (unsigned) (x)))) *)
begin
  Dec(p^.bstr_cnt);
  if (x = Chr(Int('n') - Int('a'))) or (p^.bstr_cnt = 0) then
    result := FBClient.BLOB_put(x, p)
  else
  begin
    p^.bstr_ptr^ := Char(x);
    result := UInt(x);
    Inc(p^.bstr_ptr^);
  end;
end;

function putbx(x: Char; p: PBSTREAM): Int;
(*  The C-macro reads like this:
   putbx(x,p) ((!(--(p)->bstr_cnt)) ?    // then
     BLOB_put (x,p) :                    // else
     ((int) (*(p)->bstr_ptr++ = (unsigned) (x)))) *)
begin
  Dec(p^.bstr_cnt);
  if (p^.bstr_cnt = 0) then
    result := FBClient.BLOB_put(x, p)
  else
  begin
    p^.bstr_ptr^ := Char(x);
    Inc(p^.bstr_ptr^);
    result := UInt(x);
  end;
end;

end.

