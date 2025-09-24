unit mnMultipartData;
{$M+}{$H+}
{$ifdef FPC}{$mode delphi}{$endif}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamed <belal, belalhamed@gmail.com>
 *
}
interface

uses
  Classes, SysUtils, IniFiles,
  mnUtils, mnStreams, mnClasses, mnFields, mnParams,
  mnStreamUtils;

type
  TmnMultipartData = class;

  { TmnMultipartDataItem }

  TmnMultipartDataItem = class abstract(TmnNamedObject)
  private
    FData: TmnMultipartData;
    FHeader: TmnHeader;
    procedure SetHeader(const Value: TmnHeader);
  protected
    procedure ReadUntilCallback(vData: TObject; const Buffer; Count: Longint);

    procedure DoReadPrepare; virtual;
    procedure DoReadUnPrepare; virtual;

    procedure DoPrepare; virtual;

    procedure DoWrite(vStream: TmnBufferStream); virtual;
    procedure DoRead(const Buffer; Count: Longint); virtual;
    procedure Prepare;
    function DoGetValue: string; virtual;
    function GetValue: string;
  public
    constructor Create(vData: TmnMultipartData);
    destructor Destroy; override;
    procedure Write(vStream: TmnBufferStream);
    function Read(vStream: TmnBufferStream; const vBoundary: utf8string): Boolean;
    property Data: TmnMultipartData read FData;
    property Value: string read GetValue;
    property Header: TmnHeader read FHeader write SetHeader;
  end;

  TmnMultipartDataFileName = class(TmnMultipartDataItem)
  protected
    FFileStream: TFileStream;
    procedure DoReadPrepare; override;
    procedure DoReadUnPrepare; override;
    procedure DoRead(const Buffer; Count: Longint); override;

    procedure DoWrite(vStream: TmnBufferStream); override;
    procedure DoPrepare; override;
    function DoGetValue: string; override;

  public
    FileName: string;
    LocalFileName: string;
  end;

  TmnMultipartDataValue = class(TmnMultipartDataItem)
  protected
    FValue: string;
    procedure DoRead(const Buffer; Count: Longint); override;

    procedure DoPrepare; override;
    procedure DoWrite(vStream: TmnBufferStream); override;
    function DoGetValue: string; override;
  end;

  TmnMultipartDataMemory = class(TmnMultipartDataItem)
  protected
    procedure DoWrite(vStream: TmnBufferStream); override;
    procedure DoPrepare; override;
  public
    FileName: string;
    ContentType: string;
    Memory: TMemoryStream;
  end;

  TmnMultipartDataOption = (
    fdoMemory
  );

  TmnMultipartDataOptions = set of TmnMultipartDataOption;

  TmnMultipartData = class(TmnNamedObjectList<TmnMultipartDataItem>)
  private
    FBoundary: string;
    FTempPath: string;
    function GetData(const Index: string): TmnMultipartDataItem;
    function GetValues(const Index: string): string;
  protected
    function DoCreateItem(vStream: TmnBufferStream; vHeader: TmnHeader): TmnMultipartDataItem; virtual;
    function CreateItem(vStream: TmnBufferStream): TmnMultipartDataItem;
  public
    function Read(vStream: TmnBufferStream): Boolean;
    function Write(vStream: TmnBufferStream): Boolean;
    function Find(const vName: string): TmnMultipartDataItem;

    property Boundary: string read FBoundary write FBoundary;
    property TempPath: string read FTempPath write FTempPath;
    property Values[const Index: string]: string read GetValues;
    property Data[const Index: string]: TmnMultipartDataItem read GetData;
  end;

implementation

uses
  mnMIME;

{ TmnMultipartDataItem }

constructor TmnMultipartDataItem.Create(vData: TmnMultipartData);
begin
  inherited Create;
  FData := vData;
  Header := TmnHeader.Create;
  FData.Add(Self);
end;

destructor TmnMultipartDataItem.Destroy;
begin
  FreeAndNil(FHeader);
  inherited;
end;

procedure TmnMultipartDataItem.DoWrite(vStream: TmnBufferStream);
begin

end;

function TmnMultipartDataItem.GetValue: string;
begin
  if Self <> nil then
    Result := DoGetValue
  else
    Result := '';
end;

procedure TmnMultipartDataItem.Prepare;
begin
  Header.Values['Content-Disposition'] := Format('form-data; name="%s"', [Name]);
end;

function TmnMultipartDataItem.DoGetValue: string;
begin
  Result := '';
end;

function TmnMultipartDataItem.Read(vStream: TmnBufferStream; const vBoundary: utf8string): Boolean;
begin
  DoReadPrepare;
  try
    vStream.ReadUntilCallback(Self, @vBoundary[1], Length(vBoundary), True, ReadUntilCallback, Result);
  finally
    DoReadUnPrepare;
  end;
end;

procedure TmnMultipartDataItem.ReadUntilCallback(vData: TObject; const Buffer; Count: Longint);
begin
  DoRead(Buffer, Count);
end;

procedure TmnMultipartDataItem.SetHeader(const Value: TmnHeader);
begin
  if FHeader <> nil then
    FreeAndNil(FHeader);
  FHeader := Value;
end;

procedure TmnMultipartDataItem.DoPrepare;
begin
end;

procedure TmnMultipartDataItem.DoRead(const Buffer; Count: Longint);
begin
end;

procedure TmnMultipartDataItem.DoReadPrepare;
begin
end;

procedure TmnMultipartDataItem.DoReadUnPrepare;
begin
end;

procedure TmnMultipartDataItem.Write(vStream: TmnBufferStream);
begin
  Header.WriteHeader(vStream);
  vStream.WriteUTF8Line('');
  DoWrite(vStream);
  vStream.WriteUTF8Line('');
end;

{ TmnMultipartData }

procedure CopyString(out S: utf8string; Buffer: Pointer; Len: Integer); inline;
begin
  if Len <> 0 then
  begin
		S := '';
    SetLength(S, Len div SizeOf(utf8char));
    Move(PByte(Buffer)^, PByte(S)^, Len);
  end
  else
    S := '';
end;

function TmnMultipartData.CreateItem(vStream: TmnBufferStream): TmnMultipartDataItem;
var
  aHeader: TmnHeader;
  aDisposition: string;
  s: string;
begin
  aHeader := TmnHeader.Create;
  try
    aHeader.ReadHeader(vStream);

    Result := DoCreateItem(vStream, aHeader);

    aDisposition := aHeader['Content-Disposition'];

    if Result = nil then
    begin
      if GetSubValue(aDisposition, 'filename', s) and (s <> '') then
      begin
        Result := TmnMultipartDataFileName.Create(Self);
        TmnMultipartDataFileName(Result).FileName := s;
      end
      else
        Result := TmnMultipartDataValue.Create(Self);
    end;

    GetSubValue(aDisposition, 'name', s);
    Result.Name := s;
    Result.Header := aHeader;
  except
    FreeAndNil(aHeader);
    raise;
  end;

end;

function TmnMultipartData.GetValues(const Index: string): string;
var
  item: TmnMultipartDataItem;
begin
  item := Find(Index);
  if item <> nil then
    Result := item.Value
  else
    Result := '';
end;

function TmnMultipartData.GetData(const Index: string): TmnMultipartDataItem;
begin
  Result := Find(Index);
end;

function TmnMultipartData.DoCreateItem(vStream: TmnBufferStream; vHeader: TmnHeader): TmnMultipartDataItem;
begin
  Result := nil;
end;

function TmnMultipartData.Find(const vName: string): TmnMultipartDataItem;
begin
  for Result in Self do
    if Result.Name = vName then
      Exit;

  Result := nil;
end;

function TmnMultipartData.Read(vStream: TmnBufferStream): Boolean;
var
  aItem: TmnMultipartDataItem;
  Matched: Boolean;
  aBoundary: utf8string;
  s: utf8string;
begin
  aBoundary := '--' + UTF8Encode(Boundary);

  vStream.ReadUTF8Line(S, True);
  if s = aBoundary then
  begin
    aBoundary := UTF8Encode(vStream.EndOfLine) + aBoundary;
    while True do
    begin
      aItem := CreateItem(vStream);
      Matched := aItem.Read(vStream, aBoundary);

      if not Matched then
      begin
        aItem.Name := 'Error';
        Exit(False);
      end;

      vStream.ReadUTF8Line(S, True);
      if S = '--' then
      begin
        Exit(True);
      end;
    end;
  end;

  Result := False;
end;

function TmnMultipartData.Write(vStream: TmnBufferStream): Boolean;
var
  itm: TmnMultipartDataItem;
begin
  for itm in Self do
  begin
    vStream.WriteUTF8Line('--'+Boundary);
    itm.Write(vStream);
  end;
  vStream.WriteUTF8Line('--'+Boundary+'--');
  Result := True;
end;

{ TmnMultipartDataValue }

procedure TmnMultipartDataValue.DoPrepare;
begin
  inherited;
  Header.Values['Content-Type'] := 'text/plan';
end;

procedure TmnMultipartDataValue.DoRead(const Buffer; Count: Longint);
var
  s: string;
begin
  s := StringOfUTF8(PByte(Buffer), Count);
  FValue := FValue + s;
end;

procedure TmnMultipartDataValue.DoWrite(vStream: TmnBufferStream);
begin
  inherited;
  vStream.WriteUTF8String(Value);
end;

function TmnMultipartDataValue.DoGetValue: string;
begin
  Result := FValue;
end;

{ TmnMultipartDataFileName }

procedure TmnMultipartDataFileName.DoPrepare;
begin
  inherited;
  Header.Values['Content-Disposition'] := Format('form-data; name="%s"; filename="%s"', [Name, FileName]);
  Header.Values['Content-Type'] := DocumentToContentType(FileName);
end;

procedure TmnMultipartDataFileName.DoRead(const Buffer; Count: Longint);
begin
  FFileStream.Write(PByte(Buffer)^, Count);
end;

procedure TmnMultipartDataFileName.DoReadPrepare;
begin
  LocalFileName := IncludePathDelimiter(Data.TempPath);
  if LocalFileName <> '' then
    ForceDirectories(LocalFileName);
  LocalFileName := LocalFileName + FileName;

  if FileExists(LocalFileName) then
    DeleteFile(LocalFileName);

  FFileStream := TFileStream.Create(LocalFileName, fmCreate);
end;

procedure TmnMultipartDataFileName.DoReadUnPrepare;
begin
  FreeAndNil(FFileStream);
end;

procedure TmnMultipartDataFileName.DoWrite(vStream: TmnBufferStream);
var
  f: TFileStream;
begin
  inherited;
  f := TFileStream.Create(FileName, fmOpenRead);
  try
    vStream.WriteStream(f, f.Size);
  finally
    f.Free;
  end;

end;

function TmnMultipartDataFileName.DoGetValue: string;
begin
  Result := LocalFileName;
end;

{ TmnMultipartDataMemory }

procedure TmnMultipartDataMemory.DoPrepare;
begin
  inherited;
  Header.Values['Content-Type'] := ContentType;
end;

procedure TmnMultipartDataMemory.DoWrite(vStream: TmnBufferStream);
begin
  inherited;
  vStream.WriteStream(Memory, Memory.Size);
end;

end.
