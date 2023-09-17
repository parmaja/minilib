unit mnMultipartData;

{$ifdef FPC}
{$mode Delphi}
{$endif}
{$M+}
{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  mnUtils, mnStreams, mnClasses, mnParams,
  mnLogs, mnStreamUtils;

type
  TmnMultipartData = class;

  TmnMultipartDataItem = class abstract(TmnNamedObject)
  private
    FData: TmnMultipartData;
  protected
    procedure DoPrepare; virtual;
    procedure DoWrite(vStream: TmnBufferStream); virtual;
    procedure Prepare;
  public
    Header: TmnHeader;
    constructor Create(vData: TmnMultipartData);
    destructor Destroy; override;
    procedure Write(vStream: TmnBufferStream);
    property Data: TmnMultipartData read FData;
  end;

  TmnMultipartDataValue = class(TmnMultipartDataItem)
  protected
    procedure DoPrepare; override;
    procedure DoWrite(vStream: TmnBufferStream); override;
  public
    Value: string;
  end;

  TmnMultipartDataFileName = class(TmnMultipartDataItem)
  protected
    procedure DoWrite(vStream: TmnBufferStream); override;
    procedure DoPrepare; override;
  public
    FileName: string;
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
    FBoundary: UTF8String;
    FHttpHeader: Boolean;
  public
    function NewItem(vStream: TmnBufferStream): TmnMultipartDataItem;
    procedure ReadUntilCallback(vData: TObject; const Buffer; Count: Longint);
    function ReadCallback(vStream: TmnBufferStream): Boolean;
    function Read(vStream: TmnBufferStream): Boolean;
    function Write(vStream: TmnBufferStream): Boolean;
    property Boundary: UTF8String read FBoundary write FBoundary;
    //* Write / Read http header
    property HttpHeader: Boolean read FHttpHeader write FHttpHeader;
  end;

function DocumentToContentType(FileName: string): string;

implementation

function DocumentToContentType(FileName: string): string;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  if Length(Ext) > 1 then
    Ext := Copy(Ext, 2, Length(Ext));

  if Ext = 'txt' then
    Result := 'text/plan'
  else if (Ext = 'htm') or (Ext = 'html') or (Ext = 'shtml') or (Ext = 'dhtml') then
    Result := 'text/html'
  else if Ext = 'gif' then
    Result := 'image/gif'
  else if Ext = 'bmp' then
    Result := 'image/bmp'
  else if (Ext = 'jpg') or (Ext = 'jpeg') then
    Result := 'image/jpeg'
  else if (Ext = 'png') then
    Result := 'image/png'
  else if Ext = 'txt' then
    Result := 'text/plain'
  else if Ext = 'svg' then
    Result := 'image/svg+xml'
  else if Ext = 'css' then
    Result := 'text/css'
  else if Ext = 'json' then
    Result := 'application/json'
  else if Ext = 'js' then
    Result := 'text/javascript'
  else
    Result := 'application/binary';
end;

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
  FreeAndNil(Header);
  inherited;
end;

procedure TmnMultipartDataItem.DoWrite(vStream: TmnBufferStream);
begin

end;

procedure TmnMultipartDataItem.Prepare;
begin

end;

procedure TmnMultipartDataItem.DoPrepare;
begin

end;

procedure TmnMultipartDataItem.Write(vStream: TmnBufferStream);
begin
  Header.WriteHeader(vStream);
  vStream.WriteLineUTF8('');
  DoWrite(vStream);
  vStream.WriteLineUTF8('');
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

function TmnMultipartData.NewItem(vStream: TmnBufferStream): TmnMultipartDataItem;
begin
  Result := TmnMultipartDataItem.Create(Self);
  try
    Result.Header.ReadHeader(vStream);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TmnMultipartData.Read(vStream: TmnBufferStream): Boolean;
var
  Res: PByte;
  len: TFileSize;
  S: UTF8String;
  aHeader: TmnHeader;
  aType: string;
  aItem: TmnMultipartDataItem;
  ContentType: TStringList;
  Matched: Boolean;
  aDataHeader: TmnHeader;
  aBoundary: UTF8String;
begin
  aHeader := TmnHeader.Create;
  try
    if HttpHeader then
    begin
      aHeader.ReadHeader(vStream);
      ContentType := aHeader.Field['Content-Type'].CreateSubValues;
      aBoundary := UTF8Encode(ContentType.Values['boundary']);
      if aBoundary = '' then
        aBoundary := Boundary
    end;

    if SameText(ContentType[0], 'multipart/form-data') then
    begin
      aBoundary := '--' + aBoundary;

      vStream.ReadLineUTF8(S, True);
      if S = aBoundary then
      begin
        aBoundary := vStream.EndOfLine + aBoundary;
        while True do
        begin
          aItem := TmnMultipartDataValue.Create(Self);
          try
            aItem.Header.ReadHeader(vStream);
            vStream.ReadBufferUntil(@aBoundary[1], Length(aBoundary), True, Res, Len, Matched);
            if Matched then
            begin
              //handle binary ?
              CopyString(S, Res, Len);
              FreeMem(res);
              aItem.Name := s;
              Add(aItem);
              aDataHeader := nil;
              vStream.ReadLineUTF8(S, True);
              if S = '--' then
              begin
                break;
              end;
            end;
          except
            FreeAndNil(aItem);
            raise;
          end
        end;
      end;
    end;
  finally
    aHeader.Free;
  end;
end;

function TmnMultipartData.ReadCallback(vStream: TmnBufferStream): Boolean;
var
  Res: PByte;
  len: TFileSize;
  S: UTF8String;
  aHeader: TmnHeader;
  aType: string;
  aItem: TmnMultipartDataItem;
  ContentType: TStringList;
  Matched: Boolean;
  aDataHeader: TmnHeader;
  aBoundary: UTF8String;
begin
  aHeader := TmnHeader.Create;
  try
    if HttpHeader then
    begin
      aHeader.ReadHeader(vStream);
      ContentType := aHeader.Field['Content-Type'].CreateSubValues;
      aBoundary := UTF8Encode(ContentType.Values['boundary']);
      if aBoundary = '' then
        aBoundary := Boundary;
    end;
    if SameText(ContentType[0], 'multipart/form-data') then
    begin
      aBoundary := '--' + aBoundary;

      vStream.ReadLineUTF8(S, True);
      if S = aBoundary then
      begin
        aBoundary := vStream.EndOfLine + aBoundary;
        while True do
        begin
          aItem := NewItem(vStream);
          vStream.ReadUntilCallback(aItem, @aBoundary[1], Length(aBoundary), True, ReadUntilCallback, Matched);

          if not Matched then
          begin
            Last.Name := 'Error';
            Exit(False);
          end;

          vStream.ReadLineUTF8(S, True);
          if S = '--' then
          begin
            Exit(True);
          end;
        end;
      end;
    end;
  finally
    aHeader.Free;
  end;
  Result := False;
end;

procedure TmnMultipartData.ReadUntilCallback(vData: TObject; const Buffer; Count: Longint);
var
  s: string;
begin
  s := TEncoding.UTF8.GetString(PByte(Buffer), Count);
  Last.Name := Last.Name + s;
end;

function TmnMultipartData.Write(vStream: TmnBufferStream): Boolean;
var
  itm: TmnMultipartDataItem;
begin

  for itm in Self do
  begin
    vStream.WriteLineUTF8('--'+Boundary);
    itm.Write(vStream);
  end;
  vStream.WriteLineUTF8('--'+Boundary+'--');
end;

{ TmnMultipartDataValue }

procedure TmnMultipartDataValue.DoPrepare;
begin
  inherited;
  Header.Values['Content-Type'] := 'text/plan';
end;

procedure TmnMultipartDataValue.DoWrite(vStream: TmnBufferStream);
begin
  inherited;
  vStream.WriteUTF8(Value);
end;

{ TmnMultipartDataFileName }

procedure TmnMultipartDataFileName.DoPrepare;
begin
  inherited;
  Header.Values['Content-Type'] := DocumentToContentType(FileName);
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
