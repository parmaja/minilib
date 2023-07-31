unit mnFormData;

{$ifdef FPC}
{$mode Delphi}
{$endif}
{$M+}
{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  mnUtils, mnStreams, mnClasses, mnModules, mnWebModules,
  mnLogs, mnStreamUtils, mnSockets, mnClients, mnServers;

type
  TmnFormData = class;

  TmnFormDataItem = class abstract(TmnNamedObject)
  private
    FForm: TmnFormData;
  protected
    procedure DoWrite(vStream: TmnBufferStream); virtual;
  public
    Header: TmnHeader;
    constructor Create(vForm: TmnFormData);
    destructor Destroy; override;
    procedure Write(vStream: TmnBufferStream);
    property Form: TmnFormData read FForm;
  end;

  TmnFormDataValue = class(TmnFormDataItem)
  protected
    procedure DoWrite(vStream: TmnBufferStream); override;
  public
    Value: string;
  end;

  TmnFormDataFileName = class(TmnFormDataItem)
  protected
    procedure DoWrite(vStream: TmnBufferStream); override;
  public
    FileName: string;
  end;

  TmnFormDataMemory = class(TmnFormDataItem)
  protected
    procedure DoWrite(vStream: TmnBufferStream); override;
  public
    FileName: string;
    Memory: TMemoryStream;
  end;

  TmnFormDataOption = (
    fdoMemory
  );

  TmnFormDataOptions = set of TmnFormDataOption;

  TmnFormData = class(TmnNamedObjectList<TmnFormDataItem>)
  public
    Boundary: UTF8String;
    function NewItem(vStream: TmnBufferStream): TmnFormDataItem;
    procedure ReadUntilCallback(vData: TObject; const Buffer; Count: Longint);
    function ReadCallback(vStream: TmnBufferStream): Boolean;
    function Read(vStream: TmnBufferStream): Boolean;
    function Write(vStream: TmnBufferStream): Boolean;
  end;

implementation

{ TmnFormDataItem }

constructor TmnFormDataItem.Create(vForm: TmnFormData);
begin
  inherited Create;
  FForm := vForm;
  Header := TmnHeader.Create;
  FForm.Add(Self);
end;

destructor TmnFormDataItem.Destroy;
begin
  FreeAndNil(Header);
  inherited;
end;

procedure TmnFormDataItem.DoWrite(vStream: TmnBufferStream);
begin

end;

procedure TmnFormDataItem.Write(vStream: TmnBufferStream);
begin
  Header.WriteHeader(vStream);
  vStream.WriteLineUTF8('');
  DoWrite(vStream);
  vStream.WriteLineUTF8('');
end;

{ TmnFormData }
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

function TmnFormData.NewItem(vStream: TmnBufferStream): TmnFormDataItem;
begin
  Result := TmnFormDataItem.Create(Self);
  try
    Result.Header.ReadHeader(vStream);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TmnFormData.Read(vStream: TmnBufferStream): Boolean;
var
  Res: PByte;
  len: TFileSize;
  S: UTF8String;
  aHeader: TmnHeader;
  aType: string;
  aItem: TmnFormDataItem;
  ContentType: TStringList;
  Matched: Boolean;
  aDataHeader: TmnHeader;
begin
  aHeader := TmnHeader.Create;
  try
    aHeader.ReadHeader(vStream);
    ContentType := aHeader.Field['Content-Type'].CreateSubValues;
    if SameText(ContentType[0], 'multipart/form-data') then
    begin
      Boundary := UTF8Encode(ContentType.Values['boundary']);
      Boundary := '--' + Boundary;

      vStream.ReadLineUTF8(S, True);
      if S = Boundary then
      begin
        Boundary := vStream.EndOfLine + Boundary;
        while True do
        begin
          aItem := TmnFormDataValue.Create(Self);
          try
            aItem.Header.ReadHeader(vStream);
            vStream.ReadBufferUntil(@Boundary[1], Length(Boundary), True, Res, Len, Matched);
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

function TmnFormData.ReadCallback(vStream: TmnBufferStream): Boolean;
var
  Res: PByte;
  len: TFileSize;
  S: UTF8String;
  aHeader: TmnHeader;
  aType: string;
  aItem: TmnFormDataItem;
  ContentType: TStringList;
  Matched: Boolean;
  aDataHeader: TmnHeader;
begin
  aHeader := TmnHeader.Create;
  try
    aHeader.ReadHeader(vStream);
    ContentType := aHeader.Field['Content-Type'].CreateSubValues;
    if SameText(ContentType[0], 'multipart/form-data') then
    begin
      Boundary := UTF8Encode(ContentType.Values['boundary']);
      Boundary := '--' + Boundary;

      vStream.ReadLineUTF8(S, True);
      if S = Boundary then
      begin
        Boundary := vStream.EndOfLine + Boundary;
        while True do
        begin
          aItem := NewItem(vStream);
          vStream.ReadUntilCallback(aItem, @Boundary[1], Length(Boundary), True, ReadUntilCallback, Matched);

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

procedure TmnFormData.ReadUntilCallback(vData: TObject; const Buffer; Count: Longint);
var
  s: string;
begin
  s := TEncoding.UTF8.GetString(PByte(Buffer), Count);
  Last.Name := Last.Name + s;
end;

function TmnFormData.Write(vStream: TmnBufferStream): Boolean;
var
  itm: TmnFormDataItem;
begin

  for itm in Self do
  begin
    vStream.WriteLineUTF8('--'+Boundary);
    itm.Write(vStream);
  end;
  vStream.WriteLineUTF8('--'+Boundary+'--');
end;

{ TmnFormDataValue }

procedure TmnFormDataValue.DoWrite(vStream: TmnBufferStream);
begin
  inherited;
  vStream.WriteUTF8(Value);
end;

{ TmnFormDataFileName }

procedure TmnFormDataFileName.DoWrite(vStream: TmnBufferStream);
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

{ TmnFormDataMemory }

procedure TmnFormDataMemory.DoWrite(vStream: TmnBufferStream);
begin
  inherited;
  vStream.WriteStream(Memory, Memory.Size);
end;

end.
