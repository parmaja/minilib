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
  TmnFormDataItem = class(TmnNamedObject)
  public
    Header: TmnHeader;
    constructor Create(vHeader: TmnHeader);
    destructor Destroy; override;
  end;

  TmnFormDataValue = class(TmnFormDataItem)
  public
    Value: string;
  end;

  TmnFormDataFileName = class(TmnFormDataItem)
  public
    FileName: string;
  end;

  TmnFormDataMemory = class(TmnFormDataItem)
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
    Boundary: AnsiString;
    function Read(vStream: TmnBufferStream): Boolean;
  end;

implementation

{ TmnFormDataItem }

constructor TmnFormDataItem.Create(vHeader: TmnHeader);
begin
  inherited Create;
  if vHeader <> nil then
    Header := vHeader
  else
    Header := TmnHeader.Create;
end;

destructor TmnFormDataItem.Destroy;
begin
  FreeAndNil(Header);
  inherited;
end;

{ TmnFormData }

procedure CopyString(out S: string; Buffer: Pointer; Len: Integer); inline;
begin
  if Len <> 0 then
  begin
    {$ifdef FPC}S := '';{$endif}
    SetLength(S, Len div SizeOf(widechar));
    Move(PByte(Buffer)^, PByte(S)^, Len);
  end
  else
    S := '';
end;

function TmnFormData.Read(vStream: TmnBufferStream): Boolean;
var
  m: Boolean;
  res: PByte;
  len: TFileSize;
  S: string;
  aHeader: TmnHeader;
  aType: string;
  aItem: TmnFormDataItem;

begin
  while not (cloRead in vStream.Done) do
  begin
    aHeader := TmnHeader.Create;
    aHeader.ReadHeader(vStream);
    aType := aHeader['Content-Type'];

//    ContentLength := Request.Header['Content-Length'].AsInteger;
//    aHeader.
  end;

  Result := vStream.ReadBufferUntil(@Boundary[1], ByteLength(Boundary), True, res, len, m);
  CopyString(S, res, len);
  FreeMem(res);
end;

end.
