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
  TmnFormDataItem = class abstract(TmnNamedObject)
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
    Boundary: UTF8String;
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
      Boundary := '--'+Boundary;

      vStream.ReadLineUTF8(S, True);
      if S = Boundary then
      begin
        while True do
        begin
          aDataHeader := TmnHeader.Create;
          try
            aDataHeader.ReadHeader(vStream);
            vStream.ReadBufferUntil(@Boundary[1], Length(Boundary), True, Res, Len, Matched);
            if Matched then
            begin
              aItem := TmnFormDataValue.Create(aDataHeader);
              CopyString(S, Res, Len);
              FreeMem(res);
              Add(aItem);
              aDataHeader := nil;
              vStream.ReadLineUTF8(S, True);
              if S = '--' then
              begin
                break;
              end;
            end;
          finally
            aDataHeader.Free; //if it nil it is ok
          end
        end;
      end;
    end;
  finally
    aHeader.Free;
  end;
end;

end.
