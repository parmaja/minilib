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
  mnJSON, mnDON, mnStreamUtils;

type
  TmnMultipartData = class;

  { TmnMultipartDataItem }

  TDON_MPDItem = class abstract(TDON_Pair)
  private
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
  public
    constructor Create(AParent: TDON_Object_Value);
    destructor Destroy; override;
    procedure Write(vStream: TmnBufferStream);
    function Read(vStream: TmnBufferStream; const vBoundary: utf8string): Boolean;
    property Header: TmnHeader read FHeader write SetHeader;
  end;

  { Values }

  TmnMultipartDataString = class(TDON_MPDItem)
  protected
    procedure DoPrepare; override;
    procedure DoRead(const Buffer; Count: Longint); override;
    procedure DoWrite(vStream: TmnBufferStream); override;    
  end;
  
  TmnMultipartDataFileName = class(TDON_MPDItem)
  protected
    FFileStream: TFileStream;
    function GetFileName: string;
    procedure DoReadPrepare; override;
    procedure DoReadUnPrepare; override;
    procedure DoPrepare; override;
    procedure DoRead(const Buffer; Count: Longint); override;
    procedure DoWrite(vStream: TmnBufferStream); override;    
  public
  end;

  TmnMultipartDataMemory = class(TDON_MPDItem)
  protected
    procedure DoWrite(vStream: TmnBufferStream); override;
    procedure DoPrepare; override;
  public
    FileName: string;
    ContentType: string;
    Memory: TMemoryStream;
  end;

  TmnMultipartData = class(TDON_Object_Value)
  private
    FBoundary: string;
    FOutputPath: string;
    FShortFileNames: Boolean;
  protected
    function DoCreateItem(vStream: TmnBufferStream; vHeader: TmnHeader): TDON_MPDItem; virtual;
    function CreateItem(vStream: TmnBufferStream): TDON_MPDItem;
  public
    function Read(vStream: TmnBufferStream): Boolean;
    function Write(vStream: TmnBufferStream): Boolean;

    property Boundary: string read FBoundary write FBoundary;
    property OutputPath: string read FOutputPath write FOutputPath;
    property ShortFileNames: Boolean read FShortFileNames write FShortFileNames;
  end;

implementation

uses
  mnMIME;

{ TDON_MPDItem }

constructor TDON_MPDItem.Create(AParent: TDON_Object_Value);
begin
  inherited Create(AParent);  
  Header := TmnHeader.Create;
end;

destructor TDON_MPDItem.Destroy;
begin
  FreeAndNil(FHeader);
  inherited;
end;

procedure TDON_MPDItem.DoWrite(vStream: TmnBufferStream);
begin

end;

procedure TDON_MPDItem.Prepare;
begin
  Header.Values['Content-Disposition'] := Format('form-data; name="%s"', [Name]);
end;

function TDON_MPDItem.Read(vStream: TmnBufferStream; const vBoundary: utf8string): Boolean;
begin
  DoReadPrepare;
  try
    vStream.ReadUntilCallback(Self, @vBoundary[1], Length(vBoundary), True, ReadUntilCallback, Result);
  finally
    DoReadUnPrepare;
  end;
end;

procedure TDON_MPDItem.ReadUntilCallback(vData: TObject; const Buffer; Count: Longint);
begin
  DoRead(Buffer, Count);
end;

procedure TDON_MPDItem.SetHeader(const Value: TmnHeader);
begin
  FreeAndNil(FHeader);
  FHeader := Value;
end;

procedure TDON_MPDItem.DoPrepare;
begin
end;

procedure TDON_MPDItem.DoRead(const Buffer; Count: Longint);
begin
end;

procedure TDON_MPDItem.DoReadPrepare;
begin
end;

procedure TDON_MPDItem.DoReadUnPrepare;
begin
end;

procedure TDON_MPDItem.Write(vStream: TmnBufferStream);
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

function TmnMultipartData.CreateItem(vStream: TmnBufferStream): TDON_MPDItem;
var
  aHeader: TmnHeader;
  aDisposition: string;
  s: string;
  aType: TmnJsonStringType;
begin
  aHeader := TmnHeader.Create;
  try
    aHeader.ReadHeader(vStream);

    aDisposition := aHeader['Content-Disposition'];

    Result := DoCreateItem(vStream, aHeader);
    if Result = nil then
    begin
      if GetSubValue(aDisposition, 'filename', s) and (s <> '') then
      begin
        Result := TmnMultipartDataFileName.Create(Self);
        aType.Name := 'filename';
        aType.Options := [];
        if not ShortFileNames then        
          s := IncludePathDelimiter(OutputPath) + s;
        TmnMultipartDataFileName(Result).Value := TDON_String_Value.Create(Result, s, aType);
      end
      else
      begin
        Result := TmnMultipartDataString.Create(Self);
        aType.Name := 'string';
        aType.Options := [];        
        TmnMultipartDataFileName(Result).Value := TDON_String_Value.Create(Result, '', aType);
      end;
    end;
    GetSubValue(aDisposition, 'name', s);
    Result.Name := s;
    Result.Header := aHeader;
  except
    FreeAndNil(aHeader);
    raise;
  end;

end;

function TmnMultipartData.DoCreateItem(vStream: TmnBufferStream; vHeader: TmnHeader): TDON_MPDItem;
begin
  Result := nil;
end;

function TmnMultipartData.Read(vStream: TmnBufferStream): Boolean;
var
  aItem: TDON_MPDItem;
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
      end
      else
        AddPair(aItem);

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
  itm: TDON_Pair;
begin
  for itm in Self do
  begin
    vStream.WriteUTF8Line('--' + Boundary);
    if itm is TDON_MPDItem then    
      (itm as TDON_MPDItem).Write(vStream);
  end;
  vStream.WriteUTF8Line('--' + Boundary + '--');
  Result := True;
end;

{ TmnMultipartDataString }

procedure TmnMultipartDataString.DoPrepare;
begin
  inherited;
  Header.Values['Content-Type'] := 'text/plan';
end;

procedure TmnMultipartDataString.DoRead(const Buffer; Count: Longint);
var
  s: string;
begin
  s := StringOfUTF8(PByte(Buffer), Count);
  if Value = nil then
    Value := TDON_String_Value.Create(Self, s)
  else
    (Value as TDON_String_Value).AsString := (Value as TDON_String_Value).AsString + s;
end;

procedure TmnMultipartDataString.DoWrite(vStream: TmnBufferStream);
begin
  inherited;
  vStream.WriteUTF8String(Value.AsString);
end;

{ TmnMultipartDataFileName }

procedure TmnMultipartDataFileName.DoPrepare;
begin
  inherited;
  Header.Values['Content-Disposition'] := Format('form-data; name="%s"; filename="%s"', [Name, Value.AsString]);
  Header.Values['Content-Type'] := DocumentToContentType(Value.AsString);
end;

procedure TmnMultipartDataFileName.DoRead(const Buffer; Count: Longint);
begin
  if FFileStream <> nil then  
    FFileStream.Write(PByte(Buffer)^, Count);
end;

procedure TmnMultipartDataFileName.DoReadPrepare;
var
  aPath, aFile: string;
begin
  if (Parent as TmnMultipartData).ShortFileNames then
    aFile := IncludePathDelimiter((Parent as TmnMultipartData).OutputPath) + Value.AsString
  else
    aFile := Value.AsString;
  
  if aFile <> '' then
  begin
    aPath := ExtractFilePath(aFile);
    if aPath <> '' then
      ForceDirectories(aPath);
  end;
  FFileStream := TFileStream.Create(aFile, fmCreate);
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
  f := TFileStream.Create(GetFileName, fmOpenRead);
  try
    vStream.WriteStream(f, f.Size);
  finally
    f.Free;
  end;

end;

function TmnMultipartDataFileName.GetFileName: string;
begin
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
