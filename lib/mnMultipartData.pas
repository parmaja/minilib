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

  { TMPDItem }

  TMPDItem = class abstract(TDON_Pair)
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

  TMPDString = class(TMPDItem)
  protected
    procedure DoPrepare; override;
    procedure DoRead(const Buffer; Count: Longint); override;
    procedure DoWrite(vStream: TmnBufferStream); override;    
  end;
  
  TMPDFile = class(TMPDItem)
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

  TMPDMemoy = class(TMPDItem) //Unused
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
    function DoCreateItem(vStream: TmnBufferStream; vHeader: TmnHeader): TMPDItem; virtual;
    function CreateItem(vStream: TmnBufferStream): TMPDItem;
  public
    constructor Create(AParent: TDON_Parent = nil); overload;
    constructor Create(ABoundary: string = ''; AOutputPath: string = ''); overload;

    function Find(AName: string): TMPDItem;
    
    function Read(vStream: TmnBufferStream): Boolean;
    function Write(vStream: TmnBufferStream): Boolean;

    property Boundary: string read FBoundary write FBoundary;
    property OutputPath: string read FOutputPath write FOutputPath;
    property ShortFileNames: Boolean read FShortFileNames write FShortFileNames;
  end;

implementation

uses
  mnMIME;

{ TMPDItem }

constructor TMPDItem.Create(AParent: TDON_Object_Value);
begin
  inherited Create(AParent);  
  Header := TmnHeader.Create;
end;

destructor TMPDItem.Destroy;
begin
  FreeAndNil(FHeader);
  inherited;
end;

procedure TMPDItem.DoWrite(vStream: TmnBufferStream);
begin

end;

procedure TMPDItem.Prepare;
begin
  Header.Values['Content-Disposition'] := Format('form-data; name="%s"', [Name]);
end;

function TMPDItem.Read(vStream: TmnBufferStream; const vBoundary: utf8string): Boolean;
begin
  DoReadPrepare;
  try
    vStream.ReadUntilCallback(Self, @vBoundary[1], Length(vBoundary), True, ReadUntilCallback, Result);
  finally
    DoReadUnPrepare;
  end;
end;

procedure TMPDItem.ReadUntilCallback(vData: TObject; const Buffer; Count: Longint);
begin
  DoRead(Buffer, Count);
end;

procedure TMPDItem.SetHeader(const Value: TmnHeader);
begin
  FreeAndNil(FHeader);
  FHeader := Value;
end;

procedure TMPDItem.DoPrepare;
begin
end;

procedure TMPDItem.DoRead(const Buffer; Count: Longint);
begin
end;

procedure TMPDItem.DoReadPrepare;
begin
end;

procedure TMPDItem.DoReadUnPrepare;
begin
end;

procedure TMPDItem.Write(vStream: TmnBufferStream);
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

constructor TmnMultipartData.Create(AParent: TDON_Parent);
begin
  inherited Create(AParent);
end;

constructor TmnMultipartData.Create(ABoundary, AOutputPath: string);
begin
  Create(nil);
  OutputPath := AOutputPath;
  Boundary := ABoundary;
end;

function TmnMultipartData.CreateItem(vStream: TmnBufferStream): TMPDItem;
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
        Result := TMPDFile.Create(Self);
        aType.Name := 'filename';
        aType.Options := [];
        if not ShortFileNames then        
          s := IncludePathDelimiter(OutputPath) + s;
        TMPDFile(Result).Value := TDON_String_Value.Create(Result, s, aType);
      end
      else
      begin
        Result := TMPDString.Create(Self);
        aType.Name := 'string';
        aType.Options := [];        
        TMPDFile(Result).Value := TDON_String_Value.Create(Result, '', aType);
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

function TmnMultipartData.DoCreateItem(vStream: TmnBufferStream; vHeader: TmnHeader): TMPDItem;
begin
  Result := nil;
end;

function TmnMultipartData.Find(AName: string): TMPDItem;
begin
  Result := ByPath(AName) as TMPDItem;
end;

function TmnMultipartData.Read(vStream: TmnBufferStream): Boolean;
var
  aItem: TMPDItem;
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
    if itm is TMPDItem then    
      (itm as TMPDItem).Write(vStream);
  end;
  vStream.WriteUTF8Line('--' + Boundary + '--');
  Result := True;
end;

{ TMPDString }

procedure TMPDString.DoPrepare;
begin
  inherited;
  Header.Values['Content-Type'] := 'text/plan';
end;

procedure TMPDString.DoRead(const Buffer; Count: Longint);
var
  s: string;
begin
  s := StringOfUTF8(PByte(Buffer), Count);
  if Value = nil then
    Value := TDON_String_Value.Create(Self, s)
  else
    (Value as TDON_String_Value).AsString := (Value as TDON_String_Value).AsString + s;
end;

procedure TMPDString.DoWrite(vStream: TmnBufferStream);
begin
  inherited;
  vStream.WriteUTF8String(Value.AsString);
end;

{ TMPDFile }

procedure TMPDFile.DoPrepare;
begin
  inherited;
  Header.Values['Content-Disposition'] := Format('form-data; name="%s"; filename="%s"', [Name, Value.AsString]);
  Header.Values['Content-Type'] := DocumentToContentType(Value.AsString);
end;

procedure TMPDFile.DoRead(const Buffer; Count: Longint);
begin
  if FFileStream <> nil then  
    FFileStream.Write(PByte(Buffer)^, Count);
end;

procedure TMPDFile.DoReadPrepare;
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

procedure TMPDFile.DoReadUnPrepare;
begin
  FreeAndNil(FFileStream);
end;

procedure TMPDFile.DoWrite(vStream: TmnBufferStream);
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

function TMPDFile.GetFileName: string;
begin
end;

{ TMPDMemoy }

procedure TMPDMemoy.DoPrepare;
begin
  inherited;
  Header.Values['Content-Type'] := ContentType;
end;

procedure TMPDMemoy.DoWrite(vStream: TmnBufferStream);
begin
  inherited;
  vStream.WriteStream(Memory, Memory.Size);
end;

end.
