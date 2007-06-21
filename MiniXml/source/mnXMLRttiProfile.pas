unit mnXMLRttiProfile;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  SysUtils, Variants, Classes, Contnrs,
  mnXMLStreams, mnXMLRttiReader, mnXMLRttiWriter;

type
  EmnProfileException = class(Exception);

  TmnProfile = class(TInterfacedPersistent, IStreamPersist)
  private
    FChanged: Boolean;
  protected
    procedure Loading; virtual;
    procedure Loaded; virtual;
    procedure Saving; virtual;
    procedure Saved; virtual;
    procedure LoadDefault; virtual;
  public
    constructor Create;
    procedure Clear; virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(FileName: string);
    procedure SafeLoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    property Changed: Boolean read FChanged write FChanged;
  end;

  TmnComponentProfile = class(TComponent, IStreamPersist)
  private
    FChanged: Boolean;
  protected
    procedure Loading; virtual;
    procedure Loaded; override;
    procedure Saving; virtual;
    procedure Saved; virtual;
    procedure LoadDefault; virtual;
  public
    constructor Create(AOwner:TComponent); override;
    procedure Clear; virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(FileName: string);
    procedure SafeLoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    property Changed: Boolean read FChanged write FChanged;
  end;

  TmnXMLItem = class(TPersistent)
  public
    constructor Create; virtual;
  end;

  TmnXMLItemClass = class of TmnXMLItem;

  TmnXMLItems = class(TObjectList)
  private
    function GetItem(Index: Integer): TmnXMLItem;
    procedure SetItem(Index: Integer; const Value: TmnXMLItem);
  public
    property Items[Index: Integer]: TmnXMLItem read GetItem write SetItem; default;
  end; 

procedure XMLReadObjectStream(Instance: TObject; Stream: TStream);
procedure XMLWriteObjectStream(Instance: TObject; Stream: TStream);
procedure XMLReadObjectString(Instance: TObject; S: string);
procedure XMLWriteObjectString(Instance: TObject; var S: string);
procedure XMLReadObjectFile(Instance: TObject; const FileName: string);
procedure XMLWriteObjectFile(Instance: TObject; const FileName: string);

implementation

type
  TSafeReader = class(TReader)
  private
  protected
    function Error(const Message: string): Boolean; override;
  public
  end;

function TSafeReader.Error(const Message: string): Boolean;
begin
  Result := True;
end;

{ TmnProfile }

procedure TmnProfile.Clear;
begin
end;

constructor TmnProfile.Create;
begin
  inherited Create;
  LoadDefault;
end;

procedure TmnProfile.LoadDefault;
begin
end;

procedure TmnProfile.Loaded;
begin
end;

procedure TmnProfile.LoadFromFile(FileName: string);
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

procedure TmnProfile.LoadFromStream(Stream: TStream);
var
  aReader: TmnXMLRttiReader;
begin
  Loading;
  aReader := TmnXMLRttiReader.Create(TmnXMLStream.Create(Stream, False));
  try
    aReader.ReadRoot(Self);
    aReader.Stop;
    FChanged := False;
  finally
    aReader.Free;
  end;
  Loaded;
end;

procedure TmnProfile.Loading;
begin
end;

procedure TmnProfile.Saved;
begin
end;

procedure TmnProfile.SafeLoadFromFile(FileName: string);
begin
  try
    if FileExists(FileName) then
      LoadFromFile(FileName);
  except
    Clear;
  end;
end;

procedure TmnProfile.SaveToFile(FileName: string);
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

procedure TmnProfile.SaveToStream(Stream: TStream);
var
  aWriter: TmnXMLRttiWriter;
begin
  Saving;
  aWriter := TmnXMLRttiWriter.Create(TmnXMLStream.Create(Stream, False));
  aWriter.Smart := True;
  aWriter.WriteTypes := False;
  try
    aWriter.WriteRoot(Self);
    aWriter.Stop;
    FChanged := False;
  finally
    aWriter.Free;
  end;
  Saved;
end;

procedure TmnProfile.Saving;
begin
end;

procedure XMLReadObjectStream(Instance: TObject; Stream: TStream);
var
  aReader: TmnXMLRttiReader;
begin
  aReader := TmnXMLRttiReader.Create(TmnXMLStream.Create(Stream, False));
  try
    aReader.ReadRoot(Instance);
    aReader.Stop;
  finally
    aReader.Free;
  end;
end;

procedure XMLWriteObjectStream(Instance: TObject; Stream: TStream);
var
  aWriter: TmnXMLRttiWriter;
begin
  aWriter := TmnXMLRttiWriter.Create(TmnXMLStream.Create(Stream, False));
  aWriter.Smart := True;
  aWriter.WriteTypes := False;
  try
    aWriter.WriteRoot(Instance);
    aWriter.Stop;
  finally
    aWriter.Free;
  end;
end;

procedure XMLReadObjectString(Instance: TObject; S: string);
var
  Stream: TStream;
begin
  Stream := TStringStream.Create(S);
  try
    XMLReadObjectStream(Instance, Stream);
  finally
    Stream.Free;
  end;
end;

procedure XMLWriteObjectString(Instance: TObject; var S: string);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    XMLWriteObjectStream(Instance, Stream);
    S := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure XMLReadObjectFile(Instance: TObject; const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    XMLReadObjectStream(Instance, Stream);
  finally
    Stream.Free;
  end;
end;

procedure XMLWriteObjectFile(Instance: TObject; const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    XMLWriteObjectStream(Instance, Stream);
  finally
    Stream.Free;
  end;
end;

{ TmnXMLItems }

function TmnXMLItems.GetItem(Index: Integer): TmnXMLItem;
begin
  Result := inherited Items[Index] as TmnXMLItem;
end;

procedure TmnXMLItems.SetItem(Index: Integer; const Value: TmnXMLItem);
begin
  inherited Items[Index] := Value;
end;

{ TmnXMLItem }

constructor TmnXMLItem.Create;
begin
  inherited Create;
end;

{ TmnComponentProfile }

procedure TmnComponentProfile.Clear;
begin
end;

constructor TmnComponentProfile.Create(AOwner:TComponent); 
begin
  inherited;
  LoadDefault;
end;

procedure TmnComponentProfile.LoadDefault;
begin
end;

procedure TmnComponentProfile.Loaded;
begin
  inherited;
end;

procedure TmnComponentProfile.LoadFromFile(FileName: string);
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

procedure TmnComponentProfile.LoadFromStream(Stream: TStream);
var
  aReader: TmnXMLRttiReader;
begin
  Loading;
  aReader := TmnXMLRttiReader.Create(TmnXMLStream.Create(Stream, False));
  try
    aReader.ReadRoot(Self);
    aReader.Stop;
    FChanged := False;
  finally
    aReader.Free;
  end;
  Loaded;
end;

procedure TmnComponentProfile.Loading;
begin
end;

procedure TmnComponentProfile.SafeLoadFromFile(FileName: string);
begin
  try
    if FileExists(FileName) then
      LoadFromFile(FileName);
  except
    Clear;
  end;
end;

procedure TmnComponentProfile.Saved;
begin
end;

procedure TmnComponentProfile.SaveToFile(FileName: string);
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

procedure TmnComponentProfile.SaveToStream(Stream: TStream);
var
  aWriter: TmnXMLRttiWriter;
begin
  Saving;
  aWriter := TmnXMLRttiWriter.Create(TmnXMLStream.Create(Stream, False));
  aWriter.Smart := True;
  aWriter.WriteTypes := False;
  try
    aWriter.WriteRoot(Self);
    aWriter.Stop;
    FChanged := False;
  finally
    aWriter.Free;
  end;
  Saved;
end;

procedure TmnComponentProfile.Saving;
begin
end;

end.

