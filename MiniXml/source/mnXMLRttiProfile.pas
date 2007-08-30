unit mnXMLRttiProfile;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
 
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Windows,      
  SysUtils, Variants, Classes, Contnrs,
  mnXMLStreams, mnXMLRttiReader, mnXMLRttiWriter;

type
  EmnProfileException = class(Exception);

  TmnProfile = class(TPersistent, IStreamPersist)
  private
    FChanged: Boolean;
  protected
    procedure Loading; virtual;
    procedure Loaded; virtual;
    procedure Saving; virtual;
    procedure Saved; virtual;
    procedure LoadDefault; virtual;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create;
    procedure Clear; virtual;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(FileName: string);
    procedure SafeLoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure SaveToString(var S: string);
    procedure LoadFromString(S: string);
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

{ ProfileList }

  TmnXMLItem = Class(TmnProfile)
  public
  end;
  
  { TmnXMLItems }

  TmnXMLItems = class(TmnXMLItem)
  private
    FList: TObjectList;
    function GetItem(Index: Integer): TmnXMLItem;
    procedure SetItem(Index: Integer; const Value: TmnXMLItem);
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);
  protected
    function DoCreateItem:TmnXMLItem; virtual; abstract;
  public
    constructor Create(AOwnsObjects: Boolean = True); 
    destructor Destroy; override;
    procedure Update; virtual;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Sort(Compare: TListSortCompare);
    function Add(AItem: TmnXMLItem): Integer;
    function Extract(AItem: TmnXMLItem): TmnXMLItem;
    function Remove(AItem: TmnXMLItem): Integer;
    procedure Insert(Index: Integer; AItem: TmnXMLItem);
    function IndexOf(AItem: TmnXMLItem): Integer;
    function First: TmnXMLItem;
    function Last: TmnXMLItem;
    function CreateItem:TmnXMLItem;
    property Count:Integer read GetCount write SetCount;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
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

procedure TmnProfile.LoadFromString(S: string);
var
  aStream: TStringStream;
begin
  if S <> '' then
  begin
    aStream := TStringStream.Create('');
    try
      aStream.WriteString(S);
      aStream.Seek(0, soFromBeginning);
      LoadFromStream(aStream);
    finally
      aStream.Free;
    end;
  end;
end;

procedure TmnProfile.Loading;
begin
end;

function TmnProfile.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
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

function TmnProfile._AddRef: Integer;
begin
  Result := 0;
end;

function TmnProfile._Release: Integer;
begin
  Result := 0;
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

procedure TmnProfile.SaveToString(var S: string);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToStream(Stream);
    S := Stream.DataString;
  finally
    Stream.Free;
  end;
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

{ TmnXMLItems }

type
  TMyObjectList = Class(TObjectList)
  public
    FItems: TmnXMLItems;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

{ TMyObjectList }

procedure TMyObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  FItems.Update;
end;

function TmnXMLItems.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TmnXMLItems.GetItem(Index: Integer): TmnXMLItem;
begin
  Result := FList[Index] as TmnXMLItem;
end;

function TmnXMLItems.GetOwnsObjects: Boolean;
begin
  Result := FList.OwnsObjects;
end;

function TmnXMLItems.IndexOf(AItem: TmnXMLItem): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

procedure TmnXMLItems.Insert(Index: Integer; AItem: TmnXMLItem);
begin
  FList.Insert(Index, AItem);
end;

function TmnXMLItems.Last: TmnXMLItem;
begin
  Result := FList.Last as TmnXMLItem;
end;

function TmnXMLItems.Remove(AItem: TmnXMLItem): Integer;
begin
  Result := FList.Remove(AItem);
end;

procedure TmnXMLItems.SetCount(const Value: Integer);
begin
  FList.Count := Value;
end;

procedure TmnXMLItems.SetItem(Index: Integer; const Value: TmnXMLItem);
begin
  FList[Index] := Value;
end;

procedure TmnXMLItems.SetOwnsObjects(const Value: Boolean);
begin
  FList.OwnsObjects := Value;
end;

procedure TmnXMLItems.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

procedure TmnXMLItems.Update;
begin
end;

function TmnXMLItems.Add(AItem: TmnXMLItem): Integer;
begin
  Result := FList.Add(AItem);
end;

procedure TmnXMLItems.Clear;
begin
  FList.Clear;
  inherited;
end; 

constructor TmnXMLItems.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FList := TMyObjectList.Create(AOwnsObjects);
  (FList as TMyObjectList).FItems := Self;
end;

function TmnXMLItems.CreateItem: TmnXMLItem;
begin
  Result := DoCreateItem;
  Add(Result);
end;

procedure TmnXMLItems.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

destructor TmnXMLItems.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TmnXMLItems.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TmnXMLItems.Extract(AItem: TmnXMLItem): TmnXMLItem;
begin
  Result := FList.Extract(AItem) as TmnXMLItem;
end;

function TmnXMLItems.First: TmnXMLItem;
begin
  Result := FList.First as TmnXMLItem;
end;

end.

