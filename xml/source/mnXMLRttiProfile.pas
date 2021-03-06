unit mnXMLRttiProfile;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ELSE}
{$DEFINE WINDOWS}
{$ENDIF}

interface

uses
{$IFDEF WINDOWS}
  Windows, 
{$ENDIF}
  SysUtils, Variants, Classes, Contnrs,
  mnStreams, mnXMLRtti, mnXMLRttiReader, mnXMLRttiWriter;

type
  EmnProfileException = class(Exception);

  TmnXMLProfileStates = set of (psChanged, psLoading, psSaving);

  { TmnXMLProfile }
  
  TmnXMLProfile = class(TPersistent, IStreamPersist{$ifdef FPC}, IRttiFiler{$endif})
  private
    FAge: TDateTime;
    FProfileState: TmnXMLProfileStates;
  protected
    procedure Loading; virtual;
    procedure Loaded(Failed: Boolean); virtual;
    procedure Saving; virtual;
    procedure Saved(Failed: Boolean); virtual;
    procedure LoadDefault; virtual;
    function _AddRef: Integer; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
    function _Release: Integer; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
    {$ifdef FPC}
    procedure RttiCreateObject(var vObject: TObject; vInstance: TObject; vObjectClass: TClass; const vClassName, vName: string); virtual;
    {$endif}
  public
    constructor Create;
    procedure Clear; virtual;
    function QueryInterface({$ifdef FPC}constref{$else}const{$endif} IID: TGUID; out Obj): HResult; virtual; {$ifdef WINDOWS}stdcall{$else}cdecl{$endif};
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(FileName: string); virtual;
    procedure SafeLoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string); virtual;
    procedure SaveToString(var S: string);
    procedure LoadFromString(S: string);
    property ProfileState: TmnXMLProfileStates read FProfileState write FProfileState;
    property Age: TDateTime read FAge write FAge;
  end;

  { TmnComponentProfile }

  TmnComponentProfile = class(TComponent, IStreamPersist)
  private
    FAge: TDateTime;
    FChanged: Boolean;
  protected
    procedure Loading; {$ifdef fpc} override; {$else} virtual; {$endif} //there is Loading in FPC
    procedure Loaded; override;
    procedure Saving; virtual;
    procedure Saved; virtual;
    procedure LoadDefault; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear; virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(FileName: string);
    procedure SafeLoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    property Changed: Boolean read FChanged write FChanged;
    property Age: TDateTime read FAge write FAge;
  end;

  { TmnXMLItem }

  TmnXMLItem = class(TmnXMLProfile)
  private
  public
  end;

  TmnXMLItemClass = class of TmnXMLItem;

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
    function DoCreateItem(AClass: TmnXMLItemClass): TmnXMLItem; virtual;
    procedure Deleted(Index: Integer); virtual;
    procedure Inserted(Index: Integer); virtual;
  public
    constructor Create(AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    procedure Update; virtual;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Sort(Compare: TListSortCompare);
    function Add(AItem: TmnXMLItem): Integer;
    function Extract(AItem: TmnXMLItem): TmnXMLItem;
    function Remove(AItem: TmnXMLItem): Integer;
    procedure Insert(Index: Integer; AItem: TmnXMLItem);
    function IndexOf(AItem: TmnXMLItem): Integer;
    function First: TmnXMLItem;
    function Last: TmnXMLItem;
    function CreateItem(AClass: TmnXMLItemClass): TmnXMLItem;
    property Count: Integer read GetCount write SetCount;
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

{ TmnXMLProfile }

procedure TmnXMLProfile.Clear;
begin
end;

constructor TmnXMLProfile.Create;
begin
  inherited Create;
  LoadDefault;
end;

procedure TmnXMLProfile.LoadDefault;
begin
end;

procedure TmnXMLProfile.Loaded(Failed: Boolean);
begin
end;

procedure TmnXMLProfile.LoadFromFile(FileName: string);
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

procedure TmnXMLProfile.LoadFromStream(Stream: TStream);
var
  aReader: TmnXMLRttiReader;
  Failed: Boolean;
begin
  Failed := True;
  FProfileState := FProfileState + [psLoading];
  Loading;
  try
    aReader := TmnXMLRttiReader.Create(TmnWrapperStream.Create(Stream, False));
    try
      aReader.ReadRoot(Self);
      aReader.Stop;
      FProfileState := FProfileState - [psChanged];
    finally
      aReader.Free;
    end;
    Failed := False;
  finally
    FProfileState := FProfileState - [psLoading];
    Loaded(Failed);
  end;
end;

procedure TmnXMLProfile.LoadFromString(S: string);
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

procedure TmnXMLProfile.Loading;
begin
end;

function TmnXMLProfile.QueryInterface({$ifdef FPC}constref{$else}const{$endif FPC}IID: TGUID; out Obj): HResult;  
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TmnXMLProfile.Saved(Failed: Boolean);
begin
end;

procedure TmnXMLProfile.SafeLoadFromFile(FileName: string);
begin
  try
    if FileExists(FileName) then
      LoadFromFile(FileName);
  except
    Clear;
  end;
end;

procedure TmnXMLProfile.SaveToFile(FileName: string);
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

procedure TmnXMLProfile.SaveToStream(Stream: TStream);
var
  aWriter: TmnXMLRttiWriter;
  Failed: Boolean;
begin
  Failed := True;
  FProfileState := FProfileState + [psSaving];
  Saving;
  try
    aWriter := TmnXMLRttiWriter.Create(TmnWrapperStream.Create(Stream, False));
    aWriter.Smart := True;
    aWriter.WriteTypes := False;
    try
      aWriter.WriteRoot(Self);
      aWriter.Stop;
      FProfileState := FProfileState - [psChanged];
    finally
      aWriter.Free;
    end;
    Failed := False;
  finally
    FProfileState := FProfileState - [psSaving];
    Saved(Failed);
  end;
end;

procedure TmnXMLProfile.Saving;
begin
end;

function TmnXMLProfile._AddRef: Integer;
begin
  Result := 0;
end;

function TmnXMLProfile._Release: Integer;
begin
  Result := 0;
end;

{$ifdef FPC}
procedure TmnXMLProfile.RttiCreateObject(var vObject: TObject; vInstance: TObject; vObjectClass: TClass; const vClassName, vName: string);
begin
end;
{$endif}

procedure XMLReadObjectStream(Instance: TObject; Stream: TStream);
var
  aReader: TmnXMLRttiReader;
begin
  aReader := TmnXMLRttiReader.Create(TmnWrapperStream.Create(Stream, False));
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
  aWriter := TmnXMLRttiWriter.Create(TmnWrapperStream.Create(Stream, False));
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

procedure TmnXMLProfile.SaveToString(var S: string);
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

constructor TmnComponentProfile.Create(AOwner: TComponent);
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
  aReader := TmnXMLRttiReader.Create(TmnWrapperStream.Create(Stream, False));
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
  aWriter := TmnXMLRttiWriter.Create(TmnWrapperStream.Create(Stream, False));
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
  TMyObjectList = class(TObjectList)
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
  Inserted(Index);
end;

procedure TmnXMLItems.Inserted(Index: Integer);
begin
end;

function TmnXMLItems.Last: TmnXMLItem;
begin
  Result := FList.Last as TmnXMLItem;
end;

procedure TmnXMLItems.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

function TmnXMLItems.Remove(AItem: TmnXMLItem): Integer;
begin
  Result := IndexOf(AItem);
  if Result >= 0 then
    Delete(Result);
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

function TmnXMLItems.CreateItem(AClass: TmnXMLItemClass): TmnXMLItem;
begin
  Result := DoCreateItem(AClass);
  Add(Result);
end;

procedure TmnXMLItems.Delete(Index: Integer);
begin
  FList.Delete(Index);
  Deleted(Index);
end;

procedure TmnXMLItems.Deleted(Index: Integer);
begin
end;

destructor TmnXMLItems.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TmnXMLItems.DoCreateItem(AClass: TmnXMLItemClass): TmnXMLItem;
begin
  Result := AClass.Create;
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

