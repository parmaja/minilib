unit LangClasses;
{**
 *  This file is part of the "Mini Library" http://www.sourceforge.net/projects/minilib
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$ifdef FPC}
{$mode objfpc}
{$endif}
{$M+}{$H+}

{
  Languages
    Language
      Charset
      RightToLeft
        LangContents
          LangItem
            ID
            Text
        LangContents
          LangItem
            ID
            Text
    Language
      Charset
      RightToLeft
        LangContents
          LangItem
            ID
            Text
        LangContents
          LangItem
            ID
            Text
}
interface

uses
  SysUtils, Variants, Classes, Contnrs;

type
  ELangException = class(Exception);

  TLangEncoding = (lncNone, lncAnsi, lncUTF8);

  TLangContents = class;

  { TLangItem }

  TLangItem = class(TObject)
  private
    FContents: TLangContents;
    FModified: Boolean;
    FVisible: Boolean;
    function GetDisplayID: string;
    function GetDisplayText: string;
    procedure SetDisplayID(AValue: string);
    procedure SetDisplayText(AValue: string);
    procedure SetID(const AValue: string);
    procedure SetModified(const AValue: Boolean);
    procedure SetText(const AValue: string);
  protected
    FID: string;
    FText: string;
    function GetInUpdate: Boolean;
  public
    Context: string;
    AutoComment: string;
    Comment: string;
    Reference: string;
    Flags: string;
    PreviousID: string;
    PreviousText: string;
    constructor Create;
    procedure Changed; virtual;
    procedure Clean; virtual;
    property Contents: TLangContents read FContents;
    property ID: string read FID write SetID;
    property Text: string read FText write SetText;
    property Visible: Boolean read FVisible write FVisible default True;
    property Modified: Boolean read FModified write SetModified default False;
    property DisplayText: string read GetDisplayText write SetDisplayText;
    property DisplayID: string read GetDisplayID write SetDisplayID; //deprecated;
  end;

  TLanguage = class;

  { TLangContents }

  TLangContents = class(TObjectList)
  private
    FLanguage: TLanguage;
    FEnabled: Boolean;
    FModified: Boolean;
    FName: string;
    FAttributes: TStringList;
    FIsRightToLeft: Boolean;
    FCharset: string;
    FEncoding: TLangEncoding;
    FSource: string;
    FTitle: string;
    FVisible: Boolean;
    function GetItem(Index: Integer): TLangItem;
    procedure SetModified(const AValue: Boolean);
  protected
    function GetInUpdate: Boolean;
    function DoCreateLangItem: TLangItem; virtual;
    procedure EncodeText(var Text: string); virtual;
    procedure DecodeText(var Text: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed; virtual;
    procedure Clean; virtual;
    property Name: string read FName write FName;
    property Title: string read FTitle write FTitle; //extra name PO x-name
    property Source: string read FSource write FSource;
    function GetText(const ID: string; var Text: string): Boolean; overload;
    function GetText(const ID: string): string; overload;
    function FindID(const ID: string): TLangItem; virtual;
    function Add(LangItem: TLangItem): Integer;
    function CreateLangItem: TLangItem;
    property Items[Index: Integer]: TLangItem read GetItem; default;
    property Attributes: TStringList read FAttributes;
    property Charset: string read FCharset write FCharset;
    property Encoding: TLangEncoding read FEncoding write FEncoding;
    property IsRightToLeft: Boolean read FIsRightToLeft write FIsRightToLeft;
    property Visible: Boolean read FVisible write FVisible default True;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Modified: Boolean read FModified write SetModified;
  end;

  TLanguageInfo = record
    LangID: Cardinal;
    LangName: string;
    LocalName: string;
    ShortName: string;
    IsRightToLeft: Boolean;
  end;

  ILanguageRead = interface(IInterface) 
    ['{D0D5F689-F6CE-467C-BCDD-05A62736D6AD}']
    procedure LanguageChanged(LanguageInfo: TLanguageInfo);
  end;

  ILanguageWrite = interface(IInterface)
    ['{D1D3DFD6-393B-4B4D-8F97-3AE3AF7F9FA3}']
    procedure LanguageWrite;
  end;

  TLangParser = class;

  { TLanguage }

  TLanguage = class(TObjectList)
  private
    FInfo: TLanguageInfo;
    FModified: Boolean;
    FSource: string;
    FUpdateCount: Integer;
    function GetInUpdate: Boolean;
    function GetValues(Index: string): TLangContents;
    function GetItem(Index: Integer): TLangContents;
    procedure SetModified(const AValue: Boolean);
  protected
    function FindText(const ID: string; var Text: string): Boolean; virtual;
    function DoCreateContents: TLangContents; virtual;
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function CreateContents: TLangContents;
    function Add(Contents: TLangContents): Integer;
    function GetText(const ID: string): string; overload;
    function GetText(const Section: string; const ID: string): string; overload;
    function Find(Name:string): TLangContents;
    function FindID(const ID: string): TLangItem; overload;
    function FindID(ContentsName:string; const ID: string): TLangItem; overload;
    procedure BeginUpdate;
    procedure EndUpdate;
    property InUpdate: Boolean read GetInUpdate;
    procedure Clean; virtual;// set Modified = False for all objects
    property Modified: Boolean read FModified write SetModified;
    property Name: string read FInfo.LangName write FInfo.LangName;
    property LocalName: string read FInfo.LocalName write FInfo.LocalName;
    property ShortName: string read FInfo.ShortName write FInfo.ShortName;
    property IsRightToLeft: Boolean read FInfo.IsRightToLeft write FInfo.IsRightToLeft;
    property ID: Cardinal read FInfo.LangID write FInfo.LangID;
    property Info: TLanguageInfo read FInfo write FInfo;
    property Source: string read FSource write FSource;
    property Items[Index: Integer]: TLangContents read GetItem; default;
    property Values[Index: string]: TLangContents read GetValues;
  end;

  TLanguageClass = class of TLanguage;

  TmnOnMacro = procedure(var S: string) of object;

  { TLanguages }

  TLanguages = class(TObjectList)
  private
    FDefaultLanguage: TLanguage;
    FCurrentLanguage: TLanguage;
    FUseDefaultText: Boolean;
    FTestMode: Boolean;
    FOnMacro: TmnOnMacro;
    function GetItem(Index: Integer): TLanguage;
    procedure SetItem(Index: Integer; const Value: TLanguage);
    function GetCurrent: TLanguage;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Language: TLanguage): Integer;
    procedure ResetCurrentLanguage;
    procedure SetCurrentLanguage(const Name: string; Safe: Boolean = False);
    procedure SetDefaultLanguage(const Name: string);
    function FindLanguage(const Name: string): TLanguage;
    function IndexOfLanguage(const Name: string): Integer;
    function NextLanguageName: string;
    property Current: TLanguage read GetCurrent;
    function GetText(const ID: string): string; overload;
    function GetText(const ID: string; var Text: string): Boolean; overload;
    function Macro(const S: string): string;
    property Items[Index: Integer]: TLanguage read GetItem write SetItem; default;
    //if ID not found return the default text fromdefault language
    property UseDefaultText: Boolean read FUseDefaultText write FUseDefaultText;
    //TestMode all result values of translations return ID
    property TestMode: Boolean read FTestMode write FTestMode;
    property OnMacro: TmnOnMacro read FOnMacro write FOnMacro;
  end;

  TLanguagesClass = class of TLanguages;

  { TLangParser }

  TLangParser = class(TObject)
  private
    FContents: TLangContents;
    procedure SetContents(const Value: TLangContents);
  protected
    procedure CheckContents;
    procedure DoParse(Strings: TStringList); virtual; abstract;
    procedure DoGenerate(Strings: TStringList); virtual; abstract;
  public
    constructor Create; overload;
    procedure Parse(Strings: TStringList);
    procedure Generate(Strings: TStringList);
    class function GetName: string; virtual;
    class function GetTitle: string; virtual;
    class function GetExtension: string; virtual;
    property Contents: TLangContents read FContents write SetContents;
  end;

  TLangParserClass = class of TLangParser;

  TLangFilerFlag = (
    lffDefault, //default filer for its externsion
    lffMultiple, //Multiple files
    lffDirectory, //Mutli file based on directory
    lffAlone //Single Language there is no Original language
    );
  TLangFilerFlags = set of TLangFilerFlag;

  { TLangFiler }

  TLangFiler = class(TObject)
  private
  protected
    procedure DoLoadFrom(vSource: string; vLanguage:TLanguage); virtual; abstract;
    procedure DoSaveTo(vSource: string; vLanguage:TLanguage); virtual; abstract;
  public
    constructor Create; virtual;
    function CreateParser:TLangParser; virtual; abstract;
    procedure LoadFrom(vSource: string; vLanguage:TLanguage); //vName File or Directory
    procedure SaveTo(vSource: string; vLanguage:TLanguage);
    class function GetName: string; virtual; //Name for enumrate
    class function GetTitle: string; virtual; //for UI application
    class function GetExtension: string; virtual;
    class function GetFlags: TLangFilerFlags; virtual;
    property Flags: TLangFilerFlags read GetFlags;
  end;

  TLangFilerClass = class of TLangFiler;

  { TFilerClasses }

  TFilerClasses = class(TClassList)
  private
    function GetItem(Index: Integer): TLangFilerClass;
  public
    property Items[Index: Integer]: TLangFilerClass read GetItem; default;
  end;

  { TLangOptions }

  TLangOptions = class(TObject)
  private
    FNotifyObjects: TInterfaceList;
    FFilerClasses: TFilerClasses;
  protected
    procedure NotifyObjects(const LanguageInfo: TLanguageInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotify(AObject: ILanguageRead);
    procedure RemoveNotify(AObject: ILanguageRead);
    procedure RegisterFilerClass(FilerClass: TLangFilerClass);
    function FindFiler(vName: string): TLangFilerClass;
    function FindFilerByExt(vExt: string): TLangFilerClass;
    property FilerClasses: TFilerClasses read FFilerClasses;
  end;

const
  sUTF8BOM: array[1..3] of char = (#$EF, #$BB, #$BF);

function Languages: TLanguages;
function LangOptions: TLangOptions;
procedure InitLanguages(LanguagesClass: TLanguagesClass = nil);
procedure ParseLanguageFile(FileName: string; Language: TLanguage; Parser: TLangParser);
procedure GenerateLanguageFile(FileName: string; Contents: TLangContents; Parser: TLangParser);

function LangFindText(const ID: string; var Value: string): Boolean;

var
  //default for Delphi or Lazarus
  SystemEncoding: TLangEncoding {$ifdef LCL} = lncUTF8; {$else} = lncAnsi; {$endif}

implementation

var
  FLanguages: TLanguages = nil;
  FLangOptions: TLangOptions = nil;

function LangFindText(const ID: string; var Value: string): Boolean;
begin
  if FLanguages <> nil then
    Result := FLanguages.GetText(ID, Value)
  else
    Result := False;
end;

function LangOptions: TLangOptions;
begin
  if FLangOptions = nil then
    FLangOptions := TLangOptions.Create;
  Result := FLangOptions;
end;

function Languages: TLanguages;
begin
  if FLanguages = nil then
    raise ELangException.Create('Language not intialized yet');
  Result := FLanguages;
end;

procedure InitLanguages(LanguagesClass:TLanguagesClass);
begin
  if LanguagesClass = nil then
    LanguagesClass := TLanguages;
  if FLanguages <> nil then
    FreeAndNil(FLanguages);
  FLanguages := LanguagesClass.Create;
end;

{ TLanguage }

function TLanguage.Add(Contents: TLangContents): Integer;
begin
  Result := inherited Add(Contents);
end;

constructor TLanguage.Create;
begin
  inherited Create;
end;

function TLanguage.CreateContents: TLangContents;
begin
  Result := DoCreateContents;
  Add(Result);
end;

destructor TLanguage.Destroy;
begin
  inherited;
end;

function TLanguage.DoCreateContents: TLangContents;
begin
  Result := TLangContents.Create;
  Result.FLanguage := Self;
end;

procedure TLanguage.Changed;
begin
  if not InUpdate then
    FModified := True;
end;

function TLanguage.GetItem(Index: Integer): TLangContents;
begin
  Result := inherited Items[Index] as TLangContents;
end;

procedure TLanguage.SetModified(const AValue: Boolean);
begin
  if FModified <> AValue then
  begin
    Changed;
  end;
end;

function TLanguage.GetValues(Index: string): TLangContents;
begin
  Result := Find(Index);
end;

function TLanguage.GetInUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TLanguage.FindText(const ID: string; var Text: string): Boolean;
var
  i: Integer;
  s: string;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].GetText(ID, s);
    if Result then
    begin
      Text := s;
      Break;
    end;
  end;
end;

function TLanguage.GetText(const ID: string): string;
var
  i: Integer;
  s: string;
  Founded: Boolean;
begin
  Founded := False;
  for i := 0 to Count - 1 do
  begin
    Founded := Items[i].GetText(ID, s);
    if Founded then
    begin
      Result := s;
      break;
    end;
  end;
  if not Founded then
    Result := ID;
end;

function TLanguage.GetText(const Section: string; const ID: string): string;
var
  i: Integer;
  s: string;
  Founded: Boolean;
begin
  Founded := False;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Name = Section then
    begin
      Founded := Items[i].GetText(ID, s);
      if Founded then
        Result := s;
      break;
    end;
  end;
  if not Founded then
    Result := ID;
end;

function TLanguage.Find(Name: string): TLangContents;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

function TLanguage.FindID(const ID: string): TLangItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].FindID(ID);
    if Result <> nil then
      Break;
  end;
end;

function TLanguage.FindID(ContentsName: string; const ID: string): TLangItem;
var
  aContents: TLangContents;
begin
  Result := nil;
  aContents := Find(ContentsName);
  if aContents <> nil then
    Result := aContents.FindID(ID);
end;

procedure TLanguage.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TLanguage.EndUpdate;
begin
  Dec(FUpdateCount);
end;

procedure TLanguage.Clean;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    Items[i].Clean;
  end;
  FModified := False;
end;

{ TLanguages }

function TLanguages.Add(Language: TLanguage): Integer;
begin
  Result := inherited Add(Language);
end;

constructor TLanguages.Create;
begin
  inherited Create;
end;

function TLanguages.FindLanguage(const Name: string): TLanguage;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TLanguages.GetCurrent: TLanguage;
begin
  if FCurrentLanguage <> nil then
    Result := FCurrentLanguage
  else
    Result := FDefaultLanguage;
end;

function TLanguages.GetItem(Index: Integer): TLanguage;
begin
  Result := inherited Items[Index] as TLanguage;
end;

function TLanguages.GetText(const ID: string): string;
var
  aFound: Boolean;
begin
  Result := '';
  aFound := (FCurrentLanguage <> nil) and FCurrentLanguage.FindText(ID, Result);
  if UseDefaultText and (not aFound and (FCurrentLanguage <> FDefaultLanguage)) then
  begin
    if FDefaultLanguage = nil then
      raise ELangException.Create('There is no default language set');
    aFound := FDefaultLanguage.FindText(ID, Result);
  end;
  if not aFound then
    Result := ID
  else
  begin
    Result := Macro(Result);
  end;
end;

function TLanguages.GetText(const ID: string; var Text: string): Boolean;
begin
  Result := (FCurrentLanguage <> nil) and FCurrentLanguage.FindText(ID, Text);
  if UseDefaultText and not Result and (FCurrentLanguage <> FDefaultLanguage) then
  begin
    if FDefaultLanguage = nil then
      raise ELangException.Create('There is no default language set');
    Result := FDefaultLanguage.FindText(ID, Text);
  end;
  if TestMode and not Result then
  begin
    Result := True;
    Text := ID;
  end
  else
  begin
    Text := Macro(Text);
  end;
end;

function TLanguages.IndexOfLanguage(const Name: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TLanguages.ResetCurrentLanguage;
begin
  FCurrentLanguage := nil;
end;

procedure TLanguages.SetCurrentLanguage(const Name: string; Safe: Boolean);
var
  OldLanguage: TLanguage;
begin
  OldLanguage := FCurrentLanguage;
  if Name = '' then
    FCurrentLanguage := FDefaultLanguage
  else
    FCurrentLanguage := FindLanguage(Name);
  if (FCurrentLanguage = nil) and Safe then
    FCurrentLanguage := FDefaultLanguage;
  if FCurrentLanguage = nil then
    raise ELangException.Create('Language "' + Name + '" not found');
  if OldLanguage <> FCurrentLanguage then
    LangOptions.NotifyObjects(Current.Info);
end;

procedure TLanguages.SetDefaultLanguage(const Name: string);
begin
  FDefaultLanguage := FindLanguage(Name);
end;

procedure TLanguages.SetItem(Index: Integer;
  const Value: TLanguage);
begin
  inherited Items[Index] := Value;
end;

destructor TLanguages.Destroy;
begin
  inherited;
end;

function TLanguages.Macro(const S: string): string;
begin
  Result := S;
  if Assigned(FOnMacro) then
    FOnMacro(Result);
end;

function TLanguages.NextLanguageName: string;
var
  i: Integer;
begin
  i := IndexOfLanguage(Current.Name) + 1;
  if i >= Count then
    i := 0;
  Result := Items[i].Name;
end;

{ TLangContents }

function TLangContents.Add(LangItem: TLangItem): Integer;
begin
  Result := inherited Add(LangItem);
end;

constructor TLangContents.Create;
begin
  inherited Create;
  FAttributes := TStringList.Create;
  FVisible := True;
  FEnabled := True;
end;

function TLangContents.CreateLangItem: TLangItem;
begin
  Result := DoCreateLangItem;
  Result.FContents := Self;
  Add(Result);
end;

destructor TLangContents.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

procedure TLangContents.Changed;
begin
  if not GetInUpdate then
  begin
    FModified := True;
    if FLanguage <> nil then
      FLanguage.Changed;
  end;
end;

procedure TLangContents.Clean;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
  begin
    Items[i].Clean;
  end;
  FModified := False;
end;

function TLangContents.DoCreateLangItem: TLangItem;
begin
  Result := TLangItem.Create;
end;

procedure TLangContents.EncodeText(var Text: string);
begin
  if SystemEncoding <> Encoding then
  begin
    case Encoding of
      lncAnsi:
      begin
        case SystemEncoding of
          lncUTF8: Text := AnsiToUtf8(Text); 
        end;
      end;
      lncUTF8:
      begin
        case SystemEncoding of
          lncAnsi: Text := Utf8ToAnsi(Text);
        end;
      end;
    end;
  end;
end;

procedure TLangContents.DecodeText(var Text: string);
begin
  if SystemEncoding <> Encoding then
  begin
    case Encoding of
      lncAnsi:
      begin
        case SystemEncoding of
          lncUTF8: Text := Utf8ToAnsi(Text);
        end;
      end;
      lncUTF8:
      begin
        case SystemEncoding of
          lncAnsi: Text := AnsiToUtf8(Text);
        end;
      end;
    end;
  end;
end;

function TLangContents.GetItem(Index: Integer): TLangItem;
begin
  Result := inherited Items[Index] as TLangItem;
end;

function TLangContents.GetText(const ID: string): string;
begin
  GetText(ID, Result);
end;

function TLangContents.FindID(const ID: string): TLangItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
  begin
    if SameText(ID, Items[i].ID) then
    begin
      Result := Items[i];
      break;
    end
  end;
end;

function TLangContents.GetText(const ID: string; var Text: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count -1 do
  begin
    if SameText(ID, Items[i].ID) then
    begin
      Result := True;
      Text := Items[i].DisplayText;
      break;
    end
  end;
end;

procedure TLangContents.SetModified(const AValue: Boolean);
begin
  if FModified <> AValue then
  begin
    Changed;
  end;
end;

constructor TLangParser.Create;
begin
  inherited Create;
end;

class function TLangParser.GetExtension: string;
begin
  Result := '';
end;

class function TLangParser.GetTitle: string;
begin
  Result := '';
end;

procedure TLangParser.SetContents(const Value: TLangContents);
begin
  if FContents <> Value then
  begin
    FContents := Value;
  end;
end;

procedure TLangParser.CheckContents;
begin
  if FContents = nil then
    raise ELangException.Create('Contents is nil');
end;

procedure TLangParser.Parse(Strings: TStringList);
begin
  CheckContents;
  DoParse(Strings)
end;

procedure TLangParser.Generate(Strings: TStringList);
begin
  CheckContents;
  DoGenerate(Strings);
end;

class function TLangParser.GetName: string;
begin
  Result := '';
end;

{ TLangOptions }

procedure TLangOptions.AddNotify(AObject: ILanguageRead);
begin
  if not Supports(AObject, ILanguageRead) then
    raise ELangException.Create('Object must support of ILanguageRead');
  FNotifyObjects.Add(AObject);
end;

constructor TLangOptions.Create;
begin
  FNotifyObjects := TInterfaceList.Create;
  FFilerClasses := TFilerClasses.Create;
end;

destructor TLangOptions.Destroy;
begin
  FreeAndNil(FNotifyObjects);
  FreeAndNil(FFilerClasses);
  inherited;
end;

function TLangOptions.FindFiler(vName: string): TLangFilerClass;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FFilerClasses.Count - 1 do
  begin
    if SameText(TLangFilerClass(FFilerClasses[i]).GetName, vName) then
    begin
      Result := TLangFilerClass(FFilerClasses[i]);
      Break;
    end;
  end;
end;

function TLangOptions.FindFilerByExt(vExt: string): TLangFilerClass;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FFilerClasses.Count - 1 do
  begin
    if (lffDefault in TLangFilerClass(FFilerClasses[i]).GetFlags) and SameText(TLangFilerClass(FFilerClasses[i]).GetExtension, vExt) then
    begin
      Result := TLangFilerClass(FFilerClasses[i]);
      Break;
    end;
  end;
end;

procedure TLangOptions.NotifyObjects(const LanguageInfo: TLanguageInfo);
var
  i: Integer;
begin
  for i := 0 to FNotifyObjects.Count -1 do
  begin
    ILanguageRead(FNotifyObjects[i]).LanguageChanged(LanguageInfo);
  end;
end;

procedure TLangOptions.RemoveNotify(AObject: ILanguageRead);
begin
  FNotifyObjects.Remove(AObject);
end;

procedure TLangOptions.RegisterFilerClass(FilerClass: TLangFilerClass);
begin
  FFilerClasses.Add(FilerClass);
end;

procedure ParseLanguageFile(FileName:string; Language: TLanguage; Parser: TLangParser);
var
  Contents: TLangContents;
  Strings: TStringList;
begin
  Contents := Language.CreateContents;//this auto add the contents to Language
  if Parser = nil then
    raise ELangException.Create('There is no parser for TLanguage');
  Strings := TStringList.Create;
  try
    Parser.Contents := Contents;
    Strings.LoadFromFile(FileName);
    Contents.Name := ExtractFileName(FileName);
    Contents.Source := FileName;
    Parser.Parse(Strings);
  finally
    Strings.Free;
  end;
end;

procedure GenerateLanguageFile(FileName: string; Contents: TLangContents; Parser: TLangParser);
var
  Strings: TStringList;
begin
  if Parser = nil then
    raise ELangException.Create('There is no parser for TLanguage');
  Strings := TStringList.Create;
  try
    Parser.Contents := Contents;
    Parser.Generate(Strings);
    Strings.SaveToFile(FileName);
  finally
    Strings.Free;
  end;
end;

{ TLangItem }

procedure TLangItem.SetModified(const AValue: Boolean);
begin
  if FModified <> AValue then
  begin
    Changed;
  end;
end;

procedure TLangItem.SetText(const AValue: string);
begin
  FText := AValue;
  Modified := True;
end;

function TLangItem.GetInUpdate: Boolean;
begin
  Result := (FContents <> nil) and (FContents.GetInUpdate);
end;

function TLangContents.GetInUpdate: Boolean;
begin
  Result := (FLanguage <> nil) and (FLanguage.InUpdate);
end;

constructor TLangItem.Create;
begin
  inherited Create;
  FVisible := True;
end;

function TLangItem.GetDisplayText: string;
begin
  if FContents = nil then
    raise ELangException.Create('FContents = nil');
  Result := Text;
  FContents.EncodeText(Result);
{$ifdef WINDOWS}
  Result := StringReplace(Result, #13, #13#10, [rfReplaceAll]);
{$endif}
end;

function TLangItem.GetDisplayID: string;
begin
  if FContents = nil then
    raise ELangException.Create('FContents = nil');
  Result := ID;
  FContents.EncodeText(Result);
{$ifdef WINDOWS}
  Result := StringReplace(Result, #13, #13#10, [rfReplaceAll]);
{$endif}
end;

procedure TLangItem.SetDisplayID(AValue: string);
begin
{$ifdef WINDOWS}
  AValue := StringReplace(AValue, #13#10, #13, [rfReplaceAll]);
{$endif}
  if FContents = nil then
    raise ELangException.Create('FContents = nil');
  FContents.DecodeText(AValue);
  ID := AValue;
end;

procedure TLangItem.SetDisplayText(AValue: string);
begin
{$ifdef WINDOWS}
  AValue := StringReplace(AValue, #13#10, #13, [rfReplaceAll]);
{$endif}
  if FContents = nil then
    raise ELangException.Create('FContents = nil');
  FContents.DecodeText(AValue);
  Text := AValue;
end;

procedure TLangItem.SetID(const AValue: string);
begin
  FID := AValue;
  Modified := True;
end;

procedure TLangItem.Changed;
begin
  if not GetInUpdate then
  begin
    FModified := True;
    if FContents <> nil then
      FContents.Changed;
  end;
end;

procedure TLangItem.Clean;
begin
  FModified := False;
end;

{ TLangFiler }

constructor TLangFiler.Create;
begin
  inherited Create;
end;

procedure TLangFiler.LoadFrom(vSource: string; vLanguage: TLanguage);
begin
  vLanguage.BeginUpdate;
  try
    DoLoadFrom(vSource, vLanguage);
  finally
    vLanguage.EndUpdate;
  end;
end;

procedure TLangFiler.SaveTo(vSource: string; vLanguage: TLanguage);
begin
  DoSaveTo(vSource, vLanguage);
  vLanguage.Clean;
end;

class function TLangFiler.GetName: string;
begin
  Result := '';
end;

class function TLangFiler.GetTitle: string;
begin
  Result := '';
end;

class function TLangFiler.GetFlags: TLangFilerFlags;
begin
end;

class function TLangFiler.GetExtension: string;
begin
  Result := '';
end;

{ TFilerClasses }

function TFilerClasses.GetItem(Index: Integer): TLangFilerClass;
begin
  Result := TLangFilerClass(inherited Items[Index]);
end;

initialization
  FLanguages := nil;
  FLangOptions := nil;
finalization
  FreeAndNil(FLanguages);
  FreeAndNil(FLangOptions);
end.

