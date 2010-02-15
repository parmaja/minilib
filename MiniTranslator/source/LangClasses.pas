unit LangClasses;
{**
 *  This file is part of the "Mini Library" http://www.sourceforge.net/projects/minilib
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$MODE objfpc}
{$ENDIF}
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

  Group is list of contents items usfule when your language in one file have sections like INI languages
  Group not own its items because it is already owned by the language it is like a index
}
interface

uses
  SysUtils, Variants, Classes, Contnrs, mnUtils;

type
  ELangException = class(Exception);

  TLangEncoding = (lncNone, lncAnsi, lncUTF8);

  TLangContents = class;

  { TLangItem }

  TLangItem = class(TObject)
  private
    FID: string;
    FText: string;
    FContents: TLangContents;
    FLine: Integer;
    FModified: Boolean;
    FVisible: Boolean;
    function GetDisplayID: string;
    function GetDisplayText: string;
    procedure SetDisplayID(AValue: string);
    procedure SetDisplayText(AValue: string);
    procedure SetID(const AValue: string);
    procedure SetLine(const AValue: Integer);
    procedure SetModified(const AValue: Boolean);
    procedure SetText(const AValue: string);
  protected
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
    property DisplayID: string read GetDisplayID {write SetDisplayID}; //deprecated;
    //May be usfule for show it in translator editor
    property Line: Integer read FLine write SetLine;
  end;

  TLanguage = class;


  { TLangList }

  TLangList = class(TObjectList)
  private
    function GetValues(Index: string): TLangItem;
    function GetItem(Index: Integer): TLangItem;
  public
    function Find(ID: string): TLangItem;
    function FindForText(vText: string): TLangItem;
    property Items[Index: Integer]: TLangItem read GetItem; default;
    property Values[Index: string]: TLangItem read GetValues;
  end;

  { TLangGroup }

  TLangGroup = class(TLangList)
  private
    FName: string;
  public
    constructor Create; virtual;
  published
    property Name: string read FName write FName;
  end;

  { TLangGroups }

  TLangGroups = class(TObjectList)
  private
    function GetItem(Index: Integer): TLangGroup;
  public
    function Last: TLangGroup;
    property Items[Index: Integer]: TLangGroup read GetItem; default;
  end;

  { TLangContents }

  TLangContents = class(TLangList)
  private
    FComment: string;
    FGroups: TLangGroups;
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
    FBOMFlag: Boolean;
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
    function Add(LangItem: TLangItem): Integer;
    function CreateLangItem: TLangItem;
    property Attributes: TStringList read FAttributes;
    property Charset: string read FCharset write FCharset;
    property Comment: string read FComment write FComment;
    property Encoding: TLangEncoding read FEncoding write FEncoding default lncNone;
    //If utf-8 may be it have BOM flag when save it to file
    property BOMFlag: Boolean read FBOMFlag write FBOMFlag default False;
    property IsRightToLeft: Boolean read FIsRightToLeft write FIsRightToLeft default False;
    //Visible like as properties of PO file the empty ID
    property Visible: Boolean read FVisible write FVisible default True;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Modified: Boolean read FModified write SetModified;
    //Groups: Can collect some items in a group, need it when write external tools
    property Groups: TLangGroups read FGroups;
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
    function GetItem(Index: Integer): TLangContents;
    function GetContents(Index: string): TLangContents;
    procedure SetModified(const AValue: Boolean);
  protected
    function DoCreateContents: TLangContents; virtual;
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function CreateContents: TLangContents;
    function Add(Contents: TLangContents): Integer;
    function AddItem(vContentName, vID, vText: string): TLangItem;
    //FindText result empty string if ID not founded
    function FindText(vID: string; var Founded: Boolean; Default: string = ''): string; overload;
    function FindText(vContents, vID: string; var Founded: Boolean; Default: string = ''): string; overload;
    function FindText(vID: string): string; overload;
    function FindText(vID: string; var Text: string): Boolean; overload;
    //GetText return the ID as result if ID not founded
    function GetText(const vID: string): string; overload;
    function GetText(const vContents: string; const vID: string): string; overload;
    //Find find a Contents
    function Find(vName: string): TLangContents;
    function FindID(const vID: string): TLangItem; overload;
    function FindID(vContents: string; const vID: string): TLangItem; overload;
    //Search for text in all contents
    function FindForText(const vText: string): TLangItem; overload;

    procedure BeginUpdate;
    procedure EndUpdate;
    property InUpdate: Boolean read GetInUpdate;
    procedure Clean; virtual; // set Modified = False for all objects
    property Info: TLanguageInfo read FInfo write FInfo;
    property Modified: Boolean read FModified write SetModified;
    property Name: string read FInfo.LangName write FInfo.LangName;
    property LocalName: string read FInfo.LocalName write FInfo.LocalName;
    property ShortName: string read FInfo.ShortName write FInfo.ShortName;
    property IsRightToLeft: Boolean read FInfo.IsRightToLeft write FInfo.IsRightToLeft;
    property ID: Cardinal read FInfo.LangID write FInfo.LangID;
    property Source: string read FSource write FSource;
    property Items[Index: Integer]: TLangContents read GetItem; default;
    property Contents[Index: string]: TLangContents read GetContents;
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
    function NextLanguage: TLanguage;
    property DefaultLanguage: TLanguage read FDefaultLanguage;
    property Current: TLanguage read GetCurrent;
    function GetText(const vID: string): string; overload;
    function GetText(const vID: string; var vText: string): Boolean; overload;
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
    //Parse: when load the language file it parse and create
    procedure Parse(Strings: TStringList);
    //Generate for save the language file to its syntax
    procedure Generate(Strings: TStringList);
    //GetName name of filer
    class function GetName: string; virtual; abstract;
    //GetTitle name of filer but for display it in front UI
    class function GetTitle: string; virtual; abstract;
{    //GetExtension like 'PO' do not include dot '.'
    class function GetExtension: string; virtual;}
    property Contents: TLangContents read FContents write SetContents;
  end;

  TLangParserClass = class of TLangParser;

  TLangFilerFlag = (
    lffDefault, //default filer for its externsion
    lffMultiple, //Multiple files
    lffDirectory, //Mutli file based on directory
    lffSingle, //not lffDirectory single file
    lffAlone //Single Language there is no Original language
    );
  TLangFilerFlags = set of TLangFilerFlag;

  { TLangFiler }

  TLangFiler = class(TObject)
  private
  protected
    //Default Load and Save you can used of make your own
    procedure DefaultLoadFrom(IsDirectory: Boolean; vSource: string; vLanguage: TLanguage);
    procedure DefaultSaveTo(IsDirectory: Boolean; vSource: string; vLanguage: TLanguage);

    procedure DoLoadFrom(vSource: string; vLanguage: TLanguage); virtual; abstract;
    procedure DoSaveTo(vSource: string; vLanguage: TLanguage); virtual; abstract;
  public
    constructor Create; virtual;
    function CreateParser: TLangParser; virtual; abstract;
    procedure LoadFrom(vSource: string; vLanguage: TLanguage); //vName File or Directory
    procedure SaveTo(vSource: string; vLanguage: TLanguage);
    function GetFileName(vPath, vName: string): string;
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

function Languages: TLanguages;
function LangOptions: TLangOptions;
procedure InitLanguages(LanguagesClass: TLanguagesClass = nil);
function IsLanguagesInitialized: Boolean;

function ParseLanguage(Strings: TStringList; Language: TLanguage; Parser: TLangParser): TLangContents;
procedure GenerateLanguage(Strings: TStringList; Contents: TLangContents; Parser: TLangParser);

procedure ParseLanguageFile(FileName: string; Language: TLanguage; Parser: TLangParser);
procedure GenerateLanguageFile(FileName: string; Contents: TLangContents; Parser: TLangParser);

var
  //Default for Delphi (ANSI)  or Lazarus
  SystemEncoding: TLangEncoding{$IFDEF LCL} = lncUTF8; {$ELSE} = lncAnsi; {$ENDIF}

implementation

var
  FLanguages: TLanguages = nil;
  FLangOptions: TLangOptions = nil;

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

procedure InitLanguages(LanguagesClass: TLanguagesClass);
begin
  if LanguagesClass = nil then
    LanguagesClass := TLanguages;
  if FLanguages <> nil then
    FreeAndNil(FLanguages);
  FLanguages := LanguagesClass.Create;
end;

function IsLanguagesInitialized: Boolean;
begin
  Result := FLanguages <> nil;
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
  Modified := True;
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

procedure TLanguage.SetModified(const AValue: Boolean);
begin
  if FModified <> AValue then
  begin
    Changed;
  end;
end;

function TLanguage.GetContents(Index: string): TLangContents;
begin
  Result := Find(Index);
end;

function TLanguage.GetInUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TLanguage.GetItem(Index: Integer): TLangContents;
begin
  Result := inherited Items[Index] as TLangContents;
end;

function TLanguage.GetText(const vID: string): string;
var
  Founded: Boolean;
begin
  Founded := False;
  Result := FindText(vID, Founded);
  if not Founded then
    Result := vID;
end;

function TLanguage.GetText(const vContents: string; const vID: string): string;
var
  Founded: Boolean;
begin
  Founded := False;
  Result := FindText(vContents, vID, Founded);
  if not Founded then
    Result := vID;
end;

function TLanguage.FindText(vID: string): string;
var
  Founded: Boolean;
begin
  Founded := False;
  Result := FindText(vID, Founded);
end;

function TLanguage.FindText(vID: string; var Founded: Boolean; Default: string): string;
var
  aItem: TLangItem;
begin
  aItem := FindID(vID);
  Founded := aItem <> nil;
  if Founded then
    Result := aItem.DisplayText
  else
    Result := Default
end;

function TLanguage.FindText(vContents, vID: string; var Founded: Boolean; Default: string): string;
var
  aItem: TLangItem;
begin
  aItem := FindID(vContents, vID);
  Founded := aItem <> nil;
  if Founded then
    Result := aItem.DisplayText
  else
    Result := Default
end;

function TLanguage.Find(vName: string): TLangContents;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, vName) then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

function TLanguage.FindForText(const vText: string): TLangItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].FindForText(vText);
    if Result <> nil then
      Break;
  end;
end;

function TLanguage.FindID(const vID: string): TLangItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].Find(vID);
    if Result <> nil then
      Break;
  end;
end;

function TLanguage.FindID(vContents: string; const vID: string): TLangItem;
var
  aContents: TLangContents;
begin
  Result := nil;
  aContents := Find(vContents);
  if aContents <> nil then
    Result := aContents.Find(vID);
end;

function TLanguage.FindText(vID: string; var Text: string): Boolean;
var
  aItem: TLangItem;
begin
  aItem := FindID(vID);
  Result := aItem <> nil;
  if Result then
    Text := aItem.DisplayText;
end;

function TLanguage.AddItem(vContentName, vID, vText: string): TLangItem;
var
  aContents: TLangContents;
begin
  aContents := Find(vContentName);
  if aContents = nil then
  begin
    aContents := CreateContents;
    aContents.Name := vContentName;
    if Count > 0 then //Steal a defaults from first contents
    begin
      aContents.Encoding := Items[0].Encoding;
      aContents.BOMFlag := Items[0].BOMFlag;
    end;
  end;
  Result := aContents.Find(vID);
  if Result = nil then
  begin
    Result := aContents.CreateLangItem;
    Result.ID := vID;
    Result.DisplayText := vText;
  end;
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
  for i := 0 to Count - 1 do
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

function TLanguages.GetText(const vID: string): string;
var
  aFound: Boolean;
begin
  aFound := False;
  Result := '';
  if (FCurrentLanguage <> nil) then
    Result := FCurrentLanguage.FindText(vID, aFound);
  if UseDefaultText and (not aFound and (FCurrentLanguage <> FDefaultLanguage)) then
  begin
    if FDefaultLanguage = nil then
      raise ELangException.Create('There is no default language set');
    Result := FDefaultLanguage.FindText(vID, aFound);
  end;
  if not aFound then
    Result := vID
  else
  begin
    Result := Macro(Result);
  end;
end;

function TLanguages.GetText(const vID: string; var vText: string): Boolean;
begin
  Result := False;
  if (FCurrentLanguage <> nil) then
    Result := FCurrentLanguage.FindText(vID, vText);
  if UseDefaultText and not Result and (FCurrentLanguage <> FDefaultLanguage) then
  begin
    if FDefaultLanguage = nil then
      raise ELangException.Create('There is no default language set');
    Result := FDefaultLanguage.FindText(vID, vText);
  end;
  if TestMode and not Result then
  begin
    Result := True;
    vText := vID;
  end
  else
  begin
    vText := Macro(vText);
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

function TLanguages.NextLanguage: TLanguage;
var
  i: Integer;
begin
  i := IndexOfLanguage(Current.Name) + 1;
  if i >= Count then
    i := 0;
  Result := Items[i];
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
  FGroups := TLangGroups.Create(True);
  FVisible := True;
  FEnabled := True;
end;

function TLangContents.CreateLangItem: TLangItem;
begin
  Result := DoCreateLangItem;
  Result.FContents := Self;
  Add(Result);
  Modified := True;
end;

destructor TLangContents.Destroy;
begin
  FreeAndNil(FAttributes);
  FreeAndNil(FGroups);
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
  for i := 0 to Count - 1 do
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

{class function TLangParser.GetExtension: string;
begin
  Result := '';
end;}

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
  if Contents.BOMFlag and (Strings.Count > 0) then //No not here :(
    Strings[0] := sUTF8BOM + Strings[0];
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
  for i := 0 to FNotifyObjects.Count - 1 do
  begin
    (FNotifyObjects[i] as ILanguageRead).LanguageChanged(LanguageInfo);
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

function ParseLanguage(Strings: TStringList; Language: TLanguage; Parser: TLangParser): TLangContents;
begin
  if Parser = nil then
    raise ELangException.Create('There is no parser for TLanguage');
  Result := Language.CreateContents; //this auto add the contents to Language
  Parser.Contents := Result;
  Parser.Parse(Strings);
end;

procedure ParseLanguageFile(FileName: string; Language: TLanguage; Parser: TLangParser);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(FileName);
    with ParseLanguage(Strings, Language, Parser) do
    begin
      Source := FileName;
      Name := ChangeFileExt(ExtractFileName(FileName), '');
    end;
  finally
    Strings.Free;
  end;
end;

procedure GenerateLanguage(Strings: TStringList; Contents: TLangContents; Parser: TLangParser);
begin
  if Parser = nil then
    raise ELangException.Create('There is no parser for TLanguage');
  Parser.Contents := Contents;
  Parser.Generate(Strings);
end;

procedure GenerateLanguageFile(FileName: string; Contents: TLangContents; Parser: TLangParser);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    GenerateLanguage(Strings, Contents, Parser);
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
  Result := AdjustLineBreaks(Result);
end;

function TLangItem.GetDisplayID: string;
begin
  if FContents = nil then
    raise ELangException.Create('FContents = nil');
  Result := ID;
  FContents.EncodeText(Result);
  Result := AdjustLineBreaks(Result);
end;

procedure TLangItem.SetDisplayID(AValue: string);
begin
  AValue := AdjustLineBreaks(AValue);
  if FContents = nil then
    raise ELangException.Create('FContents = nil');
  FContents.DecodeText(AValue);
  ID := AValue;
end;

procedure TLangItem.SetDisplayText(AValue: string);
begin
  AValue := AdjustLineBreaks(AValue);
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

procedure TLangItem.SetLine(const AValue: Integer);
begin
  if FLine = AValue then exit;
  FLine := AValue;
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

procedure TLangFiler.DefaultLoadFrom(IsDirectory: Boolean; vSource: string; vLanguage: TLanguage);
var
  I: Integer;
  SearchRec: TSearchRec;
  aPath: string;
  aParser: TLangParser;
begin
  with vLanguage do
  begin
    if IsDirectory then
    begin
      aPath := IncludeTrailingPathDelimiter(vSource);
      try
        I := FindFirst(aPath + '*.' + GetExtension, 0, SearchRec);
        while I = 0 do
        begin
          aParser := CreateParser;
          try
            ParseLanguageFile(aPath + SearchRec.Name, vLanguage, aParser);
          finally
            aParser.Free;
          end;
          I := FindNext(SearchRec);
        end;
      finally
        FindClose(SearchRec);
      end;
    end
    else
    begin
      aPath := IncludeTrailingPathDelimiter(ExtractFilePath(vSource));
      try
        Clear;
        aParser := CreateParser;
        try
          ParseLanguageFile(vSource, vLanguage, aParser);
        finally
          aParser.Free;
        end;
      except
        raise;
      end;
    end;
    Source := aPath;
  end;
end;

procedure TLangFiler.DefaultSaveTo(IsDirectory: Boolean; vSource: string; vLanguage: TLanguage);
var
  i: Integer;
  aFileName: string;
  aParser: TLangParser;
begin
  with vLanguage do
  begin
    try
      for i := 0 to vLanguage.Count - 1 do
      begin
        aParser := CreateParser;
        try
          if vSource <> '' then
          begin
            if IsDirectory then
              aFileName := GetFileName(IncludeTrailingPathDelimiter(vSource), vLanguage[i].Name)
            else
              aFileName := vSource
          end
          else
          begin
            aFileName := vLanguage[i].Source;
            if aFileName = '' then
            begin
              aFileName := GetFileName(IncludeTrailingPathDelimiter(vLanguage.Source), vLanguage[i].Name);
//              vLanguage[i].Source := aFileName;
            end;
          end;
          GenerateLanguageFile(aFileName, vLanguage[i], aParser);
        finally
          aParser.Free;
        end;
      end;
    except
      raise;
    end;
  end;
end;

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

function TLangFiler.GetFileName(vPath, vName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(vPath) + vName;
  if GetExtension <> '' then
    Result := Result + '.' + GetExtension;
end;

class function TLangFiler.GetFlags: TLangFilerFlags;
begin
  Result := []
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

{ TLangGroups }

function TLangGroups.GetItem(Index: Integer): TLangGroup;
begin
  Result := inherited Items[Index] as TLangGroup;
end;

function TLangGroups.Last: TLangGroup;
begin
  Result := inherited Last as TLangGroup;
end;

{ TLangGroup }

constructor TLangGroup.Create;
begin
  inherited Create(False);
end;

function TLangList.GetValues(Index: string): TLangItem;
begin
  Result := Find(Index);
end;

function TLangList.FindForText(vText: string): TLangItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(vText, Items[i].Text) then
    begin
      Result := Items[i];
      break;
    end
  end;
end;

function TLangList.GetItem(Index: Integer): TLangItem;
begin
  Result := inherited Items[Index] as TLangItem;
end;

function TLangList.Find(ID: string): TLangItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(ID, Items[i].ID) then
    begin
      Result := Items[i];
      break;
    end
  end;
end;

initialization
  FLanguages := nil;
  FLangOptions := nil;
finalization
  FreeAndNil(FLanguages);
  FreeAndNil(FLangOptions);
end.

