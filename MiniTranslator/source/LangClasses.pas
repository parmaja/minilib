unit LangClasses;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}
{$M+}

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
  Windows, Messages,
  SysUtils, Variants, Classes, Contnrs;

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
    FModified: Boolean;
    FVisible: Boolean;
    function GetDisplayID: string;
    function GetDisplayText: string;
    procedure SetDisplayID(AValue: string);
    procedure SetDisplayText(AValue: string);
    procedure SetID(const AValue: string);
    procedure SetModified(const AValue: Boolean);
    procedure SetText(const AValue: string);
  public
    AutoComment: string;
    Comment: string;
    Reference: string;
    Flags: string;
    procedure Changed;
    property Contents: TLangContents read FContents;
    property ID: string read FID write SetID;
    property Text: string read FText write SetText;
    property Visible: Boolean read FVisible write FVisible;
    property Modified: Boolean read FModified write SetModified;
    property DisplayText: string read GetDisplayText write SetDisplayText;
    property DisplayID: string read GetDisplayID write SetDisplayID;
  end;

  { TLangContents }

  TLangContents = class(TObjectList)
  private
    FEnabled: Boolean;
    FModified: Boolean;
    FName: string;
    FSettings: TStringList;
    FRightToLeft: Boolean;
    FCharset: string;
    FEncoding: TLangEncoding;
    FVisible: Boolean;
    function GetItem(Index: Integer): TLangItem;
    procedure SetItem(Index: Integer; const Value: TLangItem);
    procedure SetModified(const AValue: Boolean);
  protected
    function DoCreateLangItem: TLangItem; virtual;
    procedure EncodeText(var Text: string); virtual;
    procedure DecodeText(var Text: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed; virtual;
    property Name: string read FName write FName;
    function GetText(const ID: string; var Text: string): Boolean; overload;
    function GetText(const ID: string): string; overload;
    function FindID(const ID: string): TLangItem; virtual;
    function Add(LangItem: TLangItem): Integer;
    function CreateLangItem: TLangItem;
    property Items[Index: Integer]: TLangItem read GetItem write SetItem; default;
    property Settings: TStringList read FSettings;
    property Charset: string read FCharset write FCharset;
    property Encoding: TLangEncoding read FEncoding write FEncoding;
    property RightToLeft: Boolean read FRightToLeft write FRightToLeft;
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
    function GetContents(Index: string): TLangContents;
    function GetItem(Index: Integer): TLangContents;
    procedure SetModified(const AValue: Boolean);
  protected
    function FindText(const ID: string; var Text: string): Boolean; virtual;
    function DoCreateContents: TLangContents; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateContents: TLangContents;
    function CreateParser: TLangParser; virtual;
    function Add(Contents: TLangContents): Integer;
    function GetText(const ID: string): string; overload;
    function GetText(const Section: string; const ID: string): string; overload;
    function Find(Name:string): TLangContents;
    function FindID(const ID: string): TLangItem; virtual; overload;
    function FindID(ContentsName:string; const ID: string): TLangItem; virtual; overload;
    property Modified: Boolean read FModified write SetModified;
    property Name: string read FInfo.LangName write FInfo.LangName;
    property LocalName: string read FInfo.LocalName write FInfo.LocalName;
    property ShortName: string read FInfo.ShortName write FInfo.ShortName;
    property IsRightToLeft: Boolean read FInfo.IsRightToLeft write FInfo.IsRightToLeft;
    property ID: Cardinal read FInfo.LangID write FInfo.LangID;
    property Info: TLanguageInfo read FInfo write FInfo;
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
  public
    constructor Create; overload;
    constructor Create(AContents: TLangContents); overload;
    procedure Parse(Strings: TStringList); virtual; abstract;
    procedure Generate(Strings: TStringList); virtual; abstract;
    class function GetName: string; virtual;
    class function GetFileExtensions: string; virtual;
    class function GetTitle: string; virtual;
    class function IsMultiFiles: Boolean; virtual;
    class function IsKeyAsOriginal: Boolean; virtual;
    property Contents: TLangContents read FContents write SetContents;
  end;

  TLangParserClass = class of TLangParser;

  { TLangOptions }

  TLangOptions = class(TObject)
  private
    FNotifyObjects: TInterfaceList;
    FParserClasses: TClassList;
  protected
    procedure NotifyObjects(const LanguageInfo: TLanguageInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotify(AObject: ILanguageRead);
    procedure RemoveNotify(AObject: ILanguageRead);
    procedure RegisterParserClass(ParserClass: TLangParserClass);
    procedure RegisterLanguagesClass(LanguagesClass: TLanguagesClass);
  end;

const
  sUTF8BOM: array[1..3] of char = (#$EF, #$BB, #$BF);

function Languages: TLanguages;
function LangOptions: TLangOptions;
procedure InitLanguages(LanguagesClass: TLanguagesClass = nil);
function LangFindText(const ID: string; var Value: string): Boolean;
procedure ParseLanguageFile(FileName: string; Language: TLanguage; Parser: TLangParser = nil);
procedure GenerateLanguageFile(FileName: string; Language: TLanguage; Parser: TLangParser = nil);

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

function TLanguage.CreateParser: TLangParser;
begin
  //There is no parser for TLanguage
  Result := nil;
end;

destructor TLanguage.Destroy;
begin
  inherited;
end;

function TLanguage.DoCreateContents: TLangContents;
begin
  Result := TLangContents.Create;
end;

function TLanguage.GetItem(Index: Integer): TLangContents;
begin
  Result := inherited Items[Index] as TLangContents;
end;

procedure TLanguage.SetModified(const AValue: Boolean);
begin
  if FModified <> AValue then
  begin
    FModified :=AValue;
  end;
end;

function TLanguage.GetContents(Index: string): TLangContents;
begin
  Result := Find(Index);
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
  FSettings := TStringList.Create;
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
  FSettings.Free;
  inherited;
end;

procedure TLangContents.Changed;
begin
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

procedure TLangContents.SetItem(Index: Integer; const Value: TLangItem);
begin
  inherited Items[Index] := Value;
end;

procedure TLangContents.SetModified(const AValue: Boolean);
begin
  if FModified <> AValue then
  begin
    FModified := AValue;
    Changed;
  end;
end;

constructor TLangParser.Create;
begin
  inherited Create;
end;

class function TLangParser.GetFileExtensions: string;
begin
  Result := '';
end;

class function TLangParser.GetTitle: string;
begin
  Result := '';
end;

class function TLangParser.IsMultiFiles: Boolean;
begin
  Result := True;
end;

class function TLangParser.IsKeyAsOriginal: Boolean;
begin
  Result := False;
end;

procedure TLangParser.SetContents(const Value: TLangContents);
begin
  if FContents <> Value then
  begin
    FContents := Value;
  end;
end;

constructor TLangParser.Create(AContents: TLangContents);
begin
  Create;
  FContents := AContents;
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
  FParserClasses := TClassList.Create;
end;

destructor TLangOptions.Destroy;
begin
  FreeAndNil(FNotifyObjects);
  FreeAndNil(FParserClasses);
  inherited;
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

procedure TLangOptions.RegisterParserClass(ParserClass: TLangParserClass);
begin

end;

procedure TLangOptions.RegisterLanguagesClass(LanguagesClass: TLanguagesClass);
begin

end;

procedure ParseLanguageFile(FileName:string; Language: TLanguage; Parser: TLangParser);
var
  Contents: TLangContents;
  Strings: TStringList;
begin
  Contents := Language.CreateContents;//this auto add the contents to Language
  if Parser = nil then
    Parser := Language.CreateParser;
  if Parser = nil then
    raise ELangException.Create('There is no parser for TLanguage');
  Strings := TStringList.Create;
  try
    Parser.Contents := Contents;
    Strings.LoadFromFile(FileName);
    Parser.Parse(Strings);
  finally
    Strings.Free;
    Parser.Free;
  end;
end;

procedure GenerateLanguageFile(FileName: string; Language: TLanguage; Parser: TLangParser = nil);
var
  Contents: TLangContents;
  Strings: TStringList;
begin
  Contents := Language.CreateContents;//this auto add the contents to Language
  if Parser = nil then
    Parser := Language.CreateParser;
  if Parser = nil then
    raise ELangException.Create('There is no parser for TLanguage');
  Strings := TStringList.Create;
  try
    Parser.Generate(Strings);
    Parser.Contents := Contents;
    Strings.SaveToFile(FileName);
  finally
    Strings.Free;
    Parser.Free;
  end;
end;

{ TLangItem }

procedure TLangItem.SetModified(const AValue: Boolean);
begin
  if FModified <> AValue then
  begin
    FModified := AValue;
    Changed;
  end;
end;

procedure TLangItem.SetText(const AValue: string);
begin
  FText := AValue;
  Modified := True;
end;

function TLangItem.GetDisplayText: string;
begin
  if FContents = nil then
    raise ELangException.Create('FContents = nil');
  Result := Text;
  FContents.EncodeText(Result);
end;

function TLangItem.GetDisplayID: string;
begin
  if FContents = nil then
    raise ELangException.Create('FContents = nil');
  Result := ID;
  FContents.EncodeText(Result);
end;

procedure TLangItem.SetDisplayID(AValue: string);
begin
  if FContents = nil then
    raise ELangException.Create('FContents = nil');
  FContents.DecodeText(AValue);
  ID := AValue;
end;

procedure TLangItem.SetDisplayText(AValue: string);
begin
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
  FModified := True;
  if FContents <> nil then
    FContents.Changed;
end;

initialization
  FLanguages := nil;
  FLangOptions := nil;
finalization
  FreeAndNil(FLanguages);
  FreeAndNil(FLangOptions);
end.

