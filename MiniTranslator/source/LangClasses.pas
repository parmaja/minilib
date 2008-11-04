unit LangClasses;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
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

  TLangItem = class(TObject)
  public
    ID: string;
    Text: string;
    AutoComment: string;
    Comment: string;
    Reference: string;
    Flags: string;
//    Original: string; //i hate that :(
  end;

  TLangContents = class(TObjectList)
  private
    FName: string;
    FSettings: TStringList;
    FRightToLeft: Boolean;
    FCharset: string;
    FEncoding: TLangEncoding;
    function GetItem(Index: Integer): TLangItem;
    procedure SetItem(Index: Integer; const Value: TLangItem);
  protected
    function DoCreateLangItem: TLangItem; virtual;
    procedure EncodeText(var Text: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write FName;
    function GetText(const ID: string; var Text: string): Boolean; overload;
    function GetText(const ID: string): string; overload;
    function Add(LangItem: TLangItem): Integer;
    function CreateLangItem: TLangItem;
    property Items[Index: Integer]: TLangItem read GetItem write SetItem; default;
    property Settings: TStringList read FSettings;
    property Charset: string read FCharset write FCharset;
    property Encoding: TLangEncoding read FEncoding write FEncoding;
    property RightToLeft: Boolean read FRightToLeft write FRightToLeft;
  end;

  TLanguageInfo = record
    LangID: Cardinal;
    LangName: string;
    LocalName: string;
    ShortName: string;
    RightToLeft: Boolean;
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

  TLanguage = class(TObjectList)
  private
    FInfo: TLanguageInfo;
    function GetItem(Index: Integer): TLangContents;
    procedure SetItem(Index: Integer; const Value: TLangContents);
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
    property Name: string read FInfo.LangName write FInfo.LangName;
    property LocalName: string read FInfo.LocalName write FInfo.LocalName;
    property ShortName: string read FInfo.ShortName write FInfo.ShortName;
    property RightToLeft: Boolean read FInfo.RightToLeft write FInfo.RightToLeft;
    property ID: Cardinal read FInfo.LangID write FInfo.LangID;
    property Info: TLanguageInfo read FInfo write FInfo;
    property Items[Index: Integer]: TLangContents read GetItem write SetItem; default;
  end;

  TLanguageClass = class of TLanguage;

  TmnOnMacro = procedure(var S: string) of object;

  { TLanguages }

  TLanguages = class(TObjectList)
  private
    FNotifyObjects: TInterfaceList;
    FDefaultLanguage: TLanguage;
    FCurrentLanguage: TLanguage;
    FUseDefaultText: Boolean;
    FTestMode: Boolean;
    FOnMacro: TmnOnMacro;
    function GetItem(Index: Integer): TLanguage;
    procedure SetItem(Index: Integer; const Value: TLanguage);
    function GetCurrent: TLanguage;
    procedure NotifyObjects;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNotify(AObject: ILanguageRead);
    procedure RemoveNotify(AObject: ILanguageRead);
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
    class function GetFileExtensions: string; virtual;
    class function GetTitle: string; virtual;
    property Contents: TLangContents read FContents write SetContents;
  end;

  TLangParserClass = class of TLangParser;

const
  sUTF8BOM: array[1..3] of char = (#$EF, #$BB, #$BF);

function Languages: TLanguages;
procedure InitLanguages(LanguagesClass:TLanguagesClass = nil);
function LangFindText(const ID: string; var Value: string): Boolean;

var
  //default for Delphi or Lazarus
  SystemEncoding: TLangEncoding {$ifdef LCL} = lncUTF8; {$else} = lncAnsi; {$endif}

implementation

var
  FLanguages: TLanguages = nil;

function LangFindText(const ID: string; var Value: string): Boolean;
begin
  if FLanguages <> nil then
    Result := FLanguages.GetText(ID, Value)
  else
    Result := False;
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

procedure TLanguage.SetItem(Index: Integer; const Value: TLangContents);
begin
  inherited Items[Index] := Value;
end;

{ TLanguages }

function TLanguages.Add(Language: TLanguage): Integer;
begin
  Result := inherited Add(Language);
end;

procedure TLanguages.AddNotify(AObject: ILanguageRead);
begin
  if not Supports(AObject, ILanguageRead) then
    raise ELangException.Create('Object must support of ILanguageRead');
  FNotifyObjects.Add(AObject);
end;

constructor TLanguages.Create;
begin
  inherited Create;
  FNotifyObjects := TInterfaceList.Create;
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

procedure TLanguages.RemoveNotify(AObject: ILanguageRead);
begin
  FNotifyObjects.Remove(AObject);  
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
    NotifyObjects;
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
  FreeAndNil(FNotifyObjects);
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

procedure TLanguages.NotifyObjects;
var
  i: Integer;
begin
  for i := 0 to FNotifyObjects.Count -1 do
  begin
    ILanguageRead(FNotifyObjects[i]).LanguageChanged(Current.Info);
  end;
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
end;

function TLangContents.CreateLangItem: TLangItem;
begin
  Result := DoCreateLangItem;
  Add(Result);
end;

destructor TLangContents.Destroy;
begin
  FSettings.Free;
  inherited;
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

function TLangContents.GetItem(Index: Integer): TLangItem;
begin
  Result := inherited Items[Index] as TLangItem;
end;

function TLangContents.GetText(const ID: string): string;
begin
  GetText(ID, Result);
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
      Text := Items[i].Text;
      EncodeText(Text);
      break;
    end
  end;
end;

procedure TLangContents.SetItem(Index: Integer; const Value: TLangItem);
begin
  inherited Items[Index] := Value;
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

initialization
  FLanguages := nil;
finalization
  FreeAndNil(FLanguages);
end.

