unit trsProjects;
{**
 * This file is part of the "Mini Translator" http://www.sourceforge.net/projects/minilib
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Variants, Classes, Forms, Graphics, Controls,
  Contnrs, RTLConsts, Dialogs,
  LangClasses, PO_Languages,
  trsClasses,
  mnXMLRtti, mnXMLRttiProfile;

type
  TToolsList = class(TObjectList)
  end;

  { TtrsProject }

  TtrsProject = class(TmnXMLProfile)
  private
    FInternal: Boolean;
    FName: string;
    FOriginalName: string;
    FLocalName: string;
    FNotes: string;
    FFileName: string;
    FFiler: string;
    FFilerClass: TLangFilerClass;
    FLog: TStringList;
    FHaveWarring: Boolean;
    FToolsList: TObjectList;
    FDictionary: TtrsDictionary;
    procedure SetFiler(const Value: string);
    procedure SetFilerClass(const AValue: TLangFilerClass);
  public
    Current: TLangItem;
    FindWordStr: string;
    //
    constructor Create;
    destructor Destroy; override;
    property FilerClass: TLangFilerClass read FFilerClass write SetFilerClass;
    procedure LoadLanguage;
    procedure UpgradeLanguage;
    procedure SaveLanguage(Force: Boolean);
    procedure ExportLanguage(vParserClass: TLangParserClass);
    procedure LoadDictionary(vSource: string; var vLanguage: TLanguage);
    procedure SaveDictionary(vSource: string; vLanguage: TLanguage; Force: Boolean);
    property Log: TStringList read FLog write FLog;
    property HaveWarring: Boolean read FHaveWarring write FHaveWarring;
    property ToolsList: TObjectList read FToolsList;
    property Internal: Boolean read FInternal write FInternal default False;
    property Dictionary: TtrsDictionary read FDictionary;
  published
    property Filer: string read FFiler write SetFiler;
    property OriginalName: string read FOriginalName write FOriginalName;
    property LocalName: string read FLocalName write FLocalName;
    property Name: string read FName write FName;
    property FileName: string read FFileName write FFileName;
    property Notes: string read FNotes write FNotes;
  end;

  { TtrsOptions }

  TtrsOptions = class(TmnXMLProfile)
  private
    FFont: TFont;
    FFormHeight: Integer;
    FFormWidth: Integer;
    FTopLayout: Boolean;
    procedure SetFont(const AValue: TFont);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property FormWidth: Integer read FFormWidth write FFormWidth default 0;
    property FormHeight: Integer read FFormHeight write FFormHeight default 0;
    property TopLayout: Boolean read FTopLayout write FTopLayout default False;
    property Font: TFont read FFont write SetFont;
  end;

  { TtrsEngine }

  TtrsEngine = class(TObject)
  private
    FOptions: TtrsOptions;
  public
    WorkPath: string;
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    procedure LoadOptions;
    procedure SaveOptions;
    property Options: TtrsOptions read FOptions;
  end;

function GetConfigFileName: string;
function trsEngine: TtrsEngine;

const
  sMiniTranslator = 'miniTranslator';

implementation

uses
  StrUtils;

function GetConfigFileName: string;
begin
  Result := Application.Location + 'init.cfg';
end;

var
  FtrsEngine: TtrsEngine = nil;

function trsEngine:TtrsEngine;
begin
  if FtrsEngine = nil then
    FtrsEngine:=TtrsEngine.Create;
  Result := FtrsEngine;
end;

{ TtrsProject }

constructor TtrsProject.Create;
begin
  inherited;
  FToolsList := TObjectList.Create;
  FLog := TStringList.Create;
  FDictionary := TtrsDictionary.Create;
end;

destructor TtrsProject.Destroy;
begin
  FreeAndNil(FDictionary);
  FreeAndNil(FToolsList);
  FreeAndNil(FLog);
  inherited;
end;

procedure TtrsProject.ExportLanguage(vParserClass: TLangParserClass);
var
  aParser: TLangParser;
begin
  aParser := vParserClass.Create;
  //aParser.Contents := Dictionary.Local;
  //aParser.Generate();
  //SaveDictionary(LocalName, aParser, True);
end;

procedure TtrsProject.LoadDictionary(vSource: string; var vLanguage: TLanguage);
var
  aFiler: TLangFiler;
begin
  if FilerClass = nil then
    raise Exception.Create('FilerClass is nil');
  FreeAndNil(vLanguage);
  try
    vLanguage := TLanguage.Create;
    aFiler := FilerClass.Create;
    try
      aFiler.LoadFrom(vSource, vLanguage);
    finally
      aFiler.Free;
    end;
  except
    FreeAndNil(vLanguage);
    raise;
  end;
end;

procedure TtrsProject.LoadLanguage;
var
  b1, b2: Boolean;
begin
  if (LocalName <> '') then
  begin
    LoadDictionary(LocalName, Dictionary.Local);
    if (OriginalName <> '') then
    begin
      LoadDictionary(OriginalName, Dictionary.Original);
      try
        Log.BeginUpdate;
        Log.Add('--------- deprecated words ---------');
        Log.Add('');
        //b1 := Local.Compare(Original, Log);
        Log.Add('');
        Log.Add('--------- New words ---------');
        Log.Add('');
      finally
        Log.EndUpdate;
      end;
      //b2 := Original.Compare(Local, Log);
      if not b1 or not b2 then
        HaveWarring := True;
    end;
  end;
end;

procedure TtrsProject.SaveDictionary(vSource: string; vLanguage: TLanguage; Force: Boolean);
var
  aFiler: TLangFiler;
begin
  if FilerClass = nil then
    raise Exception.Create('FilerClass is nil');
  aFiler := FilerClass.Create;
  try
    aFiler.SaveTo(vSource, vLanguage); //add force
  finally
    aFiler.Free;
  end;
end;

procedure TtrsProject.SaveLanguage(Force: Boolean);
begin
  SaveDictionary(LocalName, Dictionary.Local, Force);
end;

procedure TtrsProject.SetFiler(const Value: string);
var
  aFilerClass: TLangFilerClass;
begin
  if FFiler <> Value then
  begin
    aFilerClass := LangOptions.FindFiler(Value);
    if aFilerClass = nil then
      raise Exception.Create(Value + ' type not found!');
    FFilerClass := aFilerClass;
    FFiler := Value;
  end;
end;

procedure TtrsProject.SetFilerClass(const AValue: TLangFilerClass);
begin
  FFilerClass := AValue;
  FFiler := FFilerClass.GetName;
end;

procedure TtrsProject.UpgradeLanguage;
var
  aOldFiles: TLanguage;
  b1: Boolean;
begin
  if (OriginalName = '') then
    LoadLanguage
  else if (LocalName <> '') and (OriginalName <> '') then
  begin
    aOldFiles := nil;
    LoadDictionary(LocalName, aOldFiles);
    LoadDictionary(OriginalName, Dictionary.Local);
    LoadDictionary(OriginalName, Dictionary.Original);
    //Local.Files[0].FileName := aOldFiles.Files[0].FileName; //zaher ya zaher
    Log.BeginUpdate;
    Log.Add('--------- deprecated words ---------');
    Log.Add('');
    //b1 := aOldFiles.Compare(Original, Log);
    Log.Add('');
    Log.Add('--------- New words ---------');
    Log.Add('');
    Log.EndUpdate;
//    Local.Import(aOldFiles, Log);
    if not b1 then
      HaveWarring := True;
    aOldFiles.Free;
  end;
end;

{ TtrsEngine }

constructor TtrsEngine.Create;
begin
  inherited Create;
  FOptions := TtrsOptions.Create;
end;

destructor TtrsEngine.Destroy;
begin
  FreeAndNil(FOptions);
  inherited;
end;

procedure TtrsEngine.Init;
var
  aStrings: TStringList;
begin
  aStrings := TStringList.Create;
  try
    if FileExists(GetConfigFileName) then
      aStrings.LoadFromFile(GetConfigFileName);
    WorkPath := aStrings.Values['WorkPath'];
  finally
    aStrings.Free;
  end;
  LoadOptions;
end;

procedure TtrsEngine.LoadOptions;
var
  s:string;
begin
  Options.SafeLoadFromFile(WorkPath + 'options.xml');
  s := Options.Font.Name;
end;

procedure TtrsEngine.SaveOptions;
begin
  Options.SaveToFile(WorkPath + 'options.xml');
end;

{ TtrsOptions }

procedure TtrsOptions.SetFont(const AValue: TFont);
begin
  if FFont <> AValue then
  begin
    FFont.Assign(AValue);
  end;
end;

constructor TtrsOptions.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.Quality := fqDefault;
  FFont.Pitch := fpDefault;
end;

destructor TtrsOptions.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

initialization
finalization
  FreeAndNil(FtrsEngine);
end.
