unit trsProjects;
{**
 * Mini Translator
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls,
  Contnrs, RTLConsts, Dialogs,
  LangClasses, PO_Languages,
  trsClasses,
  mnXMLRtti, mnXMLRttiProfile;

type
  TToolsList = class(TObjectList)
  end;

  TtrsProject = class(TmnXMLProfile)
  private
    FName: string;
    FOriginalName: string;
    FLocalName: string;
    FNotes: string;
    FFileName: string;
    FID: string;
    FFilerClass: TLangFilerClass;
    FLog: TStringList;
    FHaveWarring: Boolean;
    FToolsList: TObjectList;
    procedure SetID(const Value: string);
  public
    Current: TLangItem;
    FindWordStr: string;
    //
    Dictionary:TtrsDictionary;
    constructor Create;
    destructor Destroy; override;
    property FilerClass: TLangFilerClass read FFilerClass;
    procedure LoadLanguage;
    procedure UpgradeLanguage;
    procedure SaveLanguage(Force: Boolean);
    procedure ExportLanguage(vParserClass: TLangParserClass);
    procedure LoadDictionary(var vLanguage: TLanguage; Path: string);
    procedure SaveDictionary(const Path: string; var vDictionary: TLanguage; Force: Boolean);
    property Log: TStringList read FLog write FLog;
    property HaveWarring: Boolean read FHaveWarring write FHaveWarring;
    property ToolsList: TObjectList read FToolsList;
  published
    property ID: string read FID write SetID;
    property OriginalName: string read FOriginalName write FOriginalName;
    property LocalName: string read FLocalName write FLocalName;
    property Name: string read FName write FName;
    property FileName: string read FFileName write FFileName;
    property Notes: string read FNotes write FNotes;
  end;

const
  sSoftwareRegKey = 'Software\anyTranslator';

implementation

uses
  StrUtils;

{ TtrsProject }

constructor TtrsProject.Create;
begin
  inherited;
  FToolsList := TObjectList.Create;
  FLog := TStringList.Create;
end;

destructor TtrsProject.Destroy;
begin
  FreeAndNil(Dictionary);
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

procedure TtrsProject.LoadDictionary(var vLanguage: TLanguage; Path: string);
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
      aFiler.LoadFrom(FileName, vLanguage);
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
    LoadDictionary(Dictionary.Local, LocalName);
    if (OriginalName <> '') then
    begin
      LoadDictionary(Dictionary.Original, OriginalName);
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

procedure TtrsProject.SaveDictionary(const Path: string; var vDictionary: TLanguage; Force: Boolean);
var
  aParser: TPO_Parser;
begin
  aParser := TPO_Parser.Create;
  try
    GenerateLanguageFile(FileName, vDictionary, aParser);//add force
  finally
    aParser.Free;
  end;
end;

procedure TtrsProject.SaveLanguage(Force: Boolean);
begin
  SaveDictionary(LocalName, Dictionary.Local, Force);
end;

procedure TtrsProject.SetID(const Value: string);
begin
  if FID <> Value then
  begin
    if FFilerClass <> nil then
      raise Exception.Create('You can not change the type of project!');
    FFilerClass := LangOptions.FindFilerClass(Value);
    if FFilerClass = nil then
      raise Exception.Create(Value + ' type not found!');
    FID := Value;
  end;
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
    LoadDictionary(aOldFiles, LocalName);
    LoadDictionary(Dictionary.Local, OriginalName);
    LoadDictionary(Dictionary.Original, OriginalName);
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

initialization
finalization
end.

