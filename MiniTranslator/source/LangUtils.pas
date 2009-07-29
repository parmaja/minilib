unit LangUtils;
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

interface

uses
  Classes, SysUtils,
  LangClasses;

type

  { TPOFileFiler }

  TPOFileFiler = class(TLangFiler)
  public
    constructor Create; override;
    function CreateParser:TLangParser; override;
    procedure LoadFrom(vSource: string; vLanguage:TLanguage); override;
    procedure SaveTo(vSource: string; vLanguage:TLanguage); override;
    class function GetName: string; override;
    class function GetTitle: string; override;
    class function GetExtension: string; override;
    class function GetFlags: TLangFilerFlags; override;
  end;

  { TPODirFiler }
{
  this not standard directory PO files and setting.ini file
}
  TPODirctoryFiler = class(TLangFiler)
  public
    constructor Create; override;
    function CreateParser:TLangParser; override;
    procedure LoadFrom(vSource: string; vLanguage:TLanguage); override;
    procedure SaveTo(vSource: string; vLanguage:TLanguage); override;
    class function GetName: string; override;
    class function GetTitle: string; override;
    class function GetExtension: string; override;
    class function GetFlags: TLangFilerFlags; override;
  end;

  { TPODirctoryExFiler }

  TPODirctoryExFiler = class(TPODirctoryFiler) //with setting.ini
  public
    procedure LoadFrom(vSource: string; vLanguage:TLanguage); override;
    class function GetName: string; override;
    class function GetTitle: string; override;
  end;

procedure LoadLanguages(const vDefaultLanguage, vSource: string; vFilerClass: TLangFilerClass);

function _(const ID: string): string; overload;
function _(const ID: string; Default: string): string; overload;
function GetText(const ID: string): string; overload;

implementation

uses
  IniFiles, PO_Languages;
  
procedure LoadLanguages(const vDefaultLanguage, vSource: string; vFilerClass: TLangFilerClass);
var
  I: Integer;
  SearchRec: TSearchRec;
  aLanguage: TLanguage;
  aFiler: TLangFiler;
begin
  if vFilerClass = nil then
    raise ELangException.Create('FilerClass is nul');
  try
    I := FindFirst(IncludeTrailingPathDelimiter(vSource) + '*.*', faDirectory, SearchRec);
    while I = 0 do
    begin
      if ((SearchRec.Attr and faDirectory) > 0) and (SearchRec.Name[1] <> '.') then
      begin
        aLanguage := TLanguage.Create;
        Languages.Add(aLanguage);
        aFiler := vFilerClass.Create;
        try
          aFiler.LoadFrom(IncludeTrailingPathDelimiter(vSource) + SearchRec.Name, aLanguage);
        finally
          aFiler.Free;
        end;
      end;
      I := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  except
    raise;
  end;
  Languages.SetDefaultLanguage(vDefaultLanguage);
  Languages.SetCurrentLanguage(vDefaultLanguage);
end;

function _(const ID: string): string;
var
  s: string;
begin
  if LangFindText(ID, s) then
    Result := s
  else
    Result := ID;
end;

function _(const ID: string; Default: string): string;
var
  s: string;
begin
  if LangFindText(ID, s) then
    Result := s
  else
    Result := Default
end;

function GetText(const ID: string): string;
var
  s: string;
begin           
  if LangFindText(ID, s) then
    Result := s
  else
    Result := ID;
end;

{ TPODirctoryFiler }

constructor TPODirctoryFiler.Create;
begin
  inherited Create;
end;

function TPODirctoryFiler.CreateParser: TLangParser;
begin
  Result := TPO_Parser.Create;
end;

procedure TPODirctoryFiler.LoadFrom(vSource: string; vLanguage: TLanguage);
var
  I: Integer;
  SearchRec: TSearchRec;
  aName, aPath, aFile: String;
  aIniFile: TIniFile;
  aParser: TLangParser;
begin
  with vLanguage do
  begin
    aPath := IncludeTrailingPathDelimiter(vSource);
    try
      Clear;
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
    except
      raise;
    end;
  end;
end;

procedure TPODirctoryFiler.SaveTo(vSource: string; vLanguage: TLanguage);
begin
end;

class function TPODirctoryFiler.GetName: string;
begin
  Result :='PODir';
end;

class function TPODirctoryFiler.GetTitle: string;
begin
  Result := 'PO Directory';
end;

class function TPODirctoryFiler.GetExtension: string;
begin
  Result :='PO';
end;

class function TPODirctoryFiler.GetFlags: TLangFilerFlags;
begin
  Result := [lffDirectory, lffMultiple];
end;

{ TPOFileFiler }

constructor TPOFileFiler.Create;
begin
  inherited Create;
end;

function TPOFileFiler.CreateParser: TLangParser;
begin
  Result := TPO_Parser.Create;
end;

procedure TPOFileFiler.LoadFrom(vSource: string; vLanguage: TLanguage);
var
  aParser: TLangParser;
begin
  with vLanguage do
  begin
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
    Source := vSource;
    Name := ExtractFileName(vSource);
    ID := 0;
    //IsRightToLeft := ;
  end;
end;

procedure TPOFileFiler.SaveTo(vSource: string; vLanguage: TLanguage);
var
  aParser: TLangParser;
begin
  with vLanguage do
  begin
    try
      aParser := CreateParser;
      try
        GenerateLanguageFile(vLanguage.Source, vLanguage, aParser);
      finally
        aParser.Free;
      end;
    except
      raise;
    end;
  end;
end;

class function TPOFileFiler.GetName: string;
begin
  Result := 'POFile'
end;

class function TPOFileFiler.GetTitle: string;
begin
  Result := 'PO File';
end;

class function TPOFileFiler.GetExtension: string;
begin
  Result := 'PO';
end;

class function TPOFileFiler.GetFlags: TLangFilerFlags;
begin
  Result := [lffAlone];
end;

{ TPODirctoryExFiler }

procedure TPODirctoryExFiler.LoadFrom(vSource: string; vLanguage: TLanguage);
var
  aPath, aName, aFile: String;
  aIniFile: TIniFile;
begin
  with vLanguage do
  begin
    aPath := IncludeTrailingPathDelimiter(vSource);
    aName := ExtractFileName(ExcludeTrailingPathDelimiter(vSource));
    aFile := aPath + '\setting.ini';
    if FileExists(aFile) then
    begin
      aIniFile := TIniFile.Create(aFile);
      try
        Name := aIniFile.ReadString('options', 'Name', aName);
        IsRightToLeft := aIniFile.ReadInteger('options', 'RightToLeft', 0) = 1;
        ID := StrToIntDef('$' + aIniFile.ReadString('options', 'ID', '0401'), 0);
      finally
        aIniFile.Free;
      end;
    end;
  end;
  inherited;
end;

class function TPODirctoryExFiler.GetName: string;
begin
  Result := 'PODirEx';
end;

class function TPODirctoryExFiler.GetTitle: string;
begin
  Result := 'PO Directory with setting.ini'
end;

initialization
  LangOptions.RegisterFilerClass(TPOFileFiler);
  LangOptions.RegisterFilerClass(TPODirctoryFiler);
  LangOptions.RegisterFilerClass(TPODirctoryExFiler);
end.

