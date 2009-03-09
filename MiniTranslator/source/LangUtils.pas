unit LangUtils;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
 
interface

uses
  Classes, SysUtils,
  LangClasses;

procedure LoadLanguage(Language: TLanguage; const Path, Extension: string);
procedure LoadLanguages(const DefaultLanguage, Path, Extension: string; LanguageClass: TLanguageClass = nil);

function _(const ID: string): string; overload;
function _(const ID: string; Default: string): string; overload;
function GetText(const ID: string): string; overload;

implementation

uses
  IniFiles;
  
procedure ParseLanguage(Language: TLanguage; FileName:string);
var
  Parser: TLangParser;
  Contents: TLangContents;
  Strings: TStringList;
begin
  Contents := Language.CreateContents;//this auto add the contents to Language 
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
  
procedure LoadLanguage(Language: TLanguage; const Path, Extension: String);
var
  I: Integer;
  aIniFile: TIniFile;
  SearchRec: TSearchRec;
  aName, aPath, aFile: String;
begin
  with Language do
  begin
    aPath := IncludeTrailingPathDelimiter(Path);
    aName := ExtractFileName(ExcludeTrailingPathDelimiter(aPath));
    aFile := aPath + '\setting.ini';
    if FileExists(aFile) then
    begin
      aIniFile := TIniFile.Create(aFile);
      try
        Name := aIniFile.ReadString('options', 'Name', aName);
        RightToLeft := aIniFile.ReadInteger('options', 'RightToLeft', 0) = 1;
        ID := StrToIntDef('$' + aIniFile.ReadString('options', 'ID', '0401'), 0);
      finally
        aIniFile.Free;
      end;
    end;

    try
      Clear;
      I := FindFirst(aPath + '*' + Extension, 0, SearchRec);
      while I = 0 do
      begin
        ParseLanguage(Language, aPath + SearchRec.Name);
        I := FindNext(SearchRec);
      end;
      FindClose(SearchRec);
    except
      raise;
    end;
  end;     
end;

procedure LoadLanguages(const DefaultLanguage, Path, Extension: string; LanguageClass: TLanguageClass);
var
  I: Integer;
  SearchRec: TSearchRec;
  aLanguage: TLanguage;
begin
  if LanguageClass = nil then
    LanguageClass := TLanguage;
  try
    I := FindFirst(IncludeTrailingPathDelimiter(Path) + '*.*', faDirectory, SearchRec);
    while I = 0 do
    begin
      if ((SearchRec.Attr and faDirectory) > 0) and (SearchRec.Name[1] <> '.') then
      begin
        aLanguage := LanguageClass.Create;
        Languages.Add(aLanguage);
        LoadLanguage(aLanguage, IncludeTrailingPathDelimiter(Path) + SearchRec.Name, Extension);
      end;
      I := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  except
    raise;
  end;
  Languages.SetDefaultLanguage(DefaultLanguage);
  Languages.SetCurrentLanguage(DefaultLanguage);
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

end.

