unit LangUtils;
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

interface

uses
  Classes, SysUtils,
  LangClasses;

procedure LoadLanguages(const vDefaultLanguage, vSource, vFilter: string; vFilerClass: TLangFilerClass);
procedure SaveLanguages(vSource: string; vFilerClass: TLangFilerClass; OnlyModified: Boolean);

function _(const ID: string): string; overload;
function _(const ID: string; Default: string): string; overload;
function LangFindText(const ID: string; var Value: string): Boolean;
function GetText(const ID: string): string; overload;

implementation

uses
  IniFiles, PO_Languages;
  
procedure LoadLanguages(const vDefaultLanguage, vSource, vFilter: string; vFilerClass: TLangFilerClass);
var
  I: Integer;
  SearchRec: TSearchRec;
  aLanguage: TLanguage;
  aFiler: TLangFiler;
begin
  if vFilerClass = nil then
    raise ELangException.Create('FilerClass is nil');
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
          aFiler.LoadFrom(IncludeTrailingPathDelimiter(vSource) + SearchRec.Name, vFilter, aLanguage);
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

procedure SaveLanguages(vSource: string; vFilerClass: TLangFilerClass; OnlyModified: Boolean);
var
  i: Integer;
  aFiler: TLangFiler;
begin
  if vFilerClass = nil then
    raise ELangException.Create('FilerClass is nil');
  try
    for i := 0 to Languages.Count -1 do
    if not OnlyModified or Languages[i].Modified then
    begin
      aFiler := vFilerClass.Create;
      try
        aFiler.SaveTo(vSource, Languages[i]);
      finally
        aFiler.Free;
      end;
    end;
  except
    raise;
  end;
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

function LangFindText(const ID: string; var Value: string): Boolean;
begin
  if IsLanguagesInitialized then
    Result := Languages.GetText(ID, Value)
  else
    Result := False;
end;

initialization
end.

