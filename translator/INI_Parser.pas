unit INI_Parser;
{***********************************************************************

  Copyright (C) 2005  Zaher Dirkey (zaher@parmaja.com)

  This file is part of Parmaja tools.

  MiniTranslator is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation; either version 2 of the License,
  or (at your option) any later version.

  MiniTranslator is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
  MA  02111-1307  USA

************************************************************************}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, 
  Contnrs, RTLConsts, Dialogs;

type
  TINILangFile = class(TLangFile)
  private
    FLine: Integer;
  protected
    procedure Parse; override;
    procedure ParseLine;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TINITranslator = class(TCustomTranslator)
  protected
    function DoCreateLangFile:TCustomLangFile; override;
  public
    class function GetFileExtensions: string; override;
    class function GetTitle: string; override;
    class function GetID: string; override;
    class function GetFlags: TTranslatorFlags; override;
    function IsRightToLeft:Boolean; override;
  end;

implementation

uses
  StrUtils;

procedure TINILangFile.ParseLine;
var
  s, aLine: string;
  P: Integer;
  aSection: TSectionItem;
  aWord: TWordItem;
begin
  aLine := Contents[FLine];
  s := Trim(aLine);

  if (s <> '') and (LeftStr(s, 1) <> ';') then
  begin
    if (LeftStr(s, 1) = '[') and (RightStr(s, 1) = ']') then
    begin
      aSection := TSectionItem.Create(Self);
      aSection.Name := Copy(s, 2, Length(s) - 2);
      Sections.Add(aSection);
//      FRange := rsArray;
    end
    else
    begin
      aSection := Sections[Sections.Count - 1];
      with aSection do
      begin
        aWord := TWordItem.Create(aSection);
        aWord.Line := FLine;
        P := AnsiPos('=', aLine);
        if P > 0 then
        begin
          aWord.Pos := P + 1;
          aWord.Size := Length(aLine) - P;
          aWord.Key := Copy(aLine, 1, P - 1);
          aWord.Value := Copy(aLine, P + 1, MaxInt);
        end
        else
        begin
          aWord.Pos := Length(aLine);
          aWord.Size := 0;
          aWord.Key := Copy(aLine, 1, P - 1);
          aWord.Value := Copy(aLine, P + 1, MaxInt);
        end;
      end;
    end;
  end;
end;

procedure TINILangFile.Parse;
begin
  FLine := 0;
  while FLine < Contents.Count do
  begin
    ParseLine;
    Inc(FLine);
  end;
end;

constructor TINILangFile.Create;
begin
  inherited;
end;

destructor TINILangFile.Destroy;
begin
  inherited;
end;

{ TINITranslator }

function TINITranslator.DoCreateLangFile: TCustomLangFile;
begin
  Result := TINILangFile.Create;
end;

class function TINITranslator.GetFileExtensions: string;
begin
  Result := '.isl'
end;

class function TINITranslator.GetID: string;
begin
  Result := 'INI';               
end;

class function TINITranslator.GetTitle: string;
begin
  Result := 'INI Based language';
end;

function TINITranslator.IsRightToLeft: Boolean;
begin
   Result :=  False;
end;

class function TINITranslator.GetFlags: TTranslatorFlags;
begin
  Result := [tfExpandable];
end;

initialization
  RegisterTranslatorClass(TINITranslator);
finalization
end.

