unit IS_Parser;
{***********************************************************************

  Copyright (C) 2005  Zaher Dirkey (zaher@parmaja.com)

  This file is part of Parmaja tools.

  anyTranslator is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation; either version 2 of the License,
  or (at your option) any later version.

  anyTranslator is distributed in the hope that it will be useful, but
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
  SysUtils, Variants, Classes,
  Contnrs, RTLConsts, Dialogs, INI_Parser;

type
  TISLangFile = class(TINILangFile)
  private
  protected
  public
  end;

  TISTranslator = class(TCustomTranslator)
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

{ TISTranslator }

function TISTranslator.DoCreateLangFile: TCustomLangFile;
begin
  Result := TISLangFile.Create;
end;

class function TISTranslator.GetFileExtensions: string;
begin
  Result := '.isl'
end;

class function TISTranslator.GetID: string;
begin
  Result := 'InnoSetup';               
end;

class function TISTranslator.GetTitle: string;
begin
  Result := 'InnoSetup www.jrsoftware.org';
end;

function TISTranslator.IsRightToLeft: Boolean;
begin
   Result := DetectRightToLeft(StrToIntDef(Self['LangOptions'].Values['LanguageID'], 0));
end;

class function TISTranslator.GetFlags: TTranslatorFlags;
begin
  Result := [tfExpandable];
end;

initialization
  RegisterTranslatorClass(TISTranslator);
finalization
end.

