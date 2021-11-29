unit FluxBB_Parser;
{**
 *  This file is part of the "Mini Library" http://www.sourceforge.net/projects/minilib
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey
 *}

{
  ['lang_common'].Values['lang_encoding'];
}

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}

interface

uses
  SysUtils, Variants, Classes,
  Contnrs, RTLConsts,
  mnLangClasses;

const
  sOpenArray = 'array(';

type
  TFluxBBLangRangeState = (rsUnknown, rsArray);

  { TFluxBBLangFile }

  TFluxBBParser = class(TLangParser)
  private
    FLastComment: string;
    FRange: TFluxBBLangRangeState;
    FLangItem: TLangItem;
  protected
    procedure ParseLine(Number:Integer; ALine: string);
    procedure DoParse(Strings: TStringList); override;
    procedure DoGenerate(Strings: TStringList); override;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetName: string; override;
    class function GetTitle: string; override;
  end;

  { TFluxBBFiler }

  TFluxBBFiler = class(TLangFiler)
  protected
    procedure DoLoadFrom(vSource: string; vLanguage: TLanguage; vFiles: TStrings = nil); override;
    procedure DoSaveTo(vSource: string; vLanguage: TLanguage); override;
  public
    function CreateParser: TLangParser; override;
    class function GetExtension: string; override;
    class function GetTitle: string; override;
    class function GetName: string; override;
    class function GetFlags: TLangFilerFlags; override;
  end;

  { TFluxBBDirFiler }

  TFluxBBDirFiler = class(TLangFiler)
  protected
    procedure DoLoadFrom(vSource: string; vLanguage: TLanguage; vFiles: TStrings = nil); override;
    procedure DoSaveTo(vSource: string; vLanguage: TLanguage); override;
  public
    function CreateParser: TLangParser; override;
    class function GetExtension: string; override;
    class function GetTitle: string; override;
    class function GetName: string; override;
    class function GetFlags: TLangFilerFlags; override;
  end;

implementation

uses
  StrUtils;

procedure TFluxBBParser.ParseLine(Number:Integer; ALine: string);
var
  i: Integer;
  s: string;
  aOpened: Boolean;
  //  aOpenChar: Char;//not impl yet for both " ' supporting
  aOpenPos, aOpenCount: Integer;
  aKey, aValue: string;
  aItem: TLangItem;
  CurChar, NextChar: Char;
  aGroup: TLangGroup;
begin
  case FRange of
    rsUnKnown:
      begin
        if (aLine <> '') and (aLine[1] = '$') and (RightStr(aLine, Length(sOpenArray)) = sOpenArray) then
        begin
          aGroup := TLangGroup.Create;
          aGroup.Name := Copy(s, 2, AnsiPos(' ', s) - 2);
          Contents.Groups.Add(aGroup);
          FRange := rsArray;
        end;
      end;
    rsArray:
      begin
        aOpenPos := 0;
        aOpened := False;
        aOpenCount := 0;
        i := 1;
        while i <= Length(aLine) do
        begin
          CurChar := aLine[i];
          if i < Length(aLine) then
            NextChar := aLine[i + 1]
          else
            NextChar := #0;
          if not aOpened and (CurChar = ')') and (NextChar = ';') then
          begin
            FRange := rsUnKnown;
            Break;
          end
          else if not aOpened and (CurChar = '/') and (NextChar = '/') then
          begin
            FLastComment := Trim(Copy(aLine, i + 2, MaxInt));
            Break;
          end
          else if aOpened and (CurChar = '\') then
          begin
            Inc(i); //skip char
          end
          else if CurChar = '''' then
          begin
            if not aOpened then
            begin
              aOpenPos := i;
              Inc(aOpenCount);
              aOpened := True;
            end
            else
            begin
              if aOpenCount = 1 then
                aKey := Copy(aLine, aOpenPos + 1, i - aOpenPos - 1)
              else
              begin
                aValue := Copy(aLine, aOpenPos + 1, i - aOpenPos - 1);
                aGroup := Contents.Groups.Last;
                with aGroup do
                begin
                  aItem := Contents.CreateLangItem;
                  aItem.Line := Number;
                  aItem.ID := aKey;
                  aItem.Text := aValue;
                  aItem.Comment := FLastComment;
                  FLastComment := '';
                end;
              end;
              aOpened := False;
            end
          end;
          Inc(i);
        end;
      end;
  end;
end;

procedure TFluxBBParser.DoParse(Strings: TStringList);
var
  l: Integer;
begin
  l := 0;
  while l < Strings.Count do
  begin
    ParseLine(l, Strings[l]);
    Inc(l);
  end;
end;

procedure TFluxBBParser.DoGenerate(Strings: TStringList);
begin
end;

constructor TFluxBBParser.Create;
begin
  inherited;
end;

destructor TFluxBBParser.Destroy;
begin
  inherited;
end;

class function TFluxBBParser.GetName: string;
begin
  Result := 'FluxBBParser';
end;

class function TFluxBBParser.GetTitle: string;
begin
  Result := 'FluxBB Parser';
end;

{ TFluxBBFiler }

procedure TFluxBBFiler.DoLoadFrom(vSource: string; vLanguage: TLanguage; vFiles: TStrings);
begin
  DefaultLoadFrom(False, vSource, vLanguage);
  if vLanguage.Count > 0 then
    vLanguage.IsRightToLeft := SameText(vLanguage.GetText('lang_common', 'lang_direction') , 'rtl');
end;

procedure TFluxBBFiler.DoSaveTo(vSource: string; vLanguage: TLanguage);
begin
end;

function TFluxBBFiler.CreateParser: TLangParser;
begin
  Result := TFluxBBParser.Create;
end;

class function TFluxBBFiler.GetExtension: string;
begin
  Result := 'php';
end;

class function TFluxBBFiler.GetName: string;
begin
  Result := 'FluxBB';
end;

class function TFluxBBFiler.GetTitle: string;
begin
  Result := 'FluxBB forums www.Fluxbb.org';
end;

class function TFluxBBFiler.GetFlags: TLangFilerFlags;
begin
  Result := [lffDefault, lffSingle];
end;

{ TFluxBBDirFiler }

procedure TFluxBBDirFiler.DoLoadFrom(vSource: string; vLanguage: TLanguage; vFiles: TStrings);
begin
end;

procedure TFluxBBDirFiler.DoSaveTo(vSource: string; vLanguage: TLanguage);
begin
end;

function TFluxBBDirFiler.CreateParser: TLangParser;
begin
  Result := TFluxBBParser.Create;
end;

class function TFluxBBDirFiler.GetExtension: string;
begin
  Result := 'php';
end;

class function TFluxBBDirFiler.GetTitle: string;
begin
  Result := 'FluxBB forums language directory (www.Fluxbb.org)';
end;

class function TFluxBBDirFiler.GetName: string;
begin
  Result := 'FluxBBDir';
end;

class function TFluxBBDirFiler.GetFlags: TLangFilerFlags;
begin
  Result := [lffMultiple, lffDirectory];
end;

initialization
  LangOptions.RegisterFilerClass(TFluxBBFiler);
  LangOptions.RegisterFilerClass(TFluxBBDirFiler);
finalization
end.

