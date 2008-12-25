unit FluxBB_Parser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Contnrs, RTLConsts, Dialogs, TranslatorClasses;

const
  sOpenArray = 'array(';

type
  TFluxBBLangRangeState = (rsUnknown, rsArray);

  TFluxBBLangFile = class(TCustomLangFile)
  private
    FRange: TFluxBBLangRangeState;
    FLine: Integer;
  protected
    procedure Parse; override;
    procedure ParseLine;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TFluxBBTranslator = class(TCustomTranslator)
  protected
    function DoCreateLangFile: TCustomLangFile; override;
  public
    class function GetFileExtensions: string; override;
    class function GetTitle: string; override;
    class function GetID: string; override;
    class function GetFlags: TTranslatorFlags; override;
    function IsRightToLeft:Boolean; override;
    procedure Import(vFiles: TCustomTranslator; Log: TStrings); override;
  end;

implementation

uses
  StrUtils;

procedure TFluxBBLangFile.ParseLine;
var
  i: Integer;
  s, aLine: string;
  aOpened: Boolean;
  //  aOpenChar: Char;//not impl yet for both " ' supporting
  aOpenPos, aOpenCount: Integer;
  aKey, aValue: string;
  aSection: TSectionItem;
  aWord: TWordItem;
  CurChar, NextChar: Char;
begin
  aLine := Contents[FLine];
  s := Trim(aLine);
  case FRange of
    rsUnKnown:
      begin
        if (s <> '') and (s[1] = '$') and (RightStr(s, Length(sOpenArray)) =
          sOpenArray) then
        begin
          aSection := TSectionItem.Create(Self);
          aSection.Name := Copy(s, 2, AnsiPos(' ', s) - 2);
          Sections.Add(aSection);
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
                aSection := Sections[Sections.Count - 1];
                with aSection do
                begin
                  aWord := TWordItem.Create(aSection);
                  aWord.Line := FLine;
                  aWord.Pos := aOpenPos + 1;
                  aWord.Size := i - aOpenPos - 1;
                  aWord.InternalKey := aKey;
                  aWord.InternalValue := aValue;
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

procedure TFluxBBLangFile.Parse;
begin
  FLine := 0;
  while FLine < Contents.Count do
  begin
    ParseLine;
    Inc(FLine);
  end;
end;

constructor TFluxBBLangFile.Create;
begin
  inherited;
end;

destructor TFluxBBLangFile.Destroy;
begin
  inherited;
end;

{ TFluxBBTranslator }

function TFluxBBTranslator.DoCreateLangFile: TCustomLangFile;
begin
  Result := TFluxBBLangFile.Create;
end;

class function TFluxBBTranslator.GetFileExtensions: string;
begin
  Result := '.php'
end;

class function TFluxBBTranslator.GetID: string;
begin
  Result := 'FluxBB';
end;

class function TFluxBBTranslator.GetTitle: string;
begin
  Result := 'FluxBB forums www.Fluxbb.org';
end;

procedure TFluxBBTranslator.Import(vFiles: TCustomTranslator; Log: TStrings);
begin
  inherited;
  Sections['lang_common'].Values['lang_direction'] := vFiles['lang_common'].Values['lang_direction'];
  Sections['lang_common'].Values['lang_encoding'] := vFiles['lang_common'].Values['lang_encoding'];
end;

function TFluxBBTranslator.IsRightToLeft: Boolean;
begin
  Result := SameText(Self['lang_common'].Values['lang_direction'] , 'rtl');
end;

class function TFluxBBTranslator.GetFlags: TTranslatorFlags;
begin
  Result := [tfMultiFiles];
end;

initialization
  RegisterTranslatorClass(TFluxBBTranslator);
finalization
end.

