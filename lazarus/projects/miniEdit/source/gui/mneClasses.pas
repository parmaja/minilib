unit mneClasses;

{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

{$DEFINE SYN_HEREDOC}

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs,
  LCLintf, LCLType,
  Dialogs, EditorOptions, SynEditHighlighter, SynEditSearch, SynEdit,
  Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  SynHighlighterCSS, SynHighlighterSQL, SynHighlighterXML, SynHighlighterApache,
  SynHighlighterJScript, SynHighlighterXHTML, SynHighlighterPas;

type
  TSQLFile = class(TEditorFile)
  public
  end;

  TApacheFile = class(TEditorFile)
  public
  end;

  TINIFile = class(TEditorFile)
  public
  end;

  TTXTFile = class(TEditorFile)
  public
  end;

  TXMLFile = class(TEditorFile)
  public
    procedure NewSource; override;
  end;

  TSQLFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TApacheFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TINIFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TTXTFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TXMLFileCategory = class(TFileCategory)
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  { TmneEngine }

function ColorToRGBHex(Color: TColor): string;
function RGBHexToColor(Value: string): TColor;

const
  sSoftwareRegKey = 'Software\miniEdit\';

function GetFileImageIndex(const FileName: string): integer;

function GetWordAtRowColEx(SynEdit: TCustomSynEdit; XY: TPoint; IdentChars: TSynIdentChars; Select: boolean): string;
function GetHighlighterAttriAtRowColEx2(SynEdit: TCustomSynEdit; const XY: TPoint; var Token: string; var TokenType, Start: integer; var Attri: TSynHighlighterAttributes; var Range: Pointer): boolean;

implementation

uses
  IniFiles, mnXMLStreams, mnUtils;

function ColorToRGBHex(Color: TColor): string;
var
  aRGB: TColorRef;
begin
  aRGB := ColorToRGB(Color);
  FmtStr(Result, '%s%.2x%.2x%.2x', ['#', GetRValue(aRGB), GetGValue(aRGB), GetBValue(aRGB)]);
end;

function RGBHexToColor(Value: string): TColor;
var
  R, G, B: byte;
begin
  if LeftStr(Value, 1) = '#' then
    Delete(Value, 1, 1);
  if Value <> '' then
  begin
    if Length(Value) = 3 then
    begin
      R := StrToIntDef('$' + Copy(Value, 1, 1) + Copy(Value, 1, 1), 0);
      G := StrToIntDef('$' + Copy(Value, 2, 1) + Copy(Value, 2, 1), 0);
      B := StrToIntDef('$' + Copy(Value, 3, 1) + Copy(Value, 3, 1), 0);
      Result := RGB(R, G, B);
    end
    else
    begin
      R := StrToIntDef('$' + Copy(Value, 1, 2), 0);
      G := StrToIntDef('$' + Copy(Value, 3, 2), 0);
      B := StrToIntDef('$' + Copy(Value, 5, 2), 0);
      Result := RGB(R, G, B);
    end;
  end
  else
    Result := clBlack;
end;

type
  TSynCustomHighlighterHack = class(TSynCustomHighlighter);

function GetHighlighterAttriAtRowColEx2(SynEdit: TCustomSynEdit; const XY: TPoint; var Token: string; var TokenType, Start: integer; var Attri: TSynHighlighterAttributes; var Range: Pointer): boolean;
var
  PosX, PosY: integer;
  Line: string;
  aToken: string;
begin
  with SynEdit do
  begin
    TokenType := 0;
    Token := '';
    Attri := nil;
    Result := False;
    PosY := XY.Y - 1;
    if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then
    begin
      Line := Lines[PosY];
      if PosY = 0 then
        Highlighter.ResetRange
      else
        Highlighter.SetRange(TSynCustomHighlighterHack(Highlighter).CurrentRanges.Range[PosY - 1]);
      Highlighter.SetLine(Line, PosY);
      PosX := XY.X;
      Range := Highlighter.GetRange;
      if PosX > 0 then
        while not Highlighter.GetEol do
        begin
          Start := Highlighter.GetTokenPos + 1;
          aToken := Highlighter.GetToken;
          Range := Highlighter.GetRange;
          if (PosX >= Start) and (PosX < Start + Length(aToken)) then
          begin
            Attri := Highlighter.GetTokenAttribute;
            TokenType := Highlighter.GetTokenKind;
            Token := aToken;
            Result := True;
            exit;
          end;
          Highlighter.Next;
        end;
    end;
  end;
end;

function GetWordAtRowColEx(SynEdit: TCustomSynEdit; XY: TPoint; IdentChars: TSynIdentChars; Select: boolean): string;
var
  Line: string;
  Len, Stop: integer;
begin
  Result := '';
  if (XY.Y >= 1) and (XY.Y <= SynEdit.Lines.Count) then
  begin
    Line := SynEdit.Lines[XY.Y - 1];
    Len := Length(Line);
    if Len <> 0 then
    begin
      if (XY.X > 1) and (XY.X <= Len + 1) and not (Line[XY.X] in IdentChars) then
        XY.X := XY.X - 1;
      if (XY.X >= 1) and (XY.X <= Len + 1) and (Line[XY.X] in IdentChars) then
      begin
        Stop := XY.X;
        while (Stop <= Len) and (Line[Stop] in IdentChars) do
          Inc(Stop);
        while (XY.X > 1) and (Line[XY.X - 1] in IdentChars) do
          Dec(XY.X);
        if Stop > XY.X then
        begin
          Result := Copy(Line, XY.X, Stop - XY.X);
          if Select then
          begin
            SynEdit.CaretXY := XY;
            SynEdit.BlockBegin := XY;
            SynEdit.BlockEnd := Point(XY.x + Length(Result), XY.y);
          end;
        end;
      end;
    end;
  end;
end;

{ TmneEngine }

function GetFileImageIndex(const FileName: string): integer;
var
  AExtensions: TStringList;
  s: string;
begin
  s := ExtractFileExt(FileName);
  if LeftStr(s, 1) = '.' then
    s := Copy(s, 2, MaxInt);

  AExtensions := TStringList.Create;
  try
    Engine.Groups[0].EnumExtensions(AExtensions);
    if AExtensions.IndexOf(s) >= 0 then
      Result := 2
    else
      Result := 1;//any file
  finally
    AExtensions.Free;
  end;
end;

{ TSQLFileCategory }

function TSQLFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynSQLSyn.Create(nil);
end;

{ TTApacheFileCategory }

function TApacheFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynApacheSyn.Create(nil);
end;

{ TINIFileCategory }

function TINIFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

{ TTXTFileCategory }

function TTXTFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

{ TXMLFileCategory }

function TXMLFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynXMLSyn.Create(nil);
end;

{ TXMLFile }

procedure TXMLFile.NewSource;
begin
  SynEdit.Text := '<?xml version="1.0" encoding="iso-8859-1" ?>';
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('');
  SynEdit.CaretY := 2;
  SynEdit.CaretX := 3;
end;

initialization
  with Engine do
  begin
    //Categories.Add('', TTXTFile, TTXTFileCategory);
    Categories.Add('TXT', TTXTFile, TTXTFileCategory);
    Categories.Add('APACHE', TApacheFile, TApacheFileCategory, []);
    Categories.Add('SQL', TSQLFile, TSQLFileCategory);
    Categories.Add('INI', TINIFile, TINIFileCategory);
    Categories.Add('XML', TXMLFile, TXMLFileCategory);

    Groups.Add('SQL', 'SQL files', 'SQL', ['sql'], [fgkPublish, fgkBrowsable]);
    Groups.Add('APACHE', 'htaccess files', 'APACHE', ['htaccess', 'conf'], [fgkBrowsable]);
    Groups.Add('XML', 'XML files', 'XML', ['xml'], [fgkPublish, fgkBrowsable]);
    Groups.Add('INI', 'INI files', 'INI', ['ini'], []);
    Groups.Add('TXT', 'TXT files', 'TXT', ['txt'], []);
  end;
end.


