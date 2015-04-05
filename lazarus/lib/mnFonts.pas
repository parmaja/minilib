unit mnFonts;
{$mode objfpc}
{$H+}
{**
 *  Raster/Bitmap Fonts
 *
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  SysUtils, Classes, Graphics, FPCanvas, IniFiles;

type

  TCallbackDrawChar = function(const Target; x, y:Integer; c: char): Boolean of Object;

  { TmnfRasterFont }

  TmnfRasterFont = class(TFPCustomDrawFont)
  protected
    procedure DoDrawText(x, y: Integer; Text: String); override;
    procedure DoGetTextSize(Text: String; var w, h: Integer); override;
    function DoGetTextHeight(Text: String): Integer; override;
    function DoGetTextWidth(Text: String): Integer; override;

    function DrawCharCanvas(const Target; x, y:Integer; c: char): Boolean;
    {$ifdef windows}
    function DrawCharDC(const Target; x, y:Integer; c: char): Boolean;
    {$endif}

    procedure PrintString(CallbackDrawChar: TCallbackDrawChar; const Target; x, y: Integer; Text: String);

  public
    FontBitmap: TBitmap;
    Rows, Columns: Integer;

    CharCount: Integer;
    CharWidth: Integer;
    CharHeight: Integer;
    CharStart: Integer;

    constructor Create; override;
    procedure Generate(FontName: String = 'Courier'; FontSize: integer = 10);

    procedure PrintText(ACanvas: TFPCustomCanvas; x, y: Integer; Text: String);
    {$ifdef windows}
    procedure PrintText(DC: HDC; x, y: Integer; Text: String);
    {$endif}
    procedure LoadFromFile(FileName: String);
    procedure SaveToFile(FileName: String);
    procedure LoadInfoFromFile(FileName: String);
    procedure SaveInfoToFile(FileName: String);
  end;

implementation

{ TmnfRasterFont }

procedure TmnfRasterFont.DoDrawText(x, y: Integer; Text: String);
begin
  PrintText(Canvas, x, y, Text);
end;

procedure TmnfRasterFont.DoGetTextSize(Text: String; var w, h: Integer);
begin
  w := Length(Text) * CharWidth;
  h := CharHeight;
end;

function TmnfRasterFont.DoGetTextHeight(Text: String): Integer;
begin
  Result := CharHeight;
end;

function TmnfRasterFont.DoGetTextWidth(Text: String): Integer;
begin
  Result := Length(Text) * CharWidth;
end;

{$ifdef windows}
function TmnfRasterFont.DrawCharDC(const Target; x, y: Integer; c: char): Boolean;
var
  index: Integer;
  cx, cy: Integer;
begin
  index := Ord(c) - CharStart;
  cy := (index div Columns) * CharHeight;
  cx := (index mod Columns) * CharWidth;
  BitBlt(HDC(Target), x, y, CharWidth, CharHeight, FontBitmap.Canvas.Handle, cx, cy, MERGECOPY);
  Result := true;
end;
{$endif}

function TmnfRasterFont.DrawCharCanvas(const Target; x, y: Integer; c: char): Boolean;
var
  cRect: TRect;
  index: Integer;
  cx, cy: Integer;
  ACanvas: TFPCustomCanvas;
begin
  ACanvas:=TFPCustomCanvas(Target);
  index := Ord(c) - CharStart;
  cy := (index div Columns) * CharHeight;
  cx := (index mod Columns) * CharWidth;
  cRect := rect(cx, cy, cx + CharWidth - 1, cy + CharHeight - 1);
  if ACanvas is TCanvas then
    (ACanvas as TCanvas).CopyMode := cmMergeCopy;
  ACanvas.CopyRect(x, y, FontBitmap.Canvas, cRect);
  Result := true;
end;

constructor TmnfRasterFont.Create;
begin
  inherited Create;
  CharStart := 32;
  CharCount := 256 - CharStart;
  Columns := 16;
  Rows := 16;
  FontBitmap := TBitmap.Create;
  with FontBitmap do
  begin
    Width := 0;
    Height := 0;
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(0, 0, Width, Height);
    Transparent := True;
    TransParentColor := clWhite;
    TransparentMode := tmAuto;
  end;
end;

procedure TmnfRasterFont.Generate(FontName: String; FontSize: integer);
var
  i, c, r: Integer;
begin
  with FontBitmap do
  begin
    Canvas.Font.Size := FontSize;
    Canvas.Font.Name := FontName;
    Canvas.Brush.Color := clWhite;
    CharWidth := Canvas.TextWidth('W');
    CharHeight := Canvas.TextHeight('W');
    Rows := round(CharCount / Columns);
    Height := CharHeight * Rows;
    Width := CharWidth * Columns;

    Canvas.FillRect(0, 0, Width, Height);

    i := CharStart;
    for r := 0 to Rows -1 do
      for c := 0 to Columns -1 do
      begin
        Canvas.TextOut(c * CharWidth, r * CharHeight, chr(i));
        inc(i);
      end;
  end;
end;

procedure TmnfRasterFont.PrintText(ACanvas: TFPCustomCanvas; x, y: Integer; Text: String);
begin
  PrintString(@DrawCharCanvas, ACanvas, x, y, Text);
end;

{$ifdef windows}
procedure TmnfRasterFont.PrintText(DC: HDC; x, y: Integer; Text: String);
begin
  PrintString(@DrawCharDC, Dc, x, y, Text);
end;
{$endif}

procedure TmnfRasterFont.PrintString(CallbackDrawChar: TCallbackDrawChar; const Target; x, y: Integer; Text: String);
var
  i: Integer;
begin
  for i := 1 to Length(Text) do
  begin
    if not CallbackDrawChar(Target, x, y, text[i]) then
      break;
    x := x + CharWidth;
  end;
end;

procedure TmnfRasterFont.LoadFromFile(FileName: String);
begin
  FreeAndNil(FontBitmap);
  FontBitmap := TBitmap.Create;
  with FontBitmap do
  begin
    LoadFromFile(FileName);
    Transparent := True;
    TransParentColor := clWhite;
    TransparentMode := tmAuto;
  end;
end;

procedure TmnfRasterFont.LoadInfoFromFile(FileName: String);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(FileName);
  try
    CharStart := ini.ReadInteger('Font', 'CharStart', 0);
    Rows := ini.ReadInteger('Font', 'Rows', 0);
    Columns := ini.ReadInteger('Font', 'Columns', 0);
    CharCount := ini.ReadInteger('Font', 'CharCount', 256);
    CharWidth := ini.ReadInteger('Font', 'CharWidth', 0);
    CharHeight := ini.ReadInteger('Font', 'CharHeight', 0);
  finally
    ini.Free;
  end;
end;

procedure TmnfRasterFont.SaveToFile(FileName: String);
begin
  FontBitmap.SaveToFile(fileName);
end;

procedure TmnfRasterFont.SaveInfoToFile(FileName: String);
var
  ini: TIniFile;
begin
  ini:=TIniFile.Create(FileName);
  try
    ini.WriteInteger('Font', 'CharStart', CharStart);
    ini.WriteInteger('Font', 'Rows', Rows);
    ini.WriteInteger('Font', 'Columns', Columns);
    ini.WriteInteger('Font', 'CharCount', CharCount);
    ini.WriteInteger('Font', 'CharWidth', CharWidth);
    ini.WriteInteger('Font', 'CharHeight', CharHeight);
  finally
    ini.Free;
  end;
end;
end.
