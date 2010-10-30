unit mnESCPOSPrinters;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFDEF FPC}
  IntfGraphics,
{$ENDIF}
  Graphics,
  mnPrinters;

type

  { TmnESCPOSPage }

  TmnESCPOSPage = class(TmnCustomPage)
  private
    FBitmap: TBitmap;
  protected
    function CreateBitmap:TBitmap;
    procedure ResizePage; override;
    function GetCanvas: TCanvas; override;
    procedure PrintCanvasAsBitImage(Canvas: TCanvas);
    procedure PrintCanvasAsRasterBitImage(Canvas: TCanvas);
    procedure PrintCanvasAsRasterBitImageChunks(Canvas: TCanvas);
  public
    constructor Create(Printer: TmnPrinter); override;
    destructor Destroy; override;
    procedure SaveToFile(FileName:string); override;
    procedure PrintCanvas(Canvas: TCanvas); override;
  end;

  { TmnESCPOSPrinter }

  TmnESCPOSPrinter = class(TmnPrinter)
  private
  protected
    procedure PrintPage(vPage:TmnCustomPage); override;
    function DoCreatePage: TmnCustomPage; override;

    function GetName: string; override;
    function GetSEQ_BitImage(Dots: Word): string; virtual;
    function GetSEQ_RasterBitImage(W, H: Word): string; virtual;
    function GetSEQ_BitImageFlag: string; virtual;
    function GetSEQ_RasterBitImageFlag: string; virtual;
    function GetDensityBytes: Integer; virtual;
    function GetLineSpacing: Integer; virtual;
    //
    procedure GetInitBitImageCommands(var S: string); virtual;
    procedure GetInitRasterBitImageCommands(var S: string); virtual;
  public
    constructor Create(Style: TmnPrintStyle; Stream: TStream); override;
    class function PrinterTitle: string; override;
    class function PrinterName: string; override;

    procedure Reset;
    procedure LineFeed;
    procedure Return;
    procedure Bell;
    procedure CarriageReturn;
    procedure PrintBarcode(Barcode: AnsiString; PrintHRI:Boolean = True); virtual;
  end;

const
  seqNull = #0;
  seqWakeup = #0;
  seqBell = #$07;
  seqLF = #$0A;
  seqCR = #$0D;
  seqFF = #$0C;
  seqTAB = #$09;
  seqESC = #$1B;
  seqGS = #$1D;
  seqSetLeftMargin = seqGS + 'L';
  seqReset = seqESC + '@';
  seqSelectPrintMode = seqESC + '!';//n
  seqSelectDefaultlineSpacing = seqESC + '2';
  seqSetLineSpacing = seqESC + '3'; //n
  seqSetCharacterSpacing = seqESC + ' '; //n
  seqSetAbsolutePosition = seqESC + '$';
  seqSetRelativePosition = seqESC + '\';
  seqBitImage = seqESC + '*';
  seqRasterBitImage = seqGS + 'v0';
  seqSetBarcodeHeight = seqGS + 'k';
  seqSelectHRICharacter = seqGS + 'H';
  seqPrintBarcode = seqGS + 'k';
  seqSelectCharacter= seqESC + 't';
  seqPrintImage = #$1C + #$70 + #$01 + #$03;//Print loaded image

  seqEAN13 = #$02;
  seqEAN8 = #$03;

implementation

{$IFDEF FPC}
uses
  FPimage;
{$ENDIF}

{ TmnESCPOSPrinter }

procedure TmnESCPOSPrinter.CarriageReturn;
begin
  Print(seqCR);
end;

constructor TmnESCPOSPrinter.Create(Style: TmnPrintStyle; Stream: TStream);
begin
  inherited;
end;

class function TmnESCPOSPrinter.PrinterTitle: string;
begin
  Result := 'ESCPOS Standard';
end;

class function TmnESCPOSPrinter.PrinterName: string;
begin
  Result := 'ESCPOSStandard';
end;

procedure TmnESCPOSPrinter.Reset;
begin
  Print(seqReset);
end;

function TmnESCPOSPrinter.GetSEQ_BitImage(Dots: Word): string;
begin
  Result := seqBitImage + GetSEQ_BitImageFlag + Chr(lo(Dots)) + Chr(hi(Dots));
end;

function TmnESCPOSPrinter.GetSEQ_BitImageFlag: string;
begin
  case Density of
    mndHi: Result := #33;
    mndMedium: Result := #$17;//not worked
  else
    Result := #$01;
  end;
end;

function TmnESCPOSPrinter.GetSEQ_RasterBitImage(W, H: Word): string;
var
  c: Word;
begin
  c := W div 8;
  if (W mod 8) > 0 then
    c := W + 1;
  Result := seqRasterBitImage + GetSEQ_RasterBitImageFlag + Chr(lo(c)) + Chr(hi(c)) + Chr(lo(H)) + Chr(hi(H));
end;

function TmnESCPOSPrinter.GetSEQ_RasterBitImageFlag: string;
begin
  case Density of
    mndHi: Result := #0;
    mndMedium: Result := #0;
  else
    Result := #$0;//must #$3;
  end;
end;

function TmnESCPOSPrinter.GetDensityBytes: Integer;
begin
  case Density of
    mndHi: Result := 3;
    mndMedium: Result := 2;
  else
    Result := 1;
  end;
end;

procedure TmnESCPOSPrinter.GetInitBitImageCommands(var S: string);
begin
end;

procedure TmnESCPOSPrinter.GetInitRasterBitImageCommands(var S: string);
begin

end;

function TmnESCPOSPrinter.GetLineSpacing: Integer;
begin
  Result := 24;
end;

procedure TmnESCPOSPrinter.PrintPage(vPage: TmnCustomPage);
begin
  inherited;
  vPage.PrintCanvas(vPage.Canvas);
  Return;
end;

function TmnESCPOSPrinter.DoCreatePage: TmnCustomPage;
begin
  Result:=  TmnESCPOSPage.Create(Self);
end;

function TmnESCPOSPrinter.GetName: string;
begin
  Result := 'ESC/POS';
end;

procedure TmnESCPOSPage.PrintCanvas(Canvas: TCanvas);
begin
  PrintCanvasAsBitImage(Canvas);
//  PrintCanvasAsRasterBitImage(Canvas);
//  PrintCanvasAsRasterBitImageChunks(Canvas);
end;

procedure TmnESCPOSPage.PrintCanvasAsBitImage(Canvas: TCanvas);
var
  x: Integer;
  s: AnsiString;
{$IFDEF FPC}
  IntfImage: TLazIntfImage;
{$ENDIF}
  procedure ScanNow(FromY: Integer);
  var
    y, i: Integer;
    b: Byte;
  begin
    b := 0;
    y := FromY;
    for i := 0 to 7 do
    begin
      b := b shl 1;
{$IFDEF FPC}
      if (y < IntfImage.Height) and (IntfImage.TColors[x, y] = clBlack) then
{$ELSE}
      if (y < Height) and (Canvas.Pixels[x, y]  = clBlack) then
{$ENDIF}
        b := b or 1;
      Inc(y)
    end;
    s := s + Chr(b);
  end;
var
  l: Integer;
  c, d: Integer;
  ExtraCommands: string;
  aPrinter: TmnESCPOSPrinter;
begin
  inherited;
  aPrinter := (FPrinter as TmnESCPOSPrinter);
  l := 0;
  c := aPrinter.GetDensityBytes;
{$IFDEF FPC}
  IntfImage := FBitmap.CreateIntfImage;
{$ENDIF}
  try
    S := seqSetLineSpacing + chr(aPrinter.GetLineSpacing);
    s := s + seqSetCharacterSpacing + #0;
    aPrinter.Print(S);

    ExtraCommands := '';
    aPrinter.GetInitBitImageCommands(ExtraCommands);
    if ExtraCommands <> '' then
      aPrinter.Print(ExtraCommands);

    while l < Height do
    begin
      s := aPrinter.GetSEQ_BitImage(Width);
      for x := 0 to Width - 1 do
      begin
        for d := 0 to c - 1 do
          ScanNow(l + (d * 8));
      end;
      l := l + (c * 8);
      if l < Height then
        S := S + seqLF;
      aPrinter.Print(S);
    end;
  finally
{$IFDEF FPC}
    IntfImage.Free;
{$ENDIF}
  end;
end;

procedure TmnESCPOSPage.PrintCanvasAsRasterBitImage(Canvas: TCanvas);
var
  x, y: Integer;
  s: AnsiString;
{$IFDEF FPC}
  IntfImage: TLazIntfImage;
{$ENDIF}
  procedure ScanNow;
  var
    i: Integer;
    b: Byte;
  begin
    b := 0;
    for i := 0 to 7 do
    begin
      b := b shl 1;
{$IFDEF FPC}
      if (x < IntfImage.Width) and (IntfImage.TColors[x, y] = clBlack) then
{$ELSE}
      if (x < Width) and (Canvas.Pixels[x, y] = clBlack) then
{$ENDIF}
        b := b or 1;
      Inc(x)
    end;
    s := s + Chr(b);
  end;
var
  ExtraCommands: string;
  aPrinter: TmnESCPOSPrinter;
begin
  inherited;
  aPrinter := (FPrinter as TmnESCPOSPrinter);
{$IFDEF FPC}
  IntfImage := FBitmap.CreateIntfImage;
{$ENDIF}
  try
    ExtraCommands := '';
    aPrinter.GetInitRasterBitImageCommands(ExtraCommands);
    aPrinter.Print(ExtraCommands);

    s := aPrinter.GetSEQ_RasterBitImage(Width, Height);
    aPrinter.Print(s);

    y := 0;
    while y < Height do
    begin
      s := '';
      x := 0;
      while x < Width do
      begin
        ScanNow;
      end;
      Inc(y);
      aPrinter.Print(s);
    end;
  finally
{$IFDEF FPC}
    IntfImage.Free;
{$ENDIF}
  end;
end;

procedure TmnESCPOSPage.PrintCanvasAsRasterBitImageChunks(Canvas: TCanvas);
var
  x, y: Integer;
  s: AnsiString;
{$IFDEF FPC}
  IntfImage: TLazIntfImage;
{$ENDIF}
  procedure ScanNow;
  var
    i: Integer;
    b: Byte;
  begin
    b := 0;
    for i := 0 to 7 do
    begin
      b := b shl 1;
{$IFDEF FPC}
      if (x < IntfImage.Width) and (IntfImage.TColors[x, y] = clBlack) then
{$ELSE}
      if (x < Width) and (Canvas.Pixels[x, y] = clBlack) then
{$ENDIF}
        b := b or 1;
      Inc(x)
    end;
    s := s + Chr(b);
  end;
var
  ExtraCommands: string;
  aPrinter: TmnESCPOSPrinter;
  h, chunk: Integer;
begin
  inherited;
  aPrinter := (FPrinter as TmnESCPOSPrinter);
{$IFDEF FPC}
  IntfImage := FBitmap.CreateIntfImage;
{$ENDIF}
  try
    ExtraCommands := '';
    aPrinter.GetInitRasterBitImageCommands(ExtraCommands);

    chunk := 24;
    h := chunk;
    y := 0;
    repeat
      aPrinter.Print(ExtraCommands);
      s := aPrinter.GetSEQ_RasterBitImage(Width, chunk);
      aPrinter.Print(s);
      while y < h do
      begin
        s := '';
        x := 0;
        while x < Width do
        begin
          ScanNow;
        end;
        Inc(y);
        aPrinter.Print(s);
      end;
      if y >= Height then
        break;
      h := h + chunk;
    until h >= height;
  finally
{$IFDEF FPC}
    IntfImage.Free;
{$ENDIF}
  end;
end;

procedure TmnESCPOSPrinter.LineFeed;
begin
  Print(seqLF);
end;

procedure TmnESCPOSPrinter.Return;
begin
  Print(seqCR + seqLF);
end;

procedure TmnESCPOSPrinter.Bell;
begin
  Print(seqBell);
end;

procedure TmnESCPOSPrinter.PrintBarcode(Barcode:AnsiString; PrintHRI:Boolean);
begin
  Print(seqSelectHRICharacter + Chr(ord(PrintHRI)));//set height
  Print(seqSetBarcodeHeight + #15);//set height
  if Length(Barcode) < 13 then
    Barcode := Barcode + StringOfChar(seqNull, 13 - Length(Barcode))
  else if Length(Barcode) > 13 then
    Barcode := Copy(Barcode, 1 , 13); 
  Print(seqPrintBarcode + seqEAN13 + Barcode + seqNull);
end;

{ TmnESCPOSPage }

function TmnESCPOSPage.CreateBitmap: TBitmap;
begin
  Result := TBitmap.Create;
//  Result.Monochrome := True;//not work in WinCE
//  Result.PixelFormat:= pf8bit;
  Result.Width := Width;
  Result.Height := Height;
  Result.Canvas.Font.Quality := fqNonAntialiased;
  Result.Canvas.Brush.Color := clWhite;
  Result.Canvas.FillRect(Rect(0, 0, Width, Height));
end;

procedure TmnESCPOSPage.ResizePage;
begin
  inherited;
  if FBitmap <> nil then
  begin
    //can not resize page
  end;
end;

function TmnESCPOSPage.GetCanvas: TCanvas;
begin
  if FBitmap = nil then
  begin
    FBitmap := CreateBitmap;
  end;
  Result := FBitmap.Canvas;
end;

constructor TmnESCPOSPage.Create(Printer: TmnPrinter);
begin
  inherited;
  Width := Printer.DefaultWidth;
  Height := Printer.DefaultHeight;
end;

destructor TmnESCPOSPage.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TmnESCPOSPage.SaveToFile(FileName: string);
begin
  inherited SaveToFile(FileName);
  if FBitmap <> nil then
    FBitmap.SaveToFile(FileName);
end;

initialization
  mnRegisteredPrinters.Add(TmnESCPOSPrinter);
end.

