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
  public
    constructor Create(Printer: TmnCustomPrinter); override;
    destructor Destroy; override;
    procedure SaveToFile(FileName:string); override;
    procedure PrintCanvas(Canvas: TCanvas); override;
  end;

  { TmnESCPOSPrinter }

  TmnESCPOSPrinter = class(TmnCustomPrinter)
  private
  protected
    procedure PrintPage(vPage:TmnCustomPage); override;
    function DoCreatePage: TmnCustomPage; override;
    
    function GetName: string; override;
    function GetSEQ_BitImage(Dots: Word): string; virtual;
    function GetSEQ_BitImageFlag: string; virtual;
    function GetDensityBytes: Integer; virtual;
    function GetLineSpacing: Integer; virtual;
  public
    constructor Create(Style: TmnPrintStyle; Stream: TStream); override;
    procedure LineFeed;
    procedure CarriageReturn;
    procedure PrintBarcode(Barcode: AnsiString; PrintHRI:Boolean = True);
  end;

const
  seqNull = #0;
  seqBill = #$07;
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
  seqSetBarcodeHeight = seqGS + 'k';
  seqSelectHRICharacter = seqGS + 'H';
  seqPrintBarcode = seqGS + #$6B;

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

function TmnESCPOSPrinter.GetDensityBytes: Integer;
begin
  case Density of
    mndHi: Result := 3;
    mndMedium: Result := 2;
  else
    Result := 1;
  end;
end;

function TmnESCPOSPrinter.GetLineSpacing: Integer;
begin
  Result := 24;
end;

procedure TmnESCPOSPrinter.PrintPage(vPage: TmnCustomPage);
begin
  inherited;
  vPage.PrintCanvas(vPage.Canvas);
  LineFeed;
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
      if (y < Height) and (Canvas.Pixels[x, y] = clBlack) then
{$ENDIF}
        b := b or 1;
      Inc(y)
    end;
    s := s + Chr(b);
  end;
var
  l: Integer;
  c, d: Integer;
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
    S :=seqSetLineSpacing + chr(aPrinter.GetLineSpacing);
    s := s + seqSetLeftMargin + chr(0) + chr(0);
    s := s + seqSetCharacterSpacing + #0;
    aPrinter.Print(S);
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

procedure TmnESCPOSPrinter.LineFeed;
begin
  Print(seqLF);
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
  Result.Canvas.Brush.Color := clWhite;
  Result.Canvas.FillRect(0, 0, Width, Height);
end;

procedure TmnESCPOSPage.ResizePage;
var
  aBitmap: TBitmap;
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

constructor TmnESCPOSPage.Create(Printer: TmnCustomPrinter);
begin
  inherited;
  Width := 380;
  Height := Width;
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

end.

