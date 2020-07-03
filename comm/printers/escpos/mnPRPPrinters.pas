unit mnPRPPrinters;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
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
  mnPrinters, mnESCPOSPrinters;

type
  TPRP058Page = class(TmnESCPOSPage)
  protected
  public
    procedure PrintCanvas(Canvas: TCanvas); override;
  end;

  { TPRP058Printer }

  TPRP058Printer = class(TmnESCPOSPrinter)
  protected
    function DoCreatePage: TmnCustomPage; override;
    procedure GetInitBitImageCommands(var S: string); override;
  public
    constructor Create(Style: TmnPrintStyle; Stream: TStream); override;
    class function PrinterTitle: string; override;
    class function PrinterName: string; override;
    procedure PrintBarcode(Barcode: AnsiString; PrintHRI: Boolean =True); override;
  end;

implementation


{ TPRP058Printer }

function TPRP058Printer.DoCreatePage: TmnCustomPage;
begin
  Result := TPRP058Page.Create(Self);
end;

procedure TPRP058Printer.GetInitBitImageCommands(var S: string);
begin
  inherited;
  S := S + seqSetLeftMargin + chr(0) + chr(0);
end;

constructor TPRP058Printer.Create(Style: TmnPrintStyle; Stream: TStream);
begin
  inherited Create(Style, Stream);
  DefaultWidth := 384;
end;

class function TPRP058Printer.PrinterTitle: string;
begin
  Result := 'PRP-058';
end;

class function TPRP058Printer.PrinterName: string;
begin
  Result := 'PRP058';
end;

procedure TPRP058Printer.PrintBarcode(Barcode: AnsiString; PrintHRI: Boolean);
begin
  inherited;
  //Print(seqPrintBarcode + #67 + Char(Length(Barcode)) + Barcode);
end;

{ TPRP058Page }

procedure TPRP058Page.PrintCanvas(Canvas: TCanvas);
begin
  PrintCanvasAsRasterBitImageChunks(Canvas);
end;

initialization
  mnRegisteredPrinters.Add(TPRP058Printer);
end.

