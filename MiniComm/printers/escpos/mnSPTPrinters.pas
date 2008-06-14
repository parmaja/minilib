unit mnSPTPrinters;
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
  mnPrinters, mnESCPOSPrinters;

type
  TSPTIIIPrinter = class(TmnESCPOSPrinter)
  protected
    procedure GetInitBitImageCommands(var S:string); override;
  end;

  { TmnSPT8Page }

  TmnSPT8Page = class(TmnESCPOSPage)
  protected
  public
    procedure PrintCanvas(Canvas: TCanvas); override;
  end;
  
  { TSPT8Printer }

  TSPT8Printer = class(TmnESCPOSPrinter)
  protected
    function DoCreatePage: TmnCustomPage; override;
    procedure GetInitRasterBitImageCommands(var S: string); override;
  public
    procedure BeginDocument; override;
    constructor Create(Style: TmnPrintStyle; Stream: TStream); override;
  end;

implementation


{ TSPTIIIPrinter }

procedure TSPTIIIPrinter.GetInitBitImageCommands(var S:string);
begin
  inherited;
  S := S + seqSetLeftMargin + chr(0) + chr(0);
end;

{ TSPT8Printer }

function TSPT8Printer.DoCreatePage: TmnCustomPage;
begin
  Result := TmnSPT8Page.Create(Self);
end;

procedure TSPT8Printer.GetInitRasterBitImageCommands(var S: string);
begin
  S := #0 + seqSetAbsolutePosition + #0 + #0 + S;// first #0 for wakeup as email from support
end;

procedure TSPT8Printer.BeginDocument;
begin
  inherited;
  //this kind of printer need to wake up before send data
  Print(seqWakeup);
  Sleep(100);
  Print(seqWakeup);
end;

constructor TSPT8Printer.Create(Style: TmnPrintStyle; Stream: TStream);
begin
  inherited;
  DefaultWidth := 520;
end;

{ TmnSPT8Page }

procedure TmnSPT8Page.PrintCanvas(Canvas: TCanvas);
begin
  PrintCanvasAsRasterBitImageChunks(Canvas);
end;

end.

