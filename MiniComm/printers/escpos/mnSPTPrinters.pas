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

  TSPT8Printer = class(TmnESCPOSPrinter)
  protected
    procedure GetInitBitImageCommands(var S:string); override;
  public
//    procedure PrintCanvas(Canvas: TCanvas); override;
  end;

implementation


{ TSPTIIIPrinter }

procedure TSPTIIIPrinter.GetInitBitImageCommands(var S:string);
begin
  inherited;
  S := S + seqSetLeftMargin + chr(0) + chr(0);
end;

{ TSPT8Printer }

procedure TSPT8Printer.GetInitBitImageCommands(var S: string);
begin
  inherited;

end;

end.

