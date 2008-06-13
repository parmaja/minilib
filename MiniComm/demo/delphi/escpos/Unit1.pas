unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  mnPrinters, mnESCPOSPrinters, mnCommStreams, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    ToFileChk: TCheckBox;
    LowChk: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses mnSPTPrinters;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Printer: TmnESCPOSPrinter;
  yp:Integer;
  procedure Print(s:string);
  begin
    Printer.Page.Canvas.TextOut(0, yp, s);
    yp := yp + Printer.Page.Canvas.TextHeight(s);
  end;
var
  aStream : TStream;
begin
  yp := 0;
  if ToFileChk.Checked then
    aStream := TFileStream.Create('c:\print.prn', fmCreate)
  else
    aStream := TmnCommStream.Create(False, Edit1.Text, 9600);
  Printer := TSPT8Printer.Create(mnpsCanvas, aStream);
  Printer.DefaultWidth := 520;
  Printer.DefaultHeight := 200;
  Printer.NewPage;
  if LowChk.Checked then
    Printer.Density := mndLow;
  try
    Printer.Page.Canvas.FillRect(Printer.Page.BoundsRect);
    Printer.Page.Canvas.Font.Size := 14;
    Printer.Page.Canvas.Font.Style := [];
    Printer.Density := mndLow;
    Print('„—Õ»« »ﬂ„ ›Ì ”Â·Ì ”Ê› ');
    Printer.Page.Canvas.Font.Style := [fsBold];
    Print( '«”„ «·⁄„Ì·: “«Â— œÌ—ﬂÌ');
    Printer.Page.Canvas.Font.Style := [];
    Print( '«·—ﬁ„: 2754');
    Print( '------------------');
    Print( 'SN: 145111001');
    Print( 'SN: 654654654');
    Print( '------------------');
    Printer.EndPage;
{    Printer.PrintBarcode('546798798');
    Printer.CarriageReturn;
    Printer.LineFeed;}
  finally
    Printer.Free;
    aStream.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Printer: TmnESCPOSPrinter;
  procedure Print(s:string);
  begin
    Printer.PrintLn(s);
  end;
var
  aStream : TmnCommStream;
begin
  aStream := TmnCommStream.Create(False, Edit1.Text, 9600);
  Printer := TmnESCPOSPrinter.Create(mnpsCanvas, aStream);
  try
//    Printer.Page.
//    Printer.Page.Canvas.FillRect(Printer.Page.BoundsRect);
    Print('Hello to ESC/POS');
    Print( 'Printer');
    Print( '------------------');
    Print( '145111001');
    Print( '654654654');
    Print( '------------------');
    Printer.PrintBarcode('546798798');
  finally
    Printer.Free;
    aStream.Free;
  end;
end;


end.
