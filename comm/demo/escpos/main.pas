unit main;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ucputils, ucp864, ucp1256, minibidi,
  mnPrinters, mnESCPOSPrinters, mnCommClasses, mnCommStreams, mnSPTPrinters, mnPRPPrinters;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label3: TLabel;
    Memo2: TMemo;
    PortCbo: TComboBox;
    PrintersCbo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    PrintToFileChk: TCheckBox;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HexStr(const s; c: Integer);
  private
    procedure PrintESCPOS;
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.PrintESCPOS;
var
  aPrinter: TmnPrinter;
  aStream: TStream;
  aTH: Integer;
  aTS: TTextStyle;
begin
  Screen.Cursor := crHourGlass;
  try
    try
      if PrintToFileChk.Checked then
        aStream := TFileStream.Create(Application.Location + 'test.prn', fmCreate)
      else
      begin
        aStream := TmnCommStream.Create(True, PortCbo.Text, 9600, dbEight, prNone, sbOneStopBit, hsHardware);
        (aStream as TmnCommStream).Open;
      end;
      //You can create the printer class by name
      aPrinter := mnRegisteredPrinters.CreateByName(mnRegisteredPrinters[PrintersCbo.ItemIndex].Name, mnpsCanvas, aStream);
      try
        aTS.Alignment := taLeftJustify;
        aTS.Wordbreak := True;
        aTS.SingleLine := False;

        aPrinter.BeginDocument;
        //aPrinter.PrintLn('          ');// this for make wakeup lazy printers
        aPrinter.NewPage;

        aPrinter.Page.Canvas.Font.Name := 'Tahoma';
        aPrinter.Page.Canvas.Font.Height := 20;
        aPrinter.Page.Canvas.TextStyle := aTS;
        //Bad way to calc the rect
        aTH := aPrinter.Page.Canvas.TextHeight('AWM') * Memo1.Lines.Count + 2;
        //we need to create real page with real size

        aPrinter.CancelPage;
        aPrinter.NewPage;

        aPrinter.Page.Height:= aTH;
        aPrinter.Page.Canvas.TextStyle := aTS;
        aPrinter.Page.Canvas.Font.Name := 'Tahoma';
        aPrinter.Page.Canvas.Font.Color := clBlack;
        aPrinter.Page.Canvas.Font.Height := 20;
        aPrinter.Page.Canvas.TextRect(aPrinter.Page.BoundsRect, 0, 0, Memo1.Text);
        aPrinter.EndPage;
        aPrinter.EndDocument;
        aPrinter.Eject;
        aPrinter.Cut;
        (aPrinter as TmnESCPOSPrinter).PrintBarcode('123456789');
        if PrintToFileChk.Checked then
          (aStream as TmnCommStream).Close;
      finally
        aPrinter.Free;
        aStream.Free;
      end;
    except
      raise;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  PrintESCPOS;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  aPrinter: TmnESCPOSPrinter;
  aStream: TStream;
  i: Integer;
  s: string;
  ws: widestring;
begin
  Screen.Cursor := crHourGlass;
  try
    try
      if PrintToFileChk.Checked then
        aStream := TFileStream.Create(Application.Location + 'test.prn', fmCreate)
      else
      begin
        aStream := TmnCommStream.Create(True, PortCbo.Text, 9600, dbEight, prNone, sbOneStopBit, hsHardware);
        (aStream as TmnCommStream).Open;
      end;
      //You can create the printer class by name
      aPrinter := mnRegisteredPrinters.CreateByName(mnRegisteredPrinters[Integer(PrintersCbo.Items.Objects[PrintersCbo.ItemIndex])].Name, mnpsCanvas, aStream) as TmnESCPOSPrinter;
      try
        aPrinter.Print(seqSelectCharacter + #22);    //22 is the number of codepage Arabic cp864
        aPrinter.Print(seqSetRightAlignment);
        for i := 0 to Memo1.Lines.Count -1 do
        begin
          ws:=UTF8Decode(Memo1.Lines[i]);
          BidiString(ws);
          s := ucpUnicodeToCP864(ws);
          aPrinter.PrintLn(s);
        end;
        aPrinter.Eject;
        //aPrinter.Cut;
        //(aPrinter as TmnESCPOSPrinter).PrintBarcode('123456789');
        if PrintToFileChk.Checked then
          (aStream as TmnCommStream).Close;
      finally
        aPrinter.Free;
        aStream.Free;
      end;
    except
      raise;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ws: widestring;
  s: string;
begin
  ws := 'بسم الله الرحمن الرحيم';
  HexStr(ws, Length(ws) * 2);
  BidiString(ws);
  HexStr(ws, Length(ws) * 2);
  s := ucpUnicodeToCP864(ws);
  HexStr(s, length(s));
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to mnRegisteredPrinters.Count -1 do
  begin
    if mnRegisteredPrinters[i].PrinterClass.InheritsFrom(TmnESCPOSPrinter) then
      PrintersCbo.Items.AddObject(mnRegisteredPrinters[i].Title, TObject(i));
  end;
  PrintersCbo.ItemIndex := 0;
end;

procedure TForm1.HexStr(const s; c: Integer);
var
  i: Integer;
  h: string;
begin
  h := '';
  for i := 0 to c - 1 do
  begin
    h := h + IntToHex(PByte(s)[i], 2);
  end;
  Memo2.Lines.Add(h);
end;

end.

