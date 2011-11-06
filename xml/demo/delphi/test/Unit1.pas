unit Unit1;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  mnXMLStreams, mnXML, mnXMLWriter, mnXMLReader, mnXMLScanner, mnXMLUtils, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo: TMemo;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

type
  TMyXMLScanner = class(TmnXMLReader)
  protected
    procedure ReadOpenTag(const Name: string); override;
    procedure ReadText(const Text: string); override;
    procedure ReadAttributes(const Text: string); override;
    procedure ReadComment(const Text: string); override;
    procedure ReadCDATA(const Text: string); override;
    procedure ReadCloseTag(const Name: string); override;
  public
    Memo:TMemo;
  end;
  
implementation

uses mnXMLNodes;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  XMLWriter: TmnXMLWriter;
begin
  XMLWriter := TmnXMLWriter.Create(TmnWrapperStream.Create(TFileStream.Create('c:\1.xml', fmCreate)));
//  XMLWriter.Header.Add('myprop="myxml"');
  XMLWriter.Smart := True;
  XMLWriter.Start;
  XMLWriter.WriteOpenTag('order');
  XMLWriter.WriteOpenTag('head');
  XMLWriter.WriteOpenTag('author');
  XMLWriter.WriteText('zaher & jihad <123>');
//  XMLWriter.CloseTag('order');
  XMLWriter.Stop;
  XMLWriter.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  XMLScanner: TmnXMLScanner;
begin
  XMLScanner := TmnXMLScanner.Create;
//  XMLScanner.Memo := Memo;
  XMLScanner.Start;
  XMLScanner.ParseLine('<?xml version="1.0" encoding="iso-8859-1"?>', 1);
  XMLScanner.ParseLine('<root> <name1> text2 </name1>', 2);
  XMLScanner.ParseLine('<!-- text in comment -->', 3);
  XMLScanner.ParseLine('text3 <name2> text4 </name2>', 4);
  XMLScanner.ParseLine('</root>', 5);
  XMLScanner.Stop;
  XMLScanner.Free;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  XMLScanner: TMyXMLScanner;
begin
  XMLScanner := TMyXMLScanner.Create(TmnWrapperStream.Create(TFileStream.Create('extern.xml', fmOpenRead)));
  XMLScanner.Memo := Memo;
  try
    XMLScanner.Start;
    XMLScanner.Stop;
  finally
    XMLScanner.Free;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Close;
end;

{ TMyXMLScanner }

procedure TMyXMLScanner.ReadAttributes(const Text: string);
begin
  inherited;
  if Text <> '' then
    Memo.Lines.Add('Attributes: "' + Text + '"');
end;

procedure TMyXMLScanner.ReadCDATA(const Text: string);
begin
  inherited;
  Memo.Lines.Add('CDATA: "' + Text + '"');
end;

procedure TMyXMLScanner.ReadCloseTag(const Name: string);
begin
  inherited;
  Memo.Lines.Add('Close: "' + Name + '"');
end;

procedure TMyXMLScanner.ReadComment(const Text: string);
begin
  inherited;
  Memo.Lines.Add('Comment: "' + Text + '"');
end;

procedure TMyXMLScanner.ReadOpenTag(const Name: string);
begin
  inherited;
  Memo.Lines.Add('Open: "' + Name + '"');
end;

procedure TMyXMLScanner.ReadText(const Text: string);
begin
  inherited;
  Memo.Lines.Add('Text: "' + Text + '"');
end;

end.

