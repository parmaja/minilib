unit Unit1;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  mnStreams,
  mnXML, mnXMLWriter, mnXMLReader, mnXMLScanner, mnXMLUtils, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    Memo: TMemo;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
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

uses
  mnXMLNodes;

{$R *.lfm}

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
  XMLWriter.WriteText('John & Smith <123>');
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

procedure TForm1.Button5Click(Sender: TObject);
var
  s:string;
  i: Integer;
  c: Integer;
  aNodes: TmnXMLNodes;
  Reader: TmnXMLNodeReader;
begin
  s := '<?xml version="1.0" encoding="iso-8859-1"?>'#10;
  s := s + '<response xmlns="urn:debugger_protocol_v1" xmlns:xdebug="http://xdebug.org/dbgp/xdebug" command="stack_get" transaction_id="8">';
  s := s + '<stack where="App-&gt;__construct" level="0" type="file" filename="file:///W:/web/sites/abrash.com/websale/fw/core/ui/app.php" lineno="200"></stack>';
  s := s + '<stack where="{main}" level="1" type="file" filename="file:///W:/web/sites/abrash.com/websale/index.php" lineno="8"></stack>';
  s := s + '</response>';

  aNodes := TmnXMLNodes.Create;
  Reader := TmnXMLNodeReader.Create;
  try
    Reader.Start;
    Reader.Nodes := aNodes;
    Reader.Parse(s);
  finally
    Reader.Free;
  end;
  c := aNodes.Root.Items.Count;
  aNodes.Free;
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
