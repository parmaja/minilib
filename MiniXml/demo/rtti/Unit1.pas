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
  mnXMLStreams, mnXML, mnXMLWriter, mnXMLReader, mnXMLScanner, mnXMLUtils, Dialogs, StdCtrls,
  mnXMLRttiWriter, mnXMLRttiReader, ExtCtrls, Grids, DBGrids, Menus;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Image1: TImage;
    DBGrid1: TDBGrid;
    PopupMenu1: TPopupMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    PrintSetup1: TMenuItem;
    Print1: TMenuItem;
    N2: TMenuItem;
    SaveAs1: TMenuItem;
    Save1: TMenuItem;
    Open1: TMenuItem;
    New1: TMenuItem;
    Edit1: TMenuItem;
    Object1: TMenuItem;
    Links1: TMenuItem;
    N3: TMenuItem;
    GoTo1: TMenuItem;
    Replace1: TMenuItem;
    Find1: TMenuItem;
    N4: TMenuItem;
    PasteSpecial1: TMenuItem;
    Paste1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    N5: TMenuItem;
    Repeatcommand1: TMenuItem;
    Undo1: TMenuItem;
    PopupMenu2: TPopupMenu;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses mnXMLNodes;

{$R *.dfm}

procedure TForm1.Button4Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  XMLWriter: TmnXMLRttiWriter;
begin
  XMLWriter := TmnXMLRttiWriter.Create(TmnXMLStream.Create(TFileStream.Create('1.xml', fmCreate)));
  try
    XMLWriter.Smart := True;
    XMLWriter.WriteTypes := False;
    XMLWriter.WriteRoot(Memo1);
    XMLWriter.Stop;
  finally
    XMLWriter.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  XMLReader: TmnXMLRttiReader;
begin
  XMLReader := TmnXMLRttiReader.Create(TmnXMLStream.Create(TFileStream.Create('1.xml', fmOpenRead)));
  try
    XMLReader.ReadRoot(Memo1);
  finally
    XMLReader.Free;
  end;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  beep;
end;

end.

