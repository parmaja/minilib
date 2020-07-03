unit Unit1;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  mnXMLStreams, mnXML, mnXMLWriter, mnXMLReader, mnXMLScanner, mnXMLUtils, Dialogs, StdCtrls,
  mnXMLRttiWriter, mnXMLRttiReader, mnXMLRttiProfile, ExtCtrls, Grids, DBGrids, Menus, LResources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
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
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  { TSetting }

  TSetting = class(TmnXMLProfile)
  private
    FHost: string;
    FOffline: Boolean;
    FPort: string;
  public
    constructor Create;
  published
    property Host:string read FHost write FHost;
    property Port:string read FPort write FPort;
    property Offline:Boolean read FOffline write FOffline default True;
  end;

var
  Form1: TForm1;

implementation

uses mnXMLNodes;

{$r *.lfm}

{ TSetting }

constructor TSetting.Create;
begin
  inherited Create;
  FOffline := True;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  XMLWriter: TmnXMLRttiWriter;
begin
  XMLWriter := TmnXMLRttiWriter.Create(TmnWrapperStream.Create(TFileStream.Create(ExtractFilePath(Application.ExeName) + '1.xml', fmCreate)));
  try
    XMLWriter.Smart := True;
    XMLWriter.WriteTypes := False;
    XMLWriter.WriteRoot(Self);
    XMLWriter.Stop;
  finally
    XMLWriter.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  XMLReader: TmnXMLRttiReader;
begin
  XMLReader := TmnXMLRttiReader.Create(ExtractFilePath(Application.ExeName) + '1.xml');
  try
    XMLReader.ReadRoot(Memo1);
  finally
    XMLReader.Free;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  lSetting:TSetting;
begin
  lSetting:=TSetting.Create;
  lSetting.Offline := CheckBox1.Checked;
  lSetting.SaveToFile(ExtractFilePath(Application.ExeName) + '1.xml');
  lSetting.Free;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  lSetting:TSetting;
begin
  lSetting:=TSetting.Create;
  lSetting.LoadFromFile(ExtractFilePath(Application.ExeName) + '1.xml');
  CheckBox1.Checked := lSetting.Offline;
  lSetting.Free;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  beep;
end;

end.

