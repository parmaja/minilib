unit MainForms;
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
  Dialogs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, mnSockets,
  StdCtrls, mnHttpServer;

type
  TForm1 = class(TForm)
    Button1: TButton;
    mnHttpServer1: TmnHttpServer;
    IdTCPClient1: TIdTCPClient;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  aStream:TFileStream;
begin
  IdTCPClient1.Connect;
  IdTCPClient1.WriteLn('GET / HTTP/1.1');
  IdTCPClient1.WriteLn('Host: music');
//  graphics/newmenu.ps.jpeg
  IdTCPClient1.WriteLn('');
  aStream:=TFileStream.Create('c:\1.txt', fmCreate);
  IdTCPClient1.ReadStream(aStream);
  aStream.Free;
  IdTCPClient1.Disconnect;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  mnHttpServer1.Start;
end;

end.
