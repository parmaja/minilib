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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  mnSockets, mnServers, mnClients, mnStreams, mnConnections, mnCommandServers;

type
  TForm1 = class(TForm)
    Command: TButton;
    Memo: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure CommandClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FStream:TmnClientStream;
    //mnCommandClient: TmnCommandClient;
    mnCommandServer: TmnCommandServer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.CommandClick(Sender: TObject);
var
  s:string;
begin
  FStream.WriteLn('GET');
  s:=FStream.ReadLn;
  if s='OK' then
    Memo.Lines.Add(s);
  FStream.WriteLn('SET');
  s:=FStream.ReadLn;
  if s='TOK' then
    Memo.Lines.Add(s);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FStream.Disconnect;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  mnCommandServer.Stop;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FStream:=TmnClientStream.Create;
  FStream.Port:='11011';
  FStream.Address:='zaher';
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FStream.Connect;
end;

end.
