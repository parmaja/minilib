unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mnHttpClient, mnSockets, httpsend;

type
  TForm4 = class(TForm)
    DocList: TMemo;
    Button2: TButton;
    HeaderList: TMemo;
    HostEdit: TEdit;
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses mnClients;

{$R *.dfm}

procedure TForm4.Button2Click(Sender: TObject);
var
  Http: TmnHttpClient;
  I: Integer;
  s: string;
begin
  DocList.Clear;
  Http := TmnHttpClient.Create(nil);
  try
    Http.Request.Host := HostEdit.Text;
    Http.Request.UserAgent := 'Mozilla/4.0';
    Http.Get('/');
    for I := 0 to Http.Response.Headers.Count - 1 do
      HeaderList.Lines.Add(Http.Response.Headers[I]);
    s := Http.Response.ReadLn;
    while Http.Connected and (s <> '') do
    begin
      DocList.Lines.Add(s);
      s := Http.Response.ReadLn;
    end;
  finally
    Http.Free;
  end;
end;

end.
