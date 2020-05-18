unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mnHttpClient, mnSockets, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP;

type
  TForm4 = class(TForm)
    DocList: TMemo;
    Button2: TButton;
    HeaderList: TMemo;
    HostEdit: TEdit;
    IdHTTP1: TIdHTTP;
    Button1: TButton;
    Button3: TButton;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    HttpStream: TmnCustomHttpStream;
  end;

var
  Form4: TForm4;

implementation

uses mnClients;

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
var
  Http: TmnCustomHttpStream;
  I: Integer;
  s: string;
  st: TMemoryStream;
begin
  DocList.Clear;
  Http := TmnCustomHttpStream.Create('', '');
  st:= TMemoryStream.Create;
  try
    Http.Get(HostEdit.Text);
    for I := 0 to Http.Response.Headers.Count - 1 do
      HeaderList.Lines.Add(Http.Response.Headers[I]);
    st.LoadFromStream(http);
    st.SaveToFile('c:\1.txt');
    //DocList.Lines.LoadFromStream(http);
    //DocList.Lines.SaveToFile('c:\1.png');
    {s := Http.ReadLn;
    while Http.Connected and (s <> '') do
    begin
      DocList.Lines.Add(s);
      s := Http.ReadLn;
    end;
    DocList.Lines.SaveToFile('c:\1.png');}
  finally
    Http.Free;
    st.Free;
  end;
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  Http: TmnHttpClient;
  I: Integer;
  s: string;
  st: TMemoryStream;
begin
  st := TMemoryStream.Create;
  try
    //IdHTTP1.Get(HostEdit.Text, st);
    st.SaveToFile('c:\1.txt');
  finally
    st.Free;
  end;

  DocList.Clear;
  Http := TmnHttpClient.Create;
  try
    Http.Get(HostEdit.Text);
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

procedure TForm4.Button3Click(Sender: TObject);
var
  I: Integer;
  s: string;
  st: TMemoryStream;
begin
  DocList.Clear;
  st:= TMemoryStream.Create;
  try
    HttpStream.Get(HostEdit.Text);
    for I := 0 to HttpStream.Response.Headers.Count - 1 do
      HeaderList.Lines.Add(HttpStream.Response.Headers[I]);
    st.LoadFromStream(HttpStream);
    st.SaveToFile('c:\1.txt');
    //DocList.Lines.LoadFromStream(http);
    //DocList.Lines.SaveToFile('c:\1.png');
    {s := Http.ReadLn;
    while Http.Connected and (s <> '') do
    begin
      DocList.Lines.Add(s);
      s := Http.ReadLn;
    end;
    DocList.Lines.SaveToFile('c:\1.png');}
  finally
    st.Free;
  end;
end;

procedure TForm4.FormCreate(Sender: TObject);
var
  u: TmnHttpUrl;
begin
  u.Create('http://www.google.com:8080/index.php?test=1&dcghdh=3');
  u.Create('www.google.com/index.php?test=1&dcghdh=3');
  u.Create('https://google.com/index.php?test=1&dcghdh=3');
  HostEdit.Text := 'http://maps1.yimg.com/hx/tl?b=1&v=4.3&.intl=en&x=245&y=47&z=10&r=1';
  HttpStream := TmnCustomHttpStream.Create('', '');
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  HttpStream.Free;
end;

end.
