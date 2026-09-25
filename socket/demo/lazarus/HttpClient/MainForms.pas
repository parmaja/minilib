unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, mnLogs, mnHttpClient, mnModules;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    GetFileSizeBtn1: TButton;
    GetGetBtn: TButton;
    GetGetBtn1: TButton;
    GetFileSizeBtn: TButton;
    Image1: TImage;
    Image2: TImage;
    LogEdit: TMemo;
    ResultEdit: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure GetFileSizeBtn1Click(Sender: TObject);
    procedure GetFileSizeBtnClick(Sender: TObject);
    procedure GetGetBtn1Click(Sender: TObject);
    procedure GetGetBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  protected
    procedure LogEvent(const S: String);
    procedure LoadFromStream(ContentType: string; MemoryStream: TMemoryStream; Index: Integer = 0);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.LoadFromStream(ContentType: string; MemoryStream: TMemoryStream; Index: Integer = 0);
begin
  LogEdit.Lines.Add(ContentType);
  if SameText(ContentType, 'image/jpeg') then
  begin
    MemoryStream.SaveToFile(Application.Location + 'file.jpeg');
    if Index = 0 then
      Image1.Picture.LoadFromStream(MemoryStream)
    else
      Image2.Picture.LoadFromStream(MemoryStream)
  end
  else
  if SameText(ContentType, 'image/png') then
  begin
    MemoryStream.SaveToFile(Application.Location + 'file.png');
    if Index = 0 then
      Image1.Picture.LoadFromStream(MemoryStream)
    else
      Image2.Picture.LoadFromStream(MemoryStream)
  end
  else //if SameText(HttpClient.Response.ContentType, 'text/html;charset=utf-8') then
  begin
    MemoryStream.SaveToFile(Application.Location + 'file.txt');
    ResultEdit.Lines.Append(StrPas(MemoryStream.Memory));
  end;
end;

{ TMainForm }

const
  //sMyUserAgent = 'Embarcadero URI Client/1.0';
  sMyUserAgent = 'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:56.0) Gecko/20100101 Firefox/56.0';
  //httpbin.org serves plain http images with Content-Length and keep-alive,
  //so the whole demo works without an https connection (no OpenSSL needed)
  sURL = 'http://httpbin.org/image/png';      //first image (keep-alive demo)
  sURL2 = 'http://httpbin.org/image/jpeg';    //second image / file size demo
  sOSM_URL2 = 'https://tile.openstreetmap.org/6/38/25.png';    //second image / file size demo
  sPATH2 = '/image/jpeg';                     //second path on the same connection
  sURL3 = 'http://example.com/';              //simple page

procedure TMainForm.Button1Click(Sender: TObject);
var
  HttpClient: TmnHttpClient;
  MemoryStream: TMemoryStream;
begin
  LogEdit.Lines.Add('Getting from URL ' + sURL);
  MemoryStream := TMemoryStream.Create;
  HttpClient := TmnHttpClient.Create;
  try
    HttpClient.Request.UserAgent := sMyUserAgent;
    HttpClient.GetMemoryStream(sURL, MemoryStream);
    LoadFromStream(HttpClient.Response.ContentType, MemoryStream);
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
  LogEdit.Lines.Add('Finished');
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  HttpClient: TmnHttpClient;
  MemoryStream: TMemoryStream;
begin
  LogEdit.Lines.Add('Getting from URL ' + sURL2);
  MemoryStream := TMemoryStream.Create;
  HttpClient := TmnHttpClient.Create;
  try
    HttpClient.Request.UserAgent := sMyUserAgent;
    HttpClient.Request.Use.Compressing := ovYes;
    //HttpClient.Request.UserAgent := 'blalbla';
    HttpClient.GetMemoryStream(sURL2, MemoryStream);
    LoadFromStream(HttpClient.Response.ContentType, MemoryStream);
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
  LogEdit.Lines.Add('Finished');
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  HttpClient: TmnHttpClient;
  MemoryStream: TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  HttpClient := TmnHttpClient.Create;
  try
    HttpClient.Request.UserAgent := sMyUserAgent;
    //Open is a "connect and keep it connected" call: Connect + send GET + receive the header
    HttpClient.Open(sURL3);
    LogEdit.Lines.Add(HttpClient.Response.ContentType);
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
  LogEdit.Lines.Add('Finished');
end;

procedure TMainForm.GetFileSizeBtn1Click(Sender: TObject);
var
  aSize: Longint;
begin
  if mnHttpClient.HttpGetFileSize(sURL2, aSize) then
    LogEdit.Lines.Add(IntToStr(aSize))
  else
    LogEdit.Lines.Add('Failed to get file size');
end;

procedure TMainForm.GetFileSizeBtnClick(Sender: TObject);
var
  HttpClient: TmnHttpClient;
  aSizeStr: string;
begin
  LogEdit.Lines.Add('Getting from URL');
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
  HttpClient := TmnHttpClient.Create;
  try
    HttpClient.Request.UserAgent := sMyUserAgent;
    //manual HEAD: connect, compose the request line, send it, read the response header
    HttpClient.Connect(sURL2);
    HttpClient.Request.Head := 'HEAD ' + HttpClient.Path + ' HTTP/1.1';
    HttpClient.Request.SendHeader;
    HttpClient.Response.ReceiveHeader(True);
    aSizeStr := HttpClient.Response.Header.Values['Content-Length'];
    LogEdit.Lines.Add(aSizeStr);
    HttpClient.Disconnect;
  finally
    HttpClient.Free;
  end;
  LogEdit.Lines.Add('Finished');
  Screen.Cursor := crDefault;
end;

procedure TMainForm.GetGetBtn1Click(Sender: TObject);
var
  HttpClient: TmnHttpClient;
  MemoryStream: TMemoryStream;
begin
  LogEdit.Lines.Add('Getting from URL');
  Screen.Cursor := crHourGlass;
  MemoryStream := TMemoryStream.Create;
  HttpClient := TmnHttpClient.Create;
  try
    HttpClient.Request.UserAgent := sUserAgent;
    //Open = connect + send GET + receive the response header, keep the connection open
    HttpClient.Open(sOSM_URL2);

    HttpClient.ReceiveMemoryStream(MemoryStream);
    MemoryStream.Position := 0;
    LoadFromStream(HttpClient.Response.ContentType, MemoryStream);
    HttpClient.Disconnect;
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
  LogEdit.Lines.Add('Finished');
  Screen.Cursor := crDefault;
end;

procedure TMainForm.GetGetBtnClick(Sender: TObject);
var
  HttpClient: TmnHttpClient;
  MemoryStream: TMemoryStream;
begin
  LogEdit.Lines.Add('Getting from URL');
  Screen.Cursor := crHourGlass;
  Image1.Picture.Clear;
  Image2.Picture.Clear;
  Application.ProcessMessages;
  MemoryStream := TMemoryStream.Create;
  HttpClient := TmnHttpClient.Create;
  try
    HttpClient.Request.UserAgent := sMyUserAgent;
    HttpClient.Request.Use.KeepAlive := ovYes;
    //first GET on the connection
    HttpClient.Open(sURL);
    HttpClient.ReceiveMemoryStream(MemoryStream);
    LoadFromStream(HttpClient.Response.ContentType, MemoryStream, 0);

    Application.ProcessMessages;

    //second GET reusing the same connection, just changing the path
    MemoryStream.Clear;
    HttpClient.Path := sPATH2;
    HttpClient.Get;
    HttpClient.ReceiveMemoryStream(MemoryStream);
    LoadFromStream(HttpClient.Response.ContentType, MemoryStream, 1);

    HttpClient.Disconnect;
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
  LogEdit.Lines.Add('Finished');
  Screen.Cursor := crDefault;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InstallEventLog(@LogEvent);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //mnLogs intentionally has no public uninstall for event logs
end;

procedure TMainForm.LogEvent(const S: String);
begin
  LogEdit.Lines.Add(S);
end;

end.
