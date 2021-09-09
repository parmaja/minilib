unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mnLogs, mnHttpClient;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
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
    procedure GetFileSizeBtn1Click(Sender: TObject);
    procedure GetFileSizeBtnClick(Sender: TObject);
    procedure GetGetBtn1Click(Sender: TObject);
    procedure GetGetBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  protected
    procedure LogEvent(S: String);
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
  //sUserAgent = 'Embarcadero URI Client/1.0';
  sUserAgent = 'Mozilla/5.0';
  //'http://a.tile.openstreetmap.org/18/157418/105125.png' /crc error
  //sURL = 'http://c.tile.openstreetmap.org/18/157418/105127.png';
  sURLGoogle = 'http://mt0.google.com/vt/lyrs=m@999&hl=ar&x=78707&y=52561&z=17&s=Gal';
  sURL = 'http://www.parmaja.org/wp/wp-content/uploads/2015/07/logo-site.png';
  sURL2 = 'https://www.parmaja.org/wp/wp-content/uploads/2019/08/zaher-new-desktop-768x1024.jpg';
  sPATH2 = '/wp/wp-content/uploads/2019/08/zaher-new-desktop-768x1024.jpg';
  //sURL = 'http://placehold.it/120x120&text=image1';

procedure TMainForm.Button1Click(Sender: TObject);
var
  HttpClient: TmnHttpClient;
  MemoryStream: TMemoryStream;
begin
  LogEdit.Lines.Add('Getting from URL ' + sURLGoogle);
  MemoryStream := TMemoryStream.Create;
  HttpClient := TmnHttpClient.Create;
  try
    HttpClient.Request.UserAgent := sUserAgent;
    //HttpClient.Compressing := True;
    HttpClient.GetMemoryStream(sURLGoogle, MemoryStream);
    LoadFromStream(HttpClient.Response.ContentType, MemoryStream);
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
  LogEdit.Lines.Add('Finished');
end;

procedure TMainForm.GetFileSizeBtn1Click(Sender: TObject);
var
  aSize: Integer;
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
  Image1.Picture.Clear;
  Image2.Picture.Clear;
  Application.ProcessMessages;
  HttpClient := TmnHttpClient.Create;
  try
    HttpClient.Request.UserAgent := sUserAgent;
    //HttpClient.KeepAlive := True;
    HttpClient.Host := 'www.parmaja.org';
    HttpClient.Open(sURL2, False);
    HttpClient.Request.SendHead;
    HttpClient.Response.Receive;
    aSizeStr := HttpClient.Response.Header['Content-Length'];
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
    HttpClient.KeepAlive := True;
    //HttpClient.Connect('https://www.openstreetmap.org', False);
    HttpClient.Open('https://c.tile.openstreetmap.de/14/9765/6391.png', False);

    HttpClient.Request.SendGet;
    HttpClient.Response.Receive;
    HttpClient.ReceiveMemoryStream(MemoryStream);
    MemoryStream.Position := 0;
    MemoryStream.SaveToFile(Application.Location + '1.html');
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
    HttpClient.Request.UserAgent := sUserAgent;
    HttpClient.KeepAlive := True;
    HttpClient.Open(sURL, False);
    HttpClient.Host := 'www.parmaja.org';

    HttpClient.Request.SendGet;
    HttpClient.Response.Receive;
    HttpClient.ReceiveMemoryStream(MemoryStream);
    LoadFromStream(HttpClient.Response.ContentType, MemoryStream, 0);

    Application.ProcessMessages;

    HttpClient.Path := sPATH2;
    MemoryStream.Clear;

    HttpClient.Request.SendGet;
    HttpClient.Response.Receive;
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
  UninstallEventLog(@LogEvent);
end;

procedure TMainForm.LogEvent(S: String);
begin
  LogEdit.Lines.Add(S);
end;

end.

