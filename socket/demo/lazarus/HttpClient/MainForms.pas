unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mnModules, mnLogs, mnHttpClient;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    LogEdit: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
  protected
    procedure LogEvent(S: String);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
const
  //sUserAgent = 'Embarcadero URI Client/1.0';
  sUserAgent = 'Mozilla/5.0';
  //'http://a.tile.openstreetmap.org/18/157418/105125.png' /crc error
  //sURL = 'http://c.tile.openstreetmap.org/18/157418/105127.png';
  //sURL = 'http://mt0.google.com/vt/lyrs=m@999&hl=ar&x=78707&y=52561&z=17&s=Gal';
  sURL = 'http://www.parmaja.org/wp/wp-content/uploads/2015/07/logo-site.png';
  //sURL = 'https://www.parmaja.org/wp/wp-content/uploads/2019/08/zaher-new-desktop-768x1024.jpg';
  //sURL = 'http://placehold.it/120x120&text=image1';
var
  HttpClient: TmnHttpClient;
  MemoryStream: TMemoryStream;
begin
  LogEdit.Lines.Add('Getting from URL');
  HttpClient := TmnHttpClient.Create;
  MemoryStream := TMemoryStream.Create;
  try
    HttpClient.Request.UserAgent := sUserAgent;
    HttpClient.GetMemoryStream(sURL, MemoryStream);
    LogEdit.Lines.Add(HttpClient.Response.ContentType);
    if SameText(HttpClient.Response.ContentType, 'image/jpeg') then
    begin
      MemoryStream.SaveToFile(Application.Location + 'file.jpeg');
      Image1.Picture.LoadFromStream(MemoryStream);
    end
    else
    if SameText(HttpClient.Response.ContentType, 'image/png') then
    begin
      MemoryStream.SaveToFile(Application.Location + 'file.png');
      Image1.Picture.LoadFromStream(MemoryStream);
    end
    else //if SameText(HttpClient.Response.ContentType, 'text/html;charset=utf-8') then
    begin
      MemoryStream.SaveToFile(Application.Location + 'file.txt');
      LogEdit.Lines.Append(StrPas(MemoryStream.Memory));
    end;
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
  LogEdit.Lines.Add('Finished');
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

