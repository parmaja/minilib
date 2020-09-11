unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mnLogs, mnModules, mnHttpClient;

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
  //sURL = 'https://picsum.photos/id/237/200/300';
  sURL = 'http://www.parmaja.org/wp/wp-content/uploads/2019/08/zaher-new-desktop-768x1024.jpg';
  //sURL = 'http://placehold.it/120x120&text=image1';
var
  HttpClient: TmnHttpClient;
  MemoryStream: TMemoryStream;
begin
  LogEdit.Lines.Add('Getting from URL');
  HttpClient := TmnHttpClient.Create;
  MemoryStream := TMemoryStream.Create;
  try
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

