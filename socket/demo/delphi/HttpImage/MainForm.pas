unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mnHttpClient, mnSockets, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, Vcl.ExtCtrls, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg;

type
  TForm4 = class(TForm)
    Button2: TButton;
    HostEdit: TEdit;
    Button1: TButton;
    Button3: TButton;
    Image1: TImage;
    LogEdit: TMemo;
    procedure Button2Click(Sender: TObject);
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


procedure TForm4.Button2Click(Sender: TObject);
const
  //sURL = 'https://picsum.photos/id/237/200/300';
  //sURL = 'http://www.parmaja.org/wp/wp-content/uploads/2015/07/logo-site.png';
  //sURL = 'https://www.parmaja.org/wp/wp-content/uploads/2019/08/zaher-new-desktop-768x1024.jpg';
  //sURL = 'http://placehold.it/120x120&text=image1';
  sUrl = 'https://a.tile.openstreetmap.org/18/157418/105125.png';
var
  HttpClient: TmnHttpClient;
  MemoryStream: TMemoryStream;
begin
  LogEdit.Lines.Add('Getting from URL');
  HttpClient := TmnHttpClient.Create;
  HttpClient.Request.UserAgent := 'Embarcadero URI Client/1.0';
  MemoryStream := TMemoryStream.Create;
  try
    HttpClient.GetMemoryStream(sURL, MemoryStream);
    LogEdit.Lines.Add(HttpClient.Response.ContentType);
    if SameText(HttpClient.Response.ContentType, 'image/jpeg') then
    begin
      MemoryStream.SaveToFile('c:\temp\' + 'file.jpeg');
      Image1.Picture.LoadFromStream(MemoryStream);
    end
    else
    if SameText(HttpClient.Response.ContentType, 'image/png') then
    begin
      MemoryStream.SaveToFile('c:\temp\'  + 'file.png');
      Image1.Picture.LoadFromStream(MemoryStream);
    end
    else
    begin
      MemoryStream.SaveToFile('c:\temp\' + 'file.tmp');
    end;
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
  LogEdit.Lines.Add('Finished');
end;

end.
