unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mnHttpClient, mnSockets, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, Vcl.ExtCtrls, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg;

type
  TForm4 = class(TForm)
    Button2: TButton;
    HostEdit: TEdit;
    Panel1: TPanel;
    LogEdit: TMemo;
    Image1: TImage;
    Splitter1: TSplitter;
    procedure Button2Click(Sender: TObject);
  private
  public
    HttpStream: TmnHttpClient;
  end;

var
  Form4: TForm4;

implementation

uses mnClients;

{$R *.dfm}

procedure TForm4.Button2Click(Sender: TObject);
//const
  //sURL = 'https://picsum.photos/id/237/200/300';
  //sURL = 'http://www.parmaja.org/wp/wp-content/uploads/2015/07/logo-site.png';
  //sURL = 'https://www.parmaja.org/wp/wp-content/uploads/2019/08/zaher-new-desktop-768x1024.jpg';
  //sURL = 'http://placehold.it/120x120&text=image1';
  //sUrl = 'https://a.tile.openstreetmap.org/18/157418/105125.png';
var
  HttpClient: TmnHttpClient;
  MemoryStream: TMemoryStream;
  url: string;
  aFileName: string;
begin
  url := HostEdit.Text;
  LogEdit.Lines.Add('Downloading ' + url);
  HttpClient := TmnHttpClient.Create;
  HttpClient.Request.UserAgent := 'miniLib http Client/1.0';
  MemoryStream := TMemoryStream.Create;
  try
    try
      HttpClient.GetMemoryStream(url, MemoryStream);
      LogEdit.Lines.Add(HttpClient.Response.ContentType);
      aFileName := HttpClient.Response.DispositionFile;
      LogEdit.Lines.Add('FileName: ' + aFileName);
      if SameText(HttpClient.Response.ContentType, 'image/jpeg') then
      begin
        MemoryStream.SaveToFile('c:\temp\' + aFileName);
        Image1.Picture.LoadFromStream(MemoryStream);
      end
      else
      if SameText(HttpClient.Response.ContentType, 'image/png') then
      begin
        MemoryStream.SaveToFile('c:\temp\'  + aFileName);
        Image1.Picture.LoadFromStream(MemoryStream);
      end
      else
      begin
        MemoryStream.SaveToFile('c:\temp\' + 'file.tmp');
      end;
    except
      on E: Exception do
      begin
        LogEdit.Lines.Add(E.Message);
      end;
    end;
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
  LogEdit.Lines.Add('Finished');
end;

end.
