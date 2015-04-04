unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, mnFonts;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    RasterFont: TmnfRasterFont;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  RasterFont.PrintText(Panel1.Canvas, 1, 1, 'Hello World');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Panel1.Canvas.Clear;
  Panel1.Canvas.Font.Name := 'Courier';
  Panel1.Canvas.Font.Size := 10;
  Panel1.Canvas.TextRect(Panel1.ClientRect, 1, 1, 'Hello World');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RasterFont := TmnfRasterFont.Create;
  RasterFont.Generate();
  RasterFont.SaveToFile(Application.Location+'font.bmp');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  RasterFont.Free;
end;

end.

