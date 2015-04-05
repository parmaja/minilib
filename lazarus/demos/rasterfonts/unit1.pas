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
    Button3: TButton;
    Button4: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
  Panel1.Canvas.Clear;
  RasterFont.PrintText(Panel1.Canvas.Handle, 1, 1, 'Hello World');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Panel1.Canvas.Clear;
  Panel1.Canvas.Font.Name := 'Courier';
  Panel1.Canvas.Font.Size := 10;
  Panel1.Canvas.TextRect(Panel1.ClientRect, 1, 1, 'Hello World');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  RasterFont.Generate();
  RasterFont.CharStart := 32;
  RasterFont.CharCount := 96;
  RasterFont.SaveToFile(Application.Location+'font.bmp');
  RasterFont.SaveInfoToFile(Application.Location+'font.ini');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Panel1.Canvas.Clear;
  Panel1.Canvas.CopyMode := cmMergeCopy;
  RasterFont.PrintText(Panel1.Canvas, 1, 1, 'Hello World');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RasterFont := TmnfRasterFont.Create;
  RasterFont.LoadFromFile(Application.Location+'font.bmp');
  RasterFont.LoadInfoFromFile(Application.Location+'font.ini');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  RasterFont.Free;
end;

end.

