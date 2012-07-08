unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, MsgBox, GUIMsgBox;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Msg.Cancel('Cancel heh!');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Msg.Ok('Do you want to save?');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Msg.Yes('Do you want to exit, are you sure to exit from this program!!!'#13'Please tell me'#13'Yes or No') then
    Msg.Show('Then click the X button at the corner');
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  s: string;
begin
  s := 'No name';
  if Msg.Input(s, 'Enter your name please') then
    Msg.Show('Hi '+ s);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Msg.ShowStatus(Self, 'Uploading to internet, please wait...');
  //uploading code
  Timer1.Enabled := True;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  r: integer;
begin
  r := Msg.Ask('Before Exit', ['Save', 'Discard', 'Dont close'], 2);
  Msg.Show(IntToStr(r));
  {r := Msg.Ask('Again Before Exit', [Choice('Save', msgcOK), Choice('Discard', msgcDiscard), Choice('Dont close', msgcCancel)], 2);
  Msg.Show(IntToStr(r));}
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  Msg.HideStatus(Self);
end;

end.
