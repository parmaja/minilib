unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, mnStreams, mnCommStreams;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label3: TLabel;
    SpeedEdit: TEdit;
    StartBtn: TButton;
    PortEdit: TEdit;
    FileEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    StopBtn: TButton;
    procedure StartBtnClick(Sender: TObject);
    procedure SendTimerTimer(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    FCount: Integer;
    FStream : TmnStream;
    FCommStream: TmnCommStream;
    FStoped: Boolean;
    procedure SendLines;
  public

  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.SendTimerTimer(Sender: TObject);
begin
  SendLines;
end;

procedure TForm1.StopBtnClick(Sender: TObject);
begin
  FStoped := True;
end;

procedure TForm1.SendLines;
var
  aLine: string;
  i: Integer;
begin
  i := 0;
  try
    while not FStoped do
    begin
      FStream.ReadLn(aLine);
      FCommStream.WriteString(aLine+#13#10);
//      FCommStream.WriteString(IntToStr(FCount)+#13#10);
      Inc(FCount);
      Sleep(50);
      inc(i);
      if (i mod 5) = 0 then
      begin
        Application.ProcessMessages;
        i := 0;
      end;
    end;
  finally
    FStoped := False;
    FCommStream.Disconnect;
    FreeAndNil(FStream);
    FreeAndNil(FCommStream);
  end;
end;

procedure TForm1.StartBtnClick(Sender: TObject);
begin
  if FStream = nil then
  begin
    FCommStream := TmnCommStream.Create(True, PortEdit.Text, StrToInt(SpeedEdit.Text));
    FCommStream.WriteThrough := True;
    FCommStream.Connect;
    FStream := TmnStream.Create(TFileStream.Create(FileEdit.Text, fmOpenRead), True);
    FStream.EndOfLine := #13#10;
    SendLines;
  end;
end;

initialization
  {$I MainForms.lrs}

end.

