unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, mncPostgre, mncPGHeader, fpPDF, mncSQL;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public
    aDB : TmncPGConnection;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var

  aTR: TmncPGTransaction;
  aCmd: TmncPGCommand;
begin
  aDB := TmncPGConnection.Create;
  try
    aDB.Host := '192.168.57.230';
    aDB.Host := '192.168.57.110';
    aDB.Host := '127.0.0.1';

    aDB.Resource := 'postgres';
    aDB.UserName := 'postgres';
    aDB.Password := 'syspwd';

    aDB.Connect;

    //LogWrite(aDB.GetVersion);

    aTR := aDB.CreateSession as TmncPGTransaction;
    try
      aTR.Start;
      aCmd := aTR.CreateCommand as TmncPGCommand;
      try
        //aCmd.SQL.Text := 'select * from "Materials" where "MatID" = 11';
        aCmd.SQL.Add('select left(md5(i::text), 10), md5(random()::text), md5(random()::text), md5(random()::text), md5(random()::text), left(md5(random()::text), 4)');
        aCmd.SQL.Add('from generate_series(1, 1000000) s(i)');

        aCmd.Execute;
        Button2.Caption := 'ClickMe';
      finally
        aCmd.Free;
        Memo1.Lines.Add('Done');
      end;
    finally
      aTR.Free;
    end;

  finally
    aDB.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowMessage('Ok');
end;

end.

