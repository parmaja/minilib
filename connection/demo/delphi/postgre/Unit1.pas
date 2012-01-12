unit Unit1;

interface

uses     
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mncConnections, mncPostgre, mncSQL;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    PassEdit: TEdit;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Conn: TmncPGConnection;
  Session: TmncPGSession;
  Cmd: TmncPGCommand;
begin
  Conn := TmncPGConnection.Create;
  try
    Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\data\cars.PG');
    Conn.Connect;
    Session := TmncPGSession.Create(Conn);
    try
      Session.Start;
      Cmd := TmncPGCommand.Create(Session);
      try
        Cmd.SQL.Text := 'insert into companies';
        Cmd.SQL.Add('(id, name, nationality)');
        Cmd.SQL.Add('values (?id, ?name, ?nationality)');
        Cmd.Prepare;
        Cmd.Params['id'].AsVariant := Null;
        Cmd.Params['name'].AsString := 'าวๅั';
        Cmd.Params['nationality'].AsInteger := 22;
        Cmd.Execute;

        Session.Commit;
        Cmd.SQL.Text := 'select * from companies';
        Cmd.SQL.Add('where name = ?name');
        Cmd.Prepare;
        Cmd.Param['name'].AsString := 'zaher';
        if Cmd.Execute then
          ShowMessage(Cmd.Field['name'].AsString)
        else
          ShowMessage('not found');

      finally
//        Cmd.Free;
      end;
    finally
//      Session.Free;
    end;
  //  Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  Conn: TmncPGConnection;
  Session: TmncPGSession;
  Cmd: TmncPGCommand;
begin
  Conn := TmncPGConnection.Create;
  try
    Conn.Resource := 'pgtest';
    Conn.UserName := 'postgres';
    Conn.Password := 'masterkey';
    Conn.Connect;
    Session := TmncPGSession.Create(Conn);
    try
      Session.Start;
      Cmd := TmncPGCommand.Create(Session);
      Cmd.SQL.Text := 'select * from "Accounts"';
//      Cmd.SQL.Add('where name = ?name');
      //Cmd.Prepare;
//      Cmd.Param['name'].AsString := 'Ferrari';
      if Cmd.Execute then
      begin
        while not Cmd.EOF do
        begin
          ListBox1.Items.Add(Cmd.Field['AccID'].AsString + ' - ' + Cmd.Field['AccName'].AsString+ ' - ' + Cmd.Field['AccCode'].AsString);
          Cmd.Next;
        end;
      end;
      Cmd.Close;
    finally
      Session.Free;
    end;
    Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;

end.
