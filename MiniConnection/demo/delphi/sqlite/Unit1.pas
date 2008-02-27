unit Unit1;

interface

uses     
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mncConnections, mncSQLite, mncSQL;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
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
  Conn: TmncSQLiteConnection;
  Session: TmncSQLiteSession;
  Cmd: TmncSQLiteCommand;
begin
  Conn := TmncSQLiteConnection.Create;
  try
    Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\data\cars.sqlite');
    Conn.Connect;
    Session := TmncSQLiteSession.Create(Conn);
    try
      Session.Mode := smTransactions;
      Session.Start;
      Cmd := TmncSQLiteCommand.Create(Session);
      try
        Cmd.SQL.Text := 'insert into companies';
        Cmd.SQL.Add('(id, name, nationality)');
        Cmd.SQL.Add('values (?id, ?name, ?nationality)');
        Cmd.Prepare;
        Cmd.Params['id'] := Null;
        Cmd.Params['name'] := 'าวๅั';
        Cmd.Params['nationality'] := 22;
        Cmd.Execute;

        Cmd.Params['id'] := Null;
        Cmd.Params['name'] := 'Hamed2';
        Cmd.Params['nationality'] := 222;
        Cmd.Execute;
        Cmd.Close;
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
  Conn: TmncSQLiteConnection;
  Session: TmncSQLiteSession;
  Cmd: TmncSQLiteCommand;
begin
  Conn := TmncSQLiteConnection.Create;
  try
    Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\data\cars.sqlite');
    Conn.Connect;
    Session := TmncSQLiteSession.Create(Conn);
    try
      Cmd := TmncSQLiteCommand.Create(Session);
      Cmd.SQL.Text := 'select * from companies';
//      Cmd.SQL.Add('where name = ?name');
      Cmd.Prepare;
//      Cmd.Param['name'].AsString := 'Ferrari';
      if Cmd.Execute then
      begin
        while not Cmd.EOF do
        begin
          ListBox1.Items.Add(Cmd.Field['id'].AsString + ' - ' + Cmd.Field['name'].AsString);
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
