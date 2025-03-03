unit Unit1;

interface

uses     
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mncSQLite, mncSQL, mncSQLUtils;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    Button3: TButton;
    Button4: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
  Transaction: TmncSQliteTransaction;
  Cmd: TmncSQLiteCommand;
begin
  Conn := TmncSQLiteConnection.Create;
  try
    //Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\data\cars.sqlite');
    Conn.Resource := 'W:\work\delphi\jobs\tacbord_sql\source\mpos\pda\data\data.sqlite';
    Conn.Connect;
    Transaction := TmncSQliteTransaction.Create(Conn);
    try
      Cmd := TmncSQLiteCommand.CreateBy(Transaction);
      try
        Cmd.SQL.Text := 'insert into companies';
        Cmd.SQL.Add('(id, name, nationality)');
        Cmd.SQL.Add('values (?id, ?name, ?nationality)');
        Cmd.Prepare;
        Cmd.Params['id'].IsNull := Null;
        Cmd.Params['name'].AsString := 'zaher';
        Cmd.Params['nationality'].AsInteger := 22;
        Cmd.Execute;

        Cmd.Close;
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
//      Transaction.Free;
    end;
  //  Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  Conn: TmncSQLiteConnection;
  Transaction: TmncSQliteTransaction;
  Cmd: TmncSQLiteCommand;
  c: Currency;
begin
  ListBox1.Clear;
  Conn := TmncSQLiteConnection.Create;
  try
///    Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\data\cars.sqlite');
    Conn.Resource := 'W:\work\delphi\jobs\tacbord_sql\source\mpos\pda\data\data.sqlite';
    Conn.Connect;
    Transaction := TmncSQliteTransaction.Create(Conn);
    try
      Cmd := TmncSQLiteCommand.CreateBy(Transaction);
      Cmd.SQL.Text := 'select * from "posMaterials"';
//      Cmd.SQL.Add('where  = ?name');
      Cmd.Prepare;
//      Cmd.Param['name'].AsString := 'Ferrari';
      if Cmd.Execute then
      begin
        while not Cmd.Done do
        begin
          c := Cmd.Field['MatPrice'].AsCurrency;
          ListBox1.Items.Add(Cmd.Field['MatName'].AsString + ' - ' + CurrToStr(c));
          Cmd.Next;
        end;
      end;
      Cmd.Close;
    finally
      Transaction.Free;
    end;
    Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Conn: TmncSQLiteConnection;
  Transaction: TmncSQliteTransaction;
  Cmd: TmncSQLiteCommand;
  SQL: TSQLObject<TSQLiteMode>;
begin
  Conn := TmncSQLiteConnection.Create;
  try
    //Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\data\cars.sqlite');
    Conn.Resource := 'W:\work\delphi\jobs\tacbord_sql\source\mpos\pda\data\data.sqlite';
    Conn.Connect;
    Transaction := TmncSQliteTransaction.Create(Conn);
    try
      Cmd := TmncSQLiteCommand.CreateBy(Transaction);
      Cmd.SQL.Text := SQL.ForInsert('posMaterials', ['MatCode', 'MatName']);
      try
        CMD.Param['MatCode'].AsString := '1001';
        CMD.Param['MatName'].AsString := '“«Â—';
        CMD.Execute;

        CMD.Param['MatCode'].AsString := '1002';
        CMD.Param['MatName'].AsString := '»·«·';
        CMD.Execute;
      finally
//        Cmd.Free;
      end;
    finally
//      Transaction.Free;
    end;
  //  Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Conn: TmncSQLiteConnection;
  Transaction: TmncSQliteTransaction;
  Cmd: TmncSQLiteCommand;
  CmdSelect: TmncSQLiteCommand;
  SQL: TSQLObject<TSQLiteMode>;
begin
  Conn := TmncSQLiteConnection.Create;
  try
    Conn.Resource := 'W:\work\delphi\jobs\tacbord_sql\source\mpos\pda\data\data.sqlite';
    Conn.Connect;
    Transaction := TmncSQliteTransaction.Create(Conn);
    try
      Cmd := TmncSQLiteCommand.CreateBy(Transaction);
      CmdSelect := TmncSQLiteCommand.CreateBy(Transaction);
      CmdSelect.SQL.Text := 'select * from "posMaterials"';
      Cmd.SQL.Text := SQL.ForUpdate('posMaterials', ['MatName'], ['MatCode']);
      try
        CmdSelect.Execute;
        if not CmdSelect.Done then
        begin
          CMD.Param['MatCode'].AsString := '1001';
          CMD.Param['MatName'].AsString := '“«Â— 2';
          CMD.Execute;

          CMD.Param['MatCode'].AsString := '1002';
          CMD.Param['MatName'].AsString := '»·«· 2';
          CMD.Execute;
        end;
      finally
        Cmd.Free;
        CmdSelect.Free;
      end;
//      Transaction.Commit;
    finally
      Transaction.Free;
    end;
    Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;

end.
