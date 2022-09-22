unit Unit1;
{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses     
  SysUtils, Variants, Classes, Graphics, Controls, Forms, mncPostgre, mncSQLite,
  mysql50conn,
  Dialogs, StdCtrls, mncConnections, mncMySQL, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    CreateBtn: TButton;
    CreateBtn1: TButton;
    SelectBtn: TButton;
    Button2: TButton;
    Button3: TButton;
    ListBox1: TListBox;
    Image1: TImage;
    SelectDSBtn: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CreateBtn1Click(Sender: TObject);
    procedure CreateBtnClick(Sender: TObject);
    procedure SelectBtnClick(Sender: TObject);
    procedure SelectDSBtnClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Conn: TmncMySQLConnection;
  Transaction: TmncMySQLTransaction;
  Cmd: TmncMySQLCommand;
begin
  Conn := TmncMySQLConnection.Create;
  try
    Conn.Resource := 'test';
    Conn.UserName := 'test';
    Conn.Password := 'test'; //not a real password man!
    Conn.Connect;
    Transaction := TmncMySQLTransaction.Create(Conn);
    try
      Transaction.Start;
      Cmd :=  TmncMySQLCommand.Create(Transaction);
      try
        
        Cmd.SQL.Text := 'insert into companies';
        Cmd.SQL.Add('(id, name, nationality)');
        Cmd.SQL.Add('values (?id, ?name, ?nationality)');
        Cmd.Prepare;
        Cmd.Params['id'].AsInt64 := 1;
        Cmd.Params['name'].AsString := 'zaher';
        Cmd.Params['nationality'].AsString := 'syria';

        Cmd.Execute;

        Cmd.Params['id'].AsInt64 := 2;
        Cmd.Params['name'].AsString := 'dirkey';
        Cmd.Params['nationality'].AsString := 'damascus';

        Cmd.Execute;

      finally
        Cmd.Free;
      end;
    finally
      Transaction.Free;
    end;
  finally
    Conn.Free;
  end;
end;

procedure TForm1.CreateBtn1Click(Sender: TObject);
var
  Conn: TmncMySQLConnection;
  Transaction: TmncMySQLTransaction;
  Cmd: TmncMySQLCommand;
begin
  Conn := TmncMySQLConnection.Create;
  try
    Conn.Resource := 'test';
    Conn.UserName := 'test';
    Conn.Password := 'test'; //not a real password man!
    Conn.Connect;
    Conn.Execute('drop  database if exists test2');
    ShowMessage('Database dropped');
  finally
    Conn.Free;
  end;
end;

procedure TForm1.CreateBtnClick(Sender: TObject);
var
  Conn: TmncMySQLConnection;
  s: string;
begin
  Conn := TmncMySQLConnection.Create;
  try
    Conn.UserName := 'test';
    Conn.Password := 'test'; //not a real password man!
    Conn.Connect;
    //Conn.Execute('create database if not exists test2');
    Conn.SelectDatabase('test2');

    s := 'create table companies ('#13#10+
    '  id int not null auto_increment,'#13#10+
    '  name varchar(50) default null,'#13#10+
    '  nationality varchar(50) default null,'#13#10+
    '  start_on date,'#13#10+
    '  end_on timestamp,'#13#10+
    '  money decimal(10,2) default null,'#13#10+
    '  is_tested bit default null,'#13#10+
    '  primary key(id)'#13#10+
    ')'#13#10+
    'collate=''utf8_general_ci'''#13#10+
    'engine=innodb'#13#10+
    'auto_increment=3'#13#10;

    Conn.Execute(s);

    ShowMessage('Database Created');
  finally
    Conn.Free;
  end;
end;

procedure TForm1.SelectBtnClick(Sender: TObject);
var
  Conn: TmncMySQLConnection;
  Transaction: TmncMySQLTransaction;
  Cmd: TmncMySQLCommand;
  field: string;
begin
  ListBox1.Clear;
  Conn := TmncMySQLConnection.Create;
  try
    Conn.Resource := 'test';
    Conn.UserName := 'test';
    Conn.Password := 'test'; //not a real password man!
    Conn.Connect;
    Transaction := TmncMySQLTransaction.Create(Conn);
    Transaction.Start;
    try
      field := 'money';
      Cmd := Transaction.CreateCommand as TmncMySQLCommand;
      Cmd.SQL.Text := 'select '+field+' from companies';
      //Cmd.SQL.Add('where ID = ?ID');
      Cmd.Prepare;
      Cmd.NextOnExecute := True;
      //Cmd.Param['ID'].AsInt64 := 2;
      if Cmd.Execute then
      begin
        while not Cmd.EOF do
        begin
          //ListBox1.Items.Add(Cmd.Field['name'].Column.Name + ': ' + Cmd.Field['name'].AsString);
          ListBox1.Items.Add(Cmd.Field[field].Column.Name + ': ' + Cmd.Field[field].AsString);
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

procedure TForm1.SelectDSBtnClick(Sender: TObject);
var
  Conn: TmncMySQLConnection;
  Transaction1: TmncMySQLTransaction;
  Transaction2: TmncMySQLTransaction;
  Cmd1, Cmd2: TmncMySQLCommand;
  c: Currency;
begin
  ListBox1.Clear;
  Conn := TmncMySQLConnection.Create;
  try
    Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\data\cars.MySQL');
    Conn.Connect;
    Transaction1 := TmncMySQLTransaction.Create(Conn);
    Transaction1.Start;
    try
      Cmd1 := Transaction1.CreateCommand as TmncMySQLCommand;
      Cmd1.SQL.Text := 'select * from companies';
      Cmd1.SQL.Add('where name = ?name');
      Cmd1.Prepare;
      Cmd1.Param['name'].AsString := 'Ferrari';
      if Cmd1.Execute then
      begin
        while not Cmd1.EOF do
        begin
          ListBox1.Items.Add(Cmd1.Field['name'].AsString);
          Cmd1.Next;
        end;
      end;
      //Start Transaction2
      Transaction2 := TmncMySQLTransaction.Create(Conn);
  //    Transaction2.Start;
      try
        Cmd2 := Transaction2.CreateCommand as TmncMySQLCommand;
        Cmd2.SQL.Text := 'select * from companies';
        Cmd2.SQL.Add('where name = ?name');
           Cmd2.Prepare;
        Cmd2.Param['name'].AsString := 'Audi';
        if Cmd2.Execute then
        begin
          while not Cmd2.EOF do
          begin
            ListBox1.Items.Add(Cmd2.Field['name'].AsString);
            Cmd2.Next;
          end;
        end;
        Cmd2.Close;
      finally
//        Transaction2.Commit;
        Transaction2.Free;
      end;
      //End Transaction2
      Cmd1.Close;
    finally
      Transaction1.Commit;
      Transaction1.Free;
    end;
    Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Conn: TmncMySQLConnection;
  Transaction: TmncMySQLTransaction;
  Cmd: TmncMySQLCommand;
  s: TStringStream;
  im: string;
begin
  Conn := TmncMySQLConnection.Create;
  try
    Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\data\cars.MySQL');
    Conn.AutoStart := True;
    Conn.Connect;
    Transaction := TmncMySQLTransaction.Create(Conn);
    try
      Cmd := TmncMySQLCommand.CreateBy(Transaction);
      Cmd.SQL.Text := 'select * from companies';
//      Cmd.SQL.Add('where name = ?name');
      Cmd.Prepare;
//      Cmd.Param['name'].AsString := 'Ferrari';
      if Cmd.Execute then
      begin
        while not Cmd.EOF do
        begin
          ListBox1.Items.Add(Cmd.Field['id'].AsString + ' - ' + Cmd.Field['name'].AsString);
          im := Cmd.Field['image'].AsString;
          s := TStringStream.Create(im);
          try
            Image1.Picture.Bitmap.LoadFromStream(s);
          finally
            s.Free;
          end;
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

end.
