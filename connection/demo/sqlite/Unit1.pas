unit Unit1;
{$codepage utf8}
interface

uses     
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mncConnections, mncSQLite, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    SelectBtn: TButton;
    Button2: TButton;
    Button3: TButton;
    ListBox1: TListBox;
    Image1: TImage;
    SelectDSBtn: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  Conn: TmncSQLiteConnection;
  Transaction: TmncSQliteTransaction;
  Cmd: TmncSQLiteCommand;
begin
  Conn := TmncSQLiteConnection.Create;
  try
    Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\data\cars.sqlite');
    Conn.Connect;
    Transaction := TmncSQliteTransaction.Create(Conn);
    try
      Transaction.Start;
      Cmd :=  TmncSQLiteCommand.Create;
      Cmd.Transaction := Transaction;
      try
        
        Cmd.SQL.Text := 'insert into companies';
        Cmd.SQL.Add('(id, name, nationality)');
        Cmd.SQL.Add('values (?id, ?name, ?nationality)');
        Cmd.Prepare;
        Cmd.Param['id'].Clear;
        Cmd.Param['name'].AsString := 'زاهر';
        Cmd.Param['nationality'].AsInteger := 22;
        if Cmd.Execute then
          ListBox1.Items.Add('Executed');

{        Cmd.Params['id'] := Null;
        Cmd.Params['name'] := 'Hamed2';
        Cmd.Params['nationality'] := 222;
        Cmd.Execute;
        Cmd.Close;
        Transaction.Commit;}


        {Cmd.SQL.Text := 'select * from companies';
        Cmd.SQL.Add('where name = ?name');
        Cmd.Prepare;
        Cmd.Param['name'].AsString := 'zaher'
        if Cmd.Execute then
          ShowMessage(Cmd.Field['name'].AsString)
        else
          ShowMessage('not found');}

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

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.SelectBtnClick(Sender: TObject);
var
  Conn: TmncSQLiteConnection;
  Transaction: TmncSQliteTransaction;
  Cmd: TmncSQLiteCommand;
begin
  ListBox1.Clear;
  Conn := TmncSQLiteConnection.Create;
  try
    Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\data\cars.sqlite');
    Conn.Connect;
    Transaction := TmncSQliteTransaction.Create(Conn);
    Transaction.Start;
    try
      ListBox1.Items.Add('## not Done ##');

      Cmd := Transaction.CreateCommand as TmncSQLiteCommand;
      try
        Cmd.SQL.Text := 'select * from companies';
        //Cmd.SQL.Add('where  = ?name');
           Cmd.Prepare;
        //Cmd.Param['name'].AsString := 'Ferrari';
        if Cmd.Execute then
        begin
          while not Cmd.Done do
          begin
            ListBox1.Items.Add(Cmd.Field['name'].AsString);
            Cmd.Next;
          end;
        end;
        Cmd.Close;
      finally
        FreeAndNil(Cmd);
      end;

      ListBox1.Items.Add('## Steps ##');

      Cmd := Transaction.CreateCommand as TmncSQLiteCommand;
      try
        Cmd.SQL.Text := 'select * from companies';
        //Cmd.SQL.Add('where  = ?name');
           Cmd.Prepare;
        //Cmd.Param['name'].AsString := 'Ferrari';
        while Cmd.Fetch do
        begin
          ListBox1.Items.Add(Cmd.Field['name'].AsString);
        end;
        Cmd.Close;
      finally
          FreeAndNil(Cmd);
      end;
    finally
    end;

    Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;

procedure TForm1.SelectDSBtnClick(Sender: TObject);
var
  Conn: TmncSQLiteConnection;
  Transaction1: TmncSQliteTransaction;
  Transaction2: TmncSQliteTransaction;
  Cmd1, Cmd2: TmncSQLiteCommand;
begin
  ListBox1.Clear;
  Conn := TmncSQLiteConnection.Create;
  try
    Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\data\cars.sqlite');
    Conn.Connect;
    Transaction1 := TmncSQliteTransaction.Create(Conn);
    Transaction1.Start;
    try
      Cmd1 := Transaction1.CreateCommand as TmncSQLiteCommand;
      Cmd1.SQL.Text := 'select * from companies';
      Cmd1.SQL.Add('where name = ?name');
      Cmd1.Prepare;
      Cmd1.Param['name'].AsString := 'Ferrari';
      if Cmd1.Execute then
      begin
        while not Cmd1.Done do
        begin
          ListBox1.Items.Add(Cmd1.Field['name'].AsString);
          Cmd1.Next;
        end;
      end;
      //Start Transaction2
      Transaction2 := TmncSQliteTransaction.Create(Conn);
  //    Transaction2.Start;
      try
        Cmd2 := Transaction2.CreateCommand as TmncSQLiteCommand;
        Cmd2.SQL.Text := 'select * from companies';
        Cmd2.SQL.Add('where name = ?name');
           Cmd2.Prepare;
        Cmd2.Param['name'].AsString := 'Audi';
        if Cmd2.Execute then
        begin
          while not Cmd2.Done do
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
  Conn: TmncSQLiteConnection;
  Transaction: TmncSQliteTransaction;
  Cmd: TmncSQLiteCommand;
  s: TStringStream;
  im: string;
begin
  Conn := TmncSQLiteConnection.Create;
  try
    Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\data\cars.sqlite');
    Conn.AutoStart := True;
    Conn.Connect;
    Transaction := TmncSQliteTransaction.Create(Conn);
    try
      Cmd := TmncSQLiteCommand.CreateBy(Transaction);
      Cmd.SQL.Text := 'select * from companies';
//      Cmd.SQL.Add('where name = ?name');
      Cmd.Prepare;
//      Cmd.Param['name'].AsString := 'Ferrari';
      if Cmd.Execute then
      begin
        while not Cmd.Done do
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
