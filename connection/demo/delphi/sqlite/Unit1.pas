unit Unit1;

interface

uses     
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, mncConnections, mncSQLite, mncSQL, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    Image1: TImage;
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
  Transaction: TmncSQliteTransaction;
  Cmd: TmncSQLiteCommand;
begin
  Conn := TmncSQLiteConnection.Create;
  try
    Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\data\cars.sqlite');
    Conn.Connect;
    Transaction := TmncSQliteTransaction.Create(Conn);
    try
      Transaction.Start;
      Cmd :=  TmncSQLiteCommand.CreateBy(Transaction);
      try
        
        Cmd.SQL.Text := 'insert into companies';
        Cmd.SQL.Add('(id, name, nationality)');
        Cmd.SQL.Add('values (?id, ?name, ?nationality)');
        Cmd.Prepare;
        {Cmd.Params['id'] := Null;
        Cmd.Params['name'] := 'าวๅั';
        Cmd.Params['nationality'] := 22;
        Cmd.Execute;

        Cmd.Params['id'] := Null;
        Cmd.Params['name'] := 'Hamed2';
        Cmd.Params['nationality'] := 222;
        Cmd.Execute;
        Cmd.Close;
        Transaction.Commit;
        Cmd.SQL.Text := 'select * from companies';
        Cmd.SQL.Add('where name = ?name');
        Cmd.Prepare;
        Cmd.Param['name'].AsString := 'zaher';}
        Cmd.Execute;
      finally
        Cmd.Free;
      end;
    finally
      Transaction.Free;
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
  s: TStringStream;
  im: string;
begin
  ListBox1.Items.Clear;
  Conn := TmncSQLiteConnection.Create;
  try
    Conn.Resource := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\data\cars.sqlite');
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
