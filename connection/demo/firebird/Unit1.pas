unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, mncFirebird, mncCSV, mncCSVExchanges,
  mncConnections, mncDS, memds;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.Panel1Click(Sender: TObject);
begin

end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Conn: TmncFBConnection;
  Session: TmncFBSession;
  Cmd: TmncFBCommand;
begin
  ListBox1.Clear;
  Conn := TmncFBConnection.Create;
  try
    Conn.Resource := 'EMPLOYEE';
    Conn.Host := 'localhost';
    Conn.UserName := 'sysdba';
    Conn.Password := 'masterkey';
    Conn.Connect;
    Session := TmncFBSession.Create(Conn);
    try
      Session.Start;
      Cmd := TmncFBCommand.CreateBy(Session);
      try
        Cmd.SQL.Text := 'select * from EMPLOYEE';
        Cmd.SQL.Add('where EMP_NO < ?NO');
        Cmd.Prepare;
        Cmd.Param['NO'].AsInteger := 10;
        if Cmd.Execute then
        begin
          while not Cmd.EOF do
          begin
            ListBox1.Items.Add(Cmd.Field['EMP_NO'].AsString + ' - ' + Cmd.Field['FIRST_NAME'].AsString);
            Cmd.Next;
          end;
        end;
      finally
        Cmd.Free;
      end;
    finally
      Session.Free;
    end;
    Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Conn: TmncFBConnection;
  Session: TmncFBSession;
  Cmd: TmncFBCommand;
begin
  ListBox1.Clear;
  Conn := TmncFBConnection.Create;
  try
    Conn.Resource := 'EMPLOYEE';
    Conn.Host := 'localhost';
    Conn.UserName := 'sysdba';
    Conn.Password := 'masterkey';
    Conn.Connect;
    Session := TmncFBSession.Create(Conn);
    try
      Session.Start;
      Cmd := TmncFBCommand.CreateBy(Session);
      try
        Cmd.SQL.Text := 'select * from EMPLOYEE';
        Cmd.Prepare;
        if Cmd.Execute then
        begin
          while not Cmd.EOF do
          begin
            ListBox1.Items.Add(Cmd.Field['EMP_NO'].AsString + ' - ' + Cmd.Field['FIRST_NAME'].AsString);
            Cmd.Next;
          end;
        end;
      finally
        Cmd.Free;
      end;
    finally
      Session.Free;
    end;
    Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Conn: TmncFBConnection;
  Session: TmncFBSession;
  Cmd: TmncFBCommand;
begin
  Conn := TmncFBConnection.Create;
  try
    Conn.Resource := 'EMPLOYEE';
    Conn.Host := 'localhost';
    Conn.UserName := 'sysdba';
    Conn.Password := 'masterkey';
    Conn.Connect;
    Session := TmncFBSession.Create(Conn);
    try
      Cmd := TmncFBCommand.CreateBy(Session);
      Cmd.SQL.Text := 'select * from EMPLOYEE';
//      Cmd.SQL.Add('where EMPLOYEE = ?EMPLOYEE');
      Cmd.Prepare;
//      Cmd.Param['EMPLOYEE'].AsString := 'Robert';
      if Cmd.Execute then
      begin
        while not Cmd.EOF do
        begin
          ListBox1.Items.Add(Cmd.Field['EMP_NO'].AsString + ' - ' + Cmd.Field['FIRST_NAME'].AsString);
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

procedure TForm1.Button1Click(Sender: TObject);
var
  Conn: TmncFBConnection;
  Session: TmncFBSession;
  Cmd: TmncFBCommand;
begin
  Conn := TmncFBConnection.Create;
  try
    Conn.Resource := 'EMPLOYEE';
    Conn.Host := 'localhost';
    Conn.UserName := 'sysdba';
    Conn.Password := 'masterkey';
    Conn.Connect;
    Session := TmncFBSession.Create(Conn);
    try
      Session.Start;
      Cmd := TmncFBCommand.CreateBy(Session);
      try
        Cmd.SQL.Text := 'insert into categories';
        Cmd.SQL.Add('(CatID, CatName)');
        Cmd.SQL.Add('values (?CatID, ?CatName)');
        Cmd.Prepare;

        Cmd.Params['CatID'].AsInteger := 10;
        Cmd.Params['CatName'].AsString := 'ورد';
        Cmd.Execute;

        Cmd.Params['CatID'].AsInteger := 11;
        Cmd.Params['CatName'].AsString := 'Hamed2';
        Cmd.Execute;
        Session.Commit;
        Cmd.Close;

        Session.Start;
        Cmd.SQL.Text := 'select * from categories';
        Cmd.SQL.Add('where CatID = ?CatID');
        Cmd.Prepare;
        Cmd.Param['CatID'].AsString := '2';
        if Cmd.Execute then
          ShowMessage(Cmd.Field['CatName'].AsString)
        else
          ShowMessage('not found');
      finally
        Cmd.Free;
      end;
    finally
      Session.Free;
    end;
    Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;

initialization
  {$I unit1.lrs}
end.

