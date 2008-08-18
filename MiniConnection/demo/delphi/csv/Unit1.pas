unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, mncCSV, StdCtrls;

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
  Conn: TmncCSVConnection;
  Session: TmncCSVSession;
  Cmd: TmncCSVCommand;
  aStream: TStream;
begin
  Conn := TmncCSVConnection.Create;
  try
    Conn.Connect;
    Session := TmncCSVSession.Create(Conn);
    try
      aStream := TFileStream.Create('c:\test1.csv', fmCreate);
      try
        Cmd := TmncCSVCommand.Create(Session, aStream, csvmWrite);
        Cmd.Fields.Add('Name');
        Cmd.Fields.Add('Address');

        Cmd.Prepare;
        Cmd.Params['Name'] := 'Name1';
        Cmd.Params['Address'] := 'Address1';
        Cmd.Execute;
        Cmd.Params['Name'] := 'Name2';
        Cmd.Param['Address'].AsCurrency := 100;
        Cmd.Execute;
      finally
        aStream.Free;
      end;
    finally
      Session.Free;
    end;
    Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  Conn: TmncCSVConnection;
  Session: TmncCSVSession;
  Cmd: TmncCSVCommand;
  aStream: TStream;
begin
  ListBox1.Items.Clear;
  Conn := TmncCSVConnection.Create;
  try
    Conn.Connect;
    Session := TmncCSVSession.Create(Conn);
    try
      aStream := TFileStream.Create('c:\test1.csv', fmOpenRead);
      try
        Cmd := TmncCSVCommand.Create(Session, aStream, csvmRead);
        Cmd.Execute;
        while not Cmd.EOF do
        begin
          ListBox1.Items.Add(Cmd.Current['Address']);
          Cmd.Next;
        end;
      finally
        aStream.Free;
      end;
    finally
      Session.Free;
    end;
    Conn.Disconnect;
  finally
    Conn.Free;
  end;
end;

end.

