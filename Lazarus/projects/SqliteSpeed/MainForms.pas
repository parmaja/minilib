unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  mncSqlite;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    ExclusiveChk: TCheckBox;
    CommitChk: TCheckBox;
    SynchronousCbo: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    TempStoreCbo: TComboBox;
    Label1: TLabel;
    LogEdit: TMemo;
    JournalModeCbo: TComboBox;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    FCONN:TmncSQLiteConnection;
    FSESS: TmncSQLiteSession;
    procedure Open;
    procedure Close;
    procedure Start;
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Start;
end;

procedure TMainForm.Open;
var
  f: string;
  nc: Boolean;
  cmd: TmncSQLiteCommand;
begin
  FCONN := TmncSQLiteConnection.Create;
  FCONN.AutoCreate := True;
  f := Application.Location + 'data.sqlite';
  DeleteFile(f);
  FCONN.Resource := f;
  nc := not FileExists(f);
  FCONN.Connect;
  FSESS := TmncSQLiteSession.Create(FCONN);
  FSESS.Exclusive := ExclusiveChk.Checked;
  FSESS.JournalMode := TmncJournalMode(JournalModeCbo.ItemIndex);
  FSESS.TempStore := TmncTempStore(TempStoreCbo.ItemIndex);
  FSESS.Synchronous := TmncSynchronous(SynchronousCbo.ItemIndex);
  FSESS.Start;
  if nc then
  begin
    cmd :=TmncSQLiteCommand.Create;
    cmd.Session := FSESS;
    try
      cmd.SQL.Text := 'create table Names (';
      cmd.SQL.Add('ID integer NOT NULL,');
      cmd.SQL.Add('Name varchar(60) NOT NULL,');
      cmd.SQL.Add('constraint Names Primary Key (ID)');
      cmd.SQL.Add(')');
      cmd.Execute;
    finally
      FreeAndNil(cmd);
    end;
  end;
  FSESS.Commit;
end;

procedure TMainForm.Close;
begin
  FreeAndNil(FSESS);
  FreeAndNil(FCONN);
end;

const
  cMax = 10000;

procedure TMainForm.Start;
var
  i: Integer;
  s: string;
  c: Cardinal;
  cmd: TmncSQLiteCommand;
  aCommit: Boolean;
begin
  aCommit := CommitChk.Checked;
  Open;
  cmd :=TmncSQLiteCommand.Create;
  try
    cmd.Session := FSESS;
    c := GetTickCount;
    FSESS.Start;
    cmd.SQL.Text := 'insert into Names';
    cmd.SQL.Add('(Name)');
    cmd.SQL.Add('values (?Name)');

    cmd.Prepare;
    s := FormatDateTime('yyyy-mm-dd,hh:nn:ss', Now);
    for i:=0 to cMax -1 do
    begin
      cmd.Param['Name'].AsString := s + IntToStr(i);
      cmd.Execute;
    end;
    if aCommit then
      FSESS.Commit;
    c := GetTickCount - c;
    LogEdit.Lines.Add('Insert Time: ' + IntToStr(c));
    if not aCommit then
      FSESS.Commit;
  finally
    FreeAndNil(cmd);
  end;
  Close;
end;

end.

