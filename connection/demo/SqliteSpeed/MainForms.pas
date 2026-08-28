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
    DeleteFileChk: TCheckBox;
    Label4: TLabel;
    ResultLabel: TLabel;
    SynchronousCbo: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    TempStoreCbo: TComboBox;
    Label1: TLabel;
    JournalModeCbo: TComboBox;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    FConn:TmncSQLiteConnection;
    FTrans: TmncSQliteTransaction;
    procedure Open;
    procedure Close;
    procedure Start;
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  cMax = 1000;

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
  FConn := TmncSQLiteConnection.Create;
  FConn.AutoCreate := True;
  f := Application.Location + 'data.sqlite';
  if DeleteFileChk.Checked then
    DeleteFile(f);
  FConn.Resource := f;
  nc := not FileExists(f);
  FConn.Exclusive := ExclusiveChk.Checked;
  FConn.JournalMode := TmncJournalMode(JournalModeCbo.ItemIndex);
  FConn.TempStore := TmncTempStore(TempStoreCbo.ItemIndex);
  FConn.Synchronous := TmncSynchronous(SynchronousCbo.ItemIndex);
  FConn.Connect;
  FTrans := TmncSQliteTransaction.Create(FConn);
  FTrans.Start;
  if nc then
  begin
    cmd :=TmncSQLiteCommand.Create;
    cmd.Transaction := FTrans;
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
  FTrans.Commit;
end;

procedure TMainForm.Close;
begin
  FreeAndNil(FTrans);
  FreeAndNil(FConn);
end;

procedure TMainForm.Start;
var
  i: Integer;
  s: string;
  c: Cardinal;
  cmd: TmncSQLiteCommand;
  aCommit: Boolean;
begin
  aCommit := CommitChk.Checked;
  Screen.Cursor := crHourGlass;
  try
    Open;
    cmd :=TmncSQLiteCommand.Create;
    try
      cmd.Transaction := FTrans;
      c := GetTickCount;
      FTrans.Start;
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
        FTrans.Commit;
      c := GetTickCount - c;
      ResultLabel.Caption := IntToStr(c);
      if not aCommit then
        FTrans.Commit;
    finally
      FreeAndNil(cmd);
      Close;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
