unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IniFiles, Variants,
  SynEdit, SynHighlighterSQL,
  mnMsgBox, GUIMsgBox, ntvPanels, ntvBoard,
  mncDB, mncConnections, mncSQL, ParamsForms,
  mncSQLite, mncPostgre, mncMySQL, mncFirebird,
  mncORM, mncMySQLORM, mncSQLiteORM, mncPGORM, mncFBORM,
  appSchema;

type

  { TEngine }

  TEngine = class(TObject)
  public
    ORM: TmncORM;
    Connection: TmncSQLConnection;
    Transaction: TmncSQLTransaction;
    InitSQL: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure PostExample;
    procedure PostExamples;
  end;

  { TTestThread }

  TTestThread = class(TThread)
  protected
    Engine: TEngine;
    procedure Execute; override;
  public
  end;

  TUseSteps = (stepNo, stepNormal, stepShort);

  { TMainForm }

  TMainForm = class(TForm)
    AddRecordBtn: TButton;
    AddRecordBtn1: TButton;
    Button2: TButton;
    Button3: TButton;
    ConnectBtn1: TButton;
    LogEdit: TSynEdit;
    ntvPanel1: TntvPanel;
    ConnectBtn: TButton;
    CreateDB1Btn: TButton;
    EnginesCbo: TComboBox;
    HostEdit: TEdit;
    DataEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ntvPanel2: TntvPanel;
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    PasswordEdit: TEdit;
    SynEdit: TSynEdit;
    SynSQLSyn: TSynSQLSyn;
    TestThreadBtn: TButton;
    UserEdit: TEdit;
    procedure ReadRecordBtn1Click(Sender: TObject);
    procedure AddRecordBtnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ConnectBtn1Click(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure CreateDB1BtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TestThreadBtnClick(Sender: TObject);
  private
    procedure Log(const s: string);
    procedure Connect(CreateIt: Boolean);
    procedure ReadRecords(UseSteps: TUseSteps);
  public
    Engine: TEngine;
    TestThread: TTestThread;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TTestThread }

procedure TTestThread.Execute;
begin
  Engine.PostExamples;
end;

{ TEngine }

constructor TEngine.Create;
begin
  inherited Create;
  InitSQL := TStringList.Create;
end;

destructor TEngine.Destroy;
begin
  FreeAndNil(Transaction);
  FreeAndNil(Connection);
  FreeAndNil(InitSQL);
  FreeAndNil(ORM);
  inherited Destroy;
end;

procedure TEngine.PostExample;
var
  CMD: TmncSQLCommand;
begin
  CMD := Transaction.CreateCommand;
  try
    CMD.Options := CMD.Options + [cmoTruncate];
    CMD.SQL.Text := 'insert into Companies(ID, Name, Address) values(?ID, ?Name, ?Address)';

    CMD.Prepare;
    CMD.Param['ID'].Value := 10;
    CMD.Param['Name'].AsString := 'Test' + FormatDateTime('yyyy-mm-dd', Now);
    CMD.Param['Address'].AsString := '';
    CMD.Execute;
  finally
    CMD.Free;
  end;
end;

procedure TEngine.PostExamples;
var
  i: Integer;
begin
  for i := 0 to 10000 do
  begin
    PostExample;
  end;
end;

{ TMainForm }


procedure TMainForm.CreateDB1BtnClick(Sender: TObject);
begin
  if EnginesCbo.ItemIndex >= 0 then
  begin
    SynEdit.Clear;
    FreeAndNil(Engine);
    Engine := TEngine.Create;
    Engine.ORM := CreateORM((EnginesCbo.Items.Objects[EnginesCbo.ItemIndex] as TmncEngine).ORMClass);
    if Engine.ORM <> nil then
      Engine.ORM.GenerateSQL(SynEdit.Lines);
    FreeAndNil(Engine);
  end;
end;

procedure TMainForm.ConnectBtnClick(Sender: TObject);
begin
  Connect(True);
end;

procedure TMainForm.Connect(CreateIt: Boolean);
begin
  try
    FreeAndNil(Engine);
    Engine := TEngine.Create;
    Engine.ORM := CreateORM((EnginesCbo.Items.Objects[EnginesCbo.ItemIndex] as TmncEngine).ORMClass);
    if Engine.ORM <> nil then
      Engine.ORM.GenerateSQL(Engine.InitSQL);
    Engine.Connection := Engines.CreateConnection((EnginesCbo.Items.Objects[EnginesCbo.ItemIndex] as TmncEngine).Name) as TmncSQLConnection;
    if (ccPath in Engine.Connection.Capabilities) or (HostEdit.Text = '') then
      Engine.Connection.Resource := Application.Location + DataEdit.Text + Engine.Connection.GetExtension
    else
      Engine.Connection.Resource := DataEdit.Text;

    Engine.Connection.Host := HostEdit.Text;
    Engine.Connection.UserName := UserEdit.Text;
    Engine.Connection.Password := PasswordEdit.Text;
    if CreateIt then
    begin
      if (ccDrop in Engine.Connection.Capabilities) then
      begin
        Log(Engine.Connection.Resource + ' Droping');
        Engine.Connection.DropDatabase(True);
      end;
      if (ccCreate in Engine.Connection.Capabilities) then
      begin
        Log(Engine.Connection.Resource + ' Creating');
        Engine.Connection.CreateDatabase;
        Log(Engine.Connection.Resource + ' is Created');
      end;
    end;
    Engine.Connection.Connect;
    Log(Engine.Connection.Resource + ' is Connected');
    Engine.Transaction := Engine.Connection.CreateTransaction;
    Engine.Transaction.Start;
    if CreateIt then
      Engine.Transaction.ExecuteScript(Engine.InitSQL);
    Engine.Transaction.Commit(True);
  except
    on E: EXception do
    begin
      Log(E.Message);
      raise;
    end;
  end;
end;

procedure TMainForm.AddRecordBtnClick(Sender: TObject);
var
  CMD: TmncSQLCommand;
begin
  if Engine = nil then
  begin
    log('Not connected');
    exit;
  end;
  CMD := Engine.Transaction.CreateCommand;
  try
    CMD.Options := CMD.Options + [cmoTruncate];
    CMD.SQL.Text := 'insert into Companies(Name, Address) values(?Name, ?Address)';

    CMD.Prepare;
    CMD.Param['Name'].AsString := 'Test' + DateTimeToStr(Now);
    CMD.Param['Address'].AsString := 'On the Earth';
    CMD.Execute;
  finally
    CMD.Free;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  CMD: TmncSQLCommand;
  n: String;
begin
  if Engine = nil then
  begin
    log('Not connected');
    exit;
  end;

  if MsgBox.Input(n, 'Enter ID to delete') then
  begin
    CMD := Engine.Transaction.CreateCommand;
    try
      CMD.SQL.Text := 'delete from Companies where ID=?ID';
      CMD.Prepare;
      CMD.Param['ID'].Value := n.ToInteger;
      if CMD.Execute then
        Log('Deleted');
    finally
      CMD.Free;
    end;
  end;
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  CMD: TmncSQLCommand;
  s: string;
  i: Integer;
begin
  if Engine = nil then
  begin
    log('Not connected');
    exit;
  end;
  CMD := Engine.Transaction.CreateCommand;
  try
    CMD.SQL.Text := SynEdit.Text;
    CMD.Prepare;
    if ShowSQLParams(CMD) then
      if CMD.Execute then
      begin
        s := '';
        for i := 0 to CMD.Columns.Count - 1 do
        begin
          if s <> '' then
            s := s + #9;
          s := s + CMD.Columns[i].Name;
        end;
        Log(s);

        while not CMD.Done do
        begin
          s := '';
          for i := 0 to CMD.Columns.Count - 1 do
          begin
            if s <> '' then
              s := s + #9;
            s := s + VarToStr(CMD.Fields.Items[i].Value);
          end;
          Log(s);
          CMD.Next;
        end;
      end
      else
        Log('Nothing to read');
  finally
    CMD.Free;
  end;
end;

procedure TMainForm.ReadRecordBtn1Click(Sender: TObject);
begin
  if Engine = nil then
  begin
    log('Not connected');
    exit;
  end;
  Log('## no Steps');
  ReadRecords(stepNo);
  Log('## Step Normal');
  ReadRecords(stepNormal);
  Log('## Very Short steps');
  ReadRecords(stepShort);
end;

procedure TMainForm.ReadRecords(UseSteps: TUseSteps);
var
  CMD: TmncSQLCommand;
  procedure PrintHeader;
  var
    s: string;
    i: Integer;
  begin
    s := '';
    for i := 0 to CMD.Columns.Count - 1 do
    begin
      if s <> '' then
        s := s + #9;
      s := s + CMD.Columns[i].Name;
    end;
    Log(s);
  end;

  procedure PrintRecord;
  var
    s: string;
    i: Integer;
  begin
    s := '';
    for i := 0 to CMD.Columns.Count - 1 do
    begin
      if s <> '' then
        s := s + #9;
      s := s + VarToStr(CMD.Fields.Items[i].Value);
    end;
    Log(s);
  end;

var
  f: Boolean;
begin
  if Engine = nil then
  begin
    log('Not connected');
    exit;
  end;
  CMD := Engine.Transaction.CreateCommand;
  try
    CMD.SQL.Text := 'select * from Companies';
    //CMD.SQL.Text := 'select * from Companies where ID=?ID';
    //CMD.Param['ID'].Value := 10;

    //Cmd.SQL.Add('select ID, Name, Name from Companies');
    if UseSteps = stepShort then
    begin
      f := True;
      while CMD.Step do
      begin
        if f then
        begin
          PrintHeader;
          f := False;
        end;
        PrintRecord;
      end;
    end
    else
    begin
      Cmd.Prepare;
      Log('-----------------------------------------');
      if CMD.Execute then
      begin
        PrintHeader;
        if UseSteps = stepNormal then
        begin
          while CMD.Step do
          begin
            PrintRecord;
          end;
        end
        else
        begin
          while not CMD.Done do //Done=EOF
          begin
            PrintRecord;
            CMD.Next;
          end;
        end;
      end
      else
        Log('Nothing to read');
    end;

  finally
    CMD.Free;
  end;
end;

procedure TMainForm.ConnectBtn1Click(Sender: TObject);
begin
  Connect(False);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  IniFile: TIniFile;
begin
  Engines.EnumORMs(EnginesCbo.Items);
//  Engines.EnumConnections(EnginesCbo.Items);
  EnginesCbo.ItemIndex := 0;
  IniFile := TIniFile.Create(Application.Location + 'options.ini');
  try
    Width := IniFile.ReadInteger('Options', 'Width', Width);
    Height := IniFile.ReadInteger('Options', 'Height', Height);
    DataEdit.Text := IniFile.ReadString('Options', 'Database', 'employee');
    HostEdit.Text := IniFile.ReadString('Options', 'Host', 'localhost');
    UserEdit.Text := IniFile.ReadString('Options', 'User', '');
    PasswordEdit.Text := IniFile.ReadString('Options', 'Password', '');
    EnginesCbo.ItemIndex := EnginesCbo.Items.IndexOfObject(Engines.Find(IniFile.ReadString('Options', 'Engine', Engines[0].Name)));
  finally
    IniFile.Free;
  end;
  if FileExists(Application.Location + 'sql.sql') then
    SynEdit.Lines.LoadFromFile(Application.Location + 'sql.sql');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  IniFile: TIniFile;
begin
  if TestThread <> nil then
  begin
    TestThread.WaitFor;
    TestThread.Free;
    TestThread := nil;
  end;
  //update "Materials" set "MatCode" = "MatCode" where "MatCode" = '100200' returning "MatID"
  SynEdit.Lines.SaveToFile(Application.Location + 'sql.sql');
  IniFile := TIniFile.Create(Application.Location + 'options.ini');
  try
    IniFile.WriteInteger('Options', 'Width', Width);
    IniFile.WriteInteger('Options', 'Height', Height);
    IniFile.WriteString('Options', 'Database', DataEdit.Text);
    IniFile.WriteString('Options', 'Host', HostEdit.Text);
    IniFile.WriteString('Options', 'User', UserEdit.Text);
    IniFile.WriteString('Options', 'Password', PasswordEdit.Text);
    IniFile.WriteString('Options', 'Engine', (EnginesCbo.Items.Objects[EnginesCbo.ItemIndex] as TmncEngine).Name);
  finally
    IniFile.Free;
  end;
  FreeAndNil(Engine);
end;

procedure TMainForm.TestThreadBtnClick(Sender: TObject);
begin
  if Engine = nil then
    exit;

  if TestThread <> nil then
  begin
    TestThread := TTestThread.Create(True);
    TestThread.Engine := Engine;
    TestThread.Start;
  end;
end;

procedure TMainForm.Log(const s: string);
begin
  LogEdit.Lines.Add(s);
  LogEdit.CaretY := LogEdit.Lines.Count;
end;

end.
