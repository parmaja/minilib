unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IniFiles,
  SynEdit, SynHighlighterSQL,
  mncDB, mncConnections, mncSQL,
  mncSQLite, mncPostgre, mncMySQL, mncFirebird,
  mncORM, mncMySQLORM, mncSQLiteORM, mncPGORM, mncFBORM,
  appSchema;

type

  { TEngine }

  TEngine = class(TObject)
  public
    ORM: TmncORM;
    Connection: TmncSQLConnection;
    Session: TmncSQLSession;
    InitSQL: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    AddRecordBtn: TButton;
    AddRecordBtn1: TButton;
    Button2: TButton;
    Button3: TButton;
    ConnectBtn: TButton;
    ConnectBtn1: TButton;
    CreateDB1Btn: TButton;
    EnginesCbo: TComboBox;
    HostEdit: TEdit;
    DataEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    PasswordEdit: TEdit;
    SynEdit: TSynEdit;
    LogEdit: TSynEdit;
    SynSQLSyn: TSynSQLSyn;
    UserEdit: TEdit;
    procedure AddRecordBtn1Click(Sender: TObject);
    procedure AddRecordBtnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ConnectBtn1Click(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure CreateDB1BtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure Connect(CreateIt: Boolean);
  public
    Engine: TEngine;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TEngine }

constructor TEngine.Create;
begin
  inherited Create;
  InitSQL := TStringList.Create;
end;

destructor TEngine.Destroy;
begin
  FreeAndNil(Session);
  FreeAndNil(Connection);
  FreeAndNil(InitSQL);
  FreeAndNil(ORM);
  inherited Destroy;
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
        LogEdit.Lines.Add(Engine.Connection.Resource + ' Droping');
        Engine.Connection.DropDatabase(True);
      end;
      if (ccCreate in Engine.Connection.Capabilities) then
      begin
        LogEdit.Lines.Add(Engine.Connection.Resource + ' Creating');
        Engine.Connection.CreateDatabase;
        LogEdit.Lines.Add(Engine.Connection.Resource + ' is Created');
      end;
    end;
    Engine.Connection.Connect;
    LogEdit.Lines.Add(Engine.Connection.Resource + ' is Connected');
    Engine.Session := Engine.Connection.CreateSession;
    Engine.Session.Start;
    if CreateIt then
      Engine.Session.ExecuteScript(Engine.InitSQL);
    Engine.Session.Commit(True);
  except
    on E: EXception do
    begin
      LogEdit.Lines.Add(E.Message);
      raise;
    end;
  end;
end;

procedure TMainForm.AddRecordBtnClick(Sender: TObject);
var
  CMD: TmncSQLCommand;
begin
  CMD := Engine.Session.CreateCommand;
  try
    CMD.Options := CMD.Options + [cmoTruncate];
    CMD.SQL.Text := 'insert into Companies(ID, Name, Address) values(?ID, ?Name, ?Name)';
    CMD.Prepare;
    CMD.Param['ID'].Value := 10;
    CMD.Param['Name'].Value := 'Parmaja';
    CMD.Execute;
  finally
    CMD.Free;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  CMD: TmncSQLCommand;
begin
  CMD := Engine.Session.CreateCommand;
  try
    CMD.SQL.Text := 'delete from Companies where ID=?ID';
    CMD.Prepare;
    CMD.Param['ID'].Value := 10;
    if CMD.Execute then
      LogEdit.Lines.Add('Deleted');
  finally
    CMD.Free;
  end;
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  CMD: TmncSQLCommand;
begin
  CMD := Engine.Session.CreateCommand;
  try
    CMD.SQL.Text := 'update "Materials" set "MatCode" = "MatCode" where "MatCode" = ''100200'' returning "MatID" ';
    CMD.Prepare;
    if CMD.Execute then
    begin
      while not CMD.Done do
      begin
        LogEdit.Lines.Add(CMD.Field['MatID'].AsString);
        CMD.Next;
      end;
    end;
  finally
    CMD.Free;
  end;
end;

procedure TMainForm.AddRecordBtn1Click(Sender: TObject);
var
  CMD: TmncSQLCommand;
  i: Integer;
begin
  CMD := Engine.Session.CreateCommand;
  try
    Cmd.SQL.Add('select ID, Name, Name from Companies');
    Cmd.Prepare;
    CMD.Execute;
    for i := 0 to CMD.Columns.Count - 1 do
      LogEdit.Lines.Add(CMD.Columns[i].Name);

    while not CMD.Done do
    begin
      CMD.Next;
    end;


{    CMD.SQL.Text := 'select * from Companies where ID=?ID';
    CMD.Prepare;
    CMD.Param['ID'].Value := 10;
    if CMD.Execute then
      LogEdit.Lines.Add(CMD.Field['Name'].AsString);}

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

end.

