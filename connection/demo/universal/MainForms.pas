unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IniFiles,
  SynEdit, SynHighlighterSQL,
  mncDB, mncConnections, mncSQL, mncSQLite, mncPostgre, mncMySQL, mncFirebird,
  mncORM, mncMySQLORM, mncSQLiteORM, mncPGORM,{ mncFBORM}
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
    ConnectBtn: TButton;
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
    PasswordEdit: TEdit;
    SynEdit: TSynEdit;
    LogEdit: TSynEdit;
    SynSQLSyn: TSynSQLSyn;
    UserEdit: TEdit;
    procedure ConnectBtnClick(Sender: TObject);
    procedure CreateDB1BtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
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
    Engine.ORM.GenerateSQL(SynEdit.Lines);
    FreeAndNil(Engine);
  end;
end;

procedure TMainForm.ConnectBtnClick(Sender: TObject);
begin
  try
    FreeAndNil(Engine);
    Engine := TEngine.Create;
    Engine.ORM := CreateORM((EnginesCbo.Items.Objects[EnginesCbo.ItemIndex] as TmncEngine).ORMClass);
    Engine.ORM.GenerateSQL(Engine.InitSQL);
    Engine.Connection := Engines.CreateConnection(Engine.ORM) as TmncSQLConnection;
    if ccPath in Engine.Connection.Capabilities then
      Engine.Connection.Resource := Application.Location + DataEdit.Text + Engine.Connection.GetExtension
    else
      Engine.Connection.Resource := DataEdit.Text;
    Engine.Connection.Host := HostEdit.Text;
    Engine.Connection.UserName := UserEdit.Text;
    Engine.Connection.Password := PasswordEdit.Text;
    Engine.Connection.DropDatabase(True);
    Engine.Connection.CreateDatabase;
    Engine.Connection.Connect;
    LogEdit.Lines.Add(Engine.Connection.Resource + ' connected');
    Engine.Session := Engine.Connection.CreateSession;
    Engine.Session.Start;
    Engine.Session.ExecuteScript(Engine.InitSQL);
  except
    on E: EXception do
    begin
      LogEdit.Lines.Add(E.Message);
      raise;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(Application.Location + 'options.ini');
  try
    Width := IniFile.ReadInteger('Options', 'Width', Width);
    Height := IniFile.ReadInteger('Options', 'Height', Height);
    HostEdit.Text := IniFile.ReadString('Options', 'Host', 'localhost');
    UserEdit.Text := IniFile.ReadString('Options', 'User', '');
    PasswordEdit.Text := IniFile.ReadString('Options', 'Password', '');
  finally
    IniFile.Free;
  end;
  Engines.EnumORMs(EnginesCbo.Items);
  EnginesCbo.ItemIndex := 0;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(Application.Location + 'options.ini');
  try
    IniFile.WriteInteger('Options', 'Width', Width);
    IniFile.WriteInteger('Options', 'Height', Height);
    IniFile.WriteString('Options', 'Host', HostEdit.Text);
    IniFile.WriteString('Options', 'User', UserEdit.Text);
    IniFile.WriteString('Options', 'Password', PasswordEdit.Text);
  finally
    IniFile.Free;
  end;
  FreeAndNil(Engine);
end;

end.

