unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniFiles,
  SynEdit, SynHighlighterSQL,
  mncDB, mncConnections, mncSQL, mncSQLite, mncMySQL, mncFirebird,
  mncORM, mncMySQLORM, mncSQLiteORM, {mncPGORM, mncFBORM}
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
    EnginesCbo: TComboBox;
    CreateDB1Btn: TButton;
    SynEdit1: TSynEdit;
    SynSQLSyn: TSynSQLSyn;
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
    FreeAndNil(Engine);
    Engine := TEngine.Create;
    Engine.ORM := CreateORM((EnginesCbo.Items.Objects[EnginesCbo.ItemIndex] as TmncEngine).ORMClass);
    Engine.ORM.GenerateSQL(SynEdit1.Lines);
    FreeAndNil(Engine);
  end;
end;

procedure TMainForm.ConnectBtnClick(Sender: TObject);
begin
  FreeAndNil(Engine);
  Engine := TEngine.Create;
  Engine.ORM := CreateORM((EnginesCbo.Items.Objects[EnginesCbo.ItemIndex] as TmncEngine).ORMClass);
  Engine.ORM.GenerateSQL(Engine.InitSQL);
  Engine.Connection := Engine.ORM.CreateConnection as TmncSQLConnection;
  Engine.Connection.Resource := Application.Location + 'data.' + Engine.Connection.GetExtension;
  DeleteFile(Engine.Connection.Resource);
  Engine.Connection.AutoCreate := True;
  Engine.Connection.Connect;
  Engine.Session := Engine.Connection.CreateSession;
  Engine.Session.Start;
  Engine.Session.ExecuteScript(Engine.InitSQL);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(Application.Location + 'options.ini');
  try
    Width := IniFile.ReadInteger('Options', 'Width', Width);
    Height := IniFile.ReadInteger('Options', 'Height', Height);
  finally
    IniFile.Free;
  end;
  Engines.EnumEngines(EnginesCbo.Items);
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
  finally
    IniFile.Free;
  end;
  FreeAndNil(Engine);
end;

end.

