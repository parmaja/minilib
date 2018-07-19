unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SynEdit,
  SynHighlighterSQL, mncDB, mncSQLite, mncORM, mncMySQLORM, appSchema;

type

  { TMainForm }

  TMainForm = class(TForm)
    CreateDB1Btn: TButton;
    CreateDB2Btn: TButton;
    SynEdit1: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure CreateDB1BtnClick(Sender: TObject);
    procedure CreateDB2BtnClick(Sender: TObject);
  private

  public
    ORM: TmncORM;
    //Connection: TmncSQLiteConnection;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }


procedure TMainForm.CreateDB1BtnClick(Sender: TObject);
begin
  ORM := CreateSchema1(TmncORMMySQL);
  ORM.GenerateSQL(SynEdit1.Lines);
end;

procedure TMainForm.CreateDB2BtnClick(Sender: TObject);
begin
  ORM := CreateSchema2(TmncORMMySQL);
  ORM.GenerateSQL(SynEdit1.Lines);
end;

end.

