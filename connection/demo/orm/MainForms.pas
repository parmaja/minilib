unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SynEdit,
  mncDB, mncSQLite, mncORM, mncMeta, mncSQLiteORM, appSchema;

type

  { TMainForm }

  TMainForm = class(TForm)
    CreateDB1Btn: TButton;
    CreateDB2Btn: TButton;
    SynEdit1: TSynEdit;
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
  ORM := CreateSchema1(TmncORMSQLite);
  ORM.GenerateSQL(SynEdit1.Lines, nil);
end;

procedure TMainForm.CreateDB2BtnClick(Sender: TObject);
begin
  ORM := CreateSchema2(TmncORMSQLite);
  ORM.GenerateSQL(SynEdit1.Lines, nil);
end;

end.

