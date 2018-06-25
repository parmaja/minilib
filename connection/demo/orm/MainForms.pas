unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, mncDB,
  mncSQLite, mncORM, mncMeta, mncSQLiteORM, appSchema;

type

  { TMainForm }

  TMainForm = class(TForm)
    CreateDB1Btn: TButton;
    CreateDB2Btn: TButton;
    procedure CreateDB1BtnClick(Sender: TObject);
    procedure CreateDB2BtnClick(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }


procedure TMainForm.CreateDB1BtnClick(Sender: TObject);
begin
  CreateSchema1(TmncORMSQLite);
end;

procedure TMainForm.CreateDB2BtnClick(Sender: TObject);
begin
  CreateSchema1(TmncORMSQLite);
end;

end.

