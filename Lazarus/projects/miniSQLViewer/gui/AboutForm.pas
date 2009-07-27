unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, sqlvConsts;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    CloseBtn: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Memo1: TMemo;
    VersionLbl: TLabel;
    VersionLbl1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

implementation

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  VersionLbl.Caption := sqlvVersion;
end;

initialization
  {$I AboutForm.lrs}
end.

