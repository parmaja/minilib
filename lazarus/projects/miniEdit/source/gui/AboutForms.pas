unit AboutForms;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  lclproc, lclintf,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Button1: TButton;
    Image2: TImage;
    Label1: TLabel;
    Image1: TImage;
    Memo1: TMemo;
    SiteLbl: TLabel;
    VersionLbl: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SiteLblClick(Sender: TObject);
  private
  public
  end;

implementation

{$R *.lfm}

procedure TAboutForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  VersionLbl.Caption := '1.0.0.1';
end;

procedure TAboutForm.SiteLblClick(Sender: TObject);
begin
  openurl((Sender as TLabel).Caption);
end;

end.

