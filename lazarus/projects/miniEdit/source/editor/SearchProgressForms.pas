unit SearchProgressForms;
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
  Dialogs, StdCtrls;

type
  TSearchProgressForm = class(TForm)
    CancelBtn: TButton;
    FileNameLbl: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    FoundLbl: TLabel;
    procedure CancelBtnClick(Sender: TObject);
  private
  public
    Canceled: Boolean;
  end;

implementation

{$R *.lfm}

procedure TSearchProgressForm.CancelBtnClick(Sender: TObject);
begin
  Canceled := True;
end;

end.

