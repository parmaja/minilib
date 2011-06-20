unit GotoForms;
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

  { TGotoLineForm }

TGotoLineForm = class(TForm)
    NumberEdit: TEdit;
    Label1: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure NumberEditKeyPress(Sender: TObject; var Key: char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.lfm}

{ TGotoLineForm }

procedure TGotoLineForm.NumberEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    Key := #0;
end;

end.
