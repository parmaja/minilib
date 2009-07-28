unit AboutForms;
{**
 * Mini Translator
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  LCLIntf, LResources,
  Dialogs, StdCtrls, ExtCtrls;

type
  TAboutForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Image1: TImage;
    Memo1: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

procedure TAboutForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

initialization
  {$i AboutForms.lrs}
end.
