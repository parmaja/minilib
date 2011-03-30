unit LogForms;
{**
 * Mini Translator
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources;

type
  TLogForm = class(TForm)
    CloseBtn: TButton;
    LogMemo: TMemo;
    Button1: TButton;
    SaveDialog: TSaveDialog;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowLogForm(Log:TStrings);

implementation

procedure ShowLogForm(Log:TStrings);
begin
  with TLogForm.Create(Application) do
  begin
    LogMemo.Lines.Assign(Log);
    ShowModal;
  end;
end;

procedure TLogForm.Button1Click(Sender: TObject);
begin
  if SaveDialog.Execute then
    LogMemo.Lines.SaveToFile(SaveDialog.FileName);
end;

initialization
  {$i LogForms.lrs}
end.
