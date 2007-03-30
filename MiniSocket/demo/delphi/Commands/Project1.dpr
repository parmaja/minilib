program Project1;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  mnServers in '..\..\source\mnServers.pas',
  mnCommandClients in '..\..\source\mnCommandClients.pas',
  mnConsts in '..\..\source\mnConsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
