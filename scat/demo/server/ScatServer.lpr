program ScatServer;

{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}


uses
  {$ifndef WINDOWS}
  cthreads,
  {$endif}
  Interfaces,
  Forms,
  LCLIntf,
  SysUtils,
  MainForm in 'MainForm.pas';

begin
  Application.Initialize;
  if FindCmdLineSwitch('hide', True) then
  begin
    Application.ShowMainForm := False;
  end;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.