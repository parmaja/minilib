program SqliteViewer;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms,
  Main, AboutForm, CSVIEForms;

{$R SqliteViewer.res}
{.$R ceux.rc}

begin
  Application.Initialize;
  Application.ApplicationType := atPDA;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

