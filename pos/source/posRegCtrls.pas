unit posRegCtrls;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, LResources,
  posPanels, posImages, posGrids, POSControls, POSLists, posButtons, posEdits, posStuffs, posSelectBoxes, posCheckBoxes, posKeyboards;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('POS',[TposPanel, TposImage, TPOSButton, TposCheckBox, TposSelectBox, TposStuffs, TPOSKeyboard, TPOSEdit, TPOSVirtualGrid, TPOSList, TPOSGrid]);
end;

initialization
  {.$r 'images/TposGrid.png'}
  {$i poslib.lrs}
end.
