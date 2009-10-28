unit posRegCtrls;

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
  RegisterComponents('POS',[TposPanel, TposImage, TPOSButton, TPOSLabeledButton, TposCheckBox, TposSelectBox, TposStuffs, TPOSKeyboard, TPOSEdit, TPOSVirtualGrid, TPOSList, TPOSGrid]);
end;

initialization
  {$i poslib.lrs}
end.
