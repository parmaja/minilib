unit RegPOSCtrls;

interface

uses
  Classes,
  posImages, posPanels, posGrids, POSControls, POSLists, posButtons, posEdits, posStuffs, posSelectBoxes, posKeyboards, posCheckBoxes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('POS',[TposPanel, TposImage, TposShape, TPOSButton, TPOSCheckBox, TposStuffs, TposSelectBox, TPOSKeyboard, TPOSFrame, TPOSEdit, TPOSVirtualGrid, TPOSList, TPOSGrid]);
end;

end.
