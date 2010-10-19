unit posTypes;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Types;

type
  Amount = Double;
  Quantity = Double;

  TposDrawState = (pdsBorder, pdsFocused, pdsSelected, pdsActive, pdsAutoSize, pdsDown, pdsPending, pdsMultiLine, pdsRightToLeft);
  TposDrawStates = set of TposDrawState;

  TRightToLeftMode = (rtlDefault, rtlLeftToRight, rtlRightToLeft);

  TposBorder = (brdLeft, brdTop, brdRight, brdBottom);
  TposBorders = set of TposBorder;

  ImnInterface = interface(IInterface)
    ['{2D5F07F7-0A9E-4DED-A001-C6DF6C7AC8A1}']
  end;

  IposStuff = interface(ImnInterface)
    ['{3A4A43BE-C02C-4D6A-A603-84A44B95F539}']
    function GetObject: TObject;
    procedure SetStates(vStates: TposDrawStates);
    procedure Click;
    function GetDrawSize: Integer;
    function Draw(vCanvas: TCanvas; vRect: TRect; vColor: TColor; vStates: TposDrawStates): Boolean;
    property DrawSize: Integer read GetDrawSize;
  end;

  IposStuffList = interface(ImnInterface)
    ['{190CF232-9DFD-4A8A-8BB2-9A757455F5E8}']
    function GetItems(Index: Integer): IposStuff;
    function GetCount: Integer;
    function Release: Boolean;
    function GetDrawSize: Integer;
    procedure Clear;
    procedure ItemClick(Sender: TObject);
    procedure ItemStates(Sender: TObject; var vStates: TposDrawStates); 
    property Count: Integer read GetCount;
    property DrawSize: Integer read GetDrawSize;
    property Items[Index: Integer]: IposStuff read GetItems; default;
  end;

const
  cRightToLeftStates: array[Boolean] of TposDrawStates = ([], [pdsRightToLeft]);
  cLeftToRightByBool: array[Boolean] of TRightToLeftMode = (rtlDefault, rtlLeftToRight);

function RTLModeToRTL(RightToLeft: Boolean; Mode: TRightToLeftMode): Boolean;

implementation

function RTLModeToRTL(RightToLeft: Boolean; Mode: TRightToLeftMode): Boolean;
begin
  case Mode of
    rtlLeftToRight: Result := False;
    rtlRightToLeft: Result := True;
  else
    Result := RightToLeft;
  end;
end;

end.

