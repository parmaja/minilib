unit mnXMLFPClasses;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

{.$DEFINE FPC}//emulating FPC

uses
  Classes, SysUtils, TypInfo;

{$IFDEF FPC}

const
  LOCALE_FONTSIGNATURE            = $00000058;   { font signature } //found in JwaWinNLS

//function and classes not founded in FreePascal

var
  IntConstList: TThreadList;

type
  TGetLookupInfoEvent = procedure(var Ancestor: TPersistent;
    var Root, LookupRoot, RootAncestor: TComponent) of object;

//this functions found in classes.inc but need to declair it in the Inteface of unit classesh.inc
function FindIntToIdent(AIntegerType: Pointer): TIntToIdent;
function FindIdentToInt(AIntegerType: Pointer): TIdentToInt;

{$ENDIF}

implementation

{$IFDEF FPC}

type
  TIntConst = class
    IntegerType: PTypeInfo;
    IdentToInt: TIdentToInt;
    IntToIdent: TIntToIdent;
    constructor Create(AIntegerType: PTypeInfo; AIdentToInt: TIdentToInt; AIntToIdent: TIntToIdent);
  end;

constructor TIntConst.Create(AIntegerType: PTypeInfo; AIdentToInt: TIdentToInt;
  AIntToIdent: TIntToIdent);
begin
  IntegerType := AIntegerType;
  IdentToInt := AIdentToInt;
  IntToIdent := AIntToIdent;
end;

procedure RegisterIntegerConsts(AIntegerType: Pointer; AIdentToInt: TIdentToInt;
  AIntToIdent: TIntToIdent);
begin
  IntConstList.Add(TIntConst.Create(AIntegerType, AIdentToInt, AIntToIdent));
end;

procedure UnregisterIntegerConsts(AIntegerType: Pointer; AIdentToInt: TIdentToInt;
  AIntToIdent: TIntToIdent);
var
  I: Integer;
begin
  with IntConstList.LockList do
  try
    for I := Count-1 downto 0 do
      with TIntConst(Items[I]) do
        if (IntegerType = AIntegerType) and
           (@IntToIdent = @AIntToIdent) and
           (@IdentToInt = @AIdentToInt) then
          Delete(I);
  finally
    IntConstList.UnlockList;
  end;
end;

function FindIntToIdent(AIntegerType: Pointer): TIntToIdent;
var
  I: Integer;
begin
  Result := nil;
  with IntConstList.LockList do
  try
    for I := Count - 1 downto 0 do
      with TIntConst(Items[I]) do
        if AIntegerType = IntegerType then
        begin
          Result := @IntToIdent;
          Exit;
        end;
  finally
    IntConstList.UnlockList;
  end;
end;

function FindIdentToInt(AIntegerType: Pointer): TIdentToInt;
var
  I: Integer;
begin
  Result := nil;
  with IntConstList.LockList do
  try
    for I := Count - 1 downto 0 do
      with TIntConst(Items[I]) do
        if AIntegerType = IntegerType then
        begin
          Result := @IdentToInt;
          Exit;
        end;
  finally
    IntConstList.UnlockList;
  end;
end;

procedure FreeIntConstList;
var
  I: Integer;
begin
  with IntConstList.LockList do
  try
    for I := 0 to Count - 1 do
      TIntConst(Items[I]).Free;
  finally
    IntConstList.UnlockList;
  end;
  IntConstList.Free;
end;

initialization
  IntConstList := TThreadList.Create;
//  RegisterIntegerConsts(TypeInfo(TColor), IdentToColor, ColorToIdent);
//  RegisterIntegerConsts(TypeInfo(TFontCharset), IdentToCharset, CharsetToIdent);
finalization
  FreeIntConstList;
  
{$ENDIF}

end.

