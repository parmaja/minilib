unit mnXMLFPClasses;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 * function and classes not founded in FreePascal
 *
 *}
interface

{$DEFINE FPC}//emulating FPC functions

uses
  Classes, SysUtils, TypInfo;

{$IFDEF FPC}

//this functions found in classes.inc but need to declair it in the Inteface of unit classesh.inc
function FindIntToIdent(AIntegerType: Pointer): TIntToIdent;
function FindIdentToInt(AIntegerType: Pointer): TIdentToInt;

{$ENDIF}

implementation

{$IFDEF FPC}

function FindIntToIdent(AIntegerType: Pointer): TIntToIdent;
begin
  Result := nil;
end;

function FindIdentToInt(AIntegerType: Pointer): TIdentToInt;
begin
  Result := nil;
end;

{$ENDIF}

initialization
finalization
end.

