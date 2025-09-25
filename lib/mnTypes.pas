unit mnTypes;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$IFDEF FPC}
{$MODE delphi}
{$modeswitch arrayoperators}
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}
{$RangeChecks+}
{$ENDIF}
{$M+}{$H+}

{$ifdef mswindows}
{$define windows}
{$endif}

interface

uses
  Types;

const
  sUTF8BOM: array[1..3] of AnsiChar = (#$EF, #$BB, #$BF);
  {$ifdef FPC}
  {$else}
  JulianEpoch = TDateTime(-2415018.5);  //check EpochAsJulianDate
  {$endif}

  URLPathDelim  = '/';

type
  TmnDataType = (dtUnknown, dtString, dtBoolean, dtInteger, dtCurrency, dtFloat, dtDate, dtTime, dtDateTime, dtMemo, dtBlob, dtBig {bigint or int64}, dtUUID{, dtEnum, dtSet});
  TmnSubType = (dstBinary, dstText, dstImage, dstXML, dstJSON);
  TmnBlobType = (blobBinary, blobText);

implementation

end.

