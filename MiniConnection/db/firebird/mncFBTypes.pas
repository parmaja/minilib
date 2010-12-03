{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
{
  Do not merge it with Header, may it used by UDFs
}


{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

unit mncFBTypes;

{ Some structures, declarations that we need for the FB stuff to work, but
  that aren't really part of the fb header file. }

interface

uses
  Windows;

type
  Int                  =

  Integer; { 32 bit signed }
  UInt                 = DWORD;   { 32 bit unsigned }
  Long                 = Integer; { 32 bit signed }
  ULong                = DWORD;   { 32 bit unsigned }
  SLong                = Integer;   { 32 bit unsigned }
  Short                = SmallInt;{ 16 bit signed }
  UShort               = Word;    { 16 bit unsigned }
  Float                = Single;  { 32 bit }
  UChar                = Byte;    { 8 bit unsigned }
  Void                 = Pointer;

  ISC_LONG             = Long;    { 32 bit signed  }
  UISC_LONG            = ULong;   { 32 bit unsigned }
  ISC_INT64            = Int64;   { 64 bit signed  }
  ISC_BOOLEAN          = SmallInt; { 16 bit signed  }
  ISC_STATUS           = Long;    { 32 bit signed }
  UISC_STATUS          = ULong;   { 32 bit unsigned}

  { Delphi Pointer types }
  PPChar               = ^PChar;
  PSmallInt            = ^SmallInt;
  PInt                 = ^Int;
  PInteger             = ^Integer;
  PShort               = ^Short;
  PUShort              = ^UShort;
  PLong                = ^Long;
  PULong               = ^ULong;
  PFloat               = ^Float;
  PUChar               = ^UChar;
  PVoid                = ^Pointer;
  PDouble              = ^Double;
  PISC_LONG            = ^ISC_LONG;
  PUISC_LONG           = ^UISC_LONG;
  PISC_STATUS          = ^ISC_STATUS;
  PPISC_STATUS         = ^PISC_STATUS;
  PUISC_STATUS         = ^UISC_STATUS;

  PISC_UCHAR = ^ISC_UCHAR;
  ISC_UCHAR = UChar;

  PISC_SHORT = ^ISC_SHORT;
  ISC_SHORT = SHORT;

  PISC_USHORT = ^ISC_USHORT;
  ISC_USHORT = USHORT;

  { C Date/Time Structure }
  TCTimeStructure = record
    tm_sec : integer;   { Seconds }
    tm_min : integer;   { Minutes }
    tm_hour : integer;  { Hour (0--23) }
    tm_mday : integer;  { Day of month (1--31) }
    tm_mon : integer;   { Month (0--11) }
    tm_year : integer;  { Year (calendar year minus 1900) }
    tm_wday : integer;  { Weekday (0--6) Sunday = 0) }
    tm_yday : integer;  { Day of year (0--365) }
    tm_isdst : integer; { 0 if daylight savings time is not in effect) }
  end;

  FB_API_HANDLE = PVoid;

  PCTimeStructure = ^TCTimeStructure;
  TM              = TCTimeStructure;
  PTM             = ^TM;

  TISC_VARYING = record
    strlen: Short;
    str: array[0..0] of Char;
  end;

  TStatusVector = array[0..19] of ISC_STATUS;
  PStatusVector = ^TStatusVector;


  TISC_BlobGetSegment = function(BlobHandle: PInt;
                                 Buffer: PChar;
                                 BufferSize: Long;
                                 var ResultLength: Long): Short; cdecl;
                                 
  TISC_BlobPutSegment = procedure(BlobHandle: PInt;
                                  Buffer: PChar;
                                  BufferLength: Short); cdecl;

  TISC_BlobSeek = procedure(BlobHandle: PInt;
                                  Mode: Short;
                                  Offset: Long); cdecl;

  TBlob = record
    GetSegment         : TISC_BlobGetSegment;
    BlobHandle         : PInt;
    SegmentCount       : Long;
    MaxSegmentLength   : Long;
    TotalSize          : Long;
    PutSegment         : TISC_BlobPutSegment;
    Seek         : TISC_BlobSeek;
  end;
  
  PBlob = ^TBlob;

  PFBDateTime = ^TFBDateTime;
  TFBDateTime = record
    Days,                           // Date: Days since the start
    MSec : Integer;               // Time: Millisecond * 10 since midnight
  end;


procedure InitializeTCTimeStructure(var tm_record: TCTimeStructure);

implementation

procedure InitializeTCTimeStructure(var tm_record: TCTimeStructure);
begin
  with tm_record do begin
    tm_sec    := 0;
    tm_min    := 0;
    tm_hour   := 0;
    tm_mday   := 0;
    tm_mon    := 0;
    tm_year   := 0;
    tm_wday   := 0;
    tm_yday   := 0;
    tm_isdst  := 0;
  end;
end;

end.
