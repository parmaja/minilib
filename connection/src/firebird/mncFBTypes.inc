{* mncFBTypes.inc *}
{* ported from types_pub.h *}

{*
	PROGRAM:	Client/Server Common Code
	DESCRIPTION:	Types that are used both internally and externally
*}

type
   {$ifdef FPC}
   {$else}
   PtrInt = IntPtr;
   PtrUInt = UIntPtr;

   PIntPtr = ^PtrInt;
   PUIntPtr = ^PtrUInt;
   {$endif}

  {*
   * It is difficult to detect 64-bit long from the redistributable header
   * thus we may use plain "int" which is 32-bit on all platforms we support
   *
   * We'll move to this definition in future API releases.
   *
   *}

  {******************************************************************}
  {* API handles                                                    *}
  {******************************************************************}

  FB_API_HANDLE = Cardinal;

  Tisc_att_handle = FB_API_HANDLE; Pisc_att_handle = ^Tisc_att_handle;
  Tisc_blob_handle = FB_API_HANDLE; Pisc_blob_handle = ^Tisc_blob_handle;
  Tisc_db_handle = FB_API_HANDLE; Pisc_db_handle = ^Tisc_db_handle;
  Tisc_form_handle = FB_API_HANDLE; Pisc_form_handle = ^Tisc_form_handle;
  Tisc_req_handle = FB_API_HANDLE; Pisc_req_handle = ^Tisc_req_handle;
  Tisc_stmt_handle = FB_API_HANDLE; Pisc_stmt_handle = ^Tisc_stmt_handle;
  Tisc_svc_handle = FB_API_HANDLE; Pisc_svc_handle = ^Tisc_svc_handle;
  Tisc_tr_handle = FB_API_HANDLE; Pisc_tr_handle = ^Tisc_tr_handle;

{******************************************************************}
{* Sizes of memory blocks                                         *}
{******************************************************************}
type
  {$ifdef FB_USE_SIZE_T}
  {* NS: This is how things were done in original Firebird port to 64-bit platforms
     Basic classes use these quantities. However in many places in the engine and
     external libraries 32-bit quantities are used to hold sizes of objects.
     This produces many warnings. This also produces incredibly dirty interfaces,
     when functions take size_t as argument, but only handle 32 bits internally
     without any bounds checking.                                                    *}

   FB_SIZE_T = PtrUInt;
   FB_SSIZE_T = PtrInt;
  {$else}
  {* NS: This is more clean way to handle things for now. We admit that engine is not
     prepared to handle 64-bit memory blocks in most places, and it is not necessary really. *}
  FB_SIZE_T = Cardinal;
  FB_SSIZE_T = Integer;
  {$endif}

{******************************************************************}
{* Status vector                                                  *}
{******************************************************************}

  ISC_STATUS = IntPtr;

const
   ISC_STATUS_LENGTH	= 20;

type
  ISC_STATUS_ARRAY = array[0..ISC_STATUS_LENGTH - 1] of ISC_STATUS;

{* SQL State as defined in the SQL Standard. }

const
  FB_SQLSTATE_LENGTH = 5;
  FB_SQLSTATE_SIZE	 = (FB_SQLSTATE_LENGTH + 1);

type
  FB_SQLSTATE_STRING = array[0..FB_SQLSTATE_SIZE - 1] of AnsiChar;

{******************************************************************}
{* Define type, export and other stuff based on c/c++ and Windows *}
{******************************************************************}
{$ifdef FPC}
  {$ifdef windows}
    {$define extdecl := stdcall}
  {$else}
    {$define extdecl:=cdecl}
  {$endif}
{$endif}

{*
 * It is difficult to detect 64-bit long from the redistributable header
 * thus we may use plain "int" which is 32-bit on all platforms we support
 *
 * We'll move to this definition in future API releases.
 *
 *}

  //{$if defined(cpu64) and not(defined(win64) and defined(cpux86_64))}  //* i don't understand this part
  ISC_LONG   = Int32;
  ISC_ULONG  = UInt32;

  ISC_SHORT = SmallInt;
  ISC_USHORT = UInt16;

  ISC_UCHAR = Byte;
  ISC_SCHAR = ShortInt;

  FB_BOOLEAN = ISC_UCHAR;

const
  FB_FALSE	= 	0;
  FB_TRUE		= 	1;

{*******************************************************************}
{* 64 bit Integers                                                 *}
{*******************************************************************}

type
  ISC_INT64 = Int64;
  ISC_UINT64 = UInt64;

{*******************************************************************}
{* Time & Date support                                             *}
{*******************************************************************}

  ISC_DATE = Integer;
  PISC_DATE = ^ISC_DATE;

  ISC_TIME = Cardinal;
  PISC_TIME = ^ISC_TIME;

  TISC_TIME_TZ = record
  	utc_time: ISC_TIME;
  	time_zone: ISC_USHORT;
  end;
  PISC_TIME_TZ = ^TISC_TIME_TZ;

  TISC_TIME_TZ_EX = record
	  utc_time: ISC_TIME;
	  time_zone: ISC_USHORT;
	  ext_offset: ISC_SHORT;
  end;
  PISC_TIME_TZ_EX = ^TISC_TIME_TZ_EX;

  TISC_TIMESTAMP = record
	  timestamp_date: ISC_DATE;
	  timestamp_time: ISC_TIME;
  end;
  PISC_TIMESTAMP = ^TISC_TIMESTAMP;

  TISC_TIMESTAMP_TZ = record
  	utc_timestamp: TISC_TIMESTAMP;
	  time_zone: ISC_USHORT;
  end;
  PISC_TIMESTAMP_TZ = TISC_TIMESTAMP_TZ;

  TISC_TIMESTAMP_TZ_EX = record
	  utc_timestamp: TISC_TIMESTAMP;
	  time_zone: ISC_USHORT;
	  ext_offset:ISC_SHORT;
  end;
  PISC_TIMESTAMP_TZ_EX = TISC_TIMESTAMP_TZ_EX;

{*******************************************************************}
{* Blob Id support                                                 *}
{*******************************************************************}

  TGDS_QUAD = record
	  gds_quad_high: ISC_LONG;
	  gds_quad_low: ISC_ULONG;
  end;

  PGDS_QUAD = ^TGDS_QUAD;

  TISC_QUAD = TGDS_QUAD;
  PISC_QUAD = ^TISC_QUAD;

{ nothing todo :(
  const
    isc_quad_high	= gds_quad_high;
    isc_quad_low	= gds_quad_low;
}

type
  //typedef int (*FB_SHUTDOWN_CALLBACK)(const int reason, const int mask, void* arg);
  TFB_SHUTDOWN_CALLBACK = function(Reason: Integer; Mask: Integer; Arg: Pointer): Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  TFB_DEC16 = record
  	fb_data: array [0..0] of ISC_UINT64;
  end;

  TFB_DEC34 = record
  	fb_data: array[0..1] of ISC_UINT64;
  end;

  TFB_I128 = record
  	fb_data: array[0..1] of ISC_UINT64;
  end;
