{* ibase.h *}

const
  FB_API_VER = 40;
  {$define isc_version4}

  ISC_TRUE = 1;
  ISC_FALSE = 0;

{$define ISC_FAR}

{$i mncFBTypes.inc}

type
  TISC_CALLBACK = procedure; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  isc_resv_handle = ISC_LONG;

  TISC_PRINT_CALLBACK = procedure(P: Pointer; param2:ISC_SHORT; param3:PByte); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TISC_VERSION_CALLBACK = procedure(P: Pointer; Buffer: PByte); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TISC_EVENT_CALLBACK = procedure(P: Pointer; param2: ISC_USHORT; param3: PISC_UCHAR); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  {*******************************************************************}
  {* Blob id structure                                               *}
  {*******************************************************************}

  TISC_ARRAY_BOUND = record
    array_bound_lower: SmallInt;
    array_bound_upper: SmallInt;
  end;
  PISC_ARRAY_BOUND = ^TISC_ARRAY_BOUND;

  TISC_ARRAY_DESC = record
    array_desc_dtype: Byte;
    array_desc_scale: AnsiChar;
    array_desc_length: Word;
    array_desc_field_name: array[0..31] of AnsiChar;
    array_desc_relation_name: array[0..31] of AnsiChar;
    array_desc_dimensions: SmallInt;
    array_desc_flags: SmallInt;
    array_desc_bounds: array[0..15] of TISC_ARRAY_BOUND;
  end;
  PISC_ARRAY_DESC = ^TISC_ARRAY_DESC;

  TISC_BLOB_DESC = record
    blob_desc_subtype: SmallInt;
    blob_desc_charset: SmallInt;
    blob_desc_segment_size: SmallInt;
    blob_desc_field_name: array[0..31] of Byte;
    blob_desc_relation_name: array[0..31] of Byte;
  end; // TISC_BLOB_DESC
  PISC_BLOB_DESC = ^TISC_BLOB_DESC;

  {***************************}
  {* Blob control structure  *}
  {***************************}

  TISC_BLOB_CTL_SOURCE_FUNCTION = function: ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  PISC_BLOB_CTL = ^TISC_BLOB_CTL;

  TISC_BLOB_CTL = record
    ctl_source: TISC_BLOB_CTL_SOURCE_FUNCTION; (** Source filter **)
    ctl_source_handle: PISC_BLOB_CTL; (** Argument to pass to source filter **)
    ctl_to_sub_type: SmallInt; (** Target type **)
    ctl_from_sub_type: SmallInt; (** Source type **)
    ctl_buffer_length: Word; (** Length of buffer **)
    ctl_segment_length: Word; (** Length of current segment **)
    ctl_bpb_length: Word; (** Length of blob parameter **)
    ctl_bpb: PByte; (** Address of blob parameter **)
    ctl_buffer: PByte; (** Address of segment buffer **)
    ctl_max_segment: ISC_LONG; (** Length of longest segment **)
    ctl_number_segments: ISC_LONG; (** Total number of segments **)
    ctl_total_length: ISC_LONG; (** Total length of blob **)
    ctl_status: PISC_STATUS; (** Address of status vector **)
    ctl_data: array[0..7] of long; (** Application specific data **)
  end;

  {***************************}
  {* Blob stream definitions *}
  {***************************}
  TBSTREAM = record
    bstr_blob: PPointer; (** Blob handle **)
    bstr_buffer: PByte; (** Address of buffer **)
    bstr_ptr: PByte; (** Next character **)
    bstr_length: SmallInt; (** Length of buffer **)
    bstr_cnt: SmallInt; (** Characters in buffer **)
    bstr_mode: Char; (** (mode) ? OUTPUT : INPUT **)
  end;
  PBSTREAM = ^TBSTREAM;

{
  /* Three ugly macros, one even using octal radix... sigh... */
  #define getb(p)	(--(p)->bstr_cnt >= 0 ? *(p)->bstr_ptr++ & 0377: BLOB_get (p))
  #define putb(x, p) (((x) == '\n' || (!(--(p)->bstr_cnt))) ? BLOB_put ((x),p) : ((int) (*(p)->bstr_ptr++ = (unsigned) (x))))
  #define putbx(x, p) ((!(--(p)->bstr_cnt)) ? BLOB_put ((x),p) : ((int) (*(p)->bstr_ptr++ = (unsigned) (x))))
}

{********************************************************************}
{* CVC: Public blob interface definition held in val.h.             *}
{* For some unknown reason, it was only documented in langRef       *}
{* and being the structure passed by the engine to UDFs it never    *}
{* made its way into this public definitions file.                  *}
{* Being its original name "blob", I renamed it blobcallback here.  *}
{* I did the full definition with the proper parameters instead of  *}
{* the weak C declaration with any number and type of parameters.   *}
{* Since the first parameter -BLB- is unknown outside the engine,   *}
{* it's more accurate to use void* than int* as the blob pointer    *}
{********************************************************************}

  {* Blob passing structure *}

  {* This enum applies to parameter "mode" in blob_lseek *}
  TBlob_LSeek_Mode = (blb_seek_relative = 1, blb_seek_from_tail = 2);
  {* This enum applies to the value returned by blob_get_segment *}
  TBlob_Get_Result = (blb_got_fragment = -1, blb_got_eof = 0, blb_got_full_segment = 1);


  TBlob_get_segment = function (hnd: Pointer; buffer: PISC_UCHAR; buf_size: ISC_USHORT; result_len: PISC_USHORT): SmallInt;
  TBlob_put_segment = procedure (hnd: Pointer; const buffer: PISC_UCHAR; buf_size: ISC_USHORT);
  TBlob_lseek = function (hnd: Pointer; mode: ISC_USHORT; offset: ISC_LONG): ISC_LONG;

  TBlobCallback = record
    blob_get_segment: TBlob_get_segment;
    blob_handle: PPointer;

    blob_number_segments:ISC_LONG;
    blob_max_segment:ISC_LONG;
    blob_total_length: ISC_LONG;

    blob_put_segment: TBlob_put_segment;
    blob_lseek : TBlob_lseek;
  end;
  PBlobCallback = ^TBlobCallback;

  {********************************************************************}
  {* CVC: Public descriptor interface held in dsc2.h.                 *}
  {* We need it documented to be able to recognize NULL in UDFs.      *}
  {* Being its original name "dsc", I renamed it paramdsc here.       *}
  {* Notice that I adjust to the original definition: contrary to     *}
  {* other cases, the typedef is the same struct not the pointer.     *}
  {* I included the enumeration of dsc_dtype possible values.         *}
  {* Ultimately, dsc2.h should be part of the public interface.       *}
  {********************************************************************}

  {* This is the famous internal descriptor that UDFs can use, too. *}

  TParamDsc = record
      dsc_dtype: ISC_UCHAR;
      dsc_scale: ShortInt;
      dsc_length: ISC_USHORT;
      dsc_sub_type: SmallInt;
      dsc_flags: ISC_USHORT;
      dsc_address: PISC_UCHAR;
  end;

  {* This is a helper struct to work with varchars. *}
  TParamVary = record
    vary_length: ISC_USHORT;
    vary_string:array[0..0] of ISC_UCHAR;
  end;

  {$i mncFBDsc.inc}

{***************************}
{* Dynamic SQL definitions *}
{***************************}

  {$i mncFBSQLDA.inc}
