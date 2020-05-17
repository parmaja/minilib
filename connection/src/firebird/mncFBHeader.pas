unit mncFBHeader;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}

{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Belal Hamed <belalhamed at gmail dot com>
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @comment   for Firebird 2.5
 *}

interface

uses
  Types, Classes, SysUtils, mnLibraries;

type

  PtrInt = integer;
  PtrUInt = Cardinal;

  IntPtr_t = PtrInt;
  PIntPtr_t = ^PtrInt;

  UIntPtr_t = PtrUInt;
  PUIntPtr_t = ^PtrUInt;

  Int                  = Integer; { 32 bit signed }
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

  ISC_STATUS           = IntPtr_t;    { 32 bit signed }
  UISC_STATUS          = UIntPtr_t;   { 32 bit unsigned}

  { Delphi Pointer types }
  PPByte               = ^PByte;
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
                                 Buffer: PByte;
                                 BufferSize: Long;
                                 var ResultLength: Long): Short; cdecl;

  TISC_BlobPutSegment = procedure(BlobHandle: PInt;
                                  Buffer: PByte;
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


const
  isc_facility	: SLONG = 20;
  isc_base : SLONG = 335544320;
  isc_factor : SLONG = 1;
  isc_arg_end			: SLONG = 0;	// end of argument list
  isc_arg_gds			: SLONG = 1;	// generic DSRI status value
  isc_arg_string			: SLONG = 2;	// string argument
  isc_arg_cstring		: SLONG = 3;	// count & string argument
  isc_arg_number			: SLONG = 4;	// numeric argument (long)
  isc_arg_interpreted	: SLONG = 5;	// interpreted status code (string)
  isc_arg_vms			: SLONG = 6;	// VAX/VMS status code (long)
  isc_arg_unix			: SLONG = 7;	// UNIX error code
  isc_arg_domain			: SLONG = 8;	// Apollo/Domain error code
  isc_arg_dos			: SLONG = 9;	// MSDOS/OS2 error code
  isc_arg_mpexl			: SLONG = 10;	// HP MPE/XL error code
  isc_arg_mpexl_ipc		: SLONG = 11;	// HP MPE/XL IPC error code
  isc_arg_next_mach		: SLONG = 15;	// NeXT/Mach error code
  isc_arg_netware		: SLONG = 16;	// NetWare error code
  isc_arg_win32			: SLONG = 17;	// Win32 error code
  isc_arg_warning		: SLONG = 18;	// warning argument

  isc_arith_except                     : SLONG = 335544321;
  isc_bad_dbkey                        : SLONG = 335544322;
  isc_bad_db_format                    : SLONG = 335544323;
  isc_bad_db_handle                    : SLONG = 335544324;
  isc_bad_dpb_content                  : SLONG = 335544325;
  isc_bad_dpb_form                     : SLONG = 335544326;
  isc_bad_req_handle                   : SLONG = 335544327;
  isc_bad_segstr_handle                : SLONG = 335544328;
  isc_bad_segstr_id                    : SLONG = 335544329;
  isc_bad_tpb_content                  : SLONG = 335544330;
  isc_bad_tpb_form                     : SLONG = 335544331;
  isc_bad_trans_handle                 : SLONG = 335544332;
  isc_bug_check                        : SLONG = 335544333;
  isc_convert_error                    : SLONG = 335544334;
  isc_db_corrupt                       : SLONG = 335544335;
  isc_deadlock                         : SLONG = 335544336;
  isc_excess_trans                     : SLONG = 335544337;
  isc_from_no_match                    : SLONG = 335544338;
  isc_infinap                          : SLONG = 335544339;
  isc_infona                           : SLONG = 335544340;
  isc_infunk                           : SLONG = 335544341;
  isc_integ_fail                       : SLONG = 335544342;
  isc_invalid_blr                      : SLONG = 335544343;
  isc_io_error                         : SLONG = 335544344;
  isc_lock_conflict                    : SLONG = 335544345;
  isc_metadata_corrupt                 : SLONG = 335544346;
  isc_not_valid                        : SLONG = 335544347;
  isc_no_cur_rec                       : SLONG = 335544348;
  isc_no_dup                           : SLONG = 335544349;
  isc_no_finish                        : SLONG = 335544350;
  isc_no_meta_update                   : SLONG = 335544351;
  isc_no_priv                          : SLONG = 335544352;
  isc_no_recon                         : SLONG = 335544353;
  isc_no_record                        : SLONG = 335544354;
  isc_no_segstr_close                  : SLONG = 335544355;
  isc_obsolete_metadata                : SLONG = 335544356;
  isc_open_trans                       : SLONG = 335544357;
  isc_port_len                         : SLONG = 335544358;
  isc_read_only_field                  : SLONG = 335544359;
  isc_read_only_rel                    : SLONG = 335544360;
  isc_read_only_trans                  : SLONG = 335544361;
  isc_read_only_view                   : SLONG = 335544362;
  isc_req_no_trans                     : SLONG = 335544363;
  isc_req_sync                         : SLONG = 335544364;
  isc_req_wrong_db                     : SLONG = 335544365;
  isc_segment                          : SLONG = 335544366;
  isc_segstr_eof                       : SLONG = 335544367;
  isc_segstr_no_op                     : SLONG = 335544368;
  isc_segstr_no_read                   : SLONG = 335544369;
  isc_segstr_no_trans                  : SLONG = 335544370;
  isc_segstr_no_write                  : SLONG = 335544371;
  isc_segstr_wrong_db                  : SLONG = 335544372;
  isc_sys_request                      : SLONG = 335544373;
  isc_stream_eof                       : SLONG = 335544374;
  isc_unavailable                      : SLONG = 335544375;
  isc_unres_rel                        : SLONG = 335544376;
  isc_uns_ext                          : SLONG = 335544377;
  isc_wish_list                        : SLONG = 335544378;
  isc_wrong_ods                        : SLONG = 335544379;
  isc_wronumarg                        : SLONG = 335544380;
  isc_imp_exc                          : SLONG = 335544381;
  isc_random                           : SLONG = 335544382;
  isc_fatal_conflict                   : SLONG = 335544383;
  isc_badblk                           : SLONG = 335544384;
  isc_invpoolcl                        : SLONG = 335544385;
  isc_nopoolids                        : SLONG = 335544386;
  isc_relbadblk                        : SLONG = 335544387;
  isc_blktoobig                        : SLONG = 335544388;
  isc_bufexh                           : SLONG = 335544389;
  isc_syntaxerr                        : SLONG = 335544390;
  isc_bufinuse                         : SLONG = 335544391;
  isc_bdbincon                         : SLONG = 335544392;
  isc_reqinuse                         : SLONG = 335544393;
  isc_badodsver                        : SLONG = 335544394;
  isc_relnotdef                        : SLONG = 335544395;
  isc_fldnotdef                        : SLONG = 335544396;
  isc_dirtypage                        : SLONG = 335544397;
  isc_waifortra                        : SLONG = 335544398;
  isc_doubleloc                        : SLONG = 335544399;
  isc_nodnotfnd                        : SLONG = 335544400;
  isc_dupnodfnd                        : SLONG = 335544401;
  isc_locnotmar                        : SLONG = 335544402;
  isc_badpagtyp                        : SLONG = 335544403;
  isc_corrupt                          : SLONG = 335544404;
  isc_badpage                          : SLONG = 335544405;
  isc_badindex                         : SLONG = 335544406;
  isc_dbbnotzer                        : SLONG = 335544407;
  isc_tranotzer                        : SLONG = 335544408;
  isc_trareqmis                        : SLONG = 335544409;
  isc_badhndcnt                        : SLONG = 335544410;
  isc_wrotpbver                        : SLONG = 335544411;
  isc_wroblrver                        : SLONG = 335544412;
  isc_wrodpbver                        : SLONG = 335544413;
  isc_blobnotsup                       : SLONG = 335544414;
  isc_badrelation                      : SLONG = 335544415;
  isc_nodetach                         : SLONG = 335544416;
  isc_notremote                        : SLONG = 335544417;
  isc_trainlim                         : SLONG = 335544418;
  isc_notinlim                         : SLONG = 335544419;
  isc_traoutsta                        : SLONG = 335544420;
  isc_connect_reject                   : SLONG = 335544421;
  isc_dbfile                           : SLONG = 335544422;
  isc_orphan                           : SLONG = 335544423;
  isc_no_lock_mgr                      : SLONG = 335544424;
  isc_ctxinuse                         : SLONG = 335544425;
  isc_ctxnotdef                        : SLONG = 335544426;
  isc_datnotsup                        : SLONG = 335544427;
  isc_badmsgnum                        : SLONG = 335544428;
  isc_badparnum                        : SLONG = 335544429;
  isc_virmemexh                        : SLONG = 335544430;
  isc_blocking_signal                  : SLONG = 335544431;
  isc_lockmanerr                       : SLONG = 335544432;
  isc_journerr                         : SLONG = 335544433;
  isc_keytoobig                        : SLONG = 335544434;
  isc_nullsegkey                       : SLONG = 335544435;
  isc_sqlerr                           : SLONG = 335544436;
  isc_wrodynver                        : SLONG = 335544437;
  isc_funnotdef                        : SLONG = 335544438;
  isc_funmismat                        : SLONG = 335544439;
  isc_bad_msg_vec                      : SLONG = 335544440;
  isc_bad_detach                       : SLONG = 335544441;
  isc_noargacc_read                    : SLONG = 335544442;
  isc_noargacc_write                   : SLONG = 335544443;
  isc_read_only                        : SLONG = 335544444;
  isc_ext_err                          : SLONG = 335544445;
  isc_non_updatable                    : SLONG = 335544446;
  isc_no_rollback                      : SLONG = 335544447;
  isc_bad_sec_info                     : SLONG = 335544448;
  isc_invalid_sec_info                 : SLONG = 335544449;
  isc_misc_interpreted                 : SLONG = 335544450;
  isc_update_conflict                  : SLONG = 335544451;
  isc_unlicensed                       : SLONG = 335544452;
  isc_obj_in_use                       : SLONG = 335544453;
  isc_nofilter                         : SLONG = 335544454;
  isc_shadow_accessed                  : SLONG = 335544455;
  isc_invalid_sdl                      : SLONG = 335544456;
  isc_out_of_bounds                    : SLONG = 335544457;
  isc_invalid_dimension                : SLONG = 335544458;
  isc_rec_in_limbo                     : SLONG = 335544459;
  isc_shadow_missing                   : SLONG = 335544460;
  isc_cant_validate                    : SLONG = 335544461;
  isc_cant_start_journal               : SLONG = 335544462;
  isc_gennotdef                        : SLONG = 335544463;
  isc_cant_start_logging               : SLONG = 335544464;
  isc_bad_segstr_type                  : SLONG = 335544465;
  isc_foreign_key                      : SLONG = 335544466;
  isc_high_minor                       : SLONG = 335544467;
  isc_tra_state                        : SLONG = 335544468;
  isc_trans_invalid                    : SLONG = 335544469;
  isc_buf_invalid                      : SLONG = 335544470;
  isc_indexnotdefined                  : SLONG = 335544471;
  isc_login                            : SLONG = 335544472;
  isc_invalid_bookmark                 : SLONG = 335544473;
  isc_bad_lock_level                   : SLONG = 335544474;
  isc_relation_lock                    : SLONG = 335544475;
  isc_record_lock                      : SLONG = 335544476;
  isc_max_idx                          : SLONG = 335544477;
  isc_jrn_enable                       : SLONG = 335544478;
  isc_old_failure                      : SLONG = 335544479;
  isc_old_in_progress                  : SLONG = 335544480;
  isc_old_no_space                     : SLONG = 335544481;
  isc_no_wal_no_jrn                    : SLONG = 335544482;
  isc_num_old_files                    : SLONG = 335544483;
  isc_wal_file_open                    : SLONG = 335544484;
  isc_bad_stmt_handle                  : SLONG = 335544485;
  isc_wal_failure                      : SLONG = 335544486;
  isc_walw_err                         : SLONG = 335544487;
  isc_logh_small                       : SLONG = 335544488;
  isc_logh_inv_version                 : SLONG = 335544489;
  isc_logh_open_flag                   : SLONG = 335544490;
  isc_logh_open_flag2                  : SLONG = 335544491;
  isc_logh_diff_dbname                 : SLONG = 335544492;
  isc_logf_unexpected_eof              : SLONG = 335544493;
  isc_logr_incomplete                  : SLONG = 335544494;
  isc_logr_header_small                : SLONG = 335544495;
  isc_logb_small                       : SLONG = 335544496;
  isc_wal_illegal_attach               : SLONG = 335544497;
  isc_wal_invalid_wpb                  : SLONG = 335544498;
  isc_wal_err_rollover                 : SLONG = 335544499;
  isc_no_wal                           : SLONG = 335544500;
  isc_drop_wal                         : SLONG = 335544501;
  isc_stream_not_defined               : SLONG = 335544502;
  isc_wal_subsys_error                 : SLONG = 335544503;
  isc_wal_subsys_corrupt               : SLONG = 335544504;
  isc_no_archive                       : SLONG = 335544505;
  isc_shutinprog                       : SLONG = 335544506;
  isc_range_in_use                     : SLONG = 335544507;
  isc_range_not_found                  : SLONG = 335544508;
  isc_charset_not_found                : SLONG = 335544509;
  isc_lock_timeout                     : SLONG = 335544510;
  isc_prcnotdef                        : SLONG = 335544511;
  isc_prcmismat                        : SLONG = 335544512;
  isc_wal_bugcheck                     : SLONG = 335544513;
  isc_wal_cant_expand                  : SLONG = 335544514;
  isc_codnotdef                        : SLONG = 335544515;
  isc_xcpnotdef                        : SLONG = 335544516;
  isc_except                           : SLONG = 335544517;
  isc_cache_restart                    : SLONG = 335544518;
  isc_bad_lock_handle                  : SLONG = 335544519;
  isc_jrn_present                      : SLONG = 335544520;
  isc_wal_err_rollover2                : SLONG = 335544521;
  isc_wal_err_logwrite                 : SLONG = 335544522;
  isc_wal_err_jrn_comm                 : SLONG = 335544523;
  isc_wal_err_expansion                : SLONG = 335544524;
  isc_wal_err_setup                    : SLONG = 335544525;
  isc_wal_err_ww_sync                  : SLONG = 335544526;
  isc_wal_err_ww_start                 : SLONG = 335544527;
  isc_shutdown                         : SLONG = 335544528;
  isc_existing_priv_mod                : SLONG = 335544529;
  isc_primary_key_ref                  : SLONG = 335544530;
  isc_primary_key_notnull              : SLONG = 335544531;
  isc_ref_cnstrnt_notfound             : SLONG = 335544532;
  isc_foreign_key_notfound             : SLONG = 335544533;
  isc_ref_cnstrnt_update               : SLONG = 335544534;
  isc_check_cnstrnt_update             : SLONG = 335544535;
  isc_check_cnstrnt_del                : SLONG = 335544536;
  isc_integ_index_seg_del              : SLONG = 335544537;
  isc_integ_index_seg_mod              : SLONG = 335544538;
  isc_integ_index_del                  : SLONG = 335544539;
  isc_integ_index_mod                  : SLONG = 335544540;
  isc_check_trig_del                   : SLONG = 335544541;
  isc_check_trig_update                : SLONG = 335544542;
  isc_cnstrnt_fld_del                  : SLONG = 335544543;
  isc_cnstrnt_fld_rename               : SLONG = 335544544;
  isc_rel_cnstrnt_update               : SLONG = 335544545;
  isc_constaint_on_view                : SLONG = 335544546;
  isc_invld_cnstrnt_type               : SLONG = 335544547;
  isc_primary_key_exists               : SLONG = 335544548;
  isc_systrig_update                   : SLONG = 335544549;
  isc_not_rel_owner                    : SLONG = 335544550;
  isc_grant_obj_notfound               : SLONG = 335544551;
  isc_grant_fld_notfound               : SLONG = 335544552;
  isc_grant_nopriv                     : SLONG = 335544553;
  isc_nonsql_security_rel              : SLONG = 335544554;
  isc_nonsql_security_fld              : SLONG = 335544555;
  isc_wal_cache_err                    : SLONG = 335544556;
  isc_shutfail                         : SLONG = 335544557;
  isc_check_constraint                 : SLONG = 335544558;
  isc_bad_svc_handle                   : SLONG = 335544559;
  isc_shutwarn                         : SLONG = 335544560;
  isc_wrospbver                        : SLONG = 335544561;
  isc_bad_spb_form                     : SLONG = 335544562;
  isc_svcnotdef                        : SLONG = 335544563;
  isc_no_jrn                           : SLONG = 335544564;
  isc_transliteration_failed           : SLONG = 335544565;
  isc_start_cm_for_wal                 : SLONG = 335544566;
  isc_wal_ovflow_log_required          : SLONG = 335544567;
  isc_text_subtype                     : SLONG = 335544568;
  isc_dsql_error                       : SLONG = 335544569;
  isc_dsql_command_err                 : SLONG = 335544570;
  isc_dsql_constant_err                : SLONG = 335544571;
  isc_dsql_cursor_err                  : SLONG = 335544572;
  isc_dsql_datatype_err                : SLONG = 335544573;
  isc_dsql_decl_err                    : SLONG = 335544574;
  isc_dsql_cursor_update_err           : SLONG = 335544575;
  isc_dsql_cursor_open_err             : SLONG = 335544576;
  isc_dsql_cursor_close_err            : SLONG = 335544577;
  isc_dsql_field_err                   : SLONG = 335544578;
  isc_dsql_internal_err                : SLONG = 335544579;
  isc_dsql_relation_err                : SLONG = 335544580;
  isc_dsql_procedure_err               : SLONG = 335544581;
  isc_dsql_request_err                 : SLONG = 335544582;
  isc_dsql_sqlda_err                   : SLONG = 335544583;
  isc_dsql_var_count_err               : SLONG = 335544584;
  isc_dsql_stmt_handle                 : SLONG = 335544585;
  isc_dsql_function_err                : SLONG = 335544586;
  isc_dsql_blob_err                    : SLONG = 335544587;
  isc_collation_not_found              : SLONG = 335544588;
  isc_collation_not_for_charset        : SLONG = 335544589;
  isc_dsql_dup_option                  : SLONG = 335544590;
  isc_dsql_tran_err                    : SLONG = 335544591;
  isc_dsql_invalid_array               : SLONG = 335544592;
  isc_dsql_max_arr_dim_exceeded        : SLONG = 335544593;
  isc_dsql_arr_range_error             : SLONG = 335544594;
  isc_dsql_trigger_err                 : SLONG = 335544595;
  isc_dsql_subselect_err               : SLONG = 335544596;
  isc_dsql_crdb_prepare_err            : SLONG = 335544597;
  isc_specify_field_err                : SLONG = 335544598;
  isc_num_field_err                    : SLONG = 335544599;
  isc_col_name_err                     : SLONG = 335544600;
  isc_where_err                        : SLONG = 335544601;
  isc_table_view_err                   : SLONG = 335544602;
  isc_distinct_err                     : SLONG = 335544603;
  isc_key_field_count_err              : SLONG = 335544604;
  isc_subquery_err                     : SLONG = 335544605;
  isc_expression_eval_err              : SLONG = 335544606;
  isc_node_err                         : SLONG = 335544607;
  isc_command_end_err                  : SLONG = 335544608;
  isc_index_name                       : SLONG = 335544609;
  isc_exception_name                   : SLONG = 335544610;
  isc_field_name                       : SLONG = 335544611;
  isc_token_err                        : SLONG = 335544612;
  isc_union_err                        : SLONG = 335544613;
  isc_dsql_construct_err               : SLONG = 335544614;
  isc_field_aggregate_err              : SLONG = 335544615;
  isc_field_ref_err                    : SLONG = 335544616;
  isc_order_by_err                     : SLONG = 335544617;
  isc_return_mode_err                  : SLONG = 335544618;
  isc_extern_func_err                  : SLONG = 335544619;
  isc_alias_conflict_err               : SLONG = 335544620;
  isc_procedure_conflict_error         : SLONG = 335544621;
  isc_relation_conflict_err            : SLONG = 335544622;
  isc_dsql_domain_err                  : SLONG = 335544623;
  isc_idx_seg_err                      : SLONG = 335544624;
  isc_node_name_err                    : SLONG = 335544625;
  isc_table_name                       : SLONG = 335544626;
  isc_proc_name                        : SLONG = 335544627;
  isc_idx_create_err                   : SLONG = 335544628;
  isc_wal_shadow_err                   : SLONG = 335544629;
  isc_dependency                       : SLONG = 335544630;
  isc_idx_key_err                      : SLONG = 335544631;
  isc_dsql_file_length_err             : SLONG = 335544632;
  isc_dsql_shadow_number_err           : SLONG = 335544633;
  isc_dsql_token_unk_err               : SLONG = 335544634;
  isc_dsql_no_relation_alias           : SLONG = 335544635;
  isc_indexname                        : SLONG = 335544636;
  isc_no_stream_plan                   : SLONG = 335544637;
  isc_stream_twice                     : SLONG = 335544638;
  isc_stream_not_found                 : SLONG = 335544639;
  isc_collation_requires_text          : SLONG = 335544640;
  isc_dsql_domain_not_found            : SLONG = 335544641;
  isc_index_unused                     : SLONG = 335544642;
  isc_dsql_self_join                   : SLONG = 335544643;
  isc_stream_bof                       : SLONG = 335544644;
  isc_stream_crack                     : SLONG = 335544645;
  isc_db_or_file_exists                : SLONG = 335544646;
  isc_invalid_operator                 : SLONG = 335544647;
  isc_conn_lost                        : SLONG = 335544648;
  isc_bad_checksum                     : SLONG = 335544649;
  isc_page_type_err                    : SLONG = 335544650;
  isc_ext_readonly_err                 : SLONG = 335544651;
  isc_sing_select_err                  : SLONG = 335544652;
  isc_psw_attach                       : SLONG = 335544653;
  isc_psw_start_trans                  : SLONG = 335544654;
  isc_invalid_direction                : SLONG = 335544655;
  isc_dsql_var_conflict                : SLONG = 335544656;
  isc_dsql_no_blob_array               : SLONG = 335544657;
  isc_dsql_base_table                  : SLONG = 335544658;
  isc_duplicate_base_table             : SLONG = 335544659;
  isc_view_alias                       : SLONG = 335544660;
  isc_index_root_page_full             : SLONG = 335544661;
  isc_dsql_blob_type_unknown           : SLONG = 335544662;
  isc_req_max_clones_exceeded          : SLONG = 335544663;
  isc_dsql_duplicate_spec              : SLONG = 335544664;
  isc_unique_key_violation             : SLONG = 335544665;
  isc_srvr_version_too_old             : SLONG = 335544666;
  isc_drdb_completed_with_errs         : SLONG = 335544667;
  isc_dsql_procedure_use_err           : SLONG = 335544668;
  isc_dsql_count_mismatch              : SLONG = 335544669;
  isc_blob_idx_err                     : SLONG = 335544670;
  isc_array_idx_err                    : SLONG = 335544671;
  isc_key_field_err                    : SLONG = 335544672;
  isc_no_delete                        : SLONG = 335544673;
  isc_del_last_field                   : SLONG = 335544674;
  isc_sort_err                         : SLONG = 335544675;
  isc_sort_mem_err                     : SLONG = 335544676;
  isc_version_err                      : SLONG = 335544677;
  isc_inval_key_posn                   : SLONG = 335544678;
  isc_no_segments_err                  : SLONG = 335544679;
  isc_crrp_data_err                    : SLONG = 335544680;
  isc_rec_size_err                     : SLONG = 335544681;
  isc_dsql_field_ref                   : SLONG = 335544682;
  isc_req_depth_exceeded               : SLONG = 335544683;
  isc_no_field_access                  : SLONG = 335544684;
  isc_no_dbkey                         : SLONG = 335544685;
  isc_jrn_format_err                   : SLONG = 335544686;
  isc_jrn_file_full                    : SLONG = 335544687;
  isc_dsql_open_cursor_request         : SLONG = 335544688;
  isc_ib_error                         : SLONG = 335544689;
  isc_cache_redef                      : SLONG = 335544690;
  isc_cache_too_small                  : SLONG = 335544691;
  isc_log_redef                        : SLONG = 335544692;
  isc_log_too_small                    : SLONG = 335544693;
  isc_partition_too_small              : SLONG = 335544694;
  isc_partition_not_supp               : SLONG = 335544695;
  isc_log_length_spec                  : SLONG = 335544696;
  isc_precision_err                    : SLONG = 335544697;
  isc_scale_nogt                       : SLONG = 335544698;
  isc_expec_short                      : SLONG = 335544699;
  isc_expec_long                       : SLONG = 335544700;
  isc_expec_ushort                     : SLONG = 335544701;
  isc_escape_invalid                   : SLONG = 335544702;
  isc_svcnoexe                         : SLONG = 335544703;
  isc_net_lookup_err                   : SLONG = 335544704;
  isc_service_unknown                  : SLONG = 335544705;
  isc_host_unknown                     : SLONG = 335544706;
  isc_grant_nopriv_on_base             : SLONG = 335544707;
  isc_dyn_fld_ambiguous                : SLONG = 335544708;
  isc_dsql_agg_ref_err                 : SLONG = 335544709;
  isc_complex_view                     : SLONG = 335544710;
  isc_unprepared_stmt                  : SLONG = 335544711;
  isc_expec_positive                   : SLONG = 335544712;
  isc_dsql_sqlda_value_err             : SLONG = 335544713;
  isc_invalid_array_id                 : SLONG = 335544714;
  isc_extfile_uns_op                   : SLONG = 335544715;
  isc_svc_in_use                       : SLONG = 335544716;
  isc_err_stack_limit                  : SLONG = 335544717;
  isc_invalid_key                      : SLONG = 335544718;
  isc_net_init_error                   : SLONG = 335544719;
  isc_loadlib_failure                  : SLONG = 335544720;
  isc_network_error                    : SLONG = 335544721;
  isc_net_connect_err                  : SLONG = 335544722;
  isc_net_connect_listen_err           : SLONG = 335544723;
  isc_net_event_connect_err            : SLONG = 335544724;
  isc_net_event_listen_err             : SLONG = 335544725;
  isc_net_read_err                     : SLONG = 335544726;
  isc_net_write_err                    : SLONG = 335544727;
  isc_integ_index_deactivate           : SLONG = 335544728;
  isc_integ_deactivate_primary         : SLONG = 335544729;
  isc_cse_not_supported                : SLONG = 335544730;
  isc_tra_must_sweep                   : SLONG = 335544731;
  isc_unsupported_network_drive        : SLONG = 335544732;
  isc_io_create_err                    : SLONG = 335544733;
  isc_io_open_err                      : SLONG = 335544734;
  isc_io_close_err                     : SLONG = 335544735;
  isc_io_read_err                      : SLONG = 335544736;
  isc_io_write_err                     : SLONG = 335544737;
  isc_io_delete_err                    : SLONG = 335544738;
  isc_io_access_err                    : SLONG = 335544739;
  isc_udf_exception                    : SLONG = 335544740;
  isc_lost_db_connection               : SLONG = 335544741;
  isc_no_write_user_priv               : SLONG = 335544742;
  isc_token_too_long                   : SLONG = 335544743;
  isc_max_att_exceeded                 : SLONG = 335544744;
  isc_login_same_as_role_name          : SLONG = 335544745;
  isc_reftable_requires_pk             : SLONG = 335544746;
  isc_usrname_too_long                 : SLONG = 335544747;
  isc_password_too_long                : SLONG = 335544748;
  isc_usrname_required                 : SLONG = 335544749;
  isc_password_required                : SLONG = 335544750;
  isc_bad_protocol                     : SLONG = 335544751;
  isc_dup_usrname_found                : SLONG = 335544752;
  isc_usrname_not_found                : SLONG = 335544753;
  isc_error_adding_sec_record          : SLONG = 335544754;
  isc_error_modifying_sec_record       : SLONG = 335544755;
  isc_error_deleting_sec_record        : SLONG = 335544756;
  isc_error_updating_sec_db            : SLONG = 335544757;
  isc_sort_rec_size_err                : SLONG = 335544758;
  isc_bad_default_value                : SLONG = 335544759;
  isc_invalid_clause                   : SLONG = 335544760;
  isc_too_many_handles                 : SLONG = 335544761;
  isc_optimizer_blk_exc                : SLONG = 335544762;
  isc_invalid_string_constant          : SLONG = 335544763;
  isc_transitional_date                : SLONG = 335544764;
  isc_read_only_database               : SLONG = 335544765;
  isc_must_be_dialect_2_and_up         : SLONG = 335544766;
  isc_blob_filter_exception            : SLONG = 335544767;
  isc_exception_access_violation       : SLONG = 335544768;
  isc_exception_datatype_missalignment : SLONG = 335544769;
  isc_exception_array_bounds_exceeded  : SLONG = 335544770;
  isc_exception_float_denormal_operand : SLONG = 335544771;
  isc_exception_float_divide_by_zero   : SLONG = 335544772;
  isc_exception_float_inexact_result   : SLONG = 335544773;
  isc_exception_float_invalid_operand  : SLONG = 335544774;
  isc_exception_float_overflow         : SLONG = 335544775;
  isc_exception_float_stack_check      : SLONG = 335544776;
  isc_exception_float_underflow        : SLONG = 335544777;
  isc_exception_integer_divide_by_zero : SLONG = 335544778;
  isc_exception_integer_overflow       : SLONG = 335544779;
  isc_exception_unknown                : SLONG = 335544780;
  isc_exception_stack_overflow         : SLONG = 335544781;
  isc_exception_sigsegv                : SLONG = 335544782;
  isc_exception_sigill                 : SLONG = 335544783;
  isc_exception_sigbus                 : SLONG = 335544784;
  isc_exception_sigfpe                 : SLONG = 335544785;
  isc_ext_file_delete                  : SLONG = 335544786;
  isc_ext_file_modify                  : SLONG = 335544787;
  isc_adm_task_denied                  : SLONG = 335544788;
  isc_extract_input_mismatch           : SLONG = 335544789;
  isc_insufficient_svc_privileges      : SLONG = 335544790;
  isc_file_in_use                      : SLONG = 335544791;
  isc_service_att_err                  : SLONG = 335544792;
  isc_ddl_not_allowed_by_db_sql_dial   : SLONG = 335544793;
  isc_cancelled                        : SLONG = 335544794;
  isc_unexp_spb_form                   : SLONG = 335544795;
  isc_sql_dialect_datatype_unsupport   : SLONG = 335544796;
  isc_svcnouser                        : SLONG = 335544797;
  isc_depend_on_uncommitted_rel        : SLONG = 335544798;
  isc_svc_name_missing                 : SLONG = 335544799;
  isc_too_many_contexts                : SLONG = 335544800;
  isc_datype_notsup                    : SLONG = 335544801;
  isc_dialect_reset_warning            : SLONG = 335544802;
  isc_dialect_not_changed              : SLONG = 335544803;
  isc_database_create_failed           : SLONG = 335544804;
  isc_inv_dialect_specified            : SLONG = 335544805;
  isc_valid_db_dialects                : SLONG = 335544806;
  isc_sqlwarn                          : SLONG = 335544807;
  isc_dtype_renamed                    : SLONG = 335544808;
  isc_extern_func_dir_error            : SLONG = 335544809;
  isc_date_range_exceeded              : SLONG = 335544810;
  isc_inv_client_dialect_specified     : SLONG = 335544811;
  isc_valid_client_dialects            : SLONG = 335544812;
  isc_optimizer_between_err            : SLONG = 335544813;
  isc_service_not_supported            : SLONG = 335544814;
  isc_generator_name                   : SLONG = 335544815;
  isc_udf_name                         : SLONG = 335544816;
  isc_bad_limit_param                  : SLONG = 335544817;
  isc_bad_skip_param                   : SLONG = 335544818;
  isc_io_32bit_exceeded_err            : SLONG = 335544819;
  isc_invalid_savepoint                : SLONG = 335544820;
  isc_dsql_column_pos_err              : SLONG = 335544821;
  isc_dsql_agg_where_err               : SLONG = 335544822;
  isc_dsql_agg_group_err               : SLONG = 335544823;
  isc_dsql_agg_column_err              : SLONG = 335544824;
  isc_dsql_agg_having_err              : SLONG = 335544825;
  isc_dsql_agg_nested_err              : SLONG = 335544826;
  isc_exec_sql_invalid_arg             : SLONG = 335544827;
  isc_exec_sql_invalid_req             : SLONG = 335544828;
  isc_exec_sql_invalid_var             : SLONG = 335544829;
  isc_exec_sql_max_call_exceeded       : SLONG = 335544830;
  isc_conf_access_denied               : SLONG = 335544831;
  isc_wrong_backup_state               : SLONG = 335544832;
  isc_wal_backup_err                   : SLONG = 335544833;
  isc_cursor_not_open                  : SLONG = 335544834;
  isc_bad_shutdown_mode                : SLONG = 335544835;
  isc_concat_overflow                  : SLONG = 335544836;
  isc_bad_substring_offset             : SLONG = 335544837;
  isc_foreign_key_target_doesnt_exist  : SLONG = 335544838;
  isc_foreign_key_references_present   : SLONG = 335544839;
  isc_no_update                        : SLONG = 335544840;
  isc_cursor_already_open              : SLONG = 335544841;
  isc_stack_trace                      : SLONG = 335544842;
  isc_ctx_var_not_found                : SLONG = 335544843;
  isc_ctx_namespace_invalid            : SLONG = 335544844;
  isc_ctx_too_big                      : SLONG = 335544845;
  isc_ctx_bad_argument                 : SLONG = 335544846;
  isc_identifier_too_long              : SLONG = 335544847;
  isc_except2                          : SLONG = 335544848;
  isc_malformed_string                 : SLONG = 335544849;
  isc_prc_out_param_mismatch           : SLONG = 335544850;
  isc_command_end_err2                 : SLONG = 335544851;
  isc_partner_idx_incompat_type        : SLONG = 335544852;
  isc_bad_substring_length             : SLONG = 335544853;
  isc_charset_not_installed            : SLONG = 335544854;
  isc_collation_not_installed          : SLONG = 335544855;
  isc_att_shutdown                     : SLONG = 335544856;
  isc_blobtoobig                       : SLONG = 335544857;
  isc_must_have_phys_field             : SLONG = 335544858;
  isc_invalid_time_precision           : SLONG = 335544859;
  isc_blob_convert_error               : SLONG = 335544860;
  isc_array_convert_error              : SLONG = 335544861;
  isc_record_lock_not_supp             : SLONG = 335544862;
  isc_partner_idx_not_found            : SLONG = 335544863;
  isc_tra_num_exc                      : SLONG = 335544864;
  isc_field_disappeared                : SLONG = 335544865;
  isc_met_wrong_gtt_scope              : SLONG = 335544866;
  isc_subtype_for_internal_use         : SLONG = 335544867;
  isc_illegal_prc_type                 : SLONG = 335544868;
  isc_invalid_sort_datatype            : SLONG = 335544869;
  isc_collation_name                   : SLONG = 335544870;
  isc_domain_name                      : SLONG = 335544871;
  isc_domnotdef                        : SLONG = 335544872;
  isc_array_max_dimensions             : SLONG = 335544873;
  isc_max_db_per_trans_allowed         : SLONG = 335544874;
  isc_bad_debug_format                 : SLONG = 335544875;
  isc_bad_proc_BLR                     : SLONG = 335544876;
  isc_key_too_big                      : SLONG = 335544877;
  isc_concurrent_transaction           : SLONG = 335544878;
  isc_not_valid_for_var                : SLONG = 335544879;
  isc_not_valid_for                    : SLONG = 335544880;
  isc_need_difference                  : SLONG = 335544881;
  isc_long_login                       : SLONG = 335544882;
  isc_fldnotdef2                       : SLONG = 335544883;
  isc_invalid_similar_pattern          : SLONG = 335544884;
  isc_bad_teb_form                     : SLONG = 335544885;
  isc_tpb_multiple_txn_isolation       : SLONG = 335544886;
  isc_tpb_reserv_before_table          : SLONG = 335544887;
  isc_tpb_multiple_spec                : SLONG = 335544888;
  isc_tpb_option_without_rc            : SLONG = 335544889;
  isc_tpb_conflicting_options          : SLONG = 335544890;
  isc_tpb_reserv_missing_tlen          : SLONG = 335544891;
  isc_tpb_reserv_long_tlen             : SLONG = 335544892;
  isc_tpb_reserv_missing_tname         : SLONG = 335544893;
  isc_tpb_reserv_corrup_tlen           : SLONG = 335544894;
  isc_tpb_reserv_null_tlen             : SLONG = 335544895;
  isc_tpb_reserv_relnotfound           : SLONG = 335544896;
  isc_tpb_reserv_baserelnotfound       : SLONG = 335544897;
  isc_tpb_missing_len                  : SLONG = 335544898;
  isc_tpb_missing_value                : SLONG = 335544899;
  isc_tpb_corrupt_len                  : SLONG = 335544900;
  isc_tpb_null_len                     : SLONG = 335544901;
  isc_tpb_overflow_len                 : SLONG = 335544902;
  isc_tpb_invalid_value                : SLONG = 335544903;
  isc_tpb_reserv_stronger_wng          : SLONG = 335544904;
  isc_tpb_reserv_stronger              : SLONG = 335544905;
  isc_tpb_reserv_max_recursion         : SLONG = 335544906;
  isc_tpb_reserv_virtualtbl            : SLONG = 335544907;
  isc_tpb_reserv_systbl                : SLONG = 335544908;
  isc_tpb_reserv_temptbl               : SLONG = 335544909;
  isc_tpb_readtxn_after_writelock      : SLONG = 335544910;
  isc_tpb_writelock_after_readtxn      : SLONG = 335544911;
  isc_time_range_exceeded              : SLONG = 335544912;
  isc_datetime_range_exceeded          : SLONG = 335544913;
  isc_string_truncation                : SLONG = 335544914;
  isc_blob_truncation                  : SLONG = 335544915;
  isc_numeric_out_of_range             : SLONG = 335544916;
  isc_shutdown_timeout                 : SLONG = 335544917;
  isc_att_handle_busy                  : SLONG = 335544918;
  isc_bad_udf_freeit                   : SLONG = 335544919;
  isc_eds_provider_not_found           : SLONG = 335544920;
  isc_eds_connection                   : SLONG = 335544921;
  isc_eds_preprocess                   : SLONG = 335544922;
  isc_eds_stmt_expected                : SLONG = 335544923;
  isc_eds_prm_name_expected            : SLONG = 335544924;
  isc_eds_unclosed_comment             : SLONG = 335544925;
  isc_eds_statement                    : SLONG = 335544926;
  isc_eds_input_prm_mismatch           : SLONG = 335544927;
  isc_eds_output_prm_mismatch          : SLONG = 335544928;
  isc_eds_input_prm_not_set            : SLONG = 335544929;
  isc_too_big_blr                      : SLONG = 335544930;
  isc_montabexh                        : SLONG = 335544931;
  isc_modnotfound                      : SLONG = 335544932;
  isc_nothing_to_cancel                : SLONG = 335544933;
  isc_ibutil_not_loaded                : SLONG = 335544934;
  isc_circular_computed                : SLONG = 335544935;
  isc_psw_db_error                     : SLONG = 335544936;
  isc_invalid_type_datetime_op         : SLONG = 335544937;
  isc_onlycan_add_timetodate           : SLONG = 335544938;
  isc_onlycan_add_datetotime           : SLONG = 335544939;
  isc_onlycansub_tstampfromtstamp      : SLONG = 335544940;
  isc_onlyoneop_mustbe_tstamp          : SLONG = 335544941;
  isc_invalid_extractpart_time         : SLONG = 335544942;
  isc_invalid_extractpart_date         : SLONG = 335544943;
  isc_invalidarg_extract               : SLONG = 335544944;
  isc_sysf_argmustbe_exact             : SLONG = 335544945;
  isc_sysf_argmustbe_exact_or_fp       : SLONG = 335544946;
  isc_sysf_argviolates_uuidtype        : SLONG = 335544947;
  isc_sysf_argviolates_uuidlen         : SLONG = 335544948;
  isc_sysf_argviolates_uuidfmt         : SLONG = 335544949;
  isc_sysf_argviolates_guidigits       : SLONG = 335544950;
  isc_sysf_invalid_addpart_time        : SLONG = 335544951;
  isc_sysf_invalid_add_datetime        : SLONG = 335544952;
  isc_sysf_invalid_addpart_dtime       : SLONG = 335544953;
  isc_sysf_invalid_add_dtime_rc        : SLONG = 335544954;
  isc_sysf_invalid_diff_dtime          : SLONG = 335544955;
  isc_sysf_invalid_timediff            : SLONG = 335544956;
  isc_sysf_invalid_tstamptimediff      : SLONG = 335544957;
  isc_sysf_invalid_datetimediff        : SLONG = 335544958;
  isc_sysf_invalid_diffpart            : SLONG = 335544959;
  isc_sysf_argmustbe_positive          : SLONG = 335544960;
  isc_sysf_basemustbe_positive         : SLONG = 335544961;
  isc_sysf_argnmustbe_nonneg           : SLONG = 335544962;
  isc_sysf_argnmustbe_positive         : SLONG = 335544963;
  isc_sysf_invalid_zeropowneg          : SLONG = 335544964;
  isc_sysf_invalid_negpowfp            : SLONG = 335544965;
  isc_sysf_invalid_scale               : SLONG = 335544966;
  isc_sysf_argmustbe_nonneg            : SLONG = 335544967;
  isc_sysf_binuuid_mustbe_str          : SLONG = 335544968;
  isc_sysf_binuuid_wrongsize           : SLONG = 335544969;
  isc_missing_required_spb             : SLONG = 335544970;
  isc_net_server_shutdown              : SLONG = 335544971;
  isc_bad_conn_str                     : SLONG = 335544972;
  isc_bad_epb_form                     : SLONG = 335544973;
  isc_no_threads                       : SLONG = 335544974;
  isc_net_event_connect_timeout        : SLONG = 335544975;
  isc_sysf_argmustbe_nonzero           : SLONG = 335544976;
  isc_sysf_argmustbe_range_inc1_1      : SLONG = 335544977;
  isc_sysf_argmustbe_gteq_one          : SLONG = 335544978;
  isc_sysf_argmustbe_range_exc1_1      : SLONG = 335544979;
  isc_internal_rejected_params         : SLONG = 335544980;
  isc_sysf_fp_overflow                 : SLONG = 335544981;
  isc_udf_fp_overflow                  : SLONG = 335544982;
  isc_udf_fp_nan                       : SLONG = 335544983;
  isc_instance_conflict                : SLONG = 335544984;
  isc_out_of_temp_space                : SLONG = 335544985;
  isc_eds_expl_tran_ctrl               : SLONG = 335544986;
  isc_no_trusted_spb                   : SLONG = 335544987;
  isc_async_active                     : SLONG = 335545017;
  isc_gfix_db_name                     : SLONG = 335740929;
  isc_gfix_invalid_sw                  : SLONG = 335740930;
  isc_gfix_incmp_sw                    : SLONG = 335740932;
  isc_gfix_replay_req                  : SLONG = 335740933;
  isc_gfix_pgbuf_req                   : SLONG = 335740934;
  isc_gfix_val_req                     : SLONG = 335740935;
  isc_gfix_pval_req                    : SLONG = 335740936;
  isc_gfix_trn_req                     : SLONG = 335740937;
  isc_gfix_full_req                    : SLONG = 335740940;
  isc_gfix_usrname_req                 : SLONG = 335740941;
  isc_gfix_pass_req                    : SLONG = 335740942;
  isc_gfix_subs_name                   : SLONG = 335740943;
  isc_gfix_wal_req                     : SLONG = 335740944;
  isc_gfix_sec_req                     : SLONG = 335740945;
  isc_gfix_nval_req                    : SLONG = 335740946;
  isc_gfix_type_shut                   : SLONG = 335740947;
  isc_gfix_retry                       : SLONG = 335740948;
  isc_gfix_retry_db                    : SLONG = 335740951;
  isc_gfix_exceed_max                  : SLONG = 335740991;
  isc_gfix_corrupt_pool                : SLONG = 335740992;
  isc_gfix_mem_exhausted               : SLONG = 335740993;
  isc_gfix_bad_pool                    : SLONG = 335740994;
  isc_gfix_trn_not_valid               : SLONG = 335740995;
  isc_gfix_unexp_eoi                   : SLONG = 335741012;
  isc_gfix_recon_fail                  : SLONG = 335741018;
  isc_gfix_trn_unknown                 : SLONG = 335741036;
  isc_gfix_mode_req                    : SLONG = 335741038;
  isc_gfix_pzval_req                   : SLONG = 335741042;
  isc_dsql_dbkey_from_non_table        : SLONG = 336003074;
  isc_dsql_transitional_numeric        : SLONG = 336003075;
  isc_dsql_dialect_warning_expr        : SLONG = 336003076;
  isc_sql_db_dialect_dtype_unsupport   : SLONG = 336003077;
  isc_isc_sql_dialect_conflict_num     : SLONG = 336003079;
  isc_dsql_warning_number_ambiguous    : SLONG = 336003080;
  isc_dsql_warning_number_ambiguous1   : SLONG = 336003081;
  isc_dsql_warn_precision_ambiguous    : SLONG = 336003082;
  isc_dsql_warn_precision_ambiguous1   : SLONG = 336003083;
  isc_dsql_warn_precision_ambiguous2   : SLONG = 336003084;
  isc_dsql_ambiguous_field_name        : SLONG = 336003085;
  isc_dsql_udf_return_pos_err          : SLONG = 336003086;
  isc_dsql_invalid_label               : SLONG = 336003087;
  isc_dsql_datatypes_not_comparable    : SLONG = 336003088;
  isc_dsql_cursor_invalid              : SLONG = 336003089;
  isc_dsql_cursor_redefined            : SLONG = 336003090;
  isc_dsql_cursor_not_found            : SLONG = 336003091;
  isc_dsql_cursor_exists               : SLONG = 336003092;
  isc_dsql_cursor_rel_ambiguous        : SLONG = 336003093;
  isc_dsql_cursor_rel_not_found        : SLONG = 336003094;
  isc_dsql_cursor_not_open             : SLONG = 336003095;
  isc_dsql_type_not_supp_ext_tab       : SLONG = 336003096;
  isc_dsql_feature_not_supported_ods   : SLONG = 336003097;
  isc_primary_key_required             : SLONG = 336003098;
  isc_upd_ins_doesnt_match_pk          : SLONG = 336003099;
  isc_upd_ins_doesnt_match_matching    : SLONG = 336003100;
  isc_upd_ins_with_complex_view        : SLONG = 336003101;
  isc_dsql_incompatible_trigger_type   : SLONG = 336003102;
  isc_dsql_db_trigger_type_cant_change : SLONG = 336003103;
  isc_dyn_dup_table                    : SLONG = 336068740;
  isc_dyn_column_does_not_exist        : SLONG = 336068784;
  isc_dyn_role_does_not_exist          : SLONG = 336068796;
  isc_dyn_no_grant_admin_opt           : SLONG = 336068797;
  isc_dyn_user_not_role_member         : SLONG = 336068798;
  isc_dyn_delete_role_failed           : SLONG = 336068799;
  isc_dyn_grant_role_to_user           : SLONG = 336068800;
  isc_dyn_inv_sql_role_name            : SLONG = 336068801;
  isc_dyn_dup_sql_role                 : SLONG = 336068802;
  isc_dyn_kywd_spec_for_role           : SLONG = 336068803;
  isc_dyn_roles_not_supported          : SLONG = 336068804;
  isc_dyn_domain_name_exists           : SLONG = 336068812;
  isc_dyn_field_name_exists            : SLONG = 336068813;
  isc_dyn_dependency_exists            : SLONG = 336068814;
  isc_dyn_dtype_invalid                : SLONG = 336068815;
  isc_dyn_char_fld_too_small           : SLONG = 336068816;
  isc_dyn_invalid_dtype_conversion     : SLONG = 336068817;
  isc_dyn_dtype_conv_invalid           : SLONG = 336068818;
  isc_dyn_zero_len_id                  : SLONG = 336068820;
  isc_max_coll_per_charset             : SLONG = 336068829;
  isc_invalid_coll_attr                : SLONG = 336068830;
  isc_dyn_wrong_gtt_scope              : SLONG = 336068840;
  isc_dyn_scale_too_big                : SLONG = 336068852;
  isc_dyn_precision_too_small          : SLONG = 336068853;
  isc_dyn_miss_priv_warning            : SLONG = 336068855;
  isc_dyn_ods_not_supp_feature         : SLONG = 336068856;
  isc_dyn_cannot_addrem_computed       : SLONG = 336068857;
  isc_dyn_no_empty_pw                  : SLONG = 336068858;
  isc_dyn_dup_index                    : SLONG = 336068859;
  isc_gbak_unknown_switch              : SLONG = 336330753;
  isc_gbak_page_size_missing           : SLONG = 336330754;
  isc_gbak_page_size_toobig            : SLONG = 336330755;
  isc_gbak_redir_ouput_missing         : SLONG = 336330756;
  isc_gbak_switches_conflict           : SLONG = 336330757;
  isc_gbak_unknown_device              : SLONG = 336330758;
  isc_gbak_no_protection               : SLONG = 336330759;
  isc_gbak_page_size_not_allowed       : SLONG = 336330760;
  isc_gbak_multi_source_dest           : SLONG = 336330761;
  isc_gbak_filename_missing            : SLONG = 336330762;
  isc_gbak_dup_inout_names             : SLONG = 336330763;
  isc_gbak_inv_page_size               : SLONG = 336330764;
  isc_gbak_db_specified                : SLONG = 336330765;
  isc_gbak_db_exists                   : SLONG = 336330766;
  isc_gbak_unk_device                  : SLONG = 336330767;
  isc_gbak_blob_info_failed            : SLONG = 336330772;
  isc_gbak_unk_blob_item               : SLONG = 336330773;
  isc_gbak_get_seg_failed              : SLONG = 336330774;
  isc_gbak_close_blob_failed           : SLONG = 336330775;
  isc_gbak_open_blob_failed            : SLONG = 336330776;
  isc_gbak_put_blr_gen_id_failed       : SLONG = 336330777;
  isc_gbak_unk_type                    : SLONG = 336330778;
  isc_gbak_comp_req_failed             : SLONG = 336330779;
  isc_gbak_start_req_failed            : SLONG = 336330780;
  isc_gbak_rec_failed                  : SLONG = 336330781;
  isc_gbak_rel_req_failed              : SLONG = 336330782;
  isc_gbak_db_info_failed              : SLONG = 336330783;
  isc_gbak_no_db_desc                  : SLONG = 336330784;
  isc_gbak_db_create_failed            : SLONG = 336330785;
  isc_gbak_decomp_len_error            : SLONG = 336330786;
  isc_gbak_tbl_missing                 : SLONG = 336330787;
  isc_gbak_blob_col_missing            : SLONG = 336330788;
  isc_gbak_create_blob_failed          : SLONG = 336330789;
  isc_gbak_put_seg_failed              : SLONG = 336330790;
  isc_gbak_rec_len_exp                 : SLONG = 336330791;
  isc_gbak_inv_rec_len                 : SLONG = 336330792;
  isc_gbak_exp_data_type               : SLONG = 336330793;
  isc_gbak_gen_id_failed               : SLONG = 336330794;
  isc_gbak_unk_rec_type                : SLONG = 336330795;
  isc_gbak_inv_bkup_ver                : SLONG = 336330796;
  isc_gbak_missing_bkup_desc           : SLONG = 336330797;
  isc_gbak_string_trunc                : SLONG = 336330798;
  isc_gbak_cant_rest_record            : SLONG = 336330799;
  isc_gbak_send_failed                 : SLONG = 336330800;
  isc_gbak_no_tbl_name                 : SLONG = 336330801;
  isc_gbak_unexp_eof                   : SLONG = 336330802;
  isc_gbak_db_format_too_old           : SLONG = 336330803;
  isc_gbak_inv_array_dim               : SLONG = 336330804;
  isc_gbak_xdr_len_expected            : SLONG = 336330807;
  isc_gbak_open_bkup_error             : SLONG = 336330817;
  isc_gbak_open_error                  : SLONG = 336330818;
  isc_gbak_missing_block_fac           : SLONG = 336330934;
  isc_gbak_inv_block_fac               : SLONG = 336330935;
  isc_gbak_block_fac_specified         : SLONG = 336330936;
  isc_gbak_missing_username            : SLONG = 336330940;
  isc_gbak_missing_password            : SLONG = 336330941;
  isc_gbak_missing_skipped_bytes       : SLONG = 336330952;
  isc_gbak_inv_skipped_bytes           : SLONG = 336330953;
  isc_gbak_err_restore_charset         : SLONG = 336330965;
  isc_gbak_err_restore_collation       : SLONG = 336330967;
  isc_gbak_read_error                  : SLONG = 336330972;
  isc_gbak_write_error                 : SLONG = 336330973;
  isc_gbak_db_in_use                   : SLONG = 336330985;
  isc_gbak_sysmemex                    : SLONG = 336330990;
  isc_gbak_restore_role_failed         : SLONG = 336331002;
  isc_gbak_role_op_missing             : SLONG = 336331005;
  isc_gbak_page_buffers_missing        : SLONG = 336331010;
  isc_gbak_page_buffers_wrong_param    : SLONG = 336331011;
  isc_gbak_page_buffers_restore        : SLONG = 336331012;
  isc_gbak_inv_size                    : SLONG = 336331014;
  isc_gbak_file_outof_sequence         : SLONG = 336331015;
  isc_gbak_join_file_missing           : SLONG = 336331016;
  isc_gbak_stdin_not_supptd            : SLONG = 336331017;
  isc_gbak_stdout_not_supptd           : SLONG = 336331018;
  isc_gbak_bkup_corrupt                : SLONG = 336331019;
  isc_gbak_unk_db_file_spec            : SLONG = 336331020;
  isc_gbak_hdr_write_failed            : SLONG = 336331021;
  isc_gbak_disk_space_ex               : SLONG = 336331022;
  isc_gbak_size_lt_min                 : SLONG = 336331023;
  isc_gbak_svc_name_missing            : SLONG = 336331025;
  isc_gbak_not_ownr                    : SLONG = 336331026;
  isc_gbak_mode_req                    : SLONG = 336331031;
  isc_gbak_just_data                   : SLONG = 336331033;
  isc_gbak_data_only                   : SLONG = 336331034;
  isc_gbak_invalid_metadata            : SLONG = 336331093;
  isc_gbak_invalid_data                : SLONG = 336331094;
  isc_dsql_too_old_ods                 : SLONG = 336397205;
  isc_dsql_table_not_found             : SLONG = 336397206;
  isc_dsql_view_not_found              : SLONG = 336397207;
  isc_dsql_line_col_error              : SLONG = 336397208;
  isc_dsql_unknown_pos                 : SLONG = 336397209;
  isc_dsql_no_dup_name                 : SLONG = 336397210;
  isc_dsql_too_many_values             : SLONG = 336397211;
  isc_dsql_no_array_computed           : SLONG = 336397212;
  isc_dsql_implicit_domain_name        : SLONG = 336397213;
  isc_dsql_only_can_subscript_array    : SLONG = 336397214;
  isc_dsql_max_sort_items              : SLONG = 336397215;
  isc_dsql_max_group_items             : SLONG = 336397216;
  isc_dsql_conflicting_sort_field      : SLONG = 336397217;
  isc_dsql_derived_table_more_columns  : SLONG = 336397218;
  isc_dsql_derived_table_less_columns  : SLONG = 336397219;
  isc_dsql_derived_field_unnamed       : SLONG = 336397220;
  isc_dsql_derived_field_dup_name      : SLONG = 336397221;
  isc_dsql_derived_alias_select        : SLONG = 336397222;
  isc_dsql_derived_alias_field         : SLONG = 336397223;
  isc_dsql_auto_field_bad_pos          : SLONG = 336397224;
  isc_dsql_cte_wrong_reference         : SLONG = 336397225;
  isc_dsql_cte_cycle                   : SLONG = 336397226;
  isc_dsql_cte_outer_join              : SLONG = 336397227;
  isc_dsql_cte_mult_references         : SLONG = 336397228;
  isc_dsql_cte_not_a_union             : SLONG = 336397229;
  isc_dsql_cte_nonrecurs_after_recurs  : SLONG = 336397230;
  isc_dsql_cte_wrong_clause            : SLONG = 336397231;
  isc_dsql_cte_union_all               : SLONG = 336397232;
  isc_dsql_cte_miss_nonrecursive       : SLONG = 336397233;
  isc_dsql_cte_nested_with             : SLONG = 336397234;
  isc_dsql_col_more_than_once_using    : SLONG = 336397235;
  isc_dsql_unsupp_feature_dialect      : SLONG = 336397236;
  isc_dsql_cte_not_used                : SLONG = 336397237;
  isc_dsql_col_more_than_once_view     : SLONG = 336397238;
  isc_dsql_unsupported_in_auto_trans   : SLONG = 336397239;
  isc_dsql_eval_unknode                : SLONG = 336397240;
  isc_dsql_agg_wrongarg                : SLONG = 336397241;
  isc_dsql_agg2_wrongarg               : SLONG = 336397242;
  isc_dsql_nodateortime_pm_string      : SLONG = 336397243;
  isc_dsql_invalid_datetime_subtract   : SLONG = 336397244;
  isc_dsql_invalid_dateortime_add      : SLONG = 336397245;
  isc_dsql_invalid_type_minus_date     : SLONG = 336397246;
  isc_dsql_nostring_addsub_dial3       : SLONG = 336397247;
  isc_dsql_invalid_type_addsub_dial3   : SLONG = 336397248;
  isc_dsql_invalid_type_multip_dial1   : SLONG = 336397249;
  isc_dsql_nostring_multip_dial3       : SLONG = 336397250;
  isc_dsql_invalid_type_multip_dial3   : SLONG = 336397251;
  isc_dsql_mustuse_numeric_div_dial1   : SLONG = 336397252;
  isc_dsql_nostring_div_dial3          : SLONG = 336397253;
  isc_dsql_invalid_type_div_dial3      : SLONG = 336397254;
  isc_dsql_nostring_neg_dial3          : SLONG = 336397255;
  isc_dsql_invalid_type_neg            : SLONG = 336397256;
  isc_dsql_max_distinct_items          : SLONG = 336397257;
  isc_gsec_cant_open_db                : SLONG = 336723983;
  isc_gsec_switches_error              : SLONG = 336723984;
  isc_gsec_no_op_spec                  : SLONG = 336723985;
  isc_gsec_no_usr_name                 : SLONG = 336723986;
  isc_gsec_err_add                     : SLONG = 336723987;
  isc_gsec_err_modify                  : SLONG = 336723988;
  isc_gsec_err_find_mod                : SLONG = 336723989;
  isc_gsec_err_rec_not_found           : SLONG = 336723990;
  isc_gsec_err_delete                  : SLONG = 336723991;
  isc_gsec_err_find_del                : SLONG = 336723992;
  isc_gsec_err_find_disp               : SLONG = 336723996;
  isc_gsec_inv_param                   : SLONG = 336723997;
  isc_gsec_op_specified                : SLONG = 336723998;
  isc_gsec_pw_specified                : SLONG = 336723999;
  isc_gsec_uid_specified               : SLONG = 336724000;
  isc_gsec_gid_specified               : SLONG = 336724001;
  isc_gsec_proj_specified              : SLONG = 336724002;
  isc_gsec_org_specified               : SLONG = 336724003;
  isc_gsec_fname_specified             : SLONG = 336724004;
  isc_gsec_mname_specified             : SLONG = 336724005;
  isc_gsec_lname_specified             : SLONG = 336724006;
  isc_gsec_inv_switch                  : SLONG = 336724008;
  isc_gsec_amb_switch                  : SLONG = 336724009;
  isc_gsec_no_op_specified             : SLONG = 336724010;
  isc_gsec_params_not_allowed          : SLONG = 336724011;
  isc_gsec_incompat_switch             : SLONG = 336724012;
  isc_gsec_inv_username                : SLONG = 336724044;
  isc_gsec_inv_pw_length               : SLONG = 336724045;
  isc_gsec_db_specified                : SLONG = 336724046;
  isc_gsec_db_admin_specified          : SLONG = 336724047;
  isc_gsec_db_admin_pw_specified       : SLONG = 336724048;
  isc_gsec_sql_role_specified          : SLONG = 336724049;
  isc_license_no_file                  : SLONG = 336789504;
  isc_license_op_specified             : SLONG = 336789523;
  isc_license_op_missing               : SLONG = 336789524;
  isc_license_inv_switch               : SLONG = 336789525;
  isc_license_inv_switch_combo         : SLONG = 336789526;
  isc_license_inv_op_combo             : SLONG = 336789527;
  isc_license_amb_switch               : SLONG = 336789528;
  isc_license_inv_parameter            : SLONG = 336789529;
  isc_license_param_specified          : SLONG = 336789530;
  isc_license_param_req                : SLONG = 336789531;
  isc_license_syntx_error              : SLONG = 336789532;
  isc_license_dup_id                   : SLONG = 336789534;
  isc_license_inv_id_key               : SLONG = 336789535;
  isc_license_err_remove               : SLONG = 336789536;
  isc_license_err_update               : SLONG = 336789537;
  isc_license_err_convert              : SLONG = 336789538;
  isc_license_err_unk                  : SLONG = 336789539;
  isc_license_svc_err_add              : SLONG = 336789540;
  isc_license_svc_err_remove           : SLONG = 336789541;
  isc_license_eval_exists              : SLONG = 336789563;
  isc_gstat_unknown_switch             : SLONG = 336920577;
  isc_gstat_retry                      : SLONG = 336920578;
  isc_gstat_wrong_ods                  : SLONG = 336920579;
  isc_gstat_unexpected_eof             : SLONG = 336920580;
  isc_gstat_open_err                   : SLONG = 336920605;
  isc_gstat_read_err                   : SLONG = 336920606;
  isc_gstat_sysmemex                   : SLONG = 336920607;
  isc_fbsvcmgr_bad_am                  : SLONG = 336986113;
  isc_fbsvcmgr_bad_wm                  : SLONG = 336986114;
  isc_fbsvcmgr_bad_rs                  : SLONG = 336986115;
  isc_fbsvcmgr_info_err                : SLONG = 336986116;
  isc_fbsvcmgr_query_err               : SLONG = 336986117;
  isc_fbsvcmgr_switch_unknown          : SLONG = 336986118;
  isc_fbsvcmgr_bad_sm                  : SLONG = 336986159;
  isc_fbsvcmgr_fp_open                 : SLONG = 336986160;
  isc_fbsvcmgr_fp_read                 : SLONG = 336986161;
  isc_fbsvcmgr_fp_empty                : SLONG = 336986162;
  isc_fbsvcmgr_bad_arg                 : SLONG = 336986164;
  isc_utl_trusted_switch               : SLONG = 337051649;
  isc_err_max                          : SLONG = 964;



const
  FB_Version = 25;

  ISC_TRUE = 1;
  ISC_FALSE = 0;

  DSQL_close = 1;
  DSQL_drop = 2;
  DSQL_unprepare = 4;  

  METADATALENGTH = 68;

  {$ifdef MSWINDOWS}
  FBClient_LIB = 'fbclient.dll';
  FBEmbed_LIB = 'fbembed.dll';

  REGSTR_PATH_FB='Software\Firebird Project\Firebird Server';

  {$else}
  FBClient_LIB = 'fbclient.so';//TODO: Fix the name please
  FBEmbed_LIB = 'fbembed.so';
  {$endif}

  FB_DefaultInstance = 'DefaultInstance';

type
  //typedef int (*FB_SHUTDOWN_CALLBACK)(const int reason, const int mask, void* arg);
  TFB_SHUTDOWN_CALLBACK = function(Reason: Integer; Mask: Integer; Arg: Pointer): Integer;

  (**********************************)
  (** Firebird Handle Definitions **)
  (**********************************)
  TISC_ATT_HANDLE = FB_API_HANDLE;
  PISC_ATT_HANDLE = ^TISC_ATT_HANDLE;
  TISC_BLOB_HANDLE = FB_API_HANDLE;
  PISC_BLOB_HANDLE = ^TISC_BLOB_HANDLE;
  TISC_DB_HANDLE = FB_API_HANDLE;
  PISC_DB_HANDLE = ^TISC_DB_HANDLE;
  TISC_FORM_HANDLE = FB_API_HANDLE;
  PISC_FORM_HANDLE = ^TISC_FORM_HANDLE;
  TISC_REQ_HANDLE = FB_API_HANDLE;
  PISC_REQ_HANDLE = ^TISC_REQ_HANDLE;
  TISC_STMT_HANDLE = FB_API_HANDLE;
  PISC_STMT_HANDLE = ^TISC_STMT_HANDLE;
  TISC_SVC_HANDLE = FB_API_HANDLE;
  PISC_SVC_HANDLE = ^TISC_SVC_HANDLE;
  TISC_TR_HANDLE = FB_API_HANDLE;
  PISC_TR_HANDLE = ^TISC_TR_HANDLE;
  TISC_WIN_HANDLE = FB_API_HANDLE;
  PISC_WIN_HANDLE = ^TISC_WIN_HANDLE;
  TISC_CALLBACK = procedure;
  ISC_SVC_HANDLE = ISC_LONG;
               
  TISC_PRINT_CALLBACK = procedure (P: Pointer; param2:ISC_SHORT; param3:PByte);
  TISC_VERSION_CALLBACK = procedure (P: Pointer; Buffer:PByte);
  TISC_EVENT_CALLBACK = procedure  (P: Pointer; param2:ISC_USHORT; param3:PISC_UCHAR);

  (*******************************************************************)
  (* Time & Date Support                                             *)
  (*******************************************************************)
const
  TIME_SECONDS_PRECISION = 10000;
  TIME_SECONDS_PRECISION_SCALE = -4;

type
  ISC_DATE = Long;
  PISC_DATE = ^ISC_DATE;
  ISC_TIME = ULong;
  PISC_TIME = ^ISC_TIME;
  TISC_TIMESTAMP = record
    timestamp_date: ISC_DATE;
    timestamp_time: ISC_TIME;
  end;
  PISC_TIMESTAMP = ^TISC_TIMESTAMP;

  (*********************************************************************)
  (** Blob id structure                                               **)
  (*********************************************************************)
  TGDS_QUAD = record
    gds_quad_high: ISC_LONG;
    gds_quad_low: UISC_LONG;
  end;

  TGDS__QUAD = TGDS_QUAD;
  TISC_QUAD = TGDS_QUAD;
  PGDS_QUAD = ^TGDS_QUAD;
  PGDS__QUAD = ^TGDS__QUAD;
  PISC_QUAD = ^TISC_QUAD;

  TISC_ARRAY_BOUND = record
    array_bound_lower: Short;
    array_bound_upper: Short;
  end;

  PISC_ARRAY_BOUND = ^TISC_ARRAY_BOUND;
  
  TISC_ARRAY_DESC = record
    array_desc_dtype: UChar;
    array_desc_scale: Char;
    array_desc_length: UShort;
    array_desc_field_name: array[0..31] of Char;
    array_desc_relation_name: array[0..31] of Char;
    array_desc_dimensions: Short;
    array_desc_flags: Short;
    array_desc_bounds: array[0..15] of TISC_ARRAY_BOUND;
  end; // TISC_ARRAY_DESC
  PISC_ARRAY_DESC = ^TISC_ARRAY_DESC;

type
  TISC_BLOB_DESC = record
    blob_desc_subtype: Short;
    blob_desc_charset: Short;
    blob_desc_segment_size: Short;
    blob_desc_field_name: array[0..31] of UChar;
    blob_desc_relation_name: array[0..31] of UChar;
  end; // TISC_BLOB_DESC
  PISC_BLOB_DESC = ^TISC_BLOB_DESC;

type
  (*****************************)
  (** Blob control structure  **)
  (*****************************)
  TISC_BLOB_CTL_SOURCE_FUNCTION = function: ISC_STATUS; // ISC_FAR
  PISC_BLOB_CTL = ^TISC_BLOB_CTL; // ISC_FAR
  TISC_BLOB_CTL = record
    (** Source filter **)
    ctl_source: TISC_BLOB_CTL_SOURCE_FUNCTION;
    (** Argument to pass to source filter **)
    ctl_source_handle: PISC_BLOB_CTL;
    ctl_to_sub_type: Short; (** Target type **)
    ctl_from_sub_type: Short; (** Source type **)
    ctl_buffer_length: UShort; (** Length of buffer **)
    ctl_segment_length: UShort; (** Length of current segment **)
    ctl_bpb_length: UShort; (** Length of blob parameter **)
      (** block **)
    ctl_bpb: PByte; (** Address of blob parameter **)
      (** block **)
    ctl_buffer: PUChar; (** Address of segment buffer **)
    ctl_max_segment: ISC_LONG; (** Length of longest segment **)
    ctl_number_segments: ISC_LONG; (** Total number of segments **)
    ctl_total_length: ISC_LONG; (** Total length of blob **)
    ctl_status: PISC_STATUS; (** Address of status vector **)
    ctl_data: array[0..7] of long; (** Application specific data **)
  end;
  
  (*****************************)
  (** Blob stream definitions **)
  (*****************************)
  TBSTREAM = record
    bstr_blob: PVoid; (** Blob handle **)
    bstr_buffer: PByte; (** Address of buffer **)
    bstr_ptr: PByte; (** Next character **)
    bstr_length: Short; (** Length of buffer **)
    bstr_cnt: Short; (** Characters in buffer **)
    bstr_mode: Char; (** (mode) ? OUTPUT : INPUT **)
  end;
  PBSTREAM = ^TBSTREAM;
(* moved to FBExternal.pas

// Blob passing structure

// This enum applies to parameter "mode" in blob_lseek
  TBlob_LSeek_Mode = (blb_seek_relative = 1, blb_seek_from_tail = 2);
// This enum applies to the value returned by blob_get_segment */
  TBlob_Get_Result = (blb_got_fragment = -1, blb_got_eof = 0, blb_got_full_segment = 1);

  TBlob_get_segment = function (hnd:PVoid; buffer:PISC_UCHAR; buf_size:ISC_USHORT; result_len:PISC_USHORT):short;
  TBlob_put_segment = procedure (hnd:PVoid; const buffer:PISC_UCHAR; buf_size:ISC_USHORT);
  TBlob_lseek = function (hnd:PVoid; mode:ISC_USHORT; offset:ISC_LONG ):ISC_LONG;

  PBlobCallback = ^TBlobCallback;//zaher be shure
  TBlobCallback = record
    blob_get_segment: TBlob_get_segment;
    blob_handle:PVoid;

    blob_number_segments:ISC_LONG	;
    blob_max_segment:ISC_LONG	;
    blob_total_length:ISC_LONG	;

    blob_put_segment: TBlob_put_segment;
    blob_lseek : TBlob_lseek;
  end;
*)

(****************************************
 ****************************************
 * Older XSQLDA strucutre.
 * NOTE: This is kept only for backward
 * compatability. Please refrain from
 * using these old structures.
 * It is strongly  recomended
 * to use the newer SQLDA version
 * and related XSQLVAR structure.
 ****************************************)
  TXSQLVAR = record
    sqltype: Short; (** datatype of field **)
    sqlscale: Short; (** scale factor **)
    sqlsubtype: Short; (** datatype subtype - BLOBs **)
          (** & text types only **)
    sqllen: Short; (** length of data area **)
    sqldata: Pointer; (** address of data **)
    sqlind: PShort; (** address of indicator **)
                                             (** variable **)
    sqlname_length: Short; (** length of sqlname field **)
    (** name of field, name length + space for NULL **)
    sqlname: array[0..31] of Byte;
    relname_length: Short; (** length of relation name **)
    (** field's relation name + space for NULL **)
    relname: array[0..31] of Byte;
    ownname_length: Short; (** length of owner name **)
    (** relation's owner name + space for NULL **)
    ownname: array[0..31] of Byte;
    aliasname_length: Short; (** length of alias name **)
    (** relation's alias name + space for NULL **)
    aliasname: array[0..31] of Byte;
  end; // TXSQLVAR
  PXSQLVAR = ^TXSQLVAR;

  TXSQLDA = record
    version: Short; (** version of this XSQLDA **)
    (** XSQLDA name field **)
    sqldaid: array[0..7] of Byte;
    sqldabc: ISC_LONG; (** length in bytes of SQLDA **)
    sqln: Short; (** number of fields allocated **)
    sqld: Short; (** actual number of fields **)
    (** first field address **)
    sqlvar: array[0..0] of TXSQLVAR;
  end; // TXSQLDA
  PXSQLDA = ^TXSQLDA;

(*********************)
(** SQL definitions **)
(*********************)
const
  SQL_VARYING = 448;
  SQL_TEXT = 452;
  SQL_DOUBLE = 480;
  SQL_FLOAT = 482;
  SQL_LONG = 496;
  SQL_SHORT = 500;
  SQL_TIMESTAMP = 510;
  SQL_BLOB = 520;
  SQL_D_FLOAT = 530;
  SQL_ARRAY = 540;
  SQL_QUAD = 550;
  SQL_TYPE_TIME = 560;
  SQL_TYPE_DATE = 570;
  SQL_INT64 = 580;
  SQL_BOOLEAN = 590; //zaher huh
  SQL_NULL = 32766;

  SQLDA_VERSION1 = 1;
  SQLDA_VERSION2 = 2;
  
  SQL_DIALECT_V5 = 1;
  SQL_DIALECT_V6_TRANSITION = 2;
  SQL_DIALECT_V6 = 3;
  SQL_DIALECT_CURRENT = SQL_DIALECT_V6; 

type
(********************************************************)
(** This record type is for passing arguments to       **)
(** isc_start_transaction (See docs)                   **)
(********************************************************)
  TISC_START_TRANS = record
    db_handle: PISC_DB_HANDLE;
    tpb_length: UShort;
    tpb_address: PByte;
  end;

(********************************************************)
(** This record type is for passing arguments to       **)
(** isc_start_multiple (see docs)                      **)
(********************************************************)
  TISC_TEB = record
    db_handle: PISC_DB_HANDLE;
    tpb_length: Long;
    tpb_address: PByte;
  end;
  PISC_TEB = ^TISC_TEB;
  TISC_TEB_ARRAY = array[0..0] of TISC_TEB;
  PISC_TEB_ARRAY = ^TISC_TEB_ARRAY;

(*****************************)
(** OSRI database functions **)
(*****************************)

  Tisc_attach_database = function(status_vector: PISC_STATUS; db_name_length: Short; db_name: PByte;  db_handle: PISC_DB_HANDLE; parm_buffer_length: Short; parm_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_array_gen_sdl = function(status_vector: PISC_STATUS; isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort; isc_arg4: PByte; isc_arg5: PShort): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_array_get_slice = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC; dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_array_lookup_bounds = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PByte; descriptor: PISC_ARRAY_DESC): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_array_lookup_desc = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PByte; descriptor: PISC_ARRAY_DESC): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_array_set_desc = function(status_vector: PISC_STATUS; table_name: PByte; column_name: PByte; sql_dtype, sql_length, sql_dimensions: PShort; descriptor: PISC_ARRAY_DESC): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_array_put_slice = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC; source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_blob_default_desc = procedure(descriptor: PISC_BLOB_DESC; table_name: PUChar; column_name: PUChar); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_blob_gen_bpb = function(status_vector: PISC_STATUS; to_descriptor, from_descriptor: PISC_BLOB_DESC; bpb_buffer_length: UShort; bpb_buffer: PUChar; bpb_length: PUShort): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_blob_info = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short; item_list_buffer: PByte; result_buffer_length: Short; result_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_blob_lookup_desc = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; table_name, column_name: PByte; descriptor: PISC_BLOB_DESC; global: PUChar): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_blob_set_desc = function(status_vector: PISC_STATUS; table_name, column_name: PByte; subtype, charset, segment_size: Short; descriptor: PISC_BLOB_DESC): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_cancel_blob = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; Tisc_cancel_events = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_close_blob = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; Tisc_commit_retaining = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_commit_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; Tisc_create_blob = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_create_blob2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short; bpb_address: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_create_database = function(status_vector: PISC_STATUS; isc_arg2: Short; isc_arg3: PByte; db_handle: PISC_DB_HANDLE; isc_arg5: Short; isc_arg6: PByte; isc_arg7: Short): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_database_info = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short; item_list_buffer: PByte; result_buffer_length: Short; result_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_decode_date = procedure(ib_date: PISC_QUAD; tm_date: PCTimeStructure); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; Tisc_decode_sql_date = procedure(ib_date: PISC_DATE; tm_date: PCTimeStructure); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_decode_sql_time = procedure(ib_time: PISC_TIME; tm_date: PCTimeStructure); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_decode_timestamp = procedure(ib_timestamp: PISC_TIMESTAMP; tm_date: PCTimeStructure); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_detach_database = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_drop_database = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_allocate_statement = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_alloc_statement2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_describe = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_describe_bind = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_exec_immed2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: UShort; statement: PByte; dialect: UShort; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_execute = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_execute2 = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_execute_immediate = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: UShort; statement: PByte; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_fetch = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; Tisc_dsql_finish = function(db_handle: PISC_DB_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_free_statement = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; options: UShort): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_insert = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; arg3: UShort; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_prepare = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; length: UShort; statement: PByte; dialect: UShort; xsqlda: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_set_cursor_name = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; cursor_name: PByte; _type: UShort): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_sql_info = function(status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PByte; buffer_length: Short; buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_encode_date = procedure(tm_date: PCTimeStructure; ib_date: PISC_QUAD); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_encode_sql_date = procedure(tm_date: PCTimeStructure; ib_date: PISC_DATE); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_encode_sql_time = procedure(tm_date: PCTimeStructure; ib_time: PISC_TIME); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_encode_timestamp = procedure(tm_date: PCTimeStructure; ib_timestamp: PISC_TIMESTAMP); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_event_block = function(event_buffer: PPByte; result_buffer: PPByte; id_count: UShort; var event_list: array of PByte): ISC_LONG; cdecl;
  Tisc_event_block_a = function(event_buffer: PPByte; result_buffer: PPByte; id_count: UShort; event_list: PPByte): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_event_block_s = function(event_buffer: PPByte; result_buffer: PPByte; id_count: UShort; event_list: PPByte; arg1: PUShort): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_event_counts = procedure(status_vector: PISC_STATUS; buffer_length: Short; event_buffer: PByte; result_buffer: PByte); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_modify_dpb = function(dpb: PPByte; isc_arg2, isc_arg3: PShort; isc_arg4: UShort; isc_arg5: PByte; isc_arg6: Short): Int; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_free = function(isc_arg1: PByte): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_get_segment = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PUShort; seg_buffer_length: UShort; seg_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_get_slice = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PISC_QUAD; isc_arg5: Short; isc_arg6: PByte; isc_arg7: Short; isc_arg8: PISC_LONG; isc_arg9: ISC_LONG; isc_arg10: PVoid; isc_arg11: PISC_LONG): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_interprete = function(buffer: PByte; var status_vector: PISC_STATUS): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tfb_interpret = function(buffer: PByte; length:short; const status_vector: PPISC_STATUS): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_open_blob = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_open_blob2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short; bpb_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_prepare_transaction2 = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; msg_length: Short; msg: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_print_sqlerror = procedure(sqlcode: Short; status_vector: PISC_STATUS); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_print_status = function(status_vector: PISC_STATUS): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_put_segment = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: UShort; seg_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_put_slice = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PISC_QUAD; isc_arg5: Short; isc_arg6: PByte; isc_arg7: Short; isc_arg8: PISC_LONG; isc_arg9: ISC_LONG; isc_arg10: PVoid): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_que_events = function( status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short; event_buffer: PByte; event_function: TISC_EVENT_CALLBACK; event_function_arg: PVoid): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_release_savepoint = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; tran_name: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_rollback_retaining = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_rollback_savepoint = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; tran_name: PByte; Option: UShort): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_rollback_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_start_multiple = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; db_handle_count: Short; teb_vector_address: PISC_TEB): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_start_savepoint = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; tran_name: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_start_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; db_handle_count: Short; db_handle: PISC_DB_HANDLE; tpb_length: UShort; tpb_address: PByte): ISC_STATUS; cdecl;
  Tisc_sqlcode = function(status_vector: PISC_STATUS): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_sqlcode_s = function(status_vector: PISC_STATUS; arg1: PULong): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_sql_interprete = procedure(sqlcode: Short; buffer: PByte; buffer_length: Short); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_transaction_info = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; item_list_buffer_length: Short; item_list_buffer: PByte; result_buffer_length: Short; result_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_transact_request = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: UShort; isc_arg5: PByte; isc_arg6: UShort; isc_arg7: PByte; isc_arg8: UShort; isc_arg9: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_vax_integer = function(buffer: PByte; length: Short): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; Tisc_portable_integer = function(buffer: PByte; length: Short): ISC_INT64; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

(***************************************)
(** Security Functions and structures **)
(***************************************)

const
  sec_uid_spec           = $01;
  sec_gid_spec           = $02;
  sec_server_spec        = $04;
  sec_password_spec      = $08;
  sec_group_name_spec    = $10;
  sec_first_name_spec    = $20;
  sec_middle_name_spec   = $40;
  sec_last_name_spec     = $80;
  sec_dba_user_name_spec = $100;
  sec_dba_password_spec  = $200;

  sec_protocol_tcpip     = 1;
  sec_protocol_netbeui   = 2;
  sec_protocol_spx       = 3; // -- Deprecated Protocol. Declaration retained for compatibility
  sec_protocol_local     = 4;

type
  TUserSecData = record
    sec_flags: Short; (** which fields are specified **)
    uid: Int; (** the user's id **)
    gid: int; (** the user's group id **)
    protocol: Int; (** protocol to use for connection **)
    server: PByte; (** server to administer **)
    user_name: PByte; (** the user's name **)
    password: PByte; (** the user's password **)
    group_name: PByte; (** the group name **)
    first_name: PByte; (** the user's first name **)
    middle_name: PByte; (** the user's middle name **)
    last_name: PByte; (** the user's last name **)
    dba_user_name: PByte; (** the dba user name **)
    dba_password: PByte; (** the dba password **)
  end;
  PUserSecData = ^TUserSecData;

  Tisc_add_user = function(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_delete_user = function(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_modify_user = function(status_vector: PISC_STATUS; user_sec_data: PUserSecData): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

(************************************)
(**  Other OSRI functions          **)
(************************************)

  Tisc_compile_request = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; request_handle: PISC_REQ_HANDLE; isc_arg4: Short; isc_arg5: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_compile_request2 = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; request_handle: PISC_REQ_HANDLE; isc_arg4: Short; isc_arg5: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_ddl = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: Short; isc_arg5: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_prepare_transaction = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_receive = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; isc_arg3, isc_arg4: Short; isc_arg5: PVoid; isc_arg6: Short): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_receive2 = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; isc_arg3, isc_arg4: Short; isc_arg5: PVoid; isc_arg6, isc_arg7: Short; isc_arg8: Long): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_reconnect_transaction = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: Short; isc_arg5: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_release_request = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_request_info = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; isc_arg3: Short; isc_arg4: Short; isc_arg5: PByte; isc_arg6: Short; isc_arg7: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_seek_blob = function(status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE; isc_arg3: Short; isc_arg4: ISC_LONG; isc_arg5: PISC_LONG): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_send = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; isc_arg3, isc_arg4: Short; isc_arg5: PVoid; isc_arg6: Short): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_start_and_send = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4, isc_arg5: Short; isc_arg6: PVoid; isc_arg7: Short): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_start_request = function(status_vector: PISC_STATUS; request_handle: PISC_REQ_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: Short): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_unwind_request = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: Short): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_wait_for_event = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; length: Short; event_buffer, result_buffer: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

(*******************************)
(** Other Sql functions       **)
(*******************************)

  Tisc_close = function(status_vector: PISC_STATUS; isc_arg2: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_declare = function(status_vector: PISC_STATUS; isc_arg2, isc_arg3: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_describe = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_describe_bind = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_execute = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PByte; isc_arg4: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_execute_immediate = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PShort; isc_arg5: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_fetch = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_open = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PByte; isc_arg4: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_prepare = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PByte; isc_arg5: PShort; isc_arg6: PByte; isc_arg7: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

(***************************************)
(** Other Dynamic sql functions       **)
(***************************************)

  Tisc_dsql_execute_m = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; statement_handle: PISC_STMT_HANDLE; isc_arg4: UShort; isc_arg5: PByte; isc_arg6: UShort; isc_arg7: UShort; isc_arg8: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_execute2_m = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; statement_handle: PISC_STMT_HANDLE;
                                  isc_arg4: UShort; isc_arg5: PByte; isc_arg6: UShort; isc_arg7: UShort; isc_arg8: PByte; isc_arg9: UShort; isc_arg10: PByte;
                                  isc_arg11: UShort; isc_arg12: UShort; isc_arg13: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_execute_immediate_m = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
                                            isc_arg4: UShort; isc_arg5: PByte; isc_arg6: UShort; isc_arg7: UShort; isc_arg8: PByte; isc_arg9: UShort; isc_arg10: UShort;
                                            isc_arg11: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_exec_immed3_m = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
                                     isc_arg4: UShort; isc_arg5: PByte; isc_arg6: UShort; isc_arg7: UShort; isc_arg8: PByte; isc_arg9: UShort;
                                     isc_arg10: UShort; isc_arg11: PByte; isc_arg12: UShort; isc_arg13: PByte; isc_arg14: UShort;
                                     isc_arg15: UShort; isc_arg16: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_fetch_m = function(status_vector: PISC_STATUS; statement_handle: PISC_STMT_HANDLE; isc_arg3: UShort; isc_arg4: PByte; isc_arg5: UShort; isc_arg6: UShort; isc_arg7: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_insert_m = function(status_vector: PISC_STATUS; statement_handle: PISC_STMT_HANDLE; isc_arg3: UShort; isc_arg4: PByte; isc_arg5: UShort; isc_arg6: UShort; isc_arg7: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_prepare_m = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; statement_handle: PISC_STMT_HANDLE; isc_arg4: UShort; isc_arg5: PByte; isc_arg6: UShort; isc_arg7: UShort; isc_arg8: PByte; isc_arg9: UShort; isc_arg10: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_dsql_release = function(status_vector: PISC_STATUS; isc_arg2: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_close = function(status_vector: PISC_STATUS; isc_arg2: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_declare = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_describe = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: UShort; isc_arg4: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_describe_bind = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: UShort; isc_arg4: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_execute = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PByte; isc_arg4: UShort; isc_arg5: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_execute2 = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PByte; isc_arg4: UShort; isc_arg5: PXSQLDA; isc_arg6: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_execute_immed = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: UShort; isc_arg5: PByte; isc_arg6: UShort; isc_arg7: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_fetch = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: UShort; isc_arg4: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_fetch_a = function(status_vector: PISC_STATUS; isc_arg2: PInt; isc_arg3: PByte; isc_arg4: UShort; isc_arg5: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_length = procedure(arg1: PByte; arg2: PUShort); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; Tisc_embed_dsql_open = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PByte; isc_arg4: UShort; isc_arg5: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_open2 = function(status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE; isc_arg3: PByte; isc_arg4: UShort; isc_arg5: PXSQLDA; isc_arg6: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_insert = function(status_vector: PISC_STATUS; isc_arg2: PByte; isc_arg3: UShort; isc_arg4: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_prepare = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; isc_arg4: PByte; isc_arg5: UShort; isc_arg6: PByte; isc_arg7: UShort; isc_arg8: PXSQLDA): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_embed_dsql_release = function(status_vector: PISC_STATUS; isc_arg2: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

(********************************)
(** Other Blob functions       **)
(********************************)

  TBLOB_open = function(blob_handle: TISC_BLOB_HANDLE; isc_arg2: PByte; isc_arg3: int): PBSTREAM; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_put = function(isc_arg1: byte; isc_arg2: PBSTREAM): Int; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_close = function(isc_arg1: PBSTREAM): Int; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_get = function(isc_arg1: PBSTREAM): Int; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_display = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PByte): Int; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_dump = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PByte): Int; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_edit = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PByte): Int; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_load = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PByte): Int; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_text_dump = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PByte): Int; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBLOB_text_load = function(isc_arg1: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; isc_arg4: PByte): Int; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBopen = function(blob_id: PISC_QUAD; db_handle: TISC_DB_HANDLE; tran_handle: TISC_TR_HANDLE; mode: PByte): PBSTREAM; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  TBclose = function(Stream: PBSTREAM): PISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

(********************************)
(** Other Misc functions       **)
(********************************)

  Tisc_ftof = function(isc_arg1: PByte; isc_arg2: UShort; isc_arg3: PByte; isc_arg4: UShort): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_print_blr = function(isc_arg1: PByte; isc_arg2: TISC_PRINT_CALLBACK; isc_arg3: PVoid; isc_arg4: Short): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_set_debug = procedure(isc_arg1: Int); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_qtoq = procedure(isc_arg1: PISC_QUAD; isc_arg2: PISC_QUAD); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_vtof = procedure(isc_arg1: PByte; isc_arg2: PByte; isc_arg3: UShort); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_vtov = procedure(isc_arg1: PByte; isc_arg2: PByte; isc_arg3: Short); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_version = function(db_handle: PISC_DB_HANDLE; isc_arg2: TISC_VERSION_CALLBACK; isc_arg3: PVoid): Int; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_reset_fpe = function(isc_arg1: UShort): ISC_LONG; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_baddress = function(isc_arg1: PByte): uintptr_t; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_baddress_s = procedure(isc_arg1: PByte; isc_arg2: uintptr_t); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

(*******************************************)
(** Service manager functions             **)
(*******************************************)

  Tisc_service_attach = function(status_vector: PISC_STATUS; isc_arg2: UShort; isc_arg3: PByte; service_handle: PISC_SVC_HANDLE; isc_arg5: UShort; isc_arg6: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_service_detach = function(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_service_query = function(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE; recv_handle: PISC_SVC_HANDLE; isc_arg4: UShort; isc_arg5: PByte; isc_arg6: UShort; isc_arg7: PByte; isc_arg8: UShort; isc_arg9: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_service_start = function(status_vector: PISC_STATUS; service_handle: PISC_SVC_HANDLE; recv_handle: PISC_SVC_HANDLE; isc_arg4: UShort; isc_arg5: PByte): ISC_STATUS; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

(********************************)
(* Client information functions *)
(********************************)

{  Tfb_shutdown= function (arg1: UInt; arg2: int): Int;

  ISC_STATUS stdcall fb_shutdown_callback(ISC_STATUS*,
                         FB_SHUTDOWN_CALLBACK,
                         const int,
                         void*);

  ISC_STATUS stdcall fb_cancel_operation(ISC_STATUS*,
                        isc_db_handle*,
                        ISC_USHORT);

}
(********************************)
(* Client information functions *)
(********************************)

  Tisc_get_client_version = procedure(buffer: PByte); {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_get_client_major_version = function: Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
  Tisc_get_client_minor_version = function: Integer; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

(*****************************************************)
(** Actions to pass to the blob filter (ctl_source) **)
(*****************************************************)

const
  isc_blob_filter_open                     = 0;
  isc_blob_filter_get_segment              = 1;
  isc_blob_filter_close                    = 2;
  isc_blob_filter_create                   = 3;
  isc_blob_filter_put_segment              = 4;
  isc_blob_filter_alloc                    = 5;
  isc_blob_filter_free                     = 6;
  isc_blob_filter_seek                     = 7;

(*********************)
(** Blr definitions **)
(*********************)

  blr_text                                 = 14;
  blr_text2                                = 15;
  blr_short                                = 7;
  blr_long                                 = 8;
  blr_quad                                 = 9;
  blr_float                                = 10;
  blr_double                               = 27;
  blr_d_float                              = 11;
  blr_timestamp                            = 35;
  blr_varying                              = 37;
  blr_varying2                             = 38;
  blr_blob                                 = 261;
  blr_cstring                              = 40;
  blr_cstring2                             = 41;
  blr_blob_id                              = 45;
  blr_sql_date                             = 12;
  blr_sql_time                             = 13;
  blr_int64                                = 16;
  blr_blob2                                = 17;
  blr_domain_name                          = 18;
  blr_domain_name2                         = 19;
  blr_not_nullable                         = 20;
  blr_column_name                          = 21;
  blr_column_name2                         = 22;

  blr_domain_type_of                       = 0;
  blr_domain_full                          = 1;

  blr_date                                 = blr_timestamp;

  blr_inner                                = 0;
  blr_left                                 = 1;
  blr_right                                = 2;
  blr_full                                 = 3;

  blr_gds_code                             = 0;
  blr_sql_code                             = 1;
  blr_exception                            = 2;
  blr_trigger_code                         = 3;
  blr_default_code                         = 4;
  blr_raise                                = 5;
  blr_exception_msg                        = 6;

  blr_version4                             = 4;
  blr_version5                             = 5;
  blr_eoc                                  = 76;
  blr_end                                  = 255;

  blr_assignment                           = 1;
  blr_begin                                = 2;
  blr_dcl_variable                         = 3;
  blr_message                              = 4;
  blr_erase                                = 5;
  blr_fetch                                = 6;
  blr_for                                  = 7;
  blr_if                                   = 8;
  blr_loop                                 = 9;
  blr_modify                               = 10;
  blr_handler                              = 11;
  blr_receive                              = 12;
  blr_select                               = 13;
  blr_send                                 = 14;
  blr_store                                = 15;
  blr_label                                = 17;
  blr_leave                                = 18;
  blr_store2                               = 19;
  blr_post                                 = 20;
  blr_literal                              = 21;
  blr_dbkey                                = 22;
  blr_field                                = 23;
  blr_fid                                  = 24;
  blr_parameter                            = 25;
  blr_variable                             = 26;
  blr_average                              = 27;
  blr_count                                = 28;
  blr_maximum                              = 29;
  blr_minimum                              = 30;
  blr_total                                = 31;

  blr_add                                  = 34;
  blr_subtract                             = 35;
  blr_multiply                             = 36;
  blr_divide                               = 37;
  blr_negate                               = 38;
  blr_concatenate                          = 39;
  blr_substring                            = 40;
  blr_parameter2                           = 41;
  blr_from                                 = 42;
  blr_via                                  = 43;
  blr_parameter2_old                       = 44;
  blr_user_name                            = 44;
  blr_null                                 = 45;

  blr_equiv                                = 46;
  blr_eql                                  = 47;
  blr_neq                                  = 48;
  blr_gtr                                  = 49;
  blr_geq                                  = 50;
  blr_lss                                  = 51;
  blr_leq                                  = 52;
  blr_containing                           = 53;
  blr_matching                             = 54;
  blr_starting                             = 55;
  blr_between                              = 56;
  blr_or                                   = 57;
  blr_and                                  = 58;
  blr_not                                  = 59;
  blr_any                                  = 60;
  blr_missing                              = 61;
  blr_unique                               = 62;
  blr_like                                 = 63;

  blr_rse                                  = 67;
  blr_first                                = 68;
  blr_project                              = 69;
  blr_sort                                 = 70;
  blr_boolean                              = 71;
  blr_ascending                            = 72;
  blr_descending                           = 73;
  blr_relation                             = 74;
  blr_rid                                  = 75;
  blr_union                                = 76;
  blr_map                                  = 77;
  blr_group_by                             = 78;
  blr_aggregate                            = 79;
  blr_join_type                            = 80;

  blr_agg_count                            = 83;
  blr_agg_max                              = 84;
  blr_agg_min                              = 85;
  blr_agg_total                            = 86;
  blr_agg_average                          = 87;
  blr_parameter3                           = 88;
  blr_run_max                              = 89;
  blr_run_min                              = 90;
  blr_run_total                            = 91;
  blr_run_average                          = 92;
  blr_agg_count2                           = 93;
  blr_agg_count_distinct                   = 94;
  blr_agg_total_distinct                   = 95;
  blr_agg_average_distinct                 = 96;

  blr_function                             = 100;
  blr_gen_id                               = 101;
  blr_prot_mask                            = 102;
  blr_upcase                               = 103;
  blr_lock_state                           = 104;
  blr_value_if                             = 105;
  blr_matching2                            = 106;
  blr_index                                = 107;
  blr_ansi_like                            = 108;

  blr_seek                                 = 112;

  blr_continue                             = 0;
  blr_forward                              = 1;
  blr_backward                             = 2;
  blr_bof_forward                          = 3;
  blr_eof_backward                         = 4;

  blr_run_count                            = 118;
  blr_rs_stream                            = 119;
  blr_exec_proc                            = 120;

  blr_procedure                            = 124;
  blr_pid                                  = 125;
  blr_exec_pid                             = 126;
  blr_singular                             = 127;
  blr_abort                                = 128;
  blr_block                                = 129;
  blr_error_handler                        = 130;

  blr_cast                                 = 131;

  blr_start_savepoint                      = 134;
  blr_end_savepoint                        = 135;

  blr_plan                                 = 139;
  blr_merge                                = 140;
  blr_join                                 = 141;
  blr_sequential                           = 142;
  blr_navigational                         = 143;
  blr_indices                              = 144;
  blr_retrieve                             = 145;

  blr_relation2                            = 146;
  blr_rid2                                 = 147;

  blr_set_generator                        = 150;

  blr_ansi_any                             = 151;
  blr_exists                               = 152;

  blr_record_version                       = 154;
  blr_stall                                = 155;

  blr_ansi_all                             = 158;

  blr_extract                              = 159;

  blr_extract_year                         = 0;
  blr_extract_month                        = 1;
  blr_extract_day                          = 2;
  blr_extract_hour                         = 3;
  blr_extract_minute                       = 4;
  blr_extract_second                       = 5;
  blr_extract_weekday                      = 6;
  blr_extract_yearday                      = 7;
  blr_extract_millisecond                  = 8;
  blr_extract_week                         = 9;

  blr_current_date                         = 160;
  blr_current_timestamp                    = 161;
  blr_current_time                         = 162;

  blr_post_arg                             = 163;
  blr_exec_into                            = 164;
  blr_user_savepoint                       = 165;
  blr_dcl_cursor                           = 166;
  blr_cursor_stmt                          = 167;
  blr_current_timestamp2                   = 168;
  blr_current_time2                        = 169;
  blr_agg_list                             = 170;
  blr_agg_list_distinct                    = 171;
  blr_modify2                              = 172;

  blr_current_role                         = 174;
  blr_skip                                 = 175;

  blr_exec_sql                             = 176;
  blr_internal_info                        = 177;
  blr_nullsfirst                           = 178;
  blr_writelock                            = 179;
  blr_nullslast                            = 180;

  blr_lowcase                              = 181;
  blr_strlen                               = 182;

  blr_strlen_bit                           = 0;
  blr_strlen_char                          = 1;
  blr_strlen_octet                         = 2;

  blr_trim                                 = 183;

  blr_trim_both                            = 0;
  blr_trim_leading                         = 1;
  blr_trim_trailing                        = 2;

  blr_trim_spaces                          = 0;
  blr_trim_characters                      = 1;

  blr_savepoint_set                        = 0;
  blr_savepoint_release                    = 1;
  blr_savepoint_undo                       = 2;
  blr_savepoint_release_single             = 3;

  blr_cursor_open                          = 0;
  blr_cursor_close                         = 1;
  blr_cursor_fetch                         = 2;

  blr_init_variable                        = 184;
  blr_recurse                              = 185;
  blr_sys_function                         = 186;

  blr_auto_trans                           = 187;
  blr_similar                              = 188;
  blr_exec_stmt                            = 189;

  blr_exec_stmt_inputs                     =  1;
  blr_exec_stmt_outputs                    =  2;
  blr_exec_stmt_sql                        =  3;
  blr_exec_stmt_proc_block                 =  4;
  blr_exec_stmt_data_src                   =  5;
  blr_exec_stmt_user                       =  6;
  blr_exec_stmt_pwd                        =  7;
  blr_exec_stmt_tran                       =  8;
  blr_exec_stmt_tran_clone                 =  9;
  blr_exec_stmt_privs                      =  10;
  blr_exec_stmt_in_params                  =  11;
  blr_exec_stmt_in_params2                 =  12;
  blr_exec_stmt_out_params                 =  13;
  blr_exec_stmt_role                       =  14;

  blr_stmt_expr                            =  190;
  blr_derived_expr                         =  191;

(************************************)
(** Database parameter block stuff **)
(************************************)

  isc_dpb_version1                         = 1;
  isc_dpb_cdd_pathname                     = 1;
  isc_dpb_allocation                       = 2;
  isc_dpb_journal                          = 3;
  isc_dpb_page_size                        = 4;
  isc_dpb_num_buffers                      = 5;
  isc_dpb_buffer_length                    = 6;
  isc_dpb_debug                            = 7;
  isc_dpb_garbage_collect                  = 8;
  isc_dpb_verify                           = 9;
  isc_dpb_sweep                            = 10;
  isc_dpb_enable_journal                   = 11;
  isc_dpb_disable_journal                  = 12;
  isc_dpb_dbkey_scope                      = 13;
  isc_dpb_number_of_users                  = 14;
  isc_dpb_trace                            = 15;
  isc_dpb_no_garbage_collect               = 16;
  isc_dpb_damaged                          = 17;
  isc_dpb_license                          = 18;
  isc_dpb_sys_user_name                    = 19;
  isc_dpb_encrypt_key                      = 20;
  isc_dpb_activate_shadow                  = 21;
  isc_dpb_sweep_interval                   = 22;
  isc_dpb_delete_shadow                    = 23;
  isc_dpb_force_write                      = 24;
  isc_dpb_begin_log                        = 25;
  isc_dpb_quit_log                         = 26;
  isc_dpb_no_reserve                       = 27;
  isc_dpb_user_name                        = 28;
  isc_dpb_password                         = 29;
  isc_dpb_password_enc                     = 30;
  isc_dpb_sys_user_name_enc                = 31;
  isc_dpb_interp                           = 32;
  isc_dpb_online_dump                      = 33;
  isc_dpb_old_file_size                    = 34;
  isc_dpb_old_num_files                    = 35;
  isc_dpb_old_file                         = 36;
  isc_dpb_old_start_page                   = 37;
  isc_dpb_old_start_seqno                  = 38;
  isc_dpb_old_start_file                   = 39;
  isc_dpb_drop_walfile                     = 40;
  isc_dpb_old_dump_id                      = 41;
  isc_dpb_wal_backup_dir                   = 42;
  isc_dpb_wal_chkptlen                     = 43;
  isc_dpb_wal_numbufs                      = 44;
  isc_dpb_wal_bufsize                      = 45;
  isc_dpb_wal_grp_cmt_wait                 = 46;
  isc_dpb_lc_messages                      = 47;
  isc_dpb_lc_ctype                         = 48;
  isc_dpb_cache_manager                    = 49;
  isc_dpb_shutdown                         = 50;
  isc_dpb_online                           = 51;
  isc_dpb_shutdown_delay                   = 52;
  isc_dpb_reserved                         = 53;
  isc_dpb_overwrite                        = 54;
  isc_dpb_sec_attach                       = 55;
  isc_dpb_disable_wal                      = 56;
  isc_dpb_connect_timeout                  = 57;
  isc_dpb_dummy_packet_interval            = 58;
  isc_dpb_gbak_attach                      = 59;
  isc_dpb_sql_role_name                    = 60;
  isc_dpb_set_page_buffers                 = 61;
  isc_dpb_working_directory                = 62;
  isc_dpb_SQL_dialect                      = 63;
  isc_dpb_set_db_readonly                  = 64;
  isc_dpb_set_db_SQL_dialect               = 65;
  isc_dpb_gfix_attach                      = 66;
  isc_dpb_gstat_attach                     = 67;
  isc_dpb_set_db_charset                   = 68;
  isc_dpb_gsec_attach                      = 69;
  isc_dpb_address_path                     = 70;
  isc_dpb_process_id                       = 71;
  isc_dpb_no_db_trigge                     = 72;
  isc_dpb_trusted_auth                     = 73;
  isc_dpb_process_name                     = 74;
  isc_dpb_trusted_role                     = 75;
  isc_dpb_org_filename                     = 76;
  isc_dpb_utf8_filename                    = 77;
  isc_dpb_ext_call_depth                   = 78;

  isc_dpb_last_dpb_constant                = isc_dpb_ext_call_depth;


  isc_dpb_address                          = 1;

  isc_dpb_addr_protocol                    = 1;
  isc_dpb_addr_endpoint                    = 2;

(***********************************)
(** isc_dpb_verify specific flags **)
(***********************************)

  isc_dpb_pages                            = 1;
  isc_dpb_records                          = 2;
  isc_dpb_indices                          = 4;
  isc_dpb_transactions                     = 8;
  isc_dpb_no_update                        = 16;
  isc_dpb_repair                           = 32;
  isc_dpb_ignore                           = 64;

(*************************************)
(** isc_dpb_shutdown specific flags **)
(*************************************)

  isc_dpb_shut_cache                       = 1;
  isc_dpb_shut_attachment                  = 2;
  isc_dpb_shut_transaction                 = 4;
  isc_dpb_shut_force                       = 8;
  isc_dpb_shut_mode_mask                   = $70;

  isc_dpb_shut_default                     = $0;
  isc_dpb_shut_normal                      = $10;
  isc_dpb_shut_multi                       = $20;
  isc_dpb_shut_single                      = $30;
  isc_dpb_shut_full                        = $40;

(****************************************)
(** Bit assignments in RDB$SYSTEM_FLAG **)
(****************************************)

  RDB_system                               = 1;
  RDB_id_assigned                          = 2;

(***************************************)
(** Transaction parameter block stuff **)
(***************************************)

  isc_tpb_version1                         = 1;
  isc_tpb_version3                         = 3;
  isc_tpb_consistency                      = 1;
  isc_tpb_concurrency                      = 2;
  isc_tpb_shared                           = 3;
  isc_tpb_protected                        = 4;
  isc_tpb_exclusive                        = 5;
  isc_tpb_wait                             = 6;
  isc_tpb_nowait                           = 7;
  isc_tpb_read                             = 8;
  isc_tpb_write                            = 9;
  isc_tpb_lock_read                        = 10;
  isc_tpb_lock_write                       = 11; //it must pass the table name
  isc_tpb_verb_time                        = 12;
  isc_tpb_commit_time                      = 13;
  isc_tpb_ignore_limbo                     = 14;
  isc_tpb_read_committed                   = 15;
  isc_tpb_autocommit                       = 16;
  isc_tpb_rec_version                      = 17;
  isc_tpb_no_rec_version                   = 18;
  isc_tpb_restart_requests                 = 19;
  isc_tpb_no_auto_undo                     = 20;
  isc_tpb_lock_timeout                     = 21;

  isc_tpb_last_tpb_constant                = isc_tpb_lock_timeout;

(**************************)
(** Blob Parameter Block **)
(**************************)

  isc_bpb_version1                         = 1;
  isc_bpb_source_type                      = 1;
  isc_bpb_target_type                      = 2;
  isc_bpb_type                             = 3;
  isc_bpb_source_interp                    = 4;
  isc_bpb_target_interp                    = 5;
  isc_bpb_filter_parameter                 = 6;
  isc_bpb_storage                          = 7;

  isc_bpb_type_segmented                   = 0;
  isc_bpb_type_stream                      = 1;
  isc_bpb_storage_main                     = 0;
  isc_bpb_storage_temp                     = 2;

(***********************************)
(** Service parameter block stuff **)
(***********************************)

  c_spb_version1                           = 1;
  isc_spb_current_version                  = 2;
  isc_spb_version                          = isc_spb_current_version;
  isc_spb_user_name                        = isc_dpb_user_name;
  isc_spb_sys_user_name                    = isc_dpb_sys_user_name;
  isc_spb_sys_user_name_enc                = isc_dpb_sys_user_name_enc;
  isc_spb_password                         = isc_dpb_password;
  isc_spb_password_enc                     = isc_dpb_password_enc;
  isc_spb_command_line                     = 105;
  isc_spb_dbname                           = 106;
  isc_spb_verbose                          = 107;
  isc_spb_options                          = 108;
  isc_spb_address_path                     = 109;
  isc_spb_process_id                       = 110;
  isc_spb_trusted_auth                     = 111;
  isc_spb_process_name                     = 112;
  isc_spb_trusted_role                     = 113;

  isc_spb_connect_timeout                  = isc_dpb_connect_timeout;
  isc_spb_dummy_packet_interval            = isc_dpb_dummy_packet_interval;
  isc_spb_sql_role_name                    = isc_dpb_sql_role_name;

(*****************************************)
(* Service action items                 **)
(*****************************************)

  isc_action_svc_backup                    = 1;
  isc_action_svc_restore                   = 2;
  isc_action_svc_repair                    = 3;
  isc_action_svc_add_user                  = 4;
  isc_action_svc_delete_user               = 5;
  isc_action_svc_modify_user               = 6;
  isc_action_svc_display_user              = 7;
  isc_action_svc_properties                = 8;
  isc_action_svc_add_license               = 9;
  isc_action_svc_remove_license            = 10;
  isc_action_svc_db_stats                  = 11;
  //isc_action_svc_get_ib_log              = 12;
  isc_action_svc_get_fb_log                = 12;
  isc_action_svc_nbak                      = 20;
  isc_action_svc_nrest                     = 21;
  isc_action_svc_trace_start               = 22;
  isc_action_svc_trace_stop                = 23;
  isc_action_svc_trace_suspend             = 24;
  isc_action_svc_trace_resume              = 25;
  isc_action_svc_trace_list                = 26;
  isc_action_svc_set_mapping               = 27;
  isc_action_svc_drop_mapping              = 28;
  isc_action_svc_display_user_adm          = 29;
  isc_action_svc_last                      = 30;

(*****************************************)
(** Service information items           **)
(*****************************************)

  isc_info_svc_svr_db_info                 = 50; { Retrieves the number of attachments and databases }
  isc_info_svc_get_license                 = 51; { Retrieves all license keys and IDs from the license file }
  isc_info_svc_get_license_mask            = 52; { Retrieves a bitmask representing licensed options on the server }
  isc_info_svc_get_config                  = 53; { Retrieves the parameters and values for FB_CONFIG }
  isc_info_svc_version                     = 54; { Retrieves the version of the services manager }
  isc_info_svc_server_version              = 55; { Retrieves the version of the Firebird server }
  isc_info_svc_implementation              = 56; { Retrieves the implementation of the Firebird server }
  isc_info_svc_capabilities                = 57; { Retrieves a bitmask representing the server's capabilities }
  isc_info_svc_user_dbpath                 = 58; { Retrieves the path to the security database in use by the server }
  isc_info_svc_get_env                     = 59; { Retrieves the setting of $Firebird }
  isc_info_svc_get_env_lock                = 60; { Retrieves the setting of $Firebird_LCK }
  isc_info_svc_get_env_msg                 = 61; { Retrieves the setting of $Firebird_MSG }
  isc_info_svc_line                        = 62; { Retrieves 1 line of service output per call }
  isc_info_svc_to_eof                      = 63; { Retrieves as much of the server output as will fit in the supplied buffer }
  isc_info_svc_timeout                     = 64; { Sets / signifies a timeout value for reading service information }
  isc_info_svc_get_licensed_users          = 65; { Retrieves the number of users licensed for accessing the server }
  isc_info_svc_limbo_trans                 = 66; { Retrieve the limbo transactions }
  isc_info_svc_running                     = 67; { Checks to see if a service is running on an attachment }
  isc_info_svc_get_users                   = 68; { Returns the user information from isc_action_svc_display_users }

(*****************************************)
(* Parameters for isc_action_{add|delete|modify)_user *)
(*****************************************)

  isc_spb_sec_userid                       = 5;
  isc_spb_sec_groupid                      = 6;
  isc_spb_sec_username                     = 7;
  isc_spb_sec_password                     = 8;
  isc_spb_sec_groupname                    = 9;
  isc_spb_sec_firstname                    = 10;
  isc_spb_sec_middlename                   = 11;
  isc_spb_sec_lastname                     = 12;
  isc_spb_sec_admin                        = 13;

  isc_spb_lic_key                          = 5;
  isc_spb_lic_id                           = 6;
  isc_spb_lic_desc                         = 7;

(*****************************************)
(* Parameters for isc_action_svc_backup  *)
(*****************************************)

  isc_spb_bkp_file                         = 5;
  isc_spb_bkp_factor                       = 6;
  isc_spb_bkp_length                       = 7;
  isc_spb_bkp_ignore_checksums             = $01;
  isc_spb_bkp_ignore_limbo                 = $02;
  isc_spb_bkp_metadata_only                = $04;
  isc_spb_bkp_no_garbage_collect           = $08;
  isc_spb_bkp_old_descriptions             = $10;
  isc_spb_bkp_non_transportable            = $20;
  isc_spb_bkp_convert                      = $40;
  isc_spb_bkp_expand                       = $80;
  isc_spb_bkp_no_triggers                  = $8000;

(*****************************************)
(* Parameters for isc_action_svc_properties *)
(*****************************************)

  isc_spb_prp_page_buffers                 = 5;
  isc_spb_prp_sweep_interval               = 6;
  isc_spb_prp_shutdown_db                  = 7;
  isc_spb_prp_deny_new_attachments         = 9;
  isc_spb_prp_deny_new_transactions        = 10;
  isc_spb_prp_reserve_space                = 11;
  isc_spb_prp_write_mode                   = 12;
  isc_spb_prp_access_mode                  = 13;
  isc_spb_prp_set_sql_dialect              = 14;
  isc_spb_prp_activate                     = $0100;
  isc_spb_prp_db_online                    = $0200;
  isc_spb_prp_force_shutdown               =  41;
  isc_spb_prp_attachments_shutdown         =  42;
  isc_spb_prp_transactions_shutdown        =  43;
  isc_spb_prp_shutdown_mode                =  44;
  isc_spb_prp_online_mode                  = 45;

  isc_spb_prp_sm_normal                    = 0;
  isc_spb_prp_sm_multi                     = 1;
  isc_spb_prp_sm_single                    = 2;
  isc_spb_prp_sm_full                      = 3;

  isc_spb_prp_res_use_full                 = 35;
  isc_spb_prp_res                          = 36;

(*****************************************)
(* Parameters for isc_spb_prp_write_mode  *)
(*****************************************)

  isc_spb_prp_wm_async                     = 37;
  isc_spb_prp_wm_sync                      = 38;

(*****************************************)
(* Parameters for isc_spb_prp_access_mode *)
(*****************************************)

  isc_spb_prp_am_readonly                  = 39;
  isc_spb_prp_am_readwrite                 = 40;

(*****************************************)
(* Parameters for isc_action_svc_repair  *)
(*****************************************)

  isc_spb_rpr_commit_trans                 = 15;
  isc_spb_rpr_rollback_trans               = 34;
  isc_spb_rpr_recover_two_phase            = 17;
  isc_spb_tra_id                           = 18;
  isc_spb_single_tra_id                    = 19;
  isc_spb_multi_tra_id                     = 20;
  isc_spb_tra_state                        = 21;
  isc_spb_tra_state_limbo                  = 22;
  isc_spb_tra_state_commit                 = 23;
  isc_spb_tra_state_rollback               = 24;
  isc_spb_tra_state_unknown                = 25;
  isc_spb_tra_host_site                    = 26;
  isc_spb_tra_remote_site                  = 27;
  isc_spb_tra_db_path                      = 28;
  isc_spb_tra_advise                       = 29;
  isc_spb_tra_advise_commit                = 30;
  isc_spb_tra_advise_rollback              = 31;
  isc_spb_tra_advise_unknown               = 33;
  isc_spb_rpr_validate_db                  = $01;
  isc_spb_rpr_sweep_db                     = $02;
  isc_spb_rpr_mend_db                      = $04;
  isc_spb_rpr_list_limbo_trans             = $08;
  isc_spb_rpr_check_db                     = $10;
  isc_spb_rpr_ignore_checksum              = $20;
  isc_spb_rpr_kill_shadows                 = $40;
  isc_spb_rpr_full                         = $80;

(*****************************************)
(* Parameters for isc_action_svc_restore  *)
(*****************************************)

  isc_spb_res_buffers                      = 9;
  isc_spb_res_page_size                    = 10;
  isc_spb_res_length                       = 11;
  isc_spb_res_access_mode                  = 12;
  isc_spb_res_fix_fss_data                 = 13;
  isc_spb_res_fix_fss_metadata             = 14;
  isc_spb_res_metadata_only                = isc_spb_bkp_metadata_only;
  isc_spb_res_deactivate_idx               = $0100;
  isc_spb_res_no_shadow                    = $0200;
  isc_spb_res_no_validity                  = $0400;
  isc_spb_res_one_at_a_time                = $0800;
  isc_spb_res_replace                      = $1000;
  isc_spb_res_create                       = $2000;
  isc_spb_res_use_all_space                = $4000;

(*****************************************)
(* Parameters for isc_spb_res_access_mode  *)
(*****************************************)

  isc_spb_res_am_readonly                  = isc_spb_prp_am_readonly;
  isc_spb_res_am_readwrite                 = isc_spb_prp_am_readwrite;

(*****************************************)
(* Parameters for isc_info_svc_svr_db_info *)
(*****************************************)

  isc_spb_num_att                          = 5;
  isc_spb_num_db                           = 6;

(*****************************************)
(* Parameters for isc_info_svc_db_stats  *)
(*****************************************)

  isc_spb_sts_data_pages                   = $01;
  isc_spb_sts_db_log                       = $02;
  isc_spb_sts_hdr_pages                    = $04;
  isc_spb_sts_idx_pages                    = $08;
  isc_spb_sts_sys_relations                = $10;
  isc_spb_sts_record_versions              = $20;
  isc_spb_sts_table                        = $40;
  isc_spb_sts_nocreation                   = $80;

  isc_spb_nbk_level                        = 5;
  isc_spb_nbk_file                         = 6;
  isc_spb_nbk_direct                       = 7;
  isc_spb_nbk_no_triggers                  = $01;

  isc_spb_trc_id                           = 1;
  isc_spb_trc_name                         = 2;
  isc_spb_trc_cfg                          = 3;

(************************************************)
(** Dynamic Data Definition Language operators **)
(************************************************)

(********************)
(** Version number **)
(********************)

  isc_dyn_version_1                        = 1;
  isc_dyn_eoc                              = 255;

(********************************)
(** Operations (may be nested) **)
(********************************)

  isc_dyn_begin                            = 2;
  isc_dyn_end                              = 3;
  isc_dyn_if                               = 4;
  isc_dyn_def_database                     = 5;
  isc_dyn_def_global_fld                   = 6;
  isc_dyn_def_local_fld                    = 7;
  isc_dyn_def_idx                          = 8;
  isc_dyn_def_rel                          = 9;
  isc_dyn_def_sql_fld                      = 10;
  isc_dyn_def_view                         = 12;
  isc_dyn_def_trigger                      = 15;
  isc_dyn_def_security_class               = 120;
  isc_dyn_def_dimension                    = 140;
  isc_dyn_def_generator                    = 24;
  isc_dyn_def_function                     = 25;
  isc_dyn_def_filter                       = 26;
  isc_dyn_def_function_arg                 = 27;
  isc_dyn_def_shadow                       = 34;
  isc_dyn_def_trigger_msg                  = 17;
  isc_dyn_def_file                         = 36;
  isc_dyn_mod_database                     = 39;
  isc_dyn_mod_rel                          = 11;
  isc_dyn_mod_global_fld                   = 13;
  isc_dyn_mod_idx                          = 102;
  isc_dyn_mod_local_fld                    = 14;
  isc_dyn_mod_sql_fld                      = 216;
  isc_dyn_mod_view                         = 16;
  isc_dyn_mod_security_class               = 122;
  isc_dyn_mod_trigger                      = 113;
  isc_dyn_mod_trigger_msg                  = 28;
  isc_dyn_delete_database                  = 18;
  isc_dyn_delete_rel                       = 19;
  isc_dyn_delete_global_fld                = 20;
  isc_dyn_delete_local_fld                 = 21;
  isc_dyn_delete_idx                       = 22;
  isc_dyn_delete_security_class            = 123;
  isc_dyn_delete_dimensions                = 143;
  isc_dyn_delete_trigger                   = 23;
  isc_dyn_delete_trigger_msg               = 29;
  isc_dyn_delete_filter                    = 32;
  isc_dyn_delete_function                  = 33;
  isc_dyn_delete_shadow                    = 35;
  isc_dyn_grant                            = 30;
  isc_dyn_revoke                           = 31;
  isc_dyn_def_primary_key                  = 37;
  isc_dyn_def_foreign_key                  = 38;
  isc_dyn_def_unique                       = 40;
  isc_dyn_def_procedure                    = 164;
  isc_dyn_delete_procedure                 = 165;
  isc_dyn_def_parameter                    = 135;
  isc_dyn_delete_parameter                 = 136;

  isc_dyn_mod_procedure                    = 175;

  isc_dyn_def_exception                    = 181;
  isc_dyn_mod_exception                    = 182;
  isc_dyn_del_exception                    = 183;

  isc_dyn_def_difference                   =           220;
  isc_dyn_drop_difference                  =          221;
  isc_dyn_begin_backup                     =             222;
  isc_dyn_end_backup                       =               223;
  isc_dyn_debug_info                       =               240;

  isc_dyn_view_blr                         = 43;
  isc_dyn_view_source                      = 44;
  isc_dyn_view_relation                    = 45;
  isc_dyn_view_context                     = 46;
  isc_dyn_view_context_name                = 47;

(************************)
(** Generic attributes **)
(************************)

  isc_dyn_rel_name                         = 50;
  isc_dyn_fld_name                         = 51;
  isc_dyn_new_fld_name                     = 215;
  isc_dyn_idx_name                         = 52;
  isc_dyn_description                      = 53;
  isc_dyn_security_class                   = 54;
  isc_dyn_system_flag                      = 55;
  isc_dyn_update_flag                      = 56;
  isc_dyn_prc_name                         = 166;
  isc_dyn_prm_name                         = 137;
  isc_dyn_sql_object                       = 196;
  isc_dyn_fld_character_set_name           = 174;

(**********************************)
(** Relation specific attributes **)
(**********************************)

  isc_dyn_rel_dbkey_length                 = 61;
  isc_dyn_rel_store_trig                   = 62;
  isc_dyn_rel_modify_trig                  = 63;
  isc_dyn_rel_erase_trig                   = 64;
  isc_dyn_rel_store_trig_source            = 65;
  isc_dyn_rel_modify_trig_source           = 66;
  isc_dyn_rel_erase_trig_source            = 67;
  isc_dyn_rel_ext_file                     = 68;
  isc_dyn_rel_sql_protection               = 69;
  isc_dyn_rel_constraint                   = 162;
  isc_dyn_delete_rel_constraint            = 163;

(**************************************)
(** Global field specific attributes **)
(**************************************)

  isc_dyn_fld_type                         = 70;
  isc_dyn_fld_length                       = 71;
  isc_dyn_fld_scale                        = 72;
  isc_dyn_fld_sub_type                     = 73;
  isc_dyn_fld_segment_length               = 74;
  isc_dyn_fld_query_header                 = 75;
  isc_dyn_fld_edit_string                  = 76;
  isc_dyn_fld_validation_blr               = 77;
  isc_dyn_fld_validation_source            = 78;
  isc_dyn_fld_computed_blr                 = 79;
  isc_dyn_fld_computed_source              = 80;
  isc_dyn_fld_missing_value                = 81;
  isc_dyn_fld_default_value                = 82;
  isc_dyn_fld_query_name                   = 83;
  isc_dyn_fld_dimensions                   = 84;
  isc_dyn_fld_not_null                     = 85;
  isc_dyn_fld_precision                    = 86;
  isc_dyn_fld_char_length                  = 172;
  isc_dyn_fld_collation                    = 173;
  isc_dyn_fld_default_source               = 193;
  isc_dyn_del_default                      = 197;
  isc_dyn_del_validation                   = 198;
  isc_dyn_single_validation                = 199;
  isc_dyn_fld_character_set                = 203;
  isc_dyn_del_computed                     = 242;

(*************************************)
(** Local field specific attributes **)
(*************************************)

  isc_dyn_fld_source                       = 90;
  isc_dyn_fld_base_fld                     = 91;
  isc_dyn_fld_position                     = 92;
  isc_dyn_fld_update_flag                  = 93;

(*******************************)
(** Index specific attributes **)
(*******************************)

  isc_dyn_idx_unique                       = 100;
  isc_dyn_idx_inactive                     = 101;
  isc_dyn_idx_type                         = 103;
  isc_dyn_idx_foreign_key                  = 104;
  isc_dyn_idx_ref_column                   = 105;
  isc_dyn_idx_statistic                    = 204;

(*********************************)
(** Trigger specific attributes **)
(*********************************)

  isc_dyn_trg_type                         = 110;
  isc_dyn_trg_blr                          = 111;
  isc_dyn_trg_source                       = 112;
  isc_dyn_trg_name                         = 114;
  isc_dyn_trg_sequence                     = 115;
  isc_dyn_trg_inactive                     = 116;
  isc_dyn_trg_msg_number                   = 117;
  isc_dyn_trg_msg                          = 118;

(****************************************)
(** Security Class specific attributes **)
(****************************************)

  isc_dyn_scl_acl                          = 121;
  isc_dyn_grant_user                       = 130;
  isc_dyn_grant_proc                       = 186;
  isc_dyn_grant_trig                       = 187;
  isc_dyn_grant_view                       = 188;
  isc_dyn_grant_options                    = 132;
  isc_dyn_grant_user_group                 = 205;
  isc_dyn_grant_role                       = 218;
  isc_dyn_grant_grantor                    = 245;

(************************************)
(** Dimension specific information **)
(************************************)

  isc_dyn_dim_lower                        = 141;
  isc_dyn_dim_upper                        = 142;

(******************************)
(** File specific attributes **)
(******************************)

  isc_dyn_file_name                        = 125;
  isc_dyn_file_start                       = 126;
  isc_dyn_file_length                      = 127;
  isc_dyn_shadow_number                    = 128;
  isc_dyn_shadow_man_auto                  = 129;
  isc_dyn_shadow_conditional               = 130;


(**********************************)
(** Function specific attributes **)
(**********************************)

  isc_dyn_function_name                    = 145;
  isc_dyn_function_type                    = 146;
  isc_dyn_func_module_name                 = 147;
  isc_dyn_func_entry_point                 = 148;
  isc_dyn_func_return_argument             = 149;
  isc_dyn_func_arg_position                = 150;
  isc_dyn_func_mechanism                   = 151;
  isc_dyn_filter_in_subtype                = 152;
  isc_dyn_filter_out_subtype               = 153;

  isc_dyn_description2                     = 154;
  isc_dyn_fld_computed_source2             = 155;
  isc_dyn_fld_edit_string2                 = 156;
  isc_dyn_fld_query_header2                = 157;
  isc_dyn_fld_validation_source2           = 158;
  isc_dyn_trg_msg2                         = 159;
  isc_dyn_trg_source2                      = 160;
  isc_dyn_view_source2                     = 161;
  isc_dyn_xcp_msg2                         = 184;

(***********************************)
(** Generator specific attributes **)
(***********************************)

  isc_dyn_generator_name                   = 95;
  isc_dyn_generator_id                     = 96;

(***********************************)
(** Procedure specific attributes **)
(***********************************)

  isc_dyn_prc_inputs                       = 167;
  isc_dyn_prc_outputs                      = 168;
  isc_dyn_prc_source                       = 169;
  isc_dyn_prc_blr                          = 170;
  isc_dyn_prc_source2                      = 171;
  isc_dyn_prc_type                         = 239;

  isc_dyn_prc_t_selectable                 =  1;
  isc_dyn_prc_t_executable                 =  2;

(***********************************)
(** Parameter specific attributes **)
(***********************************)

  isc_dyn_prm_number                       = 138;
  isc_dyn_prm_type                         = 139;
  isc_dyn_prm_mechanism                    = 241;

(**********************************)
(** Relation specific attributes **)
(**********************************)

  isc_dyn_xcp_msg                          = 185;

(************************************************)
(** Cascading referential integrity values     **)
(************************************************)
  isc_dyn_foreign_key_update               = 205;
  isc_dyn_foreign_key_delete               = 206;
  isc_dyn_foreign_key_cascade              = 207;
  isc_dyn_foreign_key_default              = 208;
  isc_dyn_foreign_key_null                 = 209;
  isc_dyn_foreign_key_none                 = 210;

(*************************)
(** SQL role values     **)
(*************************)

  isc_dyn_def_sql_role                     = 211;
  isc_dyn_sql_role_name                    = 212;
  isc_dyn_grant_admin_options              = 213;
  isc_dyn_del_sql_role                     = 214;

(**********************************************)
(* Generators again                           *)
(**********************************************)

  isc_dyn_delete_generator                 = 217;

  isc_dyn_mod_function                     = 224;
  isc_dyn_mod_filter                       = 225;
  isc_dyn_mod_generator                    = 226;
  isc_dyn_mod_sql_role                     = 227;
  isc_dyn_mod_charset                      = 228;
  isc_dyn_mod_collation                    = 229;
  isc_dyn_mod_prc_parameter                = 230;

(***********************)
(* collation values    *)
(***********************)

  isc_dyn_def_collation                    = 231;
  isc_dyn_coll_for_charset                 = 232;
  isc_dyn_coll_from                        = 233;
  isc_dyn_coll_attribute                   = 234;
  isc_dyn_coll_specific_attributes_charset = 235;
  isc_dyn_coll_specific_attributes         = 236;
  isc_dyn_del_collation                    = 237;

  isc_dyn_mapping                          = 243;
  isc_dyn_map_role                         = 1;
  isc_dyn_unmap_role                       = 2;
  isc_dyn_map_user                         = 3;
  isc_dyn_unmap_user                       = 4;
  isc_dyn_automap_role                     = 5;
  isc_dyn_autounmap_role                   = 6;

  isc_dyn_user                             = 244;
  isc_dyn_user_add                         = 1;
  isc_dyn_user_mod                         = 2;
  isc_dyn_user_del                         = 3;
  isc_dyn_user_passwd                      = 4;
  isc_dyn_user_first                       = 5;
  isc_dyn_user_middle                      = 6;
  isc_dyn_user_last                        = 7;
  isc_dyn_user_admin                       = 8;
  isc_user_end                             = 0;

  isc_dyn_last_dyn_value                   = 247;

(********************************************)
(** Array slice description language (SDL) **)
(********************************************)

  isc_sdl_version1                         = 1;
  isc_sdl_eoc                              = -1;
  isc_sdl_relation                         = 2;
  isc_sdl_rid                              = 3;
  isc_sdl_field                            = 4;
  isc_sdl_fid                              = 5;
  isc_sdl_struct                           = 6;
  isc_sdl_variable                         = 7;
  isc_sdl_scalar                           = 8;
  isc_sdl_tiny_integer                     = 9;
  isc_sdl_short_integer                    = 10;
  isc_sdl_long_integer                     = 11;
  isc_sdl_literal                          = 12;
  isc_sdl_add                              = 13;
  isc_sdl_subtract                         = 14;
  isc_sdl_multiply                         = 15;
  isc_sdl_divide                           = 16;
  isc_sdl_negate                           = 17;
  isc_sdl_eql                              = 18;
  isc_sdl_neq                              = 19;
  isc_sdl_gtr                              = 20;
  isc_sdl_geq                              = 21;
  isc_sdl_lss                              = 22;
  isc_sdl_leq                              = 23;
  isc_sdl_and                              = 24;
  isc_sdl_or                               = 25;
  isc_sdl_not                              = 26;
  isc_sdl_while                            = 27;
  isc_sdl_assignment                       = 28;
  isc_sdl_label                            = 29;
  isc_sdl_leave                            = 30;
  isc_sdl_begin                            = 31;
  isc_sdl_end                              = 32;
  isc_sdl_do3                              = 33;
  isc_sdl_do2                              = 34;
  isc_sdl_do1                              = 35;
  isc_sdl_element                          = 36;

(**********************************************)
(** International text interpretation values **)
(**********************************************)

  isc_interp_eng_ascii                     = 0;
  isc_interp_jpn_sjis                      = 5;
  isc_interp_jpn_euc                       = 6;

(*******************)
(** Blob Subtypes **)
(*******************)

(** types less than zero are reserved for customer use **)

  isc_blob_untyped                         = 0;

(** internal subtypes **)

  isc_blob_text                            = 1;
  isc_blob_blr                             = 2;
  isc_blob_acl                             = 3;
  isc_blob_ranges                          = 4;
  isc_blob_summary                         = 5;
  isc_blob_format                          = 6;
  isc_blob_tra                             = 7;
  isc_blob_extfile                         = 8;
  isc_blob_debug_info                      = 9;
  isc_blob_max_predefined_subtype          = 10;

(** the range 20-30 is reserved for dBASE and Paradox types **)

  isc_blob_formatted_memo                  = 20;
  isc_blob_paradox_ole                     = 21;
  isc_blob_graphic                         = 22;
  isc_blob_dbase_ole                       = 23;
  isc_blob_typed_binary                    = 24;

  isc_info_db_SQL_dialect                  = 62;
  isc_info_db_read_only                    = 63;
  isc_info_db_size_in_pages                = 64;

  fb_shut_confirmation                     = 1;
  fb_shut_preproviders                     = 2;
  fb_shut_postproviders                    = 4;
  fb_shut_finish                           = 8;

  fb_shutrsn_svc_stopped                   =-1;
  fb_shutrsn_no_connection                 =-2;
  fb_shutrsn_app_stopped                   =-3;
  fb_shutrsn_device_removed                =-4;
  fb_shutrsn_signal                        =-5;
  fb_shutrsn_services                      =-6;
  fb_shutrsn_exit_called                   =-7;

  fb_cancel_disable                        = 1;
  fb_cancel_enable                         = 2;
  fb_cancel_raise                          = 3;
  fb_cancel_abort                          = 4;

  fb_dbg_version                           = 1;
  fb_dbg_end                               = 255;
  fb_dbg_map_src2blr                       = 2;
  fb_dbg_map_varname                       = 3;
  fb_dbg_map_argument                      = 4;

  fb_dbg_arg_input                         = 0;
  fb_dbg_arg_output                        = 1;

{*********************************}
{  Information call declarations  }
{*********************************}

  isc_info_end                             = 1;
  isc_info_truncated                       = 2;
  isc_info_error                           = 3;
  isc_info_data_not_ready                  = 4;
  isc_info_length                          = 126;
  isc_info_flag_end                        = 127;

(********************************)
(** Database information items **)
(********************************)

  isc_info_db_id                           = 4;
  isc_info_reads                           = 5;
  isc_info_writes                          = 6;
  isc_info_fetches                         = 7;
  isc_info_marks                           = 8;

  isc_info_implementation                  = 11;
  isc_info_version                         = 12;
  isc_info_base_level                      = 13;
  isc_info_svr_maj_ver                     = isc_info_base_level;
  isc_info_page_size                       = 14;
  isc_info_num_buffers                     = 15;
  isc_info_limbo                           = 16;
  isc_info_current_memory                  = 17;
  isc_info_max_memory                      = 18;
  isc_info_window_turns                    = 19;
  isc_info_license                         = 20;

  isc_info_allocation                      = 21;
  isc_info_attachment_id                   = 22;
  isc_info_read_seq_count                  = 23;
  isc_info_read_idx_count                  = 24;
  isc_info_insert_count                    = 25;
  isc_info_update_count                    = 26;
  isc_info_delete_count                    = 27;
  isc_info_backout_count                   = 28;
  isc_info_purge_count                     = 29;
  isc_info_expunge_count                   = 30;

  isc_info_sweep_interval                  = 31;
  isc_info_ods_version                     = 32;
  isc_info_ods_minor_version               = 33;
  isc_info_no_reserve                      = 34;

  isc_info_logfile                         = 35;
  isc_info_cur_logfile_name                = 36;
  isc_info_cur_log_part_offset             = 37;
  isc_info_num_wal_buffers                 = 38;
  isc_info_wal_buffer_size                 = 39;
  isc_info_wal_ckpt_length                 = 40;

  isc_info_wal_cur_ckpt_interval           = 41;
  isc_info_wal_prv_ckpt_fname              = 42;
  isc_info_wal_prv_ckpt_poffset            = 43;
  isc_info_wal_recv_ckpt_fname             = 44;
  isc_info_wal_recv_ckpt_poffset           = 45;
  isc_info_wal_grpc_wait_usecs             = 47;
  isc_info_wal_num_io                      = 48;
  isc_info_wal_avg_io_size                 = 49;
  isc_info_wal_num_commits                 = 50;
  isc_info_wal_avg_grpc_size               = 51;
  isc_info_forced_writes                   = 52;
  isc_info_user_names                      = 53;
  isc_info_page_errors                     = 54;
  isc_info_record_errors                   = 55;
  isc_info_bpage_errors                    = 56;
  isc_info_dpage_errors                    = 57;
  isc_info_ipage_errors                    = 58;
  isc_info_ppage_errors                    = 59;
  isc_info_tpage_errors                    = 60;
  isc_info_set_page_buffers                = 61;

  frb_info_att_charset                     = 101;
  isc_info_db_class                        = 102;
  isc_info_firebird_version                = 103;
  isc_info_oldest_transaction              = 104;
  isc_info_oldest_active                   = 105;
  isc_info_oldest_snapshot                 = 106;
  isc_info_next_transaction                = 107;
  isc_info_db_provider                     = 108;
  isc_info_active_transactions             = 109;
  isc_info_active_tran_count               = 110;
  isc_info_creation_date                   = 111;
  isc_info_db_file_size                    = 112;
  fb_info_page_contents                    = 113;

  isc_info_db_last_value                   = fb_info_page_contents;

(****************************************)
(** Database information return values **)
(****************************************)

  isc_info_db_impl_rdb_vms                 = 1;
  isc_info_db_impl_rdb_eln                 = 2;
  isc_info_db_impl_rdb_eln_dev             = 3;
  isc_info_db_impl_rdb_vms_y               = 4;
  isc_info_db_impl_rdb_eln_y               = 5;
  isc_info_db_impl_jri                     = 6;
  isc_info_db_impl_jsv                     = 7;

  isc_info_db_impl_isc_apl_68K             = 25;
  isc_info_db_impl_isc_vax_ultr            = 26;
  isc_info_db_impl_isc_vms                 = 27;
  isc_info_db_impl_isc_sun_68k             = 28;
  isc_info_db_impl_isc_os2                 = 29;
  isc_info_db_impl_isc_sun4                = 30;
  isc_info_db_impl_isc_hp_ux               = 31;
  isc_info_db_impl_isc_sun_386i            = 32;
  isc_info_db_impl_isc_vms_orcl            = 33;
  isc_info_db_impl_isc_mac_aux             = 34;
  isc_info_db_impl_isc_rt_aix              = 35;
  isc_info_db_impl_isc_mips_ult            = 36;
  isc_info_db_impl_isc_xenix               = 37;
  isc_info_db_impl_isc_dg                  = 38;
  isc_info_db_impl_isc_hp_mpexl            = 39;
  isc_info_db_impl_isc_hp_ux68K            = 40;
  isc_info_db_impl_isc_sgi                 = 41;
  isc_info_db_impl_isc_sco_unix            = 42;
  isc_info_db_impl_isc_cray                = 43;
  isc_info_db_impl_isc_imp                 = 44;
  isc_info_db_impl_isc_delta               = 45;

  isc_info_db_impl_isc_next                = 46;
  isc_info_db_impl_isc_dos                 = 47;
  isc_info_db_impl_m88K                    = 48;
  isc_info_db_impl_unixware                = 49;
  isc_info_db_impl_isc_winnt_x86           = 50;
  isc_info_db_impl_isc_epson               = 51;
  isc_info_db_impl_alpha_osf               = 52;
  isc_info_db_impl_alpha_vms               = 53;
  isc_info_db_impl_netware_386             = 54;
  isc_info_db_impl_win_only                = 55;
  isc_info_db_impl_ncr_3000                = 56;
  isc_info_db_impl_winnt_ppc               = 57;
  isc_info_db_impl_dg_x86                  = 58;
  isc_info_db_impl_sco_ev                  = 59;
  isc_info_db_impl_i386                    = 60;

  isc_info_db_impl_freebsd                 = 61;
  isc_info_db_impl_netbsd                  = 62;
  isc_info_db_impl_darwin                  = 63;
  isc_info_db_impl_sinixz                  = 64;

  isc_info_db_impl_linux_sparc             = 65;
  isc_info_db_impl_linux_amd64             = 66;

  isc_info_db_impl_freebsd_amd64           = 67;

  isc_info_db_impl_winnt_amd64             = 68;

  isc_info_db_impl_linux_ppc               = 69;
  isc_info_db_impl_darwin_x86              = 70;
  isc_info_db_impl_linux_mipsel            = 71;
  isc_info_db_impl_linux_mips              = 72;
  isc_info_db_impl_darwin_x64              = 73;
  isc_info_db_impl_sun_amd64               = 74;

  isc_info_db_impl_linux_arm               = 75;
  isc_info_db_impl_linux_ia64              = 76;

  isc_info_db_impl_darwin_ppc64            = 77;
  isc_info_db_impl_linux_s390x             = 78;
  isc_info_db_impl_linux_s390              = 79;

  isc_info_db_impl_linux_sh                = 80;
  isc_info_db_impl_linux_sheb              = 81;
  isc_info_db_impl_linux_hppa              = 82;
  isc_info_db_impl_linux_alpha             = 83;

  isc_info_db_impl_last_value              = isc_info_db_impl_linux_alpha;

  isc_info_db_class_access                 = 1;
  isc_info_db_class_y_valve                = 2;
  isc_info_db_class_rem_int                = 3;
  isc_info_db_class_rem_srvr               = 4;
  isc_info_db_class_pipe_int               = 7;
  isc_info_db_class_pipe_srvr              = 8;
  isc_info_db_class_sam_int                = 9;
  isc_info_db_class_sam_srvr               = 10;
  isc_info_db_class_gateway                = 11;
  isc_info_db_class_cache                  = 12;
  isc_info_db_class_classic_access         = 13;
  isc_info_db_class_server_access          = 14;

  isc_info_db_class_last_value             = isc_info_db_class_server_access;

//  info_db_provider
  isc_info_db_code_rdb_eln                 = 1;
  isc_info_db_code_rdb_vms                 = 2;
  isc_info_db_code_interbase               = 3;
  isc_info_db_code_firebird                = 4;

  isc_info_db_code_last_value              = isc_info_db_code_firebird;

(*******************************)
(** Request information items **)
(*******************************)

  isc_info_number_messages                 = 4;
  isc_info_max_message                     = 5;
  isc_info_max_send                        = 6;
  isc_info_max_receive                     = 7;
  isc_info_state                           = 8;
  isc_info_message_number                  = 9;
  isc_info_message_size                    = 10;
  isc_info_request_cost                    = 11;
  isc_info_access_path                     = 12;
  isc_info_req_select_count                = 13;
  isc_info_req_insert_count                = 14;
  isc_info_req_update_count                = 15;
  isc_info_req_delete_count                = 16;

(***********************)
(** Access path items **)
(***********************)

  isc_info_rsb_end                         = 0;
  isc_info_rsb_begin                       = 1;
  isc_info_rsb_type                        = 2;
  isc_info_rsb_relation                    = 3;
  isc_info_rsb_plan                        = 4;

(***************)
(** Rsb types **)
(***************)

  isc_info_rsb_unknown                     = 1;
  isc_info_rsb_indexed                     = 2;
  isc_info_rsb_navigate                    = 3;
  isc_info_rsb_sequential                  = 4;
  isc_info_rsb_cross                       = 5;
  isc_info_rsb_sort                        = 6;
  isc_info_rsb_first                       = 7;
  isc_info_rsb_boolean                     = 8;
  isc_info_rsb_union                       = 9;
  isc_info_rsb_aggregate                   = 10;
  isc_info_rsb_merge                       = 11;
  isc_info_rsb_ext_sequential              = 12;
  isc_info_rsb_ext_indexed                 = 13;
  isc_info_rsb_ext_dbkey                   = 14;
  isc_info_rsb_left_cross                  = 15;
  isc_info_rsb_select                      = 16;
  isc_info_rsb_sql_join                    = 17;
  isc_info_rsb_simulate                    = 18;
  isc_info_rsb_sim_cross                   = 19;
  isc_info_rsb_once                        = 20;
  isc_info_rsb_procedure                   = 21;
  isc_info_rsb_skip                        = 22;
  isc_info_rsb_virt_sequential             = 23;
  isc_info_rsb_recursive                   = 24;

(************************)
(** Bitmap expressions **)
(************************)

  isc_info_rsb_and                         = 1;
  isc_info_rsb_or                          = 2;
  isc_info_rsb_dbkey                       = 3;
  isc_info_rsb_index                       = 4;

  isc_info_req_active                      = 2;
  isc_info_req_inactive                    = 3;
  isc_info_req_send                        = 4;
  isc_info_req_receive                     = 5;
  isc_info_req_select                      = 6;
  isc_info_req_sql_stall                   = 7;

(****************************)
(** Blob information items **)
(****************************)

  isc_info_blob_num_segments               = 4;
  isc_info_blob_max_segment                = 5;
  isc_info_blob_total_length               = 6;
  isc_info_blob_type                       = 7;

(***********************************)
(** Transaction information items **)
(***********************************)

  isc_info_tra_id                          = 4;
  isc_info_tra_oldest_interesting          = 5;
  isc_info_tra_oldest_snapshot             = 6;
  isc_info_tra_oldest_active               = 7;
  isc_info_tra_isolation                   = 8;
  isc_info_tra_access                      = 9;
  isc_info_tra_lock_timeout                = 10;

  isc_info_tra_consistency                 = 1;
  isc_info_tra_concurrency                 = 2;
  isc_info_tra_read_committed              = 3;

  isc_info_tra_no_rec_version              = 0;
  isc_info_tra_rec_version                 = 1;

  isc_info_tra_readonly                    = 0;
  isc_info_tra_readwrite                   = 1;

(***************************)
(** SQL information items **)
(***************************)

  isc_info_sql_select                      = 4;
  isc_info_sql_bind                        = 5;
  isc_info_sql_num_variables               = 6;
  isc_info_sql_describe_vars               = 7;
  isc_info_sql_describe_end                = 8;
  isc_info_sql_sqlda_seq                   = 9;
  isc_info_sql_message_seq                 = 10;
  isc_info_sql_type                        = 11;
  isc_info_sql_sub_type                    = 12;
  isc_info_sql_scale                       = 13;
  isc_info_sql_length                      = 14;
  isc_info_sql_null_ind                    = 15;
  isc_info_sql_field                       = 16;
  isc_info_sql_relation                    = 17;
  isc_info_sql_owner                       = 18;
  isc_info_sql_alias                       = 19;
  isc_info_sql_sqlda_start                 = 20;
  isc_info_sql_stmt_type                   = 21;
  isc_info_sql_get_plan                    = 22;
  isc_info_sql_records                     = 23;
  isc_info_sql_batch_fetch                 = 24;
  isc_info_sql_relation_alias              = 25;

(***********************************)
(** SQL information return values **)
(***********************************)

  isc_info_sql_stmt_select                 = 1;
  isc_info_sql_stmt_insert                 = 2;
  isc_info_sql_stmt_update                 = 3;
  isc_info_sql_stmt_delete                 = 4;
  isc_info_sql_stmt_ddl                    = 5;
  isc_info_sql_stmt_get_segment            = 6;
  isc_info_sql_stmt_put_segment            = 7;
  isc_info_sql_stmt_exec_procedure         = 8;
  isc_info_sql_stmt_start_trans            = 9;
  isc_info_sql_stmt_commit                 = 10;
  isc_info_sql_stmt_rollback               = 11;
  isc_info_sql_stmt_select_for_upd         = 12;
  isc_info_sql_stmt_set_generator          = 13;
  isc_info_sql_stmt_savepoint              = 14;


{ Error codes from  }

  isc_err_base                             = 335544320;
  isc_arg_sql_state                        = 19;

  XSQLVAR_SIZE                             = sizeof(TXSQLVAR);

function XSQLDA_LENGTH(n: Long): Long;

(*
#define ADD_SPB_LENGTH(p, length)	{*(p)++ = (length); \
          *(p)++ = (length) >> 8;}

#define ADD_SPB_NUMERIC(p, data)	{*(p)++ = (data); \
          *(p)++ = (data) >> 8; \
      *(p)++ = (data) >> 16; \
      *(p)++ = (data) >> 24;}
*)
procedure add_spb_length(var p: PByte; length: integer);
procedure add_spb_numeric(var p: PByte; data: integer);


const
  sCRLF = #13#10;
  sCR   = #$0D;
  sLF   = #$0A;
  sTAB  = #9;
  sNULL_TERMINATOR = #0;
  {$ifdef WINDOWS}
  sLineFeed = sCRLF;
  {$else}
  sLineFeed = sLF;
  {$endif}

resourcestring
{ generic strings used in code }
  SFBDatabaseEditor = 'Da&tabase Editor...';
  SFBTransactionEditor = '&Transaction Editor...';
  SDatabaseFilter = 'Database Files (*.fdb)|*.fdb|All files (*.*)|*.*';
  SDisconnectDatabase = 'Database is currently connected. Disconnect and continue?';
  SCommitTransaction = 'Transaction is currently Active. Rollback and continue?';
  SExecute = 'E&xecute';
  SNoDataSet = 'No dataset association';
  SSQLGenSelect = 'Must select at least one key field and one update field';
  SSQLNotGenerated = 'Update SQL statements not generated, exit anyway?';
  SFBUpdateSQLEditor = '&UpdateSQL Editor...';
  SFBDataSetEditor = '&Dataset Editor...';
  SSQLDataSetOpen = 'Unable to determine field names for %s';
  STransaction = '%s, Default';

{ strings used in error messages}
  SUnknownError = 'Unknown error';
  SFirebirdInstallMissing = 'Firebird Install DLL ibinstall.dll not found in the path. Please install Firebird 6 to use this functionality';
  SFBCfeature = '%s is an Firebird 2.0 function. Please upgrade to Firebird 6 to use this functonality';
  SNotSupported = 'Unsupported feature';
  SNotPermitted = 'Not permitted';
  SFileAccessError = 'Temporary file access error';
  SConnectionTimeout = 'Database connection timed out';
  SCannotSetDatabase = 'Cannot set database';
  SCannotSetTransaction = 'Cannot set transaction';
  SOperationCancelled = 'Operation cancelled at user''s request';
  SDPBConstantNotSupported = 'DPB Constant (isc_dpb_%s) is unsupported';
  SDPBConstantUnknown = 'DPB Constant (%d) is unknown';
  STPBConstantNotSupported = 'TPB Constant (isc_tpb_%s) is unsupported';
  STPBConstantUnknown = 'TPB Constant (%d) is unknown';
  SDatabaseClosed = 'Cannot perform operation -- DB is not open';
  SDatabaseOpen = 'Cannot perform operation -- DB is currently open';
  SDatabaseNameMissing = 'Database name is missing';
  SNotInTransaction = 'Transaction is not active';
  SInTransaction = 'Transaction is active';
  STimeoutNegative = 'Timeout values cannot be negative';
  SUpdateWrongDB = 'Updating wrong database';
  SUpdateWrongTR = 'Updating wrong transaction. Unique transaction expected in set';
  SDatabaseNotAssigned = 'Database not assigned';
  STransactionNotAssigned = 'Transaction not assigned';
  SXSQLDAIndexOutOfRange = 'XSQLDA index out of range';
  SXSQLDANameDoesNotExist = 'XSQLDA name does not exist (%s)';
  SEOF = 'End of file';
  SBOF = 'Beginning of file';
  SInvalidStatementHandle = 'Invalid statement handle';
  SSQLOpen = 'FBDSQL Open';
  SSQLClosed = 'FBDSQL Closed';
  SDatasetOpen = 'Dataset open';
  SDatasetClosed = 'Dataset closed';
  SUnknownSQLDataType = 'Unknown SQL Data type (%d)';
  SInvalidColumnIndex = 'Invalid column index (index exceeds permitted range)';
  SInvalidParamColumnIndex = 'Invalid parameter index (index exceeds permitted range)';
  SInvalidDataConversion = 'Invalid data conversion';
  SColumnIsNotNullable = 'Column cannot be set to null (%s)';
  SBlobCannotBeRead = 'Blob stream cannot be read';
  SBlobCannotBeWritten = 'Blob stream cannot be written';
  SEmptyQuery = 'Empty query';
  SCannotOpenNonSQLSelect = 'Cannot "open" a non-select statement. Use ExecQuery';
  SNoFieldAccess = 'No access to field "%s"';
  SFieldReadOnly = 'Field "%s" is read-only';
  SFieldNotFound = 'Field "%s" not found';
  SNotEditing = 'Not in edit mode';
  SCannotInsert = 'Cannot insert into dataset. (No insert query)';
  SCannotPost = 'Cannot post. (No update/insert query)';
  SCannotUpdate = 'Cannot update. (No update query)';
  SCannotDelete = 'Cannot delete from dataset. (No delete query)';
  SCannotRefresh = 'Cannot refresh row. (No refresh query)';
  SBufferNotSet = 'Buffer not set';
  SCircularReference = 'Circular references not permitted';
  SSQLParseError = 'SQL Parse Error:' + sLineFeed + sLineFeed + '%s';
  SUserAbort = 'User abort';
  SDataSetUniDirectional = 'Data set is uni-directional';
  SCannotCreateSharedResource = 'Cannot create shared resource. (Windows error %d)';
  SWindowsAPIError = 'Windows API error. (Windows error %d [$%.8x])';
  SColumnListsDontMatch = 'Column lists do not match';
  SColumnTypesDontMatch = 'Column types don''t match. (From index: %d; To index: %d)';
  SFieldUnsupportedType = 'Unsupported Field Type';
  SCircularDataLink = 'Circular DataLink Reference';
  SEmptySQLStatement = 'Empty SQL Statement';
  SIsASelectStatement = 'use Open for a Select Statement';
  SRequiredParamNotSet = 'Required Param value not set';
  SNoStoredProcName = 'No Stored Procedure Name assigned';
  SIsAExecuteProcedure = 'use ExecProc for Procedure; use TQuery for Select procedures';
  SUpdateFailed = 'Update Failed';
  SNotCachedUpdates = 'CachedUpdates not enabled';
  SNotLiveRequest = 'Request is not live - cannot modify';
  SNoProvider = 'No Provider';
  SNoRecordsAffected = 'No Records Affected';
  SNoTableName = 'No Table Name assigned';
  SCannotCreatePrimaryIndex = 'Cannot Create Primary Index; are created automatically';
  SCannotDropSystemIndex = 'Cannot Drop System Index';
  STableNameMismatch = 'Table Name Mismatch';
  SIndexFieldMissing = 'Index Field Missing';
  SInvalidCancellation = 'Cannot Cancel events while processing';
  SInvalidEvent = 'Invalid Event';
  SMaximumEvents = 'Exceded Maximum Event limits';
  SNoEventsRegistered = 'No Events Registered';
  SInvalidQueueing = 'Invalid Queueing';
  SInvalidRegistration = 'Invalid Registration';
  SInvalidBatchMove = 'Invalid Batch Move';
  SSQLDialectInvalid = 'SQL Dialect Invalid';
  SSPBConstantNotSupported = 'SPB Constant Not supported';
  SSPBConstantUnknown = 'SPB Constant Unknown';
  SServiceActive = 'Cannot perform operation -- service is not attached';
  SServiceInActive = 'Cannot perform operation -- service is attached';
  SServerNameMissing = 'Server Name Missing';
  SQueryParamsError = 'Query Parameters missing or incorrect';
  SStartParamsError = 'start Parameters missing or incorrect';
  SOutputParsingError = 'Unexpected Output buffer value';
  SUseSpecificProcedures = 'Generic ServiceStart not applicable: Use Specific Procedures to set configuration params';
  SSQLMonitorAlreadyPresent = 'SQL Monitor Instance is already present';
  SCantPrintValue = 'Cannot print value';
  SEOFReached = 'SEOFReached';
  SEOFInComment = 'EOF in comment detected';
  SEOFInString = 'EOF in string detected';
  SParamNameExpected = 'Parameter name expected';
  SSuccess = 'Successful execution';
  SException = 'Exception %s';
  SNoOptionsSet = 'No Install Options selected';
  SNoDestinationDirectory = 'DestinationDirectory is not set';
  SNosourceDirectory = 'SourceDirectory is not set';
  SNoUninstallFile = 'Uninstall File Name is not set';
  SOptionNeedsClient = '%s component requires Client to function properly';
  SOptionNeedsServer = '%s component requires Server to function properly';
  SInvalidOption = 'Invalid option specified';
  SInvalidOnErrorResult = 'Unexpected onError return value';
  SInvalidOnStatusResult = 'Unexpected onStatus return value';

  SFirebirdExpressVersion = 'FirebirdExpress 1.1';
  SEditSQL = 'Edit SQL';
  SDPBConstantUnknownEx = 'DPB Constant (%s) is unknown';
  STPBConstantUnknownEx = 'TPB Constant (%s) is unknown';
  SFirebirdExpressVersionEx = 'FirebirdExpress %g';
  SUnknownPlan = 'Unknown Error - Can''t retrieve plan';
  SFieldSizeMismatch = 'Size Mismatch - Field %s size is too small for data';
  SEventAlreadyRegistered   = 'Events already registered';
  SStringTooLarge = 'Trying to store a string of length %d into a field that can only contain %d';
  SFBServiceEditor = '&Service Editor ...';
  SFBSuccessConnect = 'Successful Connection';
  SFBInvalidStatement = 'Invalid statement';
  SFBInvalidComment = 'Invalid Comment';

const
  DPBPrefix = 'isc_dpb_';
  DPBConstantNames: array[1..isc_dpb_last_dpb_constant] of string = (
    'cdd_pathname',
    'allocation',
    'journal',
    'page_size',
    'num_buffers',
    'buffer_length',
    'debug',
    'garbage_collect',
    'verify',
    'sweep',
    'enable_journal',
    'disable_journal',
    'dbkey_scope',
    'number_of_users',
    'trace',
    'no_garbage_collect',
    'damaged',
    'license',
    'sys_user_name',
    'encrypt_key',
    'activate_shadow',
    'sweep_interval',
    'delete_shadow',
    'force_write',
    'begin_log',
    'quit_log',
    'no_reserve',
    'user_name',
    'password',
    'password_enc',
    'sys_user_name_enc',
    'interp',
    'online_dump',
    'old_file_size',
    'old_num_files',
    'old_file',
    'old_start_page',
    'old_start_seqno',
    'old_start_file',
    'drop_walfile',
    'old_dump_id',
    'wal_backup_dir',
    'wal_chkptlen',
    'wal_numbufs',
    'wal_bufsize',
    'wal_grp_cmt_wait',
    'lc_messages',
    'lc_ctype',
    'cache_manager',
    'shutdown',
    'online',
    'shutdown_delay',
    'reserved',
    'overwrite',
    'sec_attach',
    'disable_wal',
    'connect_timeout',
    'dummy_packet_interval',
    'gbak_attach',
    'sql_role_name',
    'set_page_buffers',
    'working_directory',
    'sql_dialect',
    'set_db_readonly',
    'set_db_sql_dialect',
    'gfix_attach',
    'gstat_attach',
    'set_db_charset',
    'gsec_attach',
    'address_pat',
    'process_id ',
    'no_db_trigg',
    'trusted_auth',
    'process_name',
    'trusted_role ',
    'org_filename',
    'utf8_filename',
    'ext_call_depth'
    );

  TPBPrefix = 'isc_tpb_';
  TPBConstantNames: array[1..isc_tpb_last_tpb_constant] of string = (
    'consistency',
    'concurrency',
    'shared',
    'protected',
    'exclusive',
    'wait',
    'nowait',
    'read',
    'write',
    'lock_read',
    'lock_write',
    'verb_time',
    'commit_time',
    'ignore_limbo',
    'read_committed',
    'autocommit',
    'rec_version',
    'no_rec_version',
    'restart_requests',
    'no_auto_undo',
    'lock_timeout'//fb2
    );

  QUOTE = '''';
  DBL_QUOTE = '"';

type

  { TmncFBLib }

  TmncFBLib = class(TmnLibrary)
  protected
    procedure Loaded; override;
  public
    BLOB_get: TBLOB_get;
    BLOB_put: TBLOB_put;
    Bopen: TBopen;
    Bclose: TBclose;

    isc_sqlcode: Tisc_sqlcode;
    isc_sql_interprete: Tisc_sql_interprete;
    fb_interpret: Tfb_interpret;
    isc_interprete: Tisc_interprete;
    isc_vax_integer: Tisc_vax_integer;
    isc_portable_integer: Tisc_portable_integer;
    isc_blob_info: Tisc_blob_info;
    isc_open_blob2: Tisc_open_blob2;
    isc_close_blob: Tisc_close_blob;
    isc_get_segment: Tisc_get_segment;
    isc_put_segment: Tisc_put_segment;
    isc_create_blob2: Tisc_create_blob2;
    isc_array_gen_sdl: Tisc_array_gen_sdl;
    isc_array_get_slice: Tisc_array_get_slice;
    isc_array_lookup_bounds: Tisc_array_lookup_bounds;
    isc_array_lookup_desc: Tisc_array_lookup_desc;
    isc_array_set_desc: Tisc_array_set_desc;
    isc_array_put_slice: Tisc_array_put_slice;
    isc_blob_default_desc: Tisc_blob_default_desc;
    isc_blob_gen_bpb: Tisc_blob_gen_bpb;
    isc_blob_lookup_desc: Tisc_blob_lookup_desc;
    isc_blob_set_desc: Tisc_blob_set_desc;
    isc_cancel_blob: Tisc_cancel_blob;

    isc_version: Tisc_version;

    isc_service_attach: Tisc_service_attach;
    isc_service_detach: Tisc_service_detach;
    isc_service_query: Tisc_service_query;
    isc_service_start: Tisc_service_start;
    isc_decode_date: Tisc_decode_date;
    isc_decode_sql_date: Tisc_decode_sql_date;
    isc_decode_sql_time: Tisc_decode_sql_time;
    isc_decode_timestamp: Tisc_decode_timestamp;
    isc_encode_date: Tisc_encode_date;
    isc_encode_sql_date: Tisc_encode_sql_date;
    isc_encode_sql_time: Tisc_encode_sql_time;
    isc_encode_timestamp: Tisc_encode_timestamp;
    isc_dsql_free_statement: Tisc_dsql_free_statement;
    isc_dsql_execute2: Tisc_dsql_execute2;
    isc_dsql_execute: Tisc_dsql_execute;
    isc_dsql_set_cursor_name: Tisc_dsql_set_cursor_name;
    isc_dsql_fetch: Tisc_dsql_fetch;
    isc_dsql_sql_info: Tisc_dsql_sql_info;
    isc_dsql_alloc_statement2: Tisc_dsql_alloc_statement2;
    isc_dsql_prepare: Tisc_dsql_prepare;
    isc_dsql_describe_bind: Tisc_dsql_describe_bind;
    isc_dsql_describe: Tisc_dsql_describe;
    isc_dsql_execute_immediate: Tisc_dsql_execute_immediate;
    isc_drop_database: Tisc_drop_database;
    isc_detach_database: Tisc_detach_database;
    isc_attach_database: Tisc_attach_database;
    isc_database_info: Tisc_database_info;
    isc_start_multiple: Tisc_start_multiple;
    isc_commit_transaction: Tisc_commit_transaction;
    isc_commit_retaining: Tisc_commit_retaining;
    isc_rollback_transaction: Tisc_rollback_transaction;
    isc_rollback_retaining: Tisc_rollback_retaining;
    isc_cancel_events: Tisc_cancel_events;
    isc_que_events: Tisc_que_events;
    isc_event_counts: Tisc_event_counts;
    isc_event_block: Tisc_event_block;
    isc_free: Tisc_free;
    isc_add_user: Tisc_add_user;
    isc_delete_user: Tisc_delete_user;
    isc_modify_user: Tisc_modify_user;
    isc_prepare_transaction: Tisc_prepare_transaction;
    isc_prepare_transaction2: Tisc_prepare_transaction2;

    isc_get_client_version: Tisc_get_client_version;
    isc_get_client_major_version: Tisc_get_client_major_version;
    isc_get_client_minor_version: Tisc_get_client_minor_version;

  end;

var
  FBLib: TmncFBLib = nil;

implementation

function XSQLDA_LENGTH(n: Long): Long;
(*  The C-macro reads like this:
   XSQLDA_LENGTH(n)	(sizeof (XSQLDA) + (n-1) * sizeof (XSQLVAR)) *)
begin
  Result := SizeOf(TXSQLDA) + ((n - 1) * SizeOf(TXSQLVAR));
end;

(*******************************************)
(** Service manager functions             **)
(*******************************************)

procedure add_spb_length(var p: PByte; length: integer);
(*
#define ADD_SPB_LENGTH(p, length)	{*(p)++ = (length); \
          *(p)++ = (length) >> 8;}
*)
begin
  p^ := length;
  Inc(p);
  p^ := length shr 8;
  Inc(p);
end;

procedure add_spb_numeric(var p: PByte; data: integer);
(*
#define ADD_SPB_NUMERIC(p, data)	{*(p)++ = (data); \
          *(p)++ = (data) >> 8; \
      *(p)++ = (data) >> 16; \
      *(p)++ = (data) >> 24;}
*)
begin
  p^ := data;
  Inc(p);
  p^ := data shr 8;
  Inc(p);
  p^ := data shr 16;
  Inc(p);
  p^ := data shr 24;
  Inc(p);
end;

{ TmncFBLib }

procedure TmncFBLib.Loaded;
begin
  BLOB_get := GetAddress('BLOB_get');
  BLOB_put := GetAddress('BLOB_put');
  Bopen := GetAddress('Bopen');
  Bclose := GetAddress('BLOB_close');

  isc_sqlcode := GetAddress('isc_sqlcode');
  isc_sql_interprete := GetAddress('isc_sql_interprete');
  fb_interpret := GetAddress('fb_interpret');
  isc_interprete := GetAddress('isc_interprete');
  isc_vax_integer := GetAddress('isc_vax_integer');
  isc_portable_integer := GetAddress('isc_portable_integer');
  isc_blob_info := GetAddress('isc_blob_info');
  isc_open_blob2 := GetAddress('isc_open_blob2');
  isc_close_blob := GetAddress('isc_close_blob');
  isc_get_segment := GetAddress('isc_get_segment');
  isc_put_segment := GetAddress('isc_put_segment');
  isc_create_blob2 := GetAddress('isc_create_blob2');
  isc_cancel_blob := GetAddress('isc_cancel_blob');

  isc_version := GetAddress('isc_version');

  isc_array_gen_sdl := GetAddress('isc_array_gen_sdl');
  isc_array_get_slice := GetAddress('isc_array_get_slice');
  isc_array_lookup_bounds := GetAddress('isc_array_lookup_bounds');
  isc_array_lookup_desc := GetAddress('isc_array_lookup_desc');
  isc_array_set_desc := GetAddress('isc_array_set_desc');
  isc_array_put_slice := GetAddress('isc_array_put_slice');
  isc_blob_default_desc := GetAddress('isc_blob_default_desc');
  isc_blob_gen_bpb := GetAddress('isc_blob_gen_bpb');
  isc_blob_lookup_desc := GetAddress('isc_blob_lookup_desc');
  isc_blob_set_desc := GetAddress('isc_blob_set_desc');
  isc_decode_date := GetAddress('isc_decode_date');
  isc_encode_date := GetAddress('isc_encode_date');
  isc_dsql_free_statement := GetAddress('isc_dsql_free_statement');
  isc_dsql_execute2 := GetAddress('isc_dsql_execute2');
  isc_dsql_execute := GetAddress('isc_dsql_execute');
  isc_dsql_set_cursor_name := GetAddress('isc_dsql_set_cursor_name');
  isc_dsql_fetch := GetAddress('isc_dsql_fetch');
  isc_dsql_sql_info := GetAddress('isc_dsql_sql_info');
  isc_dsql_alloc_statement2 := GetAddress('isc_dsql_alloc_statement2');
  isc_dsql_prepare := GetAddress('isc_dsql_prepare');
  isc_dsql_describe_bind := GetAddress('isc_dsql_describe_bind');
  isc_dsql_describe := GetAddress('isc_dsql_describe');
  isc_dsql_execute_immediate := GetAddress('isc_dsql_execute_immediate');
  isc_drop_database := GetAddress('isc_drop_database');
  isc_detach_database := GetAddress('isc_detach_database');
  isc_attach_database := GetAddress('isc_attach_database', true);
  isc_database_info := GetAddress('isc_database_info');
  isc_start_multiple := GetAddress('isc_start_multiple');
  isc_commit_transaction := GetAddress('isc_commit_transaction');
  isc_commit_retaining := GetAddress('isc_commit_retaining');
  isc_rollback_transaction := GetAddress('isc_rollback_transaction');
  isc_cancel_events := GetAddress('isc_cancel_events');
  isc_que_events := GetAddress('isc_que_events');
  isc_event_counts := GetAddress('isc_event_counts');
  isc_event_block := GetAddress('isc_event_block');
  isc_free := GetAddress('isc_free');
  isc_add_user := GetAddress('isc_add_user');
  isc_delete_user := GetAddress('isc_delete_user');
  isc_modify_user := GetAddress('isc_modify_user');
  isc_prepare_transaction := GetAddress('isc_prepare_transaction');
  isc_prepare_transaction2 := GetAddress('isc_prepare_transaction2');

  isc_rollback_retaining := GetAddress('isc_rollback_retaining');
  isc_service_attach := GetAddress('isc_service_attach');
  isc_service_detach := GetAddress('isc_service_detach');
  isc_service_query := GetAddress('isc_service_query');
  isc_service_start := GetAddress('isc_service_start');
  isc_decode_sql_date := GetAddress('isc_decode_sql_date');
  isc_decode_sql_time := GetAddress('isc_decode_sql_time');
  isc_decode_timestamp := GetAddress('isc_decode_timestamp');
  isc_encode_sql_date := GetAddress('isc_encode_sql_date');
  isc_encode_sql_time := GetAddress('isc_encode_sql_time');
  isc_encode_timestamp := GetAddress('isc_encode_timestamp');
  isc_get_client_version := GetAddress('isc_get_client_version');
  isc_get_client_major_version := GetAddress('isc_get_client_major_version');
  isc_get_client_minor_version := GetAddress('isc_get_client_minor_version');
  //FClientVersion := isc_get_client_major_version + (isc_get_client_minor_version / 10);
end;

initialization
  FBLib := TmncFBLib.Create('fbclient');
finalization
  FreeAndNil(FBLib);
end.

