{* Blr.h *}

const
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

  blr_bool			= 23;
  blr_dec64			= 24;
  blr_dec128			= 25;
  blr_int128			= 26;
  blr_sql_time_tz		= 28;
  blr_timestamp_tz	= 29;
  blr_ex_time_tz		= 30;
  blr_ex_timestamp_tz	= 31;

  // first sub parameter for blr_domain_name[2]
  blr_domain_type_of                       = 0;
  blr_domain_full                          = 1;

  {* Historical alias for pre V6 applications *}
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
  blr_exception_params	                   = 7;
  blr_sql_state			                       = 8;

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
  blr_receive_batch	                       = 32;

  // unused code: 33

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
  blr_parameter3                           = 88; {* same as Rdb definition *}

  {* unsupported
  blr_run_max                              = 89;
  blr_run_min                              = 90;
  blr_run_total                            = 91;
  blr_run_average                          = 92;
  }
  blr_agg_count2                           = 93;
  blr_agg_count_distinct                   = 94;
  blr_agg_total_distinct                   = 95;
  blr_agg_average_distinct                 = 96;

  // unused codes: 97..99

  blr_function                             = 100;
  blr_gen_id                               = 101;
  //blr_prot_mask                            = 102;
  blr_upcase                               = 103;
  //blr_lock_state                           = 104;
  blr_value_if                             = 105;
  blr_matching2                            = 106;
  blr_index                                = 107;
  blr_ansi_like                            = 108;
  blr_scrollable		                       = 109;
  blr_lateral_rse		                       = 110;

  // unused codes: 111..117

  blr_run_count		= 118; {* changed from 88 to avoid conflict with blr_parameter3 *}
  blr_rs_stream		= 119;
  blr_exec_proc		= 120;

  // unused codes: 121..123

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

  // unused codes: 136..138

  blr_plan                                 = 139; {* access plan items *}
  blr_merge                                = 140;
  blr_join                                 = 141;
  blr_sequential                           = 142;
  blr_navigational                         = 143;
  blr_indices                              = 144;
  blr_retrieve                             = 145;

  blr_relation2                            = 146;
  blr_rid2                                 = 147;

  // unused codes: 148..149

  blr_set_generator                        = 150;

  blr_ansi_any                             = 151; {* required for NULL handling *}
  blr_exists                               = 152; {* required for NULL handling *}

  // unused codes: 153

  blr_record_version                       = 154; {* get tid of record *}
  blr_stall                                = 155; {* fake server stall *}

  // unused codes: 156..157

  blr_ansi_all                             = 158; {* required for NULL handling *}

  {* sub parameters for blr_extract *}

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
  blr_extract_timezone_hour	               = 10;
  blr_extract_timezone_minute	             = 11;

  blr_current_date                         = 160;
  blr_current_timestamp                    = 161;
  blr_current_time                         = 162;

  {* These codes reuse BLR code space *}

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

  // unused codes: 173

  {* FB 1.0 specific BLR *}

  blr_current_role                         = 174;
  blr_skip                                 = 175;

  {* FB 1.5 specific BLR *}

  blr_exec_sql                             = 176;
  blr_internal_info                        = 177;
  blr_nullsfirst                           = 178;
  blr_writelock                            = 179;
  blr_nullslast                            = 180;

  {* FB 2.0 specific BLR *}

  blr_lowcase                              = 181;
  blr_strlen                               = 182;

  {* sub parameter for blr_strlen *}

  blr_strlen_bit                           = 0;
  blr_strlen_char                          = 1;
  blr_strlen_octet                         = 2;

  blr_trim                                 = 183;

  {* first sub parameter for blr_trim *}

  blr_trim_both                            = 0;
  blr_trim_leading                         = 1;
  blr_trim_trailing                        = 2;

  {* second sub parameter for blr_trim *}

  blr_trim_spaces                          = 0;
  blr_trim_characters                      = 1;

  {* These codes are actions for user-defined savepoints *}

  blr_savepoint_set                        = 0;
  blr_savepoint_release                    = 1;
  blr_savepoint_undo                       = 2;
  blr_savepoint_release_single             = 3;

  {* These codes are actions for cursors *}

  blr_cursor_open                          = 0;
  blr_cursor_close                         = 1;
  blr_cursor_fetch                         = 2;
  blr_cursor_fetch_scroll                  = 3;

  {* scroll options *}

  blr_scroll_forward		= 0;
  blr_scroll_backward		= 1;
  blr_scroll_bof			= 2;
  blr_scroll_eof			= 3;
  blr_scroll_absolute		= 4;
  blr_scroll_relative		= 5;

  {* FB 2.1 specific BLR *}

  blr_init_variable                        = 184;
  blr_recurse                              = 185;
  blr_sys_function                         = 186;

  // FB 2.5 specific BLR

  blr_auto_trans                           = 187;
  blr_similar                              = 188;
  blr_exec_stmt                            = 189;

  // subcodes of blr_exec_stmt

  blr_exec_stmt_inputs                     =  1; // input parameters count
  blr_exec_stmt_outputs                    =  2; // output parameters count
  blr_exec_stmt_sql                        =  3;
  blr_exec_stmt_proc_block                 =  4;
  blr_exec_stmt_data_src                   =  5;
  blr_exec_stmt_user                       =  6;
  blr_exec_stmt_pwd                        =  7;
  blr_exec_stmt_tran                       =  8; // not implemented yet
  blr_exec_stmt_tran_clone                 =  9; // make transaction parameters equal to current transaction
  blr_exec_stmt_privs                      =  10;
  blr_exec_stmt_in_params                  =  11; // not named input parameters
  blr_exec_stmt_in_params2                 =  12; // named input parameters
  blr_exec_stmt_out_params                 =  13; // output parameters
  blr_exec_stmt_role                       =  14;
  blr_exec_stmt_in_excess		               =  15;  // excess input params numbers

  blr_stmt_expr                            =  190;
  blr_derived_expr                         =  191;

  // FB 3.0 specific BLR

  blr_procedure3				 =  192;
  blr_exec_proc2				 =  193;
  blr_function2				 =  194;
  blr_window					 =  195;
  blr_partition_by			 =  196;
  blr_continue_loop			 =  197;
  blr_procedure4				 =  198;
  blr_agg_function			 =  199;
  blr_substring_similar		 =  200;
  blr_bool_as_value			 =  201;
  blr_coalesce				 =  202;
  blr_decode					 =  203;
  blr_exec_subproc			 =  204;
  blr_subproc_decl			 =  205;
  blr_subproc					 =  206;
  blr_subfunc_decl			 =  207;
  blr_subfunc					 =  208;
  blr_record_version2			 =  209;
  blr_gen_id2					 =  210; // NEXT VALUE FOR generator

  // FB 4.0 specific BLR

  blr_window_win				 =  211;

  // subcodes of blr_window_win
  blr_window_win_partition			 =  1;
  blr_window_win_order				 =  2;
  blr_window_win_map					 =  3;
  blr_window_win_extent_unit 			 =  4;
  blr_window_win_extent_frame_bound	 =  5;
  blr_window_win_extent_frame_value	 =  6;
  blr_window_win_exclusion			 =  7;

  blr_default					 =  212;
  blr_store3					 =  213;

  // subcodes of blr_store3
  blr_store_override_user			 =  1;
  blr_store_override_system		 =  2;

  blr_local_timestamp			 =  214;
  blr_local_time				 =  215;

  blr_at						 =  216;

  // subcodes of blr_at
  blr_at_local				 =  0;
  blr_at_zone					 =  1;

  blr_marks					 =  217;		// mark some blr code with specific flags


