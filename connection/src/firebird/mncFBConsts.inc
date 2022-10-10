{*
 *	MODULE:		consts_pub.h
 *	DESCRIPTION:	Public constants' definitions
 *
 * The contents of this file are subject to the Interbase Public
 * License Version 1.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy
 * of the License at http://www.Inprise.com/IPL.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
 * or implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code was created by Inprise Corporation
 * and its predecessors. Portions created by Inprise Corporation are
 * Copyright (C) Inprise Corporation.
 *
 * All Rights Reserved.
 * Contributor(s): ______________________________________.
 *
 * 18.08.2006 Dimitry Sibiryakov: Extracted it from ibase.h
 *
 *}

{**********************************}
{* Database parameter block stuff *}
{**********************************}

isc_dpb_version1                  = 1;
isc_dpb_version2                  = 2;

isc_dpb_cdd_pathname              = 1;
isc_dpb_allocation                = 2;
isc_dpb_journal                   = 3;
isc_dpb_page_size                 = 4;
isc_dpb_num_buffers               = 5;
isc_dpb_buffer_length             = 6;
isc_dpb_debug                     = 7;
isc_dpb_garbage_collect           = 8;
isc_dpb_verify                    = 9;
isc_dpb_sweep                     = 10;
isc_dpb_enable_journal            = 11;
isc_dpb_disable_journal           = 12;
isc_dpb_dbkey_scope               = 13;
isc_dpb_number_of_users           = 14;
isc_dpb_trace                     = 15;
isc_dpb_no_garbage_collect        = 16;
isc_dpb_damaged                   = 17;
isc_dpb_license                   = 18;
isc_dpb_sys_user_name             = 19;
isc_dpb_encrypt_key               = 20;
isc_dpb_activate_shadow           = 21;
isc_dpb_sweep_interval            = 22;
isc_dpb_delete_shadow             = 23;
isc_dpb_force_write               = 24;
isc_dpb_begin_log                 = 25;
isc_dpb_quit_log                  = 26;
isc_dpb_no_reserve                = 27;
isc_dpb_user_name                 = 28;
isc_dpb_password                  = 29;
isc_dpb_password_enc              = 30;
isc_dpb_sys_user_name_enc         = 31;
isc_dpb_interp                    = 32;
isc_dpb_online_dump               = 33;
isc_dpb_old_file_size             = 34;
isc_dpb_old_num_files             = 35;
isc_dpb_old_file                  = 36;
isc_dpb_old_start_page            = 37;
isc_dpb_old_start_seqno           = 38;
isc_dpb_old_start_file            = 39;
isc_dpb_drop_walfile              = 40;
isc_dpb_old_dump_id               = 41;
isc_dpb_wal_backup_dir            = 42;
isc_dpb_wal_chkptlen              = 43;
isc_dpb_wal_numbufs               = 44;
isc_dpb_wal_bufsize               = 45;
isc_dpb_wal_grp_cmt_wait          = 46;
isc_dpb_lc_messages               = 47;
isc_dpb_lc_ctype                  = 48;
isc_dpb_cache_manager             = 49;
isc_dpb_shutdown                  = 50;
isc_dpb_online                    = 51;
isc_dpb_shutdown_delay            = 52;
isc_dpb_reserved                  = 53;
isc_dpb_overwrite                 = 54;
isc_dpb_sec_attach                = 55;
isc_dpb_disable_wal               = 56;
isc_dpb_connect_timeout           = 57;
isc_dpb_dummy_packet_interval     = 58;
isc_dpb_gbak_attach               = 59;
isc_dpb_sql_role_name             = 60;
isc_dpb_set_page_buffers          = 61;
isc_dpb_working_directory         = 62;
isc_dpb_sql_dialect               = 63;
isc_dpb_set_db_readonly           = 64;
isc_dpb_set_db_sql_dialect        = 65;
isc_dpb_gfix_attach               = 66;
isc_dpb_gstat_attach              = 67;
isc_dpb_set_db_charset            = 68;
isc_dpb_gsec_attach               = 69;		{* deprecated *}
isc_dpb_address_path              = 70;
isc_dpb_process_id                = 71;
isc_dpb_no_db_triggers            = 72;
isc_dpb_trusted_auth			  = 73;
isc_dpb_process_name              = 74;
isc_dpb_trusted_role			  = 75;
isc_dpb_org_filename			  = 76;
isc_dpb_utf8_filename			  = 77;
isc_dpb_ext_call_depth			  = 78;
isc_dpb_auth_block				  = 79;
isc_dpb_client_version			  = 80;
isc_dpb_remote_protocol			  = 81;
isc_dpb_host_name				  = 82;
isc_dpb_os_user					  = 83;
isc_dpb_specific_auth_data		  = 84;
isc_dpb_auth_plugin_list		  = 85;
isc_dpb_auth_plugin_name		  = 86;
isc_dpb_config					  = 87;
isc_dpb_nolinger				  = 88;
isc_dpb_reset_icu				  = 89;
isc_dpb_map_attach                = 90;
isc_dpb_session_time_zone         = 91;
isc_dpb_set_db_replica            = 92;
isc_dpb_set_bind                  = 93;
isc_dpb_decfloat_round            = 94;
isc_dpb_decfloat_traps            = 95;
isc_dpb_clear_map				  = 96;

isc_dpb_last_dpb_constant         = isc_dpb_clear_map;

{**************************************************}
{* clumplet tags used inside isc_dpb_address_path *}
{*						 and isc_spb_address_path *}
{**************************************************}

{* Format of this clumplet is the following:

 <address-path-clumplet> ::=
	isc_dpb_address_path <byte-clumplet-length> <address-stack>

 <address-stack> ::=
	<address-descriptor> |
	<address-stack> <address-descriptor>

 <address-descriptor> ::=
	isc_dpb_address <byte-clumplet-length> <address-elements>

 <address-elements> ::=
	<address-element> |
	<address-elements> <address-element>

 <address-element> ::=
	isc_dpb_addr_protocol <byte-clumplet-length> <protocol-string> |
	isc_dpb_addr_endpoint <byte-clumplet-length> <remote-endpoint-string> |
	isc_dpb_addr_flags <byte-clumplet-length> <flags-int> |
	isc_dpb_addr_crypt <byte-clumplet-length> <plugin-string>

 <protocol-string> ::=
	"TCPv4" |
	"TCPv6" |
	"XNET" |
	"WNET" |
	....

 <plugin-string> ::=
	"Arc4" |
	"ChaCha" |
	....

 <remote-endpoint-string> ::=
	<IPv4-address> | // such as "172.20.1.1"
	<IPv6-address> | // such as "2001:0:13FF:09FF::1"
	<xnet-process-id> | // such as "17864"
	...

 <flags-int> ::=
 	bitmask of possible flags
*}

isc_dpb_address = 1;

isc_dpb_addr_protocol = 1;
isc_dpb_addr_endpoint = 2;
isc_dpb_addr_flags = 3;
isc_dpb_addr_crypt = 4;

{* possible addr flags *}
isc_dpb_addr_flag_conn_compressed	= $01;
isc_dpb_addr_flag_conn_encrypted	= $02;

{*********************************}
{* isc_dpb_verify specific flags *}
{*********************************}

isc_dpb_pages                     = 1;
isc_dpb_records                   = 2;
isc_dpb_indices                   = 4;
isc_dpb_transactions              = 8;
isc_dpb_no_update                 = 16;
isc_dpb_repair                    = 32;
isc_dpb_ignore                    = 64;

{***********************************}
{* isc_dpb_shutdown specific flags *}
{***********************************}

isc_dpb_shut_cache               = $1;
isc_dpb_shut_attachment          = $2;
isc_dpb_shut_transaction         = $4;
isc_dpb_shut_force               = $8;
isc_dpb_shut_mode_mask          = $70;

isc_dpb_shut_default             = $0;
isc_dpb_shut_normal             = $10;
isc_dpb_shut_multi              = $20;
isc_dpb_shut_single             = $30;
isc_dpb_shut_full               = $40;

{*****************************************}
{* isc_dpb_set_db_replica specific flags *}
{*****************************************}

isc_dpb_replica_none             = 0;
isc_dpb_replica_read_only        = 1;
isc_dpb_replica_read_write       = 2;

{**************************************}
{* Bit assignments in RDB$SYSTEM_FLAG *}
{**************************************}

RDB_system                         = 1;
RDB_id_assigned                    = 2;

{* 2 is for QLI. See jrd/constants.h for more Firebird-specific values. *}


{*************************************}
{* Transaction parameter block stuff *}
{*************************************}

isc_tpb_version1                  = 1;
isc_tpb_version3                  = 3;
isc_tpb_consistency               = 1;
isc_tpb_concurrency               = 2;
isc_tpb_shared                    = 3;
isc_tpb_protected                 = 4;
isc_tpb_exclusive                 = 5;
isc_tpb_wait                      = 6;
isc_tpb_nowait                    = 7;
isc_tpb_read                      = 8;
isc_tpb_write                     = 9;
isc_tpb_lock_read                 = 10;
isc_tpb_lock_write                = 11;
isc_tpb_verb_time                 = 12;
isc_tpb_commit_time               = 13;
isc_tpb_ignore_limbo              = 14;
isc_tpb_read_committed	          = 15;
isc_tpb_autocommit                = 16;
isc_tpb_rec_version               = 17;
isc_tpb_no_rec_version            = 18;
isc_tpb_restart_requests          = 19;
isc_tpb_no_auto_undo              = 20;
isc_tpb_lock_timeout              = 21;
isc_tpb_read_consistency          = 22;
isc_tpb_at_snapshot_number        = 23;

isc_tpb_last_tpb_constant         = isc_tpb_at_snapshot_number;


{************************}
{* Blob Parameter Block *}
{************************}

isc_bpb_version1                  = 1;
isc_bpb_source_type               = 1;
isc_bpb_target_type               = 2;
isc_bpb_type                      = 3;
isc_bpb_source_interp             = 4;
isc_bpb_target_interp             = 5;
isc_bpb_filter_parameter          = 6;
isc_bpb_storage                   = 7;

isc_bpb_type_segmented            = $0;
isc_bpb_type_stream               = $1;
isc_bpb_storage_main              = $0;
isc_bpb_storage_temp              = $2;


{*********************************}
{* Service parameter block stuff *}
{*********************************}

isc_spb_version1                  = 1;
isc_spb_current_version           = 2;
isc_spb_version                   = isc_spb_current_version;
isc_spb_version3                  = 3;
isc_spb_user_name                 = isc_dpb_user_name;
isc_spb_sys_user_name             = isc_dpb_sys_user_name;
isc_spb_sys_user_name_enc         = isc_dpb_sys_user_name_enc;
isc_spb_password                  = isc_dpb_password;
isc_spb_password_enc              = isc_dpb_password_enc;
isc_spb_command_line              = 105;
isc_spb_dbname                    = 106;
isc_spb_verbose                   = 107;
isc_spb_options                   = 108;
isc_spb_address_path              = 109;
isc_spb_process_id                = 110;
isc_spb_trusted_auth			  = 111;
isc_spb_process_name              = 112;
isc_spb_trusted_role              = 113;
isc_spb_verbint                   = 114;
isc_spb_auth_block                = 115;
isc_spb_auth_plugin_name          = 116;
isc_spb_auth_plugin_list          = 117;
isc_spb_utf8_filename			  = 118;
isc_spb_client_version            = 119;
isc_spb_remote_protocol           = 120;
isc_spb_host_name                 = 121;
isc_spb_os_user                   = 122;
isc_spb_config					  = 123;
isc_spb_expected_db				  = 124;

isc_spb_connect_timeout           = isc_dpb_connect_timeout;
isc_spb_dummy_packet_interval     = isc_dpb_dummy_packet_interval;
isc_spb_sql_role_name             = isc_dpb_sql_role_name;

// This will not be used in protocol 13, therefore may be reused
isc_spb_specific_auth_data		  = isc_spb_trusted_auth;

{*****************************
 * Service action items      *
 *****************************}

isc_action_svc_backup         = 1;	{* Starts database backup process on the server *}
isc_action_svc_restore        = 2;	{* Starts database restore process on the server *}
isc_action_svc_repair         = 3;	{* Starts database repair process on the server *}
isc_action_svc_add_user       = 4;	{* Adds a new user to the security database *}
isc_action_svc_delete_user    = 5;	{* Deletes a user record from the security database *}
isc_action_svc_modify_user    = 6;	{* Modifies a user record in the security database *}
isc_action_svc_display_user   = 7;	{* Displays a user record from the security database *}
isc_action_svc_properties     = 8;	{* Sets database properties *}
isc_action_svc_add_license    = 9;	{* Adds a license to the license file *}
isc_action_svc_remove_license =10;	{* Removes a license from the license file *}
isc_action_svc_db_stats	      =11;	{* Retrieves database statistics *}
isc_action_svc_get_ib_log     =12;	{* Retrieves the InterBase log file from the server *}
isc_action_svc_get_fb_log     =12;	{* Retrieves the Firebird log file from the server *}
isc_action_svc_nbak           =20;	{* Incremental nbackup *}
isc_action_svc_nrest          =21;	{* Incremental database restore *}
isc_action_svc_trace_start    =22;	// Start trace session
isc_action_svc_trace_stop     =23;	// Stop trace session
isc_action_svc_trace_suspend  =24;	// Suspend trace session
isc_action_svc_trace_resume   =25;	// Resume trace session
isc_action_svc_trace_list     =26;	// List existing sessions
isc_action_svc_set_mapping    =27;	// Set auto admins mapping in security database
isc_action_svc_drop_mapping   =28;	// Drop auto admins mapping in security database
isc_action_svc_display_user_adm =29;	// Displays user(s) from security database with admin info
isc_action_svc_validate		  = 30;	// Starts database online validation
isc_action_svc_nfix         = 31;	// Fixup database after file system copy
isc_action_svc_last			  =32;	// keep it last !

{*****************************
 * Service information items *
 *****************************}

isc_info_svc_svr_db_info		= 50;	{* Retrieves the number of attachments and databases *}
isc_info_svc_get_license		= 51;	{* Retrieves all license keys and IDs from the license file *}
isc_info_svc_get_license_mask	=52;	{* Retrieves a bitmask representing licensed options on the server *}
isc_info_svc_get_config			=53;	{* Retrieves the parameters and values for IB_CONFIG *}
isc_info_svc_version			=54;	{* Retrieves the version of the services manager *}
isc_info_svc_server_version	=	55;	{* Retrieves the version of the Firebird server *}
isc_info_svc_implementation	=	56;	{* Retrieves the implementation of the Firebird server *}
isc_info_svc_capabilities		=57;	{* Retrieves a bitmask representing the server's capabilities *}
isc_info_svc_user_dbpath		=58;	{* Retrieves the path to the security database in use by the server *}
isc_info_svc_get_env			= 59;	{* Retrieves the setting of $FIREBIRD *}
isc_info_svc_get_env_lock		= 60;	{* Retrieves the setting of $FIREBIRD_LOCK *}
isc_info_svc_get_env_msg		= 61;	{* Retrieves the setting of $FIREBIRD_MSG *}
isc_info_svc_line				= 62;	{* Retrieves 1 line of service output per call *}
isc_info_svc_to_eof				= 63;	{* Retrieves as much of the server output as will fit in the supplied buffer *}
isc_info_svc_timeout			= 64;	{* Sets / signifies a timeout value for reading service information *}
isc_info_svc_get_licensed_users	= 65;	{* Retrieves the number of users licensed for accessing the server *}
isc_info_svc_limbo_trans		= 66;	{* Retrieve the limbo transactions *}
isc_info_svc_running			= 67;	{* Checks to see if a service is running on an attachment *}
isc_info_svc_get_users			= 68;	{* Returns the user information from isc_action_svc_display_users *}
isc_info_svc_auth_block			= 69;	{* Sets authentication block for service query() call *}
isc_info_svc_stdin				 = 78;	{* Returns maximum size of data, needed as stdin for service *}


(******************************************************
 * Parameters for isc_action_{add|del|mod|disp)_user  *
 ******************************************************)

isc_spb_sec_userid             = 5;
isc_spb_sec_groupid            = 6;
isc_spb_sec_username           = 7;
isc_spb_sec_password           = 8;
isc_spb_sec_groupname          = 9;
isc_spb_sec_firstname          = 10;
isc_spb_sec_middlename         = 11;
isc_spb_sec_lastname           = 12;
isc_spb_sec_admin              = 13;

{*******************************************************
 * Parameters for isc_action_svc_(add|remove)_license, *
 * isc_info_svc_get_license                            *
 *******************************************************}

isc_spb_lic_key                = 5;
isc_spb_lic_id                 = 6;
isc_spb_lic_desc               = 7;


{*****************************************
 * Parameters for isc_action_svc_backup  *
 *****************************************}

isc_spb_bkp_file                  = 5;
isc_spb_bkp_factor                = 6;
isc_spb_bkp_length                = 7;
isc_spb_bkp_skip_data             = 8;
isc_spb_bkp_stat                  = 15;
isc_spb_bkp_keyholder			  = 16;
isc_spb_bkp_keyname				  = 17;
isc_spb_bkp_crypt				  = 18;
isc_spb_bkp_include_data          = 19;
isc_spb_bkp_ignore_checksums      = $01;
isc_spb_bkp_ignore_limbo          = $02;
isc_spb_bkp_metadata_only         = $04;
isc_spb_bkp_no_garbage_collect    = $08;
isc_spb_bkp_old_descriptions      = $10;
isc_spb_bkp_non_transportable     = $20;
isc_spb_bkp_convert               = $40;
isc_spb_bkp_expand				  = $80;
isc_spb_bkp_no_triggers			  = $8000;
isc_spb_bkp_zip					  = $010000;

{********************************************
 * Parameters for isc_action_svc_properties *
 ********************************************}

isc_spb_prp_page_buffers		 = 5;
isc_spb_prp_sweep_interval		 = 6;
isc_spb_prp_shutdown_db			 = 7;
isc_spb_prp_deny_new_attachments	 = 9;
isc_spb_prp_deny_new_transactions	 = 10;
isc_spb_prp_reserve_space		 = 11;
isc_spb_prp_write_mode			 = 12;
isc_spb_prp_access_mode			 = 13;
isc_spb_prp_set_sql_dialect		 = 14;
isc_spb_prp_activate			 = $0100;
isc_spb_prp_db_online			 = $0200;
isc_spb_prp_nolinger			 = $0400;
isc_spb_prp_force_shutdown			 = 41;
isc_spb_prp_attachments_shutdown	 = 42;
isc_spb_prp_transactions_shutdown	 = 43;
isc_spb_prp_shutdown_mode		 = 44;
isc_spb_prp_online_mode			 = 45;
isc_spb_prp_replica_mode		 = 46;

{********************************************
 * Parameters for isc_spb_prp_shutdown_mode *
 *            and isc_spb_prp_online_mode   *
 ********************************************}
isc_spb_prp_sm_normal		 = 0;
isc_spb_prp_sm_multi		 = 1;
isc_spb_prp_sm_single		 = 2;
isc_spb_prp_sm_full			 = 3;

{********************************************
 * Parameters for isc_spb_prp_reserve_space *
 ********************************************}

isc_spb_prp_res_use_full	 = 35;
isc_spb_prp_res				 = 36;

{******************************************
 * Parameters for isc_spb_prp_write_mode  *
 ******************************************}

isc_spb_prp_wm_async		 = 37;
isc_spb_prp_wm_sync			 = 38;

{******************************************
 * Parameters for isc_spb_prp_access_mode *
 ******************************************}

isc_spb_prp_am_readonly		 = 39;
isc_spb_prp_am_readwrite	 = 40;

{*******************************************
 * Parameters for isc_spb_prp_replica_mode *
 *******************************************}

isc_spb_prp_rm_none			 = 0;
isc_spb_prp_rm_readonly		 = 1;
isc_spb_prp_rm_readwrite	 = 2;

{*****************************************
 * Parameters for isc_action_svc_repair  *
 *****************************************}

isc_spb_rpr_commit_trans		 = 15;
isc_spb_rpr_rollback_trans		 = 34;
isc_spb_rpr_recover_two_phase	 = 17;
isc_spb_tra_id					 = 18;
isc_spb_single_tra_id			 = 19;
isc_spb_multi_tra_id			 = 20;
isc_spb_tra_state				 = 21;
isc_spb_tra_state_limbo			 = 22;
isc_spb_tra_state_commit		 = 23;
isc_spb_tra_state_rollback		 = 24;
isc_spb_tra_state_unknown		 = 25;
isc_spb_tra_host_site			 = 26;
isc_spb_tra_remote_site			 = 27;
isc_spb_tra_db_path				 = 28;
isc_spb_tra_advise				 = 29;
isc_spb_tra_advise_commit		 = 30;
isc_spb_tra_advise_rollback		 = 31;
isc_spb_tra_advise_unknown		 = 33;
isc_spb_tra_id_64				 = 46;
isc_spb_single_tra_id_64		 = 47;
isc_spb_multi_tra_id_64			 = 48;
isc_spb_rpr_commit_trans_64		 = 49;
isc_spb_rpr_rollback_trans_64	 = 50;
isc_spb_rpr_recover_two_phase_64	 = 51;

isc_spb_rpr_validate_db			 = $01;
isc_spb_rpr_sweep_db			 = $02;
isc_spb_rpr_mend_db				 = $04;
isc_spb_rpr_list_limbo_trans	 = $08;
isc_spb_rpr_check_db			 = $10;
isc_spb_rpr_ignore_checksum		 = $20;
isc_spb_rpr_kill_shadows		 = $40;
isc_spb_rpr_full				 = $80;
isc_spb_rpr_icu				   = $0800;

{*****************************************
 * Parameters for isc_action_svc_restore *
 *****************************************}

isc_spb_res_skip_data			 = isc_spb_bkp_skip_data;
isc_spb_res_include_data		 = isc_spb_bkp_include_data;
isc_spb_res_buffers				 = 9;
isc_spb_res_page_size			 = 10;
isc_spb_res_length				 = 11;
isc_spb_res_access_mode			 = 12;
isc_spb_res_fix_fss_data		 = 13;
isc_spb_res_fix_fss_metadata	 = 14;
isc_spb_res_keyholder			 = isc_spb_bkp_keyholder;
isc_spb_res_keyname				 = isc_spb_bkp_keyname;
isc_spb_res_crypt				 = isc_spb_bkp_crypt;
isc_spb_res_stat				 = isc_spb_bkp_stat;
isc_spb_res_metadata_only		 = isc_spb_bkp_metadata_only;
isc_spb_res_deactivate_idx		 = $0100;
isc_spb_res_no_shadow			 = $0200;
isc_spb_res_no_validity			 = $0400;
isc_spb_res_one_at_a_time		 = $0800;
isc_spb_res_replace				 = $1000;
isc_spb_res_create				 = $2000;
isc_spb_res_use_all_space		 = $4000;
isc_spb_res_replica_mode		 = 20;

{*****************************************
 * Parameters for isc_action_svc_validate *
 *****************************************}

isc_spb_val_tab_incl	=	1;	// include filter based on regular expression
isc_spb_val_tab_excl	=	2;	// exclude filter based on regular expression
isc_spb_val_idx_incl	=	3;	// regexp of indices to validate
isc_spb_val_idx_excl	=	4;	// regexp of indices to NOT validate
isc_spb_val_lock_timeout =5;	// how long to wait for table lock

{******************************************
 * Parameters for isc_spb_res_access_mode  *
 ******************************************}

isc_spb_res_am_readonly			 = isc_spb_prp_am_readonly;
isc_spb_res_am_readwrite		 = isc_spb_prp_am_readwrite;

{*******************************************
 * Parameters for isc_spb_res_replica_mode *
 *******************************************}

isc_spb_res_rm_none				 = isc_spb_prp_rm_none;
isc_spb_res_rm_readonly			 = isc_spb_prp_rm_readonly;
isc_spb_res_rm_readwrite		 = isc_spb_prp_rm_readwrite;

{*******************************************
 * Parameters for isc_info_svc_svr_db_info *
 *******************************************}

isc_spb_num_att			 = 5;
isc_spb_num_db			 = 6;

{*****************************************
 * Parameters for isc_info_svc_db_stats  *
 *****************************************}

isc_spb_sts_table			 = 64;

isc_spb_sts_data_pages		 = $01;
isc_spb_sts_db_log			 = $02;
isc_spb_sts_hdr_pages		 = $04;
isc_spb_sts_idx_pages		 = $08;
isc_spb_sts_sys_relations	 = $10;
isc_spb_sts_record_versions	 = $20;
//isc_spb_sts_table			 = $40;
isc_spb_sts_nocreation		 = $80;
isc_spb_sts_encryption	    = $100;

{***********************************}
{* Server configuration key values *}
{***********************************}

{* Not available in Firebird 1.5 *}

{***************************************
 * Parameters for isc_action_svc_nbak  *
 ***************************************}

isc_spb_nbk_level			 = 5;
isc_spb_nbk_file			 = 6;
isc_spb_nbk_direct			 = 7;
isc_spb_nbk_guid			 = 8;
isc_spb_nbk_no_triggers		 = $01;
isc_spb_nbk_inplace			 = $02;
isc_spb_nbk_sequence		 = $04;

{***************************************
 * Parameters for isc_action_svc_trace *
 ***************************************}

isc_spb_trc_id				 = 1;
isc_spb_trc_name			 = 2;
isc_spb_trc_cfg				 = 3;

{******************************************}
{* Array slice description language (SDL) *}
{******************************************}

isc_sdl_version1                   = 1;
isc_sdl_eoc                        = 255;
isc_sdl_relation                   = 2;
isc_sdl_rid                        = 3;
isc_sdl_field                      = 4;
isc_sdl_fid                        = 5;
isc_sdl_struct                     = 6;
isc_sdl_variable                   = 7;
isc_sdl_scalar                     = 8;
isc_sdl_tiny_integer               = 9;
isc_sdl_short_integer              = 10;
isc_sdl_long_integer               = 11;
//isc_sdl_literal                    = 12;
isc_sdl_add                        = 13;
isc_sdl_subtract                   = 14;
isc_sdl_multiply                   = 15;
isc_sdl_divide                     = 16;
isc_sdl_negate                   = 17; // only used in pretty.cpp; nobody generates it
//isc_sdl_eql                     =  18;
//isc_sdl_neq                        = 19;
//isc_sdl_gtr                        = 20;
//isc_sdl_geq                        = 21;
//isc_sdl_lss                        = 22;
//isc_sdl_leq                        = 23;
//isc_sdl_and                        = 24;
//isc_sdl_or                         = 25;
//isc_sdl_not                        = 26;
//isc_sdl_while                      = 27;
//isc_sdl_assignment                 = 28;
//isc_sdl_label                      = 29;
//isc_sdl_leave                      = 30;
isc_sdl_begin                     = 31; // only used in pretty.cpp; nobody generates it
isc_sdl_end                        = 32;
isc_sdl_do3                        = 33;
isc_sdl_do2                        = 34;
isc_sdl_do1                        = 35;
isc_sdl_element                    = 36;

{********************************************}
{* International text interpretation values *}
{********************************************}

//isc_interp_eng_ascii               = 0;
//isc_interp_jpn_sjis                = 5;
//isc_interp_jpn_euc                 = 6;

{*****************}
{* Blob Subtypes *}
{*****************}

{* types less than zero are reserved for customer use *}

isc_blob_untyped                   = 0;

{* internal subtypes *}

isc_blob_text                      = 1;
isc_blob_blr                       = 2;
isc_blob_acl                       = 3;
isc_blob_ranges                    = 4;
isc_blob_summary                   = 5;
isc_blob_format                    = 6;
isc_blob_tra                       = 7;
isc_blob_extfile                   = 8;
isc_blob_debug_info                = 9;
isc_blob_max_predefined_subtype    = 10;

{* the range 20-30 is reserved for dBASE and Paradox types *}

//isc_blob_formatted_memo            = 20;
//isc_blob_paradox_ole               = 21;
//isc_blob_graphic                   = 22;
//isc_blob_dbase_ole                 = 23;
//isc_blob_typed_binary              = 24;

{*****************}
{* Text Subtypes *}
{*****************}

fb_text_subtype_text	 = 0;
fb_text_subtype_binary	 = 1;

{* Deprecated definitions maintained for compatibility only *}

//isc_info_db_SQL_dialect            = 62;
//isc_dpb_SQL_dialect                = 63;
//isc_dpb_set_db_SQL_dialect         = 65;

{***********************************}
{* Masks for fb_shutdown_callback  *}
{***********************************}

fb_shut_confirmation			   = 1;
fb_shut_preproviders			   = 2;
fb_shut_postproviders			   = 4;
fb_shut_finish					   = 8;
fb_shut_exit					  = 16;

{****************************************}
{* Shutdown reasons, used by engine     *}
{* Users should provide positive values *}
{****************************************}

fb_shutrsn_svc_stopped			=  -1;
fb_shutrsn_no_connection		=  -2;
fb_shutrsn_app_stopped			=  -3;
//fb_shutrsn_device_removed	=	  -4;
fb_shutrsn_signal				 = -5;
fb_shutrsn_services			=	  -6;
fb_shutrsn_exit_called		=	  -7;
fb_shutrsn_emergency			=  -8;

{****************************************}
{* Cancel types for fb_cancel_operation *}
{****************************************}

fb_cancel_disable				   = 1;
fb_cancel_enable				   = 2;
fb_cancel_raise					   = 3;
fb_cancel_abort					   = 4;

{********************************************}
{* Debug information items					*}
{********************************************}

fb_dbg_version				 = 1;
fb_dbg_end					 = 255;
fb_dbg_map_src2blr			 = 2;
fb_dbg_map_varname			 = 3;
fb_dbg_map_argument			 = 4;
fb_dbg_subproc				 = 5;
fb_dbg_subfunc				 = 6;
fb_dbg_map_curname			 = 7;

// sub code for fb_dbg_map_argument
fb_dbg_arg_input			 = 0;
fb_dbg_arg_output			 = 1;
