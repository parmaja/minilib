{*
 *	PROGRAM:	JRD access method
 *	MODULE:		dsc.h
 *	DESCRIPTION:	Definitions associated with descriptors
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
 * 2002.04.16  Paul Beach - HP10 Define changed from -4 to (-4) to make it
 *             compatible with the HP Compiler
 *}

{*
 * The following flags are used in an internal structure dsc (dsc.h) or in the external one paramdsc (ibase.h)
 *}

{* values for dsc_flags
 * Note: DSC_null is only reliably set for local variables (blr_variable)
 *}

const
  DSC_null		   = 1;
  DSC_no_subtype = 2;	{* dsc has no sub type specified *}
  DSC_nullable   = 4;	{* not stored. instead, is derived
							   from metadata primarily to flag
							   SQLDA (in DSQL)               *}

  dtype_unknown	= 0;
  dtype_text		= 1;
  dtype_cstring	= 2;
  dtype_varying	= 3;

  dtype_packed   = 6;
  dtype_byte	 	 = 7;
  dtype_short	 	 = 8;
  dtype_long	 	 = 9;
  dtype_quad	 	 = 10;
  dtype_real	 	 = 11;
  dtype_double 	 = 12;
  dtype_d_float  = 13;
  dtype_sql_date = 14;
  dtype_sql_time = 15;
  dtype_timestamp =	16;
  dtype_blob    	=	17;
  dtype_array	    =	18;
  dtype_int64	    =	19;
  dtype_dbkey   	=	20;
  dtype_boolean	  = 21;
  dtype_dec64		  = 22;
  dtype_dec128	  = 23;
  dtype_int128	  = 24;
  dtype_sql_time_tz	  = 25;
  dtype_timestamp_tz	= 26;
  dtype_ex_time_tz    =	27;
  dtype_ex_timestamp_tz	= 28;
  DTYPE_TYPE_MAX	      = 29;

  ISC_TIME_SECONDS_PRECISION	=	10000;
  ISC_TIME_SECONDS_PRECISION_SCALE =(-4);

