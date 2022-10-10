{* sqlda_pub.h *}
{*
 *	PROGRAM:	C preprocessor
 *	MODULE:		sqlda_pub.h
 *	DESCRIPTION:	Public DSQL definitions (included in ibase.h)
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
 *}

{* Definitions for DSQL free_statement routine *}

const
  DSQL_close	=	1;
  DSQL_drop	=	2;
  DSQL_unprepare = 4;

{* Declare the extended SQLDA *}

type
  TXSQLVAR = record
    sqltype: ISC_SHORT;			{* datatype of field *}
    sqlscale: ISC_SHORT;			{* scale factor *}
    sqlsubtype: ISC_SHORT;			{* datatype subtype - currently BLOBs only *}
    sqllen: ISC_SHORT;				{* length of data area *}
    sqldata: Pointer;			{* address of data *}
    sqlind: PISC_SHORT;				{* address of indicator variable *}
    sqlname_length: ISC_SHORT;		{* length of sqlname field *}
    sqlname: array[0..31] of ISC_SCHAR;		{* name of field, name length + space for NULL *}
    relname_length: ISC_SHORT;		{* length of relation name *}
    relname: array[0..31] of ISC_SCHAR;		{* field's relation name + space for NULL *}
    ownname_length: ISC_SHORT;		{* length of owner name *}
    ownname: array[0..31] of ISC_SCHAR;		{* relation's owner name + space for NULL *}
    aliasname_length: ISC_SHORT;	{* length of alias name *}
    aliasname: array[0..31] of ISC_SCHAR;		{* relation's alias name + space for NULL *}
  end;
  PXSQLVAR = ^TXSQLVAR;

const
  SQLDA_VERSION1	=	1;

type
  TXSQLDA = record
    version: ISC_SHORT;			{* version of this XSQLDA *}
    sqldaid: array[0..7] of ISC_SCHAR;		{* XSQLDA name field *}
    sqldabc: ISC_LONG;		 	{* length in bytes of SQLDA *}
    sqln: ISC_SHORT;				  {* number of fields allocated *}
    sqld: ISC_SHORT;				  {* actual number of fields *}
    sqlvar: array[0..0] of TXSQLVAR; {* first field address *}
  end;
  PXSQLDA = ^TXSQLDA;

const
  (*
  function XSQLDA_LENGTH(n: Long): Long;

  function XSQLDA_LENGTH(n: Long): Long;
  begin
    Result := SizeOf(TXSQLDA) + ((n - 1) * SizeOf(TXSQLVAR));
  end;
  *)

  SQL_TEXT                           = 452;
  SQL_VARYING                        = 448;
  SQL_SHORT                          = 500;
  SQL_LONG                           = 496;
  SQL_FLOAT                          = 482;
  SQL_DOUBLE                         = 480;
  SQL_D_FLOAT                        = 530;
  SQL_TIMESTAMP                      = 510;
  SQL_BLOB                           = 520;
  SQL_ARRAY                          = 540;
  SQL_QUAD                           = 550;
  SQL_TYPE_TIME                      = 560;
  SQL_TYPE_DATE                      = 570;
  SQL_INT64                          = 580;
  SQL_TIMESTAMP_TZ_EX               = 32748;
  SQL_TIME_TZ_EX                    = 32750;
  SQL_INT128                        = 32752;
  SQL_TIMESTAMP_TZ                  = 32754;
  SQL_TIME_TZ                       = 32756;
  SQL_DEC16                         = 32760;
  SQL_DEC34                         = 32762;
  SQL_BOOLEAN                       = 32764;
  SQL_NULL                          = 32766;

{* Historical alias for pre v6 code *}
  SQL_DATE                           = SQL_TIMESTAMP;

{***************************}
{* SQL Dialects            *}
{***************************}

  SQL_DIALECT_V5				= 1;	{* meaning is same as DIALECT_xsqlda *}
  SQL_DIALECT_V6_TRANSITION	= 2;	{* flagging anything that is delimited
                                           by double quotes as an error and
                                           flagging keyword DATE as an error *}
  SQL_DIALECT_V6				= 3;	{* supports SQL delimited identifier,
                                           SQLDATE/DATE, TIME, TIMESTAMP,
                                           CURRENT_DATE, CURRENT_TIME,
                                           CURRENT_TIMESTAMP, and 64-bit exact
                                           numeric type *}
  SQL_DIALECT_CURRENT		= SQL_DIALECT_V6;	{* latest IB DIALECT *}
