unit mncSQLiteHeader;
{}
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
 {
   This file sqlite3.inc ported from Lazarus just to be compatiple in both Delphi and FPC
 }

{$IFDEF FPC}
Error do not compile it in FPC
{$else}
{$i 'sqlite3.inc'}
{$endif}
