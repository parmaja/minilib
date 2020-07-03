unit mnCommStreams;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes,
  {$ifdef Linux}
  mnLinuxCommStreams,
  {$else}
  {$ifdef WINCE}
  mnWinCECommStreams,
  {$else}
  mnWinCommStreams,
  {$endif}
  {$endif}
  SysUtils;


type
  TmnCommStream = class(TmnOSCommStream)
  end;
  
implementation

end.

