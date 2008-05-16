unit mnCommStreams;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$ifdef Linux}
  mnLinuxCommStreams,
  {$else}
  mnWinCommStreams,
  {$endif}
  mnCommClasses;


type
  TmnCommStream = class(TmnOSCommStream)
  end;
  
implementation

end.

