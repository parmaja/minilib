unit posThemes;
{**
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
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
  SysUtils, Classes, Graphics, Controls, StdCtrls, Forms,
  Contnrs, Types,
{$IFDEF FPC}
  LCLIntf,
  LCLType,
{$ELSE}
  Windows,
{$ENDIF}
  posControls, posUtils, posTypes;

type
  TposThemes = class(TObject)
  private
    FSoundDirectory: string;
    FPlaySounds: Boolean;
  published
  public
    constructor Create;
    procedure PlaySound(Sound: string; NoStop: Boolean = False; Async: Boolean = False);
    property PlaySounds: Boolean read FPlaySounds write FPlaySounds default False;
    property SoundDirectory: string read FSoundDirectory write FSoundDirectory;
  end;

function Themes: TposThemes;

implementation

{$IFDEF FPC}
{$ELSE}
uses
  MMSystem;
{$ENDIF}

var
  FThemes: TposThemes = nil;

function Themes: TposThemes;
begin
  if FThemes = nil then
    FThemes := TposThemes.Create;
  Result := FThemes;
end;

{ TposThemes }

constructor TposThemes.Create;
begin
  inherited;
  FPlaySounds := True;
end;

procedure TposThemes.PlaySound(Sound: string; NoStop, Async: Boolean);
{$IFDEF FPC}
begin
  //not yet
end;
{$ELSE}
var
  c: Cardinal;
begin
  if PlaySounds then
  begin
    c := SND_FILENAME or SND_NODEFAULT;
    if NoStop then
      c := c or SND_NOSTOP;
    if Async then
      c := c or SND_ASYNC;
    MMSystem.PlaySound(PChar(IncludeTrailingPathDelimiter(SoundDirectory) + Sound + '.WAV'), 0, c);
  end;
end;
{$ENDIF}

end.

