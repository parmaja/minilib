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
{$ifdef WINCE}
  WinCEmf,
{$endif}
  LCLIntf,
  LCLType,
{$ELSE}
  Windows,
{$ENDIF}
  posControls, posUtils, posTypes;

type

  { TposThemes }

  TposThemes = class(TObject)
  private
    FSoundDirectory: string;
    FPlaySounds: Boolean;
    FSoundExt: string;
  published
  public
    constructor Create;
    procedure PlaySound(Sound: string; NoStop: Boolean = False; Async: Boolean = False);
    property PlaySounds: Boolean read FPlaySounds write FPlaySounds default False;
    property SoundExt: string read FSoundExt write FSoundExt;
    property SoundDirectory: string read FSoundDirectory write FSoundDirectory;
  end;

function Themes: TposThemes;

implementation

{$ifdef WINDOWS}
uses
  MMSystem;
{$endif}

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
  FSoundExt := '.wav';
end;

procedure TposThemes.PlaySound(Sound: string; NoStop, Async: Boolean);
var
  f: string;
  c: Cardinal;
begin
  if PlaySounds then
  begin
    f := ExpandFileName(IncludeTrailingPathDelimiter(SoundDirectory) + Sound + SoundExt);
    if FileExists(f) then
    begin
     {$ifdef WINCE}
      c := SND_FILENAME or SND_NODEFAULT;
      if NoStop then
        c := c or SND_NOSTOP;
      if Async then
        c := c or SND_ASYNC;
      PlaySoundW(PWideChar(UTF8Decode(f)), 0, c);
    {$else}
    {$ifdef Windows}
      c := SND_FILENAME or SND_NODEFAULT;
      if NoStop then
        c := c or SND_NOSTOP;
      if Async then
        c := c or SND_ASYNC;
      MMSystem.PlaySound(PChar(f), 0, c);
    {$endif}
    {$endif}
    end;
  end;
end;

end.

