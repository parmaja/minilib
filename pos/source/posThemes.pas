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
{$ELSE}
{$DEFINE WINDOWS}
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

  { TposThemes }

  TposThemes = class(TObject)
  private
    FSoundDirectory: string;
    FPlaySounds: Boolean;
    FSoundExt: string;
  published
  public
    constructor Create;
    procedure PlaySound(Sound: string; StopPrevious: Boolean = False; Wait: Boolean = False);
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

procedure TposThemes.PlaySound(Sound: string; StopPrevious, Wait: Boolean);
var
  f: string;
  c: Cardinal;
begin
  if PlaySounds then
  begin
    f := ExpandFileName(IncludeTrailingPathDelimiter(SoundDirectory) + Sound + SoundExt);
    if FileExists(f) then
    begin
     {$ifdef Windows}
     c := SND_FILENAME or SND_NODEFAULT;
     if not StopPrevious then
       c := c or SND_NOSTOP;
     if Wait then
       c := c or SND_SYNC
     else
       c := c or SND_ASYNC;
     {$ifdef WINCE}
      PlaySoundW(PWideChar(UTF8Decode(f)), 0, c);
    {$else}
      MMSystem.PlaySound(PChar(f), 0, c);
    {$endif}
    {$else}
    {$endif}
    end;
  end;
end;

end.

