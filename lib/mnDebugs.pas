unit mnDebugs;
{-----------------------------------------------------------------------------
 Title:
 Author:    Zaher <zaherdirkey at yahoo.com>
 History:
-----------------------------------------------------------------------------}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}

interface

uses
  {$ifdef WINDOWS}
  Windows,
  {$else}
  {$endif}
  Classes, Types, SysUtils, Graphics, DateUtils, Math, mnClasses;

type
  TmnDebug = record
    procedure Write(S: string); overload;
    procedure Write(R: TRect); overload;
  end;

var
  Debug: TmnDebug;

implementation

uses
  mnUtils;

{ TmnDebug }

procedure TmnDebug.Write(R: TRect);
begin
  Write('Rect(Left:'+IntToStr(R.Left)+', Right:' + IntToStr(R.Right)+', Top:' + IntToStr(R.Top)+', Bottom:' + IntToStr(R.Bottom) + ')');
end;

procedure TmnDebug.Write(S: string);
begin
  {$ifdef WINDOWS}
  s := IntToStr(GetTickCount) + ': ' +s;
  {$ifdef FPC}
  OutputDebugString(PAnsiChar(S));
  {$else}
  OutputDebugString(PWideChar(S));
  {$endif}
  {$else}
  {$endif}
end;

initialization
finalization
end.

