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
  Classes, Types, SysUtils, DateUtils;

type

  { TmnDebug }

  TmnDebug = record
    procedure Write(S: string); overload;
    procedure Write(R: TRect); overload;
    procedure Write(I: Integer); overload;
    procedure Write(X, Y: Integer); overload;
    procedure Write(S: string; I: Integer); overload;
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

procedure TmnDebug.Write(I: Integer);
begin
  Write(IntToStr(I));
end;

procedure TmnDebug.Write(X, Y: Integer);
begin
  Write(IntToStr(X) + ', ' + IntToStr(Y));
end;

procedure TmnDebug.Write(S: string; I: Integer);
begin
  Write(S + ': ' + IntToStr(I));
end;

procedure TmnDebug.Write(S: string);
begin
  {$ifdef WINDOWS}
  s := IntToStr(GetTickCount64) + ': ' +s;
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

