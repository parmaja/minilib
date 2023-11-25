unit ucputils;

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$H+}
{$M+}

interface

uses
  SysUtils, Variants, Classes;

type
  TMBToWC_Proc = procedure(S: AnsiChar; var R: WideChar);
  TWCToMB_Proc = procedure(S: WideChar; var R: AnsiChar);

function ucpAnsiToUnicode(const S: AnsiString): WideString; overload;
function ucpAnsiToUnicode(const S: AnsiString; Proc: Tmbtowc_proc): WideString; overload;

function ucpUnicodeToAnsi(const S: WideString): AnsiString; overload;
function ucpUnicodeToAnsi(const S: WideString; Proc: Twctomb_proc): AnsiString; overload;

procedure ucpInstall(MBToWCProc: Tmbtowc_proc; WCtoMBProc: Twctomb_proc{$IFDEF FPC}; Hook: Boolean{$ENDIF});

implementation

uses
  ucp1250; //the default code page

type
  TucpConverter = record
    MBToWCProc: procedure(S: AnsiChar; var R: WideChar);
    WCToMBProc: procedure(S: WideChar; var R: AnsiChar);
  end;

var
  FConverter: TucpConverter;

{$IFDEF FPC}
procedure Ansi2WideMove(source: PAnsiChar; cp : TSystemCodePage; var dest: WideString; len:SizeInt);
begin
  dest := ucpAnsiToUnicode(source);
end;

procedure Wide2AnsiMove(source: PWideChar; var dest: RawByteString; cp : TSystemCodePage; len:SizeInt);
begin
  dest := ucpUnicodeToAnsi(source);
end;

procedure Ansi2UnicodeMove(source: PAnsiChar; cp : TSystemCodePage; var dest: UnicodeString; len: SizeInt);
begin
  dest := ucpAnsiToUnicode(source);
end;

procedure Unicode2AnsiMove(source: punicodechar; var dest:RawByteString; cp : TSystemCodePage; len:SizeInt);
begin
  dest := ucpUnicodeToAnsi(source);
end;
{$ENDIF}

procedure ucpInstall(MBToWCProc: Tmbtowc_proc; WCtoMBProc: Twctomb_proc{$IFDEF FPC}; Hook: Boolean{$ENDIF});
{$IFDEF FPC}
var
  Manager: TUnicodeStringManager;
{$ENDIF}
begin
  FConverter.MBToWCProc := MBToWCProc;
  FConverter.WCToMBProc := WCtoMBProc;
{$IFDEF FPC}
  if Hook then
  begin
    GetWideStringManager(Manager);
    Manager.Ansi2WideMoveProc := Ansi2WideMove;
    Manager.Wide2AnsiMoveProc := Wide2AnsiMove;
    Manager.Ansi2UnicodeMoveProc := Ansi2UnicodeMove;
    Manager.Unicode2AnsiMoveProc := Unicode2AnsiMove;
    SetWideStringManager(Manager);
  end;
{$ENDIF}
end;

function ucpAnsiToUnicode(const s: AnsiString; Proc: Tmbtowc_proc): WideString; overload;
var
  i: Integer;
  r: WideChar;
begin
  if not Assigned(Proc) then
    raise Exception.Create('AnsiToUnicode: Proc params = nil!');
  SetLength(Result, length(s));
  for i := 1 to Length(s) do
  begin
    Proc(s[i], r);
    Result[i] := r;
  end;
end;

function ucpAnsiToUnicode(const S: AnsiString): WideString; overload;
begin
  Result := ucpAnsiToUnicode(S, FConverter.MBToWCProc);
end;

function ucpUnicodeToAnsi(const S: WideString; Proc: Twctomb_proc): AnsiString; overload;
var
  i: Integer;
  r: AnsiChar;
begin
  if not Assigned(Proc) then
    raise Exception.Create('UnicodeToAnsi: Proc params = nil!');
  SetLength(Result, length(s));
  for i := 1 to Length(s) do
  begin
    Proc(s[i], r);
    Result[i] := r;
  end;
end;

function ucpUnicodeToAnsi(const S: WideString): AnsiString; overload;
begin
  Result := ucpUnicodeToAnsi(S, FConverter.WCToMBProc);
end;

initialization
  FConverter.MBToWCProc := cp1250_mbtowc;
  FConverter.WCToMBProc := cp1250_wctomb;
end.

