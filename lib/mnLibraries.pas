unit mnLibraries;
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{$M+}{$H+}

{**
 *  This file is part of the "MiniLib"
 *
 * @license   Mit
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey zaherdirkey
 *}
interface

uses
  Classes,
  {$ifdef FPC}
    dynlibs,
  {$else}
    {$IFDEF MSWINDOWS}
    Windows,
    {$endif}
  {$endif}
    SysUtils;

type
  //* ported from freepascl ctypes, to be compatible with Delphi
  cchar = shortint;
  pcchar = ^cchar;
  cuchar = byte;
  pcuchar = ^cuchar;

  cuint16 = word;
  cuint64 = Int64;
  cdouble = double;

{$if defined(cpu64) and not(defined(win64) and defined(cpux86_64))}
  cint = longint;
  cuint = longword;
  culong = qword;
  size_t = QWord;
{$else}
  cint = longint;
  cuint = longword;
  culong = cardinal;
  size_t = Cardinal;
{$endif}
  pcint = ^cint;
  pcuint = ^cuint;
  pculong = ^culong;

  my_bool = cchar;
  Pmy_bool  = ^my_bool;
  ppcchar = ^pcchar;
  psize_t = pointer;

{$ifdef FPC}
{$PACKRECORDS C}
{$else DELPHI}
type
  TLibHandle = System.THandle;
{$endif}

type

  { TmnLibrary }

  TmnLibrary = class(TObject)
  private
  protected
    RefCount: Integer;
    FHandle: TLibHandle;
    FLibraryName: string;
    LoadedLibrary: string;
    RaiseError: Boolean;
    procedure Link; virtual; abstract;
    procedure Init; virtual;
  public
    constructor Create(ALibraryName: string); virtual;
    function Load: Boolean;
    function IsLoaded: Boolean;
    procedure Release;
    function GetAddress(const ProcedureName: string; ARaiseError: Boolean = False): Pointer;
    property Handle: TLibHandle read FHandle;
    property LibraryName: string read FLibraryName;
  end;

implementation

resourcestring
  SErrLoadFailed = 'Can not load library "%s". Check your installation.';
  SErrAlreadyLoaded = 'Already initialized from library %s.';

function InternalLoadLibrary(LibraryName: string):TLibHandle;
begin
  {$ifdef FPC}
    Result := LoadLibrary(LibraryName);
  {$else}
    Result := LoadLibrary(PChar(LibraryName));
  {$endif}
end;

{ TmnLibrary }

constructor TmnLibrary.Create(ALibraryName: string);
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
  if ExtractFileExt(ALibraryName) = '' then
    ALibraryName := ALibraryName + '.dll';
  FLibraryName := ALibraryName;
  {$ELSE}
  {$IFDEF MACOS}
  FLibraryName := ALibraryName + '.dylib';
  {$ELSE}
{  if ExtractFileExt(ALibraryName) = '' then
    ALibraryName := ALibraryName + '.so';}
  FLibraryName := ALibraryName;
  {$ENDIF}
  {$ENDIF}
end;

function TmnLibrary.Load: Boolean;
begin
  Result := not IsLoaded;
  if Result then
  begin

    RefCount := RefCount + 1;
    if RefCount = 1 then
    begin
      FHandle := InternalLoadLibrary(LibraryName);
      if (FHandle = 0) then
      begin
        RefCount := 0;
        raise EInOutError.CreateFmt(SErrLoadFailed,[LibraryName]);
      end
      else
      begin
        LoadedLibrary := LibraryName;
        Link;
        Init;
      end;
    end;
  end;
end;

procedure TmnLibrary.Init;
begin
end;

function TmnLibrary.IsLoaded: Boolean;
begin
  Result := Handle <> 0;
end;

procedure TmnLibrary.Release;
begin
  RefCount := RefCount - 1;
  if RefCount <= 0 then
  begin
    if Handle <> 0 then
      FreeLibrary(Handle);
    FHandle := 0;
    LoadedLibrary := '';
    RefCount := 0;
  end;
end;

function TmnLibrary.GetAddress(const ProcedureName: string; ARaiseError: Boolean): Pointer;
begin
  if FHandle <> 0 then
    Result := GetProcAddress(Handle, PChar(ProcedureName)) //Use PChar not PAnsiChar
  else
    Result := nil;
  if (Result = nil) and (RaiseError or ARaiseError) then
    raise Exception.Create(ProcedureName + ' not found in ' + LoadedLibrary);
end;

end.

