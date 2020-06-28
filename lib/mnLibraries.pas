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
    Windows,
  {$endif}
    SysUtils;

type
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
{$else}
  cint = longint;
  cuint = longword;
  culong = cardinal;
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
    procedure Loaded; virtual; abstract;
  public
    constructor Create(ALibraryName: string); virtual;
    procedure Load(ALibraryName: string = '');
    procedure Init; virtual;
    function IsLoaded: Boolean;
    procedure Release;
    function GetAddress(const ProcedureName: string; ARaiseError: Boolean = False): Pointer;
    property Handle: TLibHandle read FHandle;
    property LibraryName: string read FLibraryName;
  end;

implementation

resourcestring
  SErrLoadFailed = 'Can not load library "%s". Check your installation.';
  SErrAlreadyLoaded = 'interface already initialized from library %s.';

{$ifdef FPC}
{$else}
function LoadLibrary(LibraryName: string):TLibHandle;
begin
  Result := Windows.LoadLibrary(PChar(LibraryName));
end;
{$endif}

{ TmnLibrary }

constructor TmnLibrary.Create(ALibraryName: string);
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
  if ExtractFileExt(ALibraryName) = '' then
    ALibraryName := ALibraryName + '.dll';
  FLibraryName := ALibraryName;
  {$ELSE}
  if ExtractFileExt(ALibraryName) = '' then
    ALibraryName := ALibraryName + '.so';
  FLibraryName := ALibraryName;
  {$ENDIF}
end;

procedure TmnLibrary.Load(ALibraryName: string);
var
  Usage: Integer;
begin
  if ALibraryName = '' then
    ALibraryName := LibraryName;

  if (LoadedLibrary <> '') and (LoadedLibrary <> ALibraryName) then
    raise EInoutError.CreateFmt(SErrAlreadyLoaded,[LoadedLibrary]);

  if not IsLoaded then
  begin
    Usage := InterlockedIncrement(RefCount);
    if Usage  = 1 then
    begin
      FHandle := LoadLibrary(ALibraryName);
      if (FHandle = 0) then
      begin
        RefCount := 0;
        raise EInOutError.CreateFmt(SErrLoadFailed,[ALibraryName]);
      end
      else
      begin
        LoadedLibrary := LibraryName;
        Loaded;
      end;
    end;
  end;
end;

procedure TmnLibrary.Init;
begin
  Load;
end;

function TmnLibrary.IsLoaded: Boolean;
begin
  Result := Handle <> 0;
end;

procedure TmnLibrary.Release;
begin
  if InterlockedDecrement(RefCount) <= 0 then
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

