{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

unit mncFBUtils;

interface

uses
  SysUtils, Classes,
  mnUtils,
  mncFBClient, mncFBTypes, mncFBHeader, mncFBErrors, mncFBStrings;

const
  FBLocalBufferLength = 512;
  FBBigLocalBufferLength = FBLocalBufferLength * 2;
  FBHugeLocalBufferLength = FBBigLocalBufferLength * 20;

procedure FBRaiseError(Error: TFBError; const Args: array of const); overload;
procedure FBRaiseError(const StatusVector: TStatusVector); overload;
function FBCall(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;

function CheckStatusVector(const StatusVector: TStatusVector; ErrorCodes: array of ISC_STATUS): Boolean;
function GetStatusErrorNumber(const StatusVector: TStatusVector; Index: Integer; var Value: Integer): Boolean;
function GetStatusErrorMsg(const StatusVector: TStatusVector; Index: Integer; var Value: string): Boolean;
function StatusVectorAsText(const StatusVector: TStatusVector): string;

procedure FBAlloc(var P; OldSize, NewSize: Integer; ZeroInit: Boolean = True);

function FBMax(n1, n2: Integer): Integer;
function FBMin(n1, n2: Integer): Integer;

function FBScaleCurrency(Value: Int64; Scale: Integer): Currency;
function FBScaleDouble(Value: Int64; Scale: Integer): Double;
function FBScaleInt64(Value: Int64; Scale: Integer): Int64;
function FBStripString(st: string; CharsToStrip: string): string;

function FBQuoteName(const Name: string): string;
function FBDequoteName(const Name: string): string;

function FormatIdentifier(Dialect: Integer; Value: string): string;
function FormatIdentifierValue(Dialect: Integer; Value: string): string;
function ExtractIdentifier(Dialect: Integer; Value: string): string;

function FindTokenReq(Buffer: PChar; Flag: Byte): PChar;
function GetInfoReqRecord(Buffer: PChar; Flag: Byte): Integer;
function GetInfoReqString(Buffer: PChar; Flag: Byte; var Value: string): Boolean;
function GetInfoReqInteger(Buffer: PChar; Flag: Byte; var Value: Integer): Boolean;

procedure SetFBDataBaseErrorMessages(Value: TFBDataBaseErrorMessages);
function GetFBDataBaseErrorMessages: TFBDataBaseErrorMessages;

function IsAliasName(FileName: string): Boolean;
function GetAliasName(FileName: string): string;
function GetAliasFile(AliasName: string): string;
procedure SetAliasFile(AliasName, FileName: string);

procedure FBHostInfo(const Host, UserName, Password, Role, CharacterSet: string; vParams: TStrings; CachedPasswords: Boolean);
procedure GenerateDPB(sl: TStrings; var DPB: string; var DPBLength: Short);
procedure GenerateTPB(sl: TStrings; var TPB: string; var TPBLength: Short);

function FBComposeConnectionString(DatabaseName, Host, Port: string; IsEmbed: Boolean; Protocol: TFBProtocol = dpTCP): string;
procedure FBDecomposeConnectionString(DatabaseName: string; var Host, FileName: string; var Protocol: TFBProtocol);

implementation

const
  CRLF = #0;

function FBMax(n1, n2: Integer): Integer;
begin
  if (n1 > n2) then
    result := n1
  else
    result := n2;
end;

function FBMin(n1, n2: Integer): Integer;
begin
  if (n1 < n2) then
    result := n1
  else
    result := n2;
end;

function FBScaleCurrency(Value: Int64; Scale: Integer): Currency;
var
  i: Integer;
begin
//Here we know 10000 in Int64 = 1 in Currency so
//if we have scale -4 that mean not need to * or / just tupe casting
//or make bias of -4 to correct the problem, this code must fast than FBScaleDouble
  Scale := Scale + 4;
  if Scale > 0 then
  begin
    for i := 1 to Scale do
      Value := Value * 10;
  end
  else if Scale < 0 then
  begin
    for i := 1 to -Scale do
      Value := Value div 10;
  end;
  Result := PCurrency(@Value)^;
end;

function FBScaleDouble(Value: Int64; Scale: Integer): Double;
var
  Scaling: Int64;
  i: Integer;
  Val: Double;
begin
  Scaling := 1;
  Val := Value;
  if Scale > 0 then
  begin
    for i := 1 to Scale do
      Scaling := Scaling * 10;
    Result := Val * Scaling;
  end
  else if Scale < 0 then
  begin
    for i := -1 downto Scale do
      Scaling := Scaling * 10;
    Result := Val / Scaling;
  end
  else
    Result := Val;
end;

function FBScaleInt64(Value: Int64; Scale: Integer): Int64;
var
  Scaling: Int64;
  i: Integer;
  Val: Int64;
begin
  Scaling := 1;
  Val := Value;
  if Scale > 0 then
  begin
    for i := 1 to Scale do
      Scaling := Scaling * 10;
    Result := Val * Scaling;
  end
  else if Scale < 0 then
  begin
    for i := -1 downto Scale do
      Scaling := Scaling * 10;
    Result := Val div Scaling;
  end
  else
    Result := Val;
end;

function FBStripString(st: string; CharsToStrip: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(st) do
  begin
    if AnsiPos(st[i], CharsToStrip) = 0 then
      Result := Result + st[i];
  end;
end;

function FormatIdentifier(Dialect: Integer; Value: string): string;
begin
  Value := Trim(Value);
  if Dialect = 1 then
    Value := AnsiUpperCase(Value)
  else if (Value <> '') and (Value[1] = '"') then
    Value := '"' + StringReplace(TrimRight(Value), '"', '""', [rfReplaceAll]) + '"'
  else
    Value := AnsiUpperCase(Value);
  Result := Value;
end;

function FormatIdentifierValue(Dialect: Integer; Value: string): string;
begin
  Value := Trim(Value);
  if Dialect = 1 then
    Value := AnsiUpperCase(Value)
  else
  begin
    if (Value <> '') and (Value[1] = '"') then
    begin
      Delete(Value, 1, 1);
      Delete(Value, Length(Value), 1);
      Value := StringReplace(Value, '""', '"', [rfReplaceAll]);
    end
    else
      Value := AnsiUpperCase(Value);
  end;
  Result := Value;
end;

function ExtractIdentifier(Dialect: Integer; Value: string): string;
begin
  Value := Trim(Value);
  if Dialect = 1 then
    Value := AnsiUpperCase(Value)
  else
  begin
    if (Value <> '') and (Value[1] = '"') then
    begin
      Delete(Value, 1, 1);
      Delete(Value, Length(Value), 1);
      Value := StringReplace(Value, '""', '"', [rfReplaceAll]);
    end
    else
      Value := AnsiUpperCase(Value);
  end;
  Result := Value;
end;

function FBQuoteName(const Name: string): string;
var
  i: Integer;
  b: Boolean;
begin
  b := False;
  for i := 1 to Length(Name) do
  begin
    if not (Name[i] in ['A'..'Z', '0'..'9', '_']) then
    begin
      b := true;
      break;
    end
  end;
  if b then
    Result := '"' + Name + '"'
  else
    Result := Name;
end;

function FBDequoteName(const Name: string): string;
begin
  if (Name <> '') and (Name[1] = '"') then
  begin
    if Name[Length(Name)] = '"' then
      Result := Copy(Name, 2, Length(Name) - 2)
    else
      Result := Copy(Name, 2, Length(Name) - 1);
  end
  else
    Result := UpperCase(Name);
end;

procedure FBRaiseError(Error: TFBError; const Args: array of const);
begin
  raise EFBClientError.Create(Ord(Error), Format(FBErrorMessages[Error], Args));
end;

procedure FBRaiseError(const StatusVector: TStatusVector);
var
  Msg: string;
  SqlCode: Long;
  ErrorCode: Long;

  StatusVectorWalk: PISC_STATUS;
  local_buffer: array[0..FBHugeLocalBufferLength - 1] of char;
  FBDataBaseErrorMessages: TFBDataBaseErrorMessages;
  procedure AddMsg(const vMsg: string);
  begin
    if Msg <> '' then
      Msg := Msg + CRLF;
    Msg := Msg + vMsg;
  end;
var
  ExceptionName, ExceptionMsg: string;
  ExceptionID: Long;
begin
  Msg := '';
  { Get a local reference to the status vector.
    Get a local copy of the FBDataBaseErrorMessages options.
    Get the SQL error code }
  FBDataBaseErrorMessages := GetFBDataBaseErrorMessages;
  SqlCode := FBClient.isc_sqlcode(@StatusVector);
  if StatusVector[0] = 1 then
    ErrorCode := StatusVector[1]
  else
    ErrorCode := 0;
  if (ShowSQLMessage in FBDataBaseErrorMessages) and (SqlCode <> -999) then
  begin
    FBClient.isc_sql_interprete(SqlCode, local_buffer, FBLocalBufferLength);
    AddMsg(string(local_buffer));
  end;

  if (ShowFBMessage in FBDataBaseErrorMessages) then
  begin
    StatusVectorWalk := @StatusVector;
//    while (FBClient.fb_interpret(local_buffer, FBLocalBufferLength, StatusVectorWalk,) > 0) do
    while (FBClient.isc_interprete(local_buffer, StatusVectorWalk) > 0) do
    begin
      AddMsg(string(local_buffer));
    end;
  end;

  if (ShowSqlCode in FBDataBaseErrorMessages) then
  begin
    AddMsg('sqlcode: ' + IntToStr(Sqlcode) + ', fberrorcode: ' + IntToStr(ErrorCode));
  end;

  case SqlCode of
    -836:
    begin
      GetStatusErrorNumber(StatusVector, 0, ExceptionID); //this is a stupid way
      GetStatusErrorMsg(StatusVector, 0, ExceptionName);
      GetStatusErrorMsg(StatusVector, 1, ExceptionMsg);
      raise EFBExceptionError.Create(SqlCode, ErrorCode, ExceptionID, ExceptionName, ExceptionMsg, Msg);
    end;
    -551: raise EFBRoleError.Create(SqlCode, ErrorCode, Msg);
  else
    raise EFBError.Create(SqlCode, ErrorCode, Msg) {$ifdef FPC} {$endif};
  end;
end;

function FBCall(ErrCode: ISC_STATUS; const StatusVector: TStatusVector; RaiseError: Boolean): ISC_STATUS;
begin
  Result := ErrCode;
  if RaiseError and (Result > 0) then
    FBRaiseError(StatusVector);
end;

{ Return the status vector for the current thread }

function CheckStatusVector(const StatusVector: TStatusVector; ErrorCodes: array of ISC_STATUS): Boolean;
var
  p: PISC_STATUS;
  i: Integer;
  procedure NextP(i: Integer);
  begin
    p := PISC_STATUS(PChar(p) + (i * SizeOf(ISC_STATUS)));
  end;
begin
  p := @StatusVector;
  Result := False;
  while (p^ <> 0) and (not Result) do
    case p^ of
      3: NextP(3);
      1, 4:
        begin
          NextP(1);
          i := 0;
          while (i < Length(ErrorCodes)) and (not Result) do
          begin
            Result := p^ = ErrorCodes[i];
            Inc(i);
          end;
          NextP(1);
        end;
    else
      NextP(2);
    end;
end;

function GetStatusErrorNumber(const StatusVector: TStatusVector; Index: Integer; var Value: Integer): Boolean;
var
  p: PISC_STATUS;
  procedure NextP(i: Integer);
  begin
    p := PISC_STATUS(PChar(p) + (i * SizeOf(ISC_STATUS)));
  end;
var
  c:Integer;
begin
  p := @StatusVector;
  Result := False;
  Value := 0;
  c:=0;
  while (p^ <> 0) and (not Result) do
    case p^ of
      3: NextP(3);
      4:
        begin
          NextP(1);
          Result := c >= Index;
          if Result then
          begin
            Value := p^;
            break;
          end;
          NextP(1);
          Inc(c);
        end;
    else
      NextP(2);
    end;
end;

function GetStatusErrorMsg(const StatusVector: TStatusVector; Index: Integer; var Value: string): Boolean;
var
  p: PISC_STATUS;
  function NextP(i: Integer): PISC_STATUS;
  begin
    p := PISC_STATUS(PChar(p) + (i * SizeOf(ISC_STATUS)));
    Result := p;
  end;
var
  c: Integer;
begin
  Result := False;
  Value := '';
  c := 0;
  p := @StatusVector;
  while (p^ <> 0) do
    case p^ of
      3: NextP(3);
      2:
        begin
          NextP(1);
          Result := c >= Index;
          if Result then
          begin
            Value := PChar(P^);
            break;
          end;
          NextP(1);
          Inc(c);
        end
    else
      NextP(2);
    end;
end;

function StatusVectorAsText(const StatusVector: TStatusVector): string;
var
  p: PISC_STATUS;
  function NextP(i: Integer): PISC_STATUS;
  begin
    p := PISC_STATUS(PChar(p) + (i * SizeOf(ISC_STATUS)));
    Result := p;
  end;
begin
  Result := '';
  p := @StatusVector;
  while (p^ <> 0) do
    if (p^ = 3) then
    begin
      Result := Result + Format('%d %d %d', [p^, NextP(1)^, NextP(1)^]) + CRLF;
      NextP(1);
    end
    else
    begin
      Result := Result + Format('%d %d', [p^, NextP(1)^]) + CRLF;
      NextP(1);
    end;
end;

var
  FBDataBaseErrorMessages: TFBDataBaseErrorMessages;

procedure FBAlloc(var P; OldSize, NewSize: Integer; ZeroInit: boolean);
var
  i: Integer;
begin
  if Assigned(Pointer(P)) then
    ReallocMem(Pointer(P), NewSize)
  else
    GetMem(Pointer(P), NewSize);
  if ZeroInit then
    for i := OldSize to NewSize - 1 do
      PChar(P)[i] := #0;
end;

function GetInfoReqRecord(Buffer: PChar; Flag: Byte): Integer;
var
  p: PChar;
  item: Char;
  l: Integer;
begin
  // must use FindToken see _rb.cpp in IBPP
  p := Buffer;
  Result := -1;
  if (p^ = Char(isc_info_sql_records)) then
  begin
    Inc(p);
//    l := FBClient.isc_portable_integer(p, 2);
    Inc(p, 2);
    Item := p^;
    while (Item <> Char(isc_info_end)) do
    begin
      Inc(p);
      l := FBClient.isc_portable_integer(p, 2);
      Inc(p, 2);
      if (Item = Char(Flag)) then
      begin
        Result := FBClient.isc_portable_integer(p, l);
        break;
      end;
      Inc(p, l);
      Item := p^;
    end;
  end;
end;

function FindTokenReq(Buffer: PChar; Flag: Byte): PChar;
var
  p: PChar;
  Item: Char;
  l: Integer;
begin
  Result := nil;
  p := Buffer;
  Item := p^;
  while (Item <> Char(isc_info_end)) do
  begin
    Inc(p);
    l := FBClient.isc_portable_integer(p, 2);
    if (Item = Char(Flag)) then
    begin
      Result := p;
      break;
    end;
    Inc(p, l + 2);
    Item := p^;
  end;
end;

function GetInfoReqString(Buffer: PChar; Flag: Byte; var Value: string): Boolean;
var
  p: PChar;
  l: Integer;
begin
  Value := '';
  p := FindTokenReq(Buffer, Flag);
  Result := p <> nil;
  if Result then
  begin
    l := FBClient.isc_portable_integer(p, 2);
    Inc(p, 2);
    Value := Copy(p, 1, l);
  end;
end;

function GetInfoReqInteger(Buffer: PChar; Flag: Byte; var Value: Integer): Boolean;
var
  p: PChar;
begin
  Value := 0;
  p := FindTokenReq(Buffer, Flag);
  Result := p <> nil;
  if Result then
  begin
    Inc(p, 2);
    Value := FBClient.isc_portable_integer(p, 2);
  end;
end;

procedure SetFBDataBaseErrorMessages(Value: TFBDataBaseErrorMessages);
begin
  FBDataBaseErrorMessages := Value;
end;

function GetFBDataBaseErrorMessages: TFBDataBaseErrorMessages;
begin
  Result := FBDataBaseErrorMessages;
end;

function IsAliasName(FileName: string): Boolean;
begin
  // not a file name e.g. '..\1.fdb' or '.\1.fdb' or 'c:\1.fdb' or '\\server\files\1.fdb'
  Result := (FileName <> '') and (FileName[1] <> '.') and (Copy(FileName, 2, 2) <> ':\') and (Copy(FileName, 1, 2) <> '\\');
end;

function GetAliasName(FileName: string): string;
var
  aAliases: TStringList;
  i, p: Integer;
  s: string;
begin
  Result := '';
  if DirectoryExists(FBClient.InstancePath) then
  begin
    aAliases := TStringList.Create;
    try
      aAliases.LoadFromFile(FBClient.InstancePath + 'aliases.conf');
      for i := 0 to aAliases.Count - 1 do
      begin
        s := aAliases[i];
        if (s <> '') and (s[1] <> '#') then
        begin
          p := AnsiPos('=', s);
          if P > 0 then
          begin
            if SameText(Copy(s, p + 1, MaxInt), FileName) then
            begin
              Result := Copy(s, 1, p - 1);
              break;
            end;
          end;
        end;
      end;
    finally
      aAliases.Free;
    end;
  end;
end;

function GetAliasFile(AliasName: string): string;
var
  aAliases: TStringList;
  i, p: Integer;
  s: string;
begin
  Result := '';
  if DirectoryExists(FBClient.InstancePath) then
  begin
    aAliases := TStringList.Create;
    try
      aAliases.LoadFromFile(FBClient.InstancePath + 'aliases.conf');
      for i := 0 to aAliases.Count - 1 do
      begin
        s := aAliases[i];
        if (s <> '') and (s[1] <> '#') then
        begin
          p := AnsiPos('=', s);
          if P > 0 then
          begin
            if SameText(Copy(s, 1, p - 1), AliasName) then
            begin
              Result := Copy(s, p + 1, MaxInt);
              break;
            end;
          end;
        end;
      end;
    finally
      aAliases.Free;
    end;
  end;
end;

procedure SetAliasFile(AliasName, FileName: string);
var
  aAliases: TStringList;
  i, p: Integer;
  s: string;
begin
  if DirectoryExists(FBClient.InstancePath) then
  begin
    aAliases := TStringList.Create;
    try
      aAliases.LoadFromFile(FBClient.InstancePath + 'aliases.conf');
      for i := 0 to aAliases.Count - 1 do
      begin
        s := aAliases[i];
        if (s <> '') and (s[1] <> '#') then
        begin
          p := AnsiPos('=', s);
          if P > 0 then
          begin
            if SameText(Copy(s, 1, p - 1), AliasName) then
            begin
              if FileName = '' then
              begin
                aAliases.Delete(i);
                break;
              end
              else
                raise EFBError.Create('Alias already exists');
            end;
          end;
        end;
      end;
      if FileName <> '' then
        aAliases.Add(AliasName + '=' + FileName);
      aAliases.SaveToFile(FBClient.InstancePath + 'aliases.conf');
    finally
      aAliases.Free;
    end;
  end;
end;

procedure FBHostInfo(const Host, UserName, Password, Role, CharacterSet: string; vParams: TStrings; CachedPasswords: Boolean);
begin
{  if CachedPasswords and (FBConfig[Host] <> nil) then
  begin
    if (vParams.IndexOfName(DPBConstantNames[isc_dpb_user_name]) < 0) and (FBConfig[Host].UserName <> '') then
      vParams.Values[DPBConstantNames[isc_dpb_user_name]] := FBConfig[Host].UserName;
    if (vParams.IndexOfName(DPBConstantNames[isc_dpb_password]) < 0) and (FBConfig[Host].Password <> '') then
      vParams.Values[DPBConstantNames[isc_dpb_password]] := FBConfig[Host].Password;
  end;}
  if (vParams.IndexOfName(DPBConstantNames[isc_dpb_user_name]) < 0) or (UserName <> '') then
    vParams.Values[DPBConstantNames[isc_dpb_user_name]] := UserName;
  if (vParams.IndexOfName(DPBConstantNames[isc_dpb_password]) < 0) or (Password <> '') then
    vParams.Values[DPBConstantNames[isc_dpb_password]] := Password;
  if vParams.IndexOfName(DPBConstantNames[isc_dpb_sql_role_name]) < 0 then
    vParams.Values[DPBConstantNames[isc_dpb_sql_role_name]] := Role;
{  if vParams.IndexOfName(DPBConstantNames[isc_dpb_set_db_charset]) < 0 then
    vParams.Values[DPBConstantNames[isc_dpb_set_db_charset]] := CharacterSet;}
  if vParams.IndexOfName(DPBConstantNames[isc_dpb_lc_ctype]) < 0 then
    vParams.Values[DPBConstantNames[isc_dpb_lc_ctype]] := CharacterSet;
  if (vParams.IndexOfName(DPBConstantNames[isc_dpb_user_name]) < 0) and (vParams.Values[DPBConstantNames[isc_dpb_user_name]] = '') then
    vParams.Values[DPBConstantNames[isc_dpb_user_name]] := 'sysdba';
  if (vParams.IndexOfName(DPBConstantNames[isc_dpb_password]) < 0) and (vParams.Values[DPBConstantNames[isc_dpb_password]] = '') then
    vParams.Values[DPBConstantNames[isc_dpb_password]] := 'masterkey';
end;

procedure GenerateDPB(sl: TStrings; var DPB: string; var DPBLength: Short);
var
  i, j, pval: Integer;
  DPBVal: UShort;
  ParamName, ParamValue: string;
begin
  { The DPB is initially empty, with the exception that
    the DPB version must be the first byte of the string. }
  DPBLength := 1;
  DPB := Char(isc_dpb_version1);

  {Iterate through the textual database parameters, constructing
   a DPB on-the-fly }
  for i := 0 to sl.Count - 1 do
  begin
    { Get the parameter's name and value from the list,
      and make sure that the name is all lowercase with
      no leading 'isc_dpb_' prefix
    }
    if (Trim(sl.Names[i]) = '') then
      continue;
    ParamName := LowerCase(sl.Names[i]);
    ParamValue := Copy(sl[i], Pos('=', sl[i]) + 1, Length(sl[i]));
    if (Pos(DPBPrefix, ParamName) = 1) then
      Delete(ParamName, 1, Length(DPBPrefix));
     { We want to translate the parameter name to some Integer
       value. We do this by scanning through a list of known
       database parameter names (DPBConstantNames, defined above) }
    DPBVal := 0;
    { Find the parameter }
    for j := 1 to isc_dpb_last_dpb_constant do
      if (ParamName = DPBConstantNames[j]) then
      begin
        DPBVal := j;
        break;
      end;
     {  A database parameter either contains a string value (case 1)
       or an Integer value (case 2)
       or no value at all (case 3)
       or an error needs to be generated (case else)  }
    case DPBVal of
      isc_dpb_user_name, isc_dpb_password, isc_dpb_password_enc,
        isc_dpb_sys_user_name, isc_dpb_license, isc_dpb_encrypt_key,
        isc_dpb_lc_messages, isc_dpb_lc_ctype, isc_dpb_set_db_charset,
        isc_dpb_sql_role_name, isc_dpb_sql_dialect:
        begin
          if DPBVal = isc_dpb_sql_dialect then
            ParamValue[1] := Char(Ord(ParamValue[1]) - 48);
          DPB := DPB +
            Char(DPBVal) +
            Char(Length(ParamValue)) +
            ParamValue;
          Inc(DPBLength, 2 + Length(ParamValue));
        end;
      isc_dpb_num_buffers, isc_dpb_dbkey_scope, isc_dpb_force_write,
        isc_dpb_no_reserve, isc_dpb_damaged, isc_dpb_verify:
        begin
          DPB := DPB +
            Char(DPBVal) +
            #1 +
            Char(StrToInt(ParamValue));
          Inc(DPBLength, 3);
        end;
      isc_dpb_sweep:
        begin
          DPB := DPB +
            Char(DPBVal) +
            #1 +
            Char(isc_dpb_records);
          Inc(DPBLength, 3);
        end;
      isc_dpb_sweep_interval:
        begin
          pval := StrToInt(ParamValue);
          DPB := DPB +
            Char(DPBVal) +
            #4 +
            PChar(@pval)[0] +
            PChar(@pval)[1] +
            PChar(@pval)[2] +
            PChar(@pval)[3];
          Inc(DPBLength, 6);
        end;
      isc_dpb_activate_shadow, isc_dpb_delete_shadow, isc_dpb_begin_log,
        isc_dpb_quit_log:
        begin
          DPB := DPB +
            Char(DPBVal) +
            #1 + #0;
          Inc(DPBLength, 3);
        end;
    else
      begin
        if (DPBVal > 0) and
          (DPBVal <= isc_dpb_last_dpb_constant) then
          FBRaiseError(fbceDPBConstantNotSupported, [DPBConstantNames[DPBVal]])
        else
          FBRaiseError(fbceDPBConstantUnknownEx, [sl.Names[i]]);
      end;
    end;
  end;
end;

{ GenerateTPB -
  Given a string containing a textual representation
  of the transaction parameters, generate a transaction
  parameter buffer, and return it and its length in
  TPB and TPBLength, respectively. }

procedure GenerateTPB(sl: TStrings; var TPB: string; var TPBLength: Short);
var
  i, j, TPBVal, ParamLength: Integer;
  ParamName, ParamValue: string;
begin
  TPB := '';
  if (sl.Count = 0) then
    TPBLength := 0
  else
  begin
    TPBLength := sl.Count + 1;
    TPB := TPB + Char(isc_tpb_version3);
  end;
  for i := 0 to sl.Count - 1 do
  begin
    if (Trim(sl[i]) = '') then
    begin
      Dec(TPBLength);
      Continue;
    end;
    if (Pos('=', sl[i]) = 0) then
      ParamName := LowerCase(sl[i])
    else
    begin
      ParamName := LowerCase(sl.Names[i]);
      ParamValue := Copy(sl[i], Pos('=', sl[i]) + 1, Length(sl[i]));
    end;
    if (Pos(TPBPrefix, ParamName) = 1) then
      Delete(ParamName, 1, Length(TPBPrefix));
    TPBVal := 0;
    { Find the parameter }
    for j := 1 to isc_tpb_last_tpb_constant do
      if (ParamName = TPBConstantNames[j]) then
      begin
        TPBVal := j;
        break;
      end;
    { Now act on it }
    case TPBVal of
      isc_tpb_consistency, isc_tpb_exclusive, isc_tpb_protected,
        isc_tpb_concurrency, isc_tpb_shared, isc_tpb_wait, isc_tpb_nowait,
        isc_tpb_read, isc_tpb_write, isc_tpb_ignore_limbo,
        isc_tpb_read_committed, isc_tpb_rec_version, isc_tpb_no_rec_version,
        isc_tpb_restart_requests, isc_tpb_no_auto_undo:
        TPB := TPB + Char(TPBVal);

      isc_tpb_lock_read, isc_tpb_lock_write, isc_tpb_lock_timeout {fb2}:
        begin
          TPB := TPB + Char(TPBVal);
        { Now set the string parameter }
          ParamLength := Length(ParamValue);
          Inc(TPBLength, ParamLength + 1);
          TPB := TPB + Char(ParamLength) + ParamValue;
        end;
    else
      begin
        if (TPBVal > 0) and
          (TPBVal <= isc_tpb_last_tpb_constant) then
          FBRaiseError(fbceTPBConstantNotSupported, [TPBConstantNames[TPBVal]])
        else
          FBRaiseError(fbceTPBConstantUnknownEx, [sl.Names[i]]);
      end;
    end;
  end;
end;

function FBComposeConnectionString(DatabaseName, Host, Port: string; IsEmbed: Boolean; Protocol: TFBProtocol = dpTCP): string;
begin
  if IsEmbed and (Host = '') then
  begin
    Protocol := dpLocal;
  end;

  if not IsEmbed and (Protocol <> dpLocal) and (Host = '') then
    Host := 'localhost';

  if (Port <> '') and (Host <> '') and (AnsiPos('/', Host) = 0) then
    Host := Host + '/' + Port;

  case Protocol of
    dpTCP: Result := Host + ':' + DatabaseName;
    dpNamedPipe: Result := '\\' + Host + '\' + DatabaseName;
    dpSPX: Result := Host + '@' + DatabaseName;
    dpLocal: Result := DatabaseName;
  end;
end;

procedure FBDecomposeConnectionString(DatabaseName: string; var Host, FileName: string; var Protocol: TFBProtocol);
var
  Idx1, Idx2: Integer;
  st: string;
begin
  if Pos('\\', DatabaseName) <> 0 then
  begin
    Protocol := dpNamedPipe;
    st := Copy(DatabaseName, 3, Length(DatabaseName));
    Idx1 := Pos('\', st);
    if Idx1 = 0 then
      FBRaiseError(fbceUnknownError, [nil])
    else
    begin
      Host := Copy(st, 1, Idx1 - 1);
      FileName := Copy(st, Idx1 + 1, Length(st));
    end;
  end
  else
  begin
    Idx1 := Pos(':', DatabaseName);
    if (Idx1 = 0) or (Idx1 = 2) then
    begin
      FileName := DatabaseName;
      Host := '';
      Protocol := dpTCP;
    end
    else
    begin
      Idx2 := Pos('@', DatabaseName);
      if Idx2 = 0 then
      begin
        Protocol := dpTCP;
        Host := Copy(DatabaseName, 1, Idx1 - 1);
        FileName := Copy(DatabaseName, Idx1 + 1, Length(DatabaseName));
      end
      else
      begin
        Protocol := dpSPX;
        Host := Copy(DatabaseName, 1, Idx2 - 1);
        FileName := Copy(DatabaseName, Idx2 + 1, Length(DatabaseName));
      end;
    end;
  end;
end;

initialization
  IsMultiThread := True;
  FBDataBaseErrorMessages := [ShowSQLCode, ShowFBMessage, ShowSQLMessage];
finalization
end.

