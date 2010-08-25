unit GPSUtils;
{**
 *  This file is part of the "MiniLib"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
 
{
  see
  http://en.wikipedia.org/wiki/Geographic_coordinate_system
  http://geography.about.com/c/ht/00/07/How_Convert_Decimal_Degrees0962932697.htm
  http://gpsinformation.org/dale/nmea.htm#position
  http://www.csgnetwork.com/gpscoordconv.html
  http://boulter.com/gps
  
Sample

  $GPGGA,180924.000,4036.9101,N,07359.9423,W,1,......

  4036.9101,N,07359.9423,W

  N40.369101 W73.599423

  http://boulter.com/gps
  enter N40.369101 W73.599423
  Result DMS
  N40 22 08	         W73 35 57
  and DM
  N 40 22.146	W 73 35.965

  $GPGGA,205226.000,4038.0325,N,07401.2500,W,1,9,0.90,41.6,M,-34.3,M,,*5B
  
  $GPGGA,204852.000,4038.0022,N,07401.2578,W,1,6,1.64,31.7,M,-34.3,M,,*5B

}
 
{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  mnStreams, mnCommClasses;
  
type
  EGPSError = class(Exception);
  
  TGPSDecimalInfo = record
    Latitude: Extended;
    Longitude: Extended;
  end;

  TGPSInfo = record
    Source: string;
    Decimal: TGPSDecimalInfo;
    LastAccess: TDateTime;
  end;

function GPSDecimalToDMS(Value: Extended; D, M, S:string): string; overload;//you can call with (v, '°','''','"')
function GPSDecimalToDMS(Value: Extended): string; overload;
function GPSDecimalToDM(Value: Extended; D, M:string): string; overload;
function GPSDecimalToDM(Value: Extended): string; overload;
function GPSToMAP(Info: TGPSDecimalInfo): string; overload;
function GPSToMAP(Latitude: Extended; Longitude: Extended): string; overload;
function GPSPrase(Command:string; var Info:TGPSInfo):Boolean;

function GPSInfo: TGPSInfo;
procedure SetGPSInfo(Info: TGPSInfo);
function GPSLock: TCriticalSection;

implementation

function GPSDecimalToDMS(Value:Extended; D, M, S:string):string;
var
  t: Integer;
begin
  t := Trunc(Value);
  Result := IntToStr(t) + ' ';
  Value := (Value - t) * 60;

  t := Trunc(Value);
  Result := Result + IntToStr(t) + ' ';
  Value := (Value - t) * 60;
  Result := Result + FormatFloat('0.0000', Value);
end;

function GPSDecimalToDMS(Value:Extended):string;
begin
  Result := GPSDecimalToDMS(Value, ' ', ' ', '');
end;

function GPSDecimalToDM(Value:Extended; D, M:string):string;
var
  t: Integer;
begin
  t := Trunc(Value);
  Result := IntToStr(t) + D;
  Value := (Value - t) * 60;

  Result := Result + FormatFloat('0.0000', Value) + M;
end;

function GPSDecimalToDM(Value:Extended):string;
begin
  Result := GPSDecimalToDM(Value, ' ', '');
end;

function GPSToMAP(Latitude: Extended; Longitude: Extended) :string; overload;
begin
  if Latitude < 0 then
    Result := '-'
  else
    Result := '+';
  Result := Result + FormatFloat('0.000000', Abs(Latitude));
  Result := Result + ',';
  if Longitude < 0 then
    Result := Result + '-'
  else
    Result := Result + '+';
  Result := Result + FormatFloat('0.000000', Abs(Longitude));
end;

function GPSToMAP(Info: TGPSDecimalInfo):string;
begin
  Result := GPSToMAP(Info.Latitude, Info.Longitude)
end;

function GPSPrase(Command:string; var Info:TGPSInfo):Boolean;
var
  P: Integer;
  CS:string;
  D, M: string;
  Params:TStringList;
  CMD: string;
begin
  Result := False;
  Command := Trim(Command);
  if LeftStr(Command, 3) = '$GP' then //is it GPS command?
  begin
    Command := Copy(Command, 4, MaxInt);//remove '$GP'
    P := AnsiPos('*', Command);
    if P > 0 then // not bad for now
    begin
      CS := Copy(Command, P + 1, MaxInt);
      Command := Copy(Command, 1, P - 1);
      Params := TStringList.Create;
      try
        ExtractStrings([','], [], PChar(Command), Params);
        CMD := Params[0];
        if CMD = 'GSV' then //Satellite info
        else if CMD = 'GSA' then //Fix
        else if CMD = 'GGA' then
        begin
          Result := True;
          Info.Source:= Params[3] + Params[2] + ',' + Params[5] + Params[4];
          D := Copy(Params[2], 1, 2);//3 char for Degree
          M := Copy(Params[2], 3, MaxInt);
          Info.Decimal.Latitude := StrToIntDef(D, 0) + (StrToFloatDef(M, 0) / 60);
          if Params[3] = 'S' then
            Info.Decimal.Latitude := -Info.Decimal.Latitude;

          D := Copy(Params[4], 1, 3);//3 char for Degree
          M := Copy(Params[4], 4, MaxInt);
          Info.Decimal.Longitude := StrToIntDef(D, 0) + (StrToFloatDef(M, 0) / 60);
          if Params[5] = 'W' then
            Info.Decimal.Longitude := -Info.Decimal.Longitude;
          Info.LastAccess := Now;
        end;
      finally
        Params.Free;
      end;
    end;
  end;
end;

var
  FGPSLock : TCriticalSection = nil;
  FGPSInfo: TGPSInfo;

function GPSLock:TCriticalSection;
begin
  if FGPSLock = nil then
    FGPSLock := TCriticalSection.Create;
  Result := FGPSLock;
end;

function GPSInfo: TGPSInfo;
begin
  GPSLock.Enter;
  try
    Result := FGPSInfo;
  finally
    GPSLock.Leave;
  end;
end;

procedure SetGPSInfo(Info: TGPSInfo);
begin
  GPSLock.Enter;
  try
    FGPSInfo := Info;
  finally
    GPSLock.Leave;
  end;
end;

initialization
finalization
  FreeAndNil(FGPSLock);
end.

