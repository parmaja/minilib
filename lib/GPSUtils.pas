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
  http://gpsinformation.org/dale/nmea.htm#position
  http://boulter.com/gps
}
 
{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  mnStreams, mnCommClasses, mnCommThreads;
  
type
  EGPSError = class(Exception);
  
  TGPSSourceInfo = record
    Latitude: string;
    Longitude: string;
//    Altitude: Double;
  end;

  TGPSDataInfo = record
    Latitude: double;
    Longitude: double;
  end;

  TGPSDegreeInfo = record
    Latitude: Double;
    Longitude: Double;
  end;

  TGPSGMapInfo = record
    Latitude: string;
    Longitude: string;
  end;

  TGPSInfo = record
    Source:TGPSSourceInfo;
    Data: TGPSDataInfo;
    Degree: TGPSDegreeInfo;
    GMap: TGPSGMapInfo;
  end;

  { TmnGPSThread }

  TmnGPSThread = class(TmnCommThread)
  private
    FEndOfLine: string;
    FBuffer: string;
  protected
    procedure StringArrived(S: string); override;
    procedure Parse(S: string); virtual;
  public
    property EndOfLine: string read FEndOfLine write FEndOfLine;
  end;

function GPSInfo:TGPSInfo;
procedure SetGPSInfo(Info: TGPSInfo);

function GPSPrase(Command:string; var Info:TGPSInfo):Boolean;

implementation

var
  FGPSLock : TCriticalSection = nil;
  FGPSInfo: TGPSInfo;

function GPSInfo: TGPSInfo;
begin
  if FGPSLock = nil then
    FGPSLock := TCriticalSection.Create;
  FGPSLock.Enter;
  try
    Result := FGPSInfo;
  finally
    FGPSLock.Leave;
  end;
end;

procedure SetGPSInfo(Info: TGPSInfo);
begin
  if FGPSLock = nil then
    FGPSLock := TCriticalSection.Create;
  FGPSLock.Enter;
  try
    FGPSInfo := Info;
  finally
    FGPSLock.Leave;
  end;
end;

function GPSPrase(Command:string; var Info:TGPSInfo):Boolean;
var
  P: Integer;
  CS:string;
  Params:TStringList;
  CMD: string;
  function GetDegree(S, D, N:String):Double;
  var
    v: Double;
  begin
    v := StrToFloatDef(S, 99999);
    if v <> 99999 then
    begin
      Result := Trunc(v / 100);
      Result := Result + (v - (Result * 100)) / 60;
      if D = N then
        Result := - Result;

      //Result := D + Format('%.d', Trunc(v)) + frac(v);
    end;
  end;
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
        else if CMD = 'GGA' then //Fix
        begin
          Result := True;
          Info.Source.Latitude:= Params[2] + ' ' + Params[3];
          Info.Source.Longitude:= Params[4] + ' ' + Params[5];
          Info.Data.Latitude:=StrToFloatDef(Params[2], 0);
          if Params[3] = 'S' then
            Info.Data.Latitude := -Info.Data.Latitude;
          Info.Data.Longitude:=StrToFloatDef(Params[4], 0);
          if Params[5] = 'W' then
            Info.Data.Longitude := -Info.Data.Longitude;
          Info.Degree.Latitude := GetDegree(Params[2], Params[3], 'S');
          Info.Degree.Longitude := GetDegree(Params[4], Params[5], 'W');
          Info.GMap.Latitude := Params[3] + FormatFloat('0.0000', Abs(Info.Degree.Latitude));
          Info.GMap.Longitude := Params[5] + FormatFloat('0.0000', Abs(Info.Degree.Longitude));
        end;
      finally
        Params.Free;
      end;
    end;
  end;
end;

procedure TmnGPSThread.StringArrived(S: string);
var
  P: Integer;
begin
  FBuffer := FBuffer + S;
  P := AnsiPos(FEndOfLine, FBuffer);
  if P > 0 then
  begin
    Parse(Copy(FBuffer, 1, P - 1));
    FBuffer := Copy(FBuffer, P + 1, MaxInt);
  end;
end;

procedure TmnGPSThread.Parse(S: string);
var
  Info:TGPSInfo;
begin
  if GPSPrase(S, Info) then
    SetGPSInfo(Info);
end;

initialization
finalization
  FreeAndNil(FGPSLock);
end.

