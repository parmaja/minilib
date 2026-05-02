unit mnGPSUtils;
{$M+}{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}
{**
 *  This file is part of the "MiniLib"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
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
 
interface

uses
  Classes, SysUtils;
  
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

function GPSDecimalToDMS(Value: Extended; D, M, S:string): string; overload;//you can call with (v, '�','''','"')
function GPSDecimalToDMS(Value: Extended): string; overload;
function GPSDecimalToDM(Value: Extended; D, M:string): string; overload;
function GPSDecimalToDM(Value: Extended): string; overload;
function GPSToMAP(Info: TGPSDecimalInfo): string; overload;
function GPSToMAP(Latitude: Extended; Longitude: Extended): string; overload;
function GPSPrase(Command:string; var Info:TGPSInfo):Boolean;

implementation

function GPSDecimalToDMS(Value:Extended; D, M, S:string):string;
var
  t: Integer;
begin
  t := Trunc(Value);
  Result := IntToStr(t) + D;
  Value := (Value - t) * 60;

  t := Trunc(Value);
  Result := Result + IntToStr(t) + M;
  Value := (Value - t) * 60;
  Result := Result + FormatFloat('0.0000', Value) + S;
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

function GPSToMAP(Latitude: Extended; Longitude: Extended) :string;
var
  SLat, SLon: string;
begin
  SLat := FormatFloat('0.000000', Abs(Latitude));
  SLon := FormatFloat('0.000000', Abs(Longitude));
  if Latitude < 0 then
    Result := '-'
  else
    Result := '+';
  if Longitude < 0 then
    Result := Result + SLat + ',-' + SLon
  else
    Result := Result + SLat + ',+' + SLon;
end;

function GPSToMAP(Info: TGPSDecimalInfo):string;
begin
  Result := GPSToMAP(Info.Latitude, Info.Longitude)
end;

function GPSPrase(Command:string; var Info:TGPSInfo):Boolean;
var
  P, I, Start, FieldIndex: Integer;
  CS, CMD: string;
  Params: array[0..5] of string;
begin
  Result := False;
  Command := Trim(Command);
  if (Length(Command) >= 3) and (Copy(Command, 1, 3) = '$GP') then //is it GPS command?
  begin
    Delete(Command, 1, 3);//remove '$GP'
    P := Pos('*', Command);
    if P > 0 then // not bad for now
    begin
      CS := Copy(Command, P + 1, Length(Command));
      SetLength(Command, P - 1);

      // Lightweight CSV parser, extract first 6 fields
      Start := 1;
      FieldIndex := 0;
      for I := 1 to Length(Command) + 1 do
      begin
        if (I > Length(Command)) or (Command[I] = ',') then
        begin
          if FieldIndex <= 5 then
          begin
            Params[FieldIndex] := Copy(Command, Start, I - Start);
            Inc(FieldIndex);
          end;
          Start := I + 1;
          if FieldIndex > 5 then
            Break;
        end;
      end;

      if FieldIndex > 0 then
      begin
        CMD := Params[0];
        if CMD = 'GSV' then //Satellite info
        else if CMD = 'GSA' then //Fix
        else if CMD = 'GGA' then
        begin
          if FieldIndex > 5 then
          begin
            Result := True;
            Info.Source:= Params[3] + Params[2] + ',' + Params[5] + Params[4];
            Info.Decimal.Latitude := StrToIntDef(Copy(Params[2], 1, 2), 0) + (StrToFloatDef(Copy(Params[2], 3, Length(Params[2])), 0) / 60);
            if Params[3] = 'S' then
              Info.Decimal.Latitude := -Info.Decimal.Latitude;

            Info.Decimal.Longitude := StrToIntDef(Copy(Params[4], 1, 3), 0) + (StrToFloatDef(Copy(Params[4], 4, Length(Params[4])), 0) / 60);
            if Params[5] = 'W' then
              Info.Decimal.Longitude := -Info.Decimal.Longitude;
            Info.LastAccess := Now;
          end;
        end;
      end;
    end;
  end;
end;

end.
