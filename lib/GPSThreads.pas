unit GPSThreads;
{**
 *  This file is part of the "MiniLib"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
 
{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs,
  mnStreams, mnCommClasses, mnCommThreads, GPSUtils;

type
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

implementation

procedure TmnGPSThread.StringArrived(S: string);
var
  P: Integer;
begin
  FBuffer := FBuffer + S;
  P := AnsiPos(FEndOfLine, FBuffer);
  while P > 0 do
  begin
    Parse(Copy(FBuffer, 1, P - 1));
    FBuffer := Copy(FBuffer, P + 1, MaxInt);
    P := AnsiPos(FEndOfLine, FBuffer);
  end;
end;

procedure TmnGPSThread.Parse(S: string);
var
  Info:TGPSInfo;
begin
  if GPSPrase(S, Info) then
  begin
    SetGPSInfo(Info);
//    if CommStream.Connected then
//    begin
//      FBuffer:='';
//      CommStream.Purge;//try to clear cache, we need a runtime values from GPS receiver
//    end;
  end;
end;

initialization
finalization
end.
