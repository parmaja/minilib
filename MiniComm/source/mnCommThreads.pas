unit mnCommThreads;
{**
 *  This file is part of the "Mini Comm"
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
  Classes, SysUtils,
  mnStreams, mnCommClasses;

type

  { TmnCommThread }

  TmnCommThread = class(TThread)
  private
    FCommStream: TmnCustomCommStream;
    FUseWaitEvent: Boolean;
  protected
    procedure DoTerminate; override;
    procedure StringArrived(S: string); virtual;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; CommStream: TmnCustomCommStream);
    destructor Destroy; override;
    property UseWaitEvent: Boolean read FUseWaitEvent write FUseWaitEvent;
    property CommStream: TmnCustomCommStream read FCommStream;
  end;

  TmnCommStreamThread = class(TmnCommThread)
  private
    procedure InternalStringArrived; 
  protected
    FBuffer: string;
    procedure StringArrived(S: string); override;
    procedure DoStringArrived(S: string); virtual;
  public
    constructor Create(CreateSuspended: Boolean; CommStream: TmnCustomCommStream);
  end;

  
implementation


{ TmnCommThread }

constructor TmnCommThread.Create(CreateSuspended: Boolean;
  CommStream: TmnCustomCommStream);
begin
  inherited Create(CreateSuspended);
  FCommStream := CommStream;
end;

destructor TmnCommThread.Destroy;
begin
  if FreeOnTerminate then
    FreeAndNil(FCommStream);
  inherited Destroy;
end;

procedure TmnCommThread.DoTerminate;
begin
  if FCommStream <> nil then
  begin
    FCommStream.Cancel;
    if FreeOnTerminate then
      FCommStream.Close;
  end;
  inherited;
end;

procedure TmnCommThread.Execute;
begin
  try
    // You muse have EventChar and QueMode
    while not Terminated and FCommStream.Connected do
    begin
      if not UseWaitEvent or (FCommStream.WaitEvent([evError, evRxFlag]) <> []) then;
      begin
        if not Terminated and FCommStream.Connected then
        begin
          StringArrived(FCommStream.ReadString);
        end;
      end;
    end;
  finally
  end;
end;

procedure TmnCommThread.StringArrived(S: string);
begin
end;

{ TmnCommStreamThread }

constructor TmnCommStreamThread.Create(CreateSuspended: Boolean;
  CommStream: TmnCustomCommStream);
begin
  inherited Create(CreateSuspended, CommStream);
  if CommStream.ReadTimeout = 0 then //We will use QueMode
    CommStream.QueMode := True;
end;

procedure TmnCommStreamThread.DoStringArrived(S: string);
begin
end;

procedure TmnCommStreamThread.InternalStringArrived;
begin
  DoStringArrived(FBuffer);
end;

procedure TmnCommStreamThread.StringArrived(S: string);
begin
  FBuffer := S;
  InternalStringArrived;
end;

end.

