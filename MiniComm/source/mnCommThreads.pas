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
  TmnCommThread = class(TThread)
  private
    FCommStream: TmnCustomCommStream;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
    procedure StringArrived(S: string); virtual;
  public
    constructor Create(CreateSuspended: Boolean; CommStream: TmnCustomCommStream);
    property CommStream: TmnCustomCommStream read FCommStream;
  end;

  TmnCommStreamThread = class(TmnCommThread)
  protected
    FBuffer: string;
    procedure DoStringArrived; virtual;
    procedure StringArrived(S: string); override;
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

procedure TmnCommThread.DoTerminate;
begin
  if FCommStream <> nil then
    FCommStream.Cancel;
  inherited;
end;

procedure TmnCommThread.Execute;
begin
  inherited;
  try
    // You muse have EventChar and QueMode
    while not Terminated and FCommStream.Connected do
    begin
      if FCommStream.WaitEvent([evRxFlag]) <> [] then;
        StringArrived(FCommStream.ReadString);
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

procedure TmnCommStreamThread.DoStringArrived;
begin
end;

procedure TmnCommStreamThread.StringArrived(S: string);
begin
  inherited;
  FBuffer := S;
  Synchronize(DoStringArrived);
end;

end.

