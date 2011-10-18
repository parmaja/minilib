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
  mnCommClasses;

type

  { TmnCommThread }

  TmnCommThread = class(TThread)
  private
    FCommStream: TmnCustomCommStream;
    FUseWait: Boolean;
  protected
    procedure DoTerminate; override;
    procedure StringArrived(S: string); virtual;
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; CommStream: TmnCustomCommStream);
    destructor Destroy; override;
    property UseWait: Boolean read FUseWait write FUseWait;
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
    //FCommStream.Cancel;
    if FreeOnTerminate then
      FCommStream.Close;
  end;
  inherited;
end;

procedure TmnCommThread.Execute;
begin
  try
    while not Terminated and FCommStream.Connected do
    begin
      if not UseWait or (FCommStream.WaitRead) then;
      begin
        if not Terminated and FCommStream.Connected then
          StringArrived(FCommStream.ReadString);
      end;
    end;
  finally
  end;
end;

procedure TmnCommThread.StringArrived(S: string);
begin
end;

{ TmnCommStreamThread }

constructor TmnCommStreamThread.Create(CreateSuspended: Boolean; CommStream: TmnCustomCommStream);
begin
  inherited Create(CreateSuspended, CommStream);
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

