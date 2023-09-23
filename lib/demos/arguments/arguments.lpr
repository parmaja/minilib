program arguments;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, mnUtils,
	TestUnit;

type

  { TMyArguments }

  TMyArguments = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

procedure TMyArguments.DoRun;
begin
  inherited DoRun;
  RunTest;
    // stop program loop
  Terminate;
end;

constructor TMyArguments.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException :=True;
end;

destructor TMyArguments.Destroy;
begin
  inherited Destroy;
end;

procedure TMyArguments.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMyArguments;
begin
  Application :=TMyArguments.Create(nil);
  Application.Title :='Arguments';
  Application.Run;
  ReadLn();
  Application.Free;
end.

