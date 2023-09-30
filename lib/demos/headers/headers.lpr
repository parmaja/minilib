program header;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, mnUtils,
	TestUnit;

type

  { TMyApp }

  TMyApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

procedure TMyApp.DoRun;
begin
  inherited DoRun;
  RunTest;
    // stop program loop
  Terminate;
end;

constructor TMyApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException :=True;
end;

destructor TMyApp.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMyApp;
begin
  Application :=TMyApp.Create(nil);
  Application.Title :='Headers';
  Application.Run;
  ReadLn();
  Application.Free;
end.

