program headers;


{$APPTYPE CONSOLE}

uses
  Classes, SysUtils, mnUtils, TestUnit;


type

  { TMyApp }

  TMyApp = class
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

procedure TMyApp.Run;
begin
    // stop program loop
  RunTest;
end;

constructor TMyApp.Create;
begin
  inherited Create;

end;

destructor TMyApp.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TMyApp;
begin
  Application :=TMyApp.Create;
  Application.Run;
  ReadLn;
  Application.Free;
end.


