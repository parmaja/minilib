program testcipher;

{$mode objfpc}{$H+}

uses
  Forms, LResources, Interfaces,
  main in 'main.pas' {MainForm};

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
