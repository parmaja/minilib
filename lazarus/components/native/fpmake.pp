{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for NativeLib 1.0.1.1

   This file was generated on 20/05/2014
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_NativeLib;

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPAckage('nativelib');
    P.Version:='1.0.1.1';

{$ifdef ALLPACKAGES}
    // when this is part of a meta package, set here the sub directory
    // P.Directory:='put here the relative path';
{$endif ALLPACKAGES}

    P.Dependencies.Add('minicommons');
    P.Dependencies.Add('ideintf');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scgim');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-gv');
    P.Options.Add('-vew');
    P.Options.Add('-l');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCL_PLATFORM)');
    P.Options.Add('-FuP:/dev/lazarus/packager/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-FuP:/dev/lazarus/components/lazutils/lib/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-Fu../../../lib/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-FuP:/dev/lazarus/lcl/units/$(CPU_TARGET)-$(OS_TARGET)');
    P.Options.Add('-FuP:/dev/lazarus/lcl/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-FuP:/dev/lazarus/components/lazcontrols/lib/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-FuP:/dev/lazarus/components/ideintf/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_PLATFORM)');
    P.Options.Add('-Fu.');
    T:=P.Targets.AddUnit('NativeLib.pas');
    t.Dependencies.AddUnit('ntvPageControls');
    t.Dependencies.AddUnit('ntvProgressBars');
    t.Dependencies.AddUnit('ntvDotMatrix');
    t.Dependencies.AddUnit('ntvTabSets');
    t.Dependencies.AddUnit('ntvTabs');
    t.Dependencies.AddUnit('ntvSplitters');

    P.Sources.AddSrc('ntvRegCtrls.pas');
    P.Sources.AddSrc('ntvCtrls.pas');
    P.Sources.AddSrc('ntvUtils.pas');
    T:=P.Targets.AddUnit('ntvPageControls.pas');
    T:=P.Targets.AddUnit('ntvProgressBars.pas');
    T:=P.Targets.AddUnit('ntvDotMatrix.pas');
    T:=P.Targets.AddUnit('ntvTabSets.pas');
    T:=P.Targets.AddUnit('ntvTabs.pas');
    T:=P.Targets.AddUnit('ntvSplitters.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('NativeLib.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_NativeLib;
  Installer.Run;
end.
{$endif ALLPACKAGES}
