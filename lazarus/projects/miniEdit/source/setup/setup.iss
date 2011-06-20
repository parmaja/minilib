;
; Light PHP Edit
;
; @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
; @author    Zaher Dirkey <zaher at parmaja dot com>
;
[Setup]
AppName=Mini Edit
AppVerName=Mini Edit 1.0.1.1
AppPublisher=PARMAJA
AppPublisherURL=http://www.parmaja.com
AppSupportURL=http://www.parmaja.com
AppUpdatesURL=http://www.parmaja.com
DefaultDirName={pf}\Parmaja\MiniEdit
DefaultGroupName=Mini Edit
LicenseFile=..\copying.txt
OutputDir=..\..\install
OutputBaseFilename=mne-setup-1.0.1-beta
VersionInfoTextVersion=lpe-setup-1.0.1-beta
VersionInfoVersion=1.0.1
VersionInfoCompany=parmaja.com
VersionInfoDescription=Mini Edit
Compression=lzma/ultra
SolidCompression=true
InternalCompressLevel=ultra
AppMutex=Parmaja.MiniEdit
UninstallDisplayIcon={app}\mne.exe
ShowLanguageDialog=yes

[Languages]
Name: eng; MessagesFile: compiler:Default.isl
Name: ara; MessagesFile: compiler:Languages\Arabic.isl

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked
Name: quicklaunchicon; Description: {cm:CreateQuickLaunchIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
Source: ..\..\bin\mne.exe; DestDir: {app}; Flags: ignoreversion
Source: ..\..\changed.txt; DestDir: {app}; Flags: ignoreversion
Source: ..\..\copying.txt; DestDir: {app}; Flags: ignoreversion
Source: ..\..\readme.txt; DestDir: {app}; Flags: ignoreversion

[INI]
Filename: {app}\mne.url; Section: InternetShortcut; Key: URL; String: http://www.parmaja.com/projects/miniedit

[Icons]
Name: {group}\Mini Edit; Filename: {app}\mne.exe
Name: {group}\{cm:ProgramOnTheWeb,Mini Edit}; Filename: {app}\mne.url
Name: {group}\{cm:UninstallProgram,Mini Edit}; Filename: {uninstallexe}
Name: {userdesktop}\Mini Edit; Filename: {app}\mne.exe; Tasks: desktopicon
Name: {userappdata}\Microsoft\Internet Explorer\Quick Launch\Mini Edit; Filename: {app}\mne.exe; Tasks: quicklaunchicon

[Run]
Filename: {app}\mne.exe; Description: {cm:LaunchProgram,Light PHP Edit}; Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: files; Name: {app}\mne.url
[_ISTool]
UseAbsolutePaths=true
