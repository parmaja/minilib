unit EditorEngine;

{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Messages, SysUtils, Forms, StrUtils, Dialogs, Variants, Classes, Controls, Graphics, Contnrs, Types,
  IniFiles, EditorOptions, EditorProfiles, SynEditMarks, SynCompletion, SynEditTypes,
  SynEditMiscClasses, SynEditHighlighter, SynEditKeyCmds, SynEditMarkupBracket, SynEditSearch, SynEdit,
  SynEditTextTrimmer, SynTextDrawer, EditorDebugger, SynGutterBase,
  dbgpServers, PHP_xDebug,
  mnXMLRttiProfile, mnXMLUtils, mnUtils, LCLType;

type
  TEditorChangeState = set of (ecsChanged, ecsState, ecsRefresh, ecsDebug, ecsShow, ecsEdit, ecsFolder, ecsProject); //ecsShow bring to front
  TSynCompletionType = (ctCode, ctHint, ctParams);

  TEditorEngine = class;
  TFileCategory = class;
  TFileGroup = class;
  TFileGroups = class;
  TEditorFile = class;
  TEditorProject = class;

  EEditorException = class(Exception)
  private
    FErrorLine: integer;
  public
    property ErrorLine: integer read FErrorLine write FErrorLine;
  end;

  TEditorDesktopFile = class(TCollectionItem)
  private
    FFileName: string;
    FCaretY: integer;
    FCaretX: integer;
    FTopLine: integer;
  public
  published
    property FileName: string read FFileName write FFileName;
    property CaretX: integer read FCaretX write FCaretX default 1;
    property CaretY: integer read FCaretY write FCaretY default 1;
    property TopLine: integer read FTopLine write FTopLine default 1;
  end;

  TEditorDesktopFiles = class(TCollection)
  private
    FCurrentFile: string;
    function GetItems(Index: integer): TEditorDesktopFile;
  protected
  public
    function Add(FileName: string): TEditorDesktopFile;
    function Find(vName: string): TEditorDesktopFile;
    function IsExist(vName: string): Boolean;
    property Items[Index: integer]: TEditorDesktopFile read GetItems; default;
  published
    property CurrentFile: string read FCurrentFile write FCurrentFile;
  end;

  TEditorDesktop = class(TPersistent)
  private
    FFiles: TEditorDesktopFiles;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  published
    property Files: TEditorDesktopFiles read FFiles;
  end;

  TRunMode = (prunNone, prunConsole, prunUrl);

  TEditorProjectOptions = class(TPersistent)
  public
  end;

  { TEditorElement }

  TEditorElement = class(TPersistent)
  private
  protected
    FName: string;
    FTitle: string;
    FDescription: string;
    FImageIndex: integer;
    function GetDescription: string; virtual;
  public
    constructor Create; virtual;

    property Name: string read FName write FName;
    property Title: string read FTitle write FTitle;
    property Description: string read GetDescription write FDescription;
    property ImageIndex: integer read FImageIndex write FImageIndex;
  end;

  { TEditorElements }

  TEditorElements = class(TObjectList)
  private
    function GetItem(Index: integer): TEditorElement;
  public
    function Find(vName: string): TEditorElement;
    function IndexOf(vName: string): Integer;
    property Items[Index: integer]: TEditorElement read GetItem; default;
  end;

  {
    TEditorPerspective
    Run, Compile, Collect file groups and have special properties
  }

  { TEditorPerspective }

  TEditorPerspective = class(TEditorElement)
  private
    FGroups: TFileGroups;
    FDebug: TEditorDebugger;
  protected
    FOSDepended: Boolean;
    procedure AddGroup(vName, vCategory: string);
    function CreateDebugger: TEditorDebugger; virtual;
    function GetGroups: TFileGroups; virtual;
    procedure Init; virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;
    function FindExtension(vExtension: string): TFileGroup;
    function CreateEditorFile(vGroup: string): TEditorFile; virtual;
    function CreateEditorFile(vGroup: TFileGroup): TEditorFile; virtual;
    function CreateEditorProject: TEditorProject; virtual;
    function GetDefaultGroup: TFileGroup; virtual;
    //OSDepended: When save to file, the filename changed depend on the os system name
    property OSDepended: Boolean read FOSDepended;
    property Groups: TFileGroups read GetGroups;
    property Debug: TEditorDebugger read FDebug;//todo
  end;

  TEditorPerspectiveClass = class of TEditorPerspective;

  { TDefaultPerspective }
  {
    used only if no perspective defined
  }

  TDefaultPerspective = class(TEditorPerspective)
  protected
    procedure Init; override;
    function GetGroups: TFileGroups; override;
  public
    function GetDefaultGroup: TFileGroup; override;
  end;

  { TEditorSCM }

  TEditorSCM = class(TEditorElement)
  private
  protected
  public
    constructor Create; override;
    procedure CommitDirectory(Directory: string); virtual; abstract;
    procedure CommitFile(FileName: string); virtual; abstract;
    procedure UpdateDirectory(Directory: string); virtual; abstract;
    procedure UpdateFile(FileName: string); virtual; abstract;
    procedure RevertDirectory(Directory: string); virtual; abstract;
    procedure RevertFile(FileName: string); virtual; abstract;
    procedure DiffFile(FileName: string); virtual; abstract;
    procedure DiffToFile(FileName, ToFileName: string); virtual; abstract;
  end;

  TEditorSCMClass = class of TEditorSCM;

  { TEditorProject }

  TEditorProject = class(TmnXMLProfile)
  private
    FOptions: TEditorProjectOptions;
    FPerspectiveName: string;
    FRunMode: TRunMode;
    FDescription: string;
    FRootUrl: string;
    FRootDir: string;
    FFileName: string;
    FName: string;
    FSaveDesktop: Boolean;
    FDesktop: TEditorDesktop;
    FCachedIdentifiers: THashedStringList;
    FCachedVariables: THashedStringList;
    FCachedAge: DWORD;
    FPerspective: TEditorPerspective;
    FSCM: TEditorSCM;
    procedure SetPerspectiveName(AValue: string);
    procedure SetSCM(AValue: TEditorSCM);
  protected
    procedure Loaded(Failed: Boolean); override;
    procedure Saving; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property FileName: string read FFileName write FFileName;
    function Save: Boolean;
    function SaveAs: Boolean;
    procedure SetSCMClass(SCMClass: TEditorSCM);
    property CachedVariables: THashedStringList read FCachedVariables;
    property CachedIdentifiers: THashedStringList read FCachedIdentifiers;
    property CachedAge: Cardinal read FCachedAge write FCachedAge;
    //Perspective here point to one of Engine.Perspectives so it is not owned by project
    property Perspective: TEditorPerspective read FPerspective default nil;
  published
    property Name: string read FName write FName;
    property PerspectiveName: string read FPerspectiveName write SetPerspectiveName;
    //SCM now owned by project and saved or loaded with it, the SCM object so assigned to will be freed with the project
    property SCM: TEditorSCM read FSCM write SetSCM;

    property Description: string read FDescription write FDescription;
    property RootDir: string read FRootDir write FRootDir;
    property RootUrl: string read FRootUrl write FRootUrl;
    property RunMode: TRunMode read FRunMode write FRunMode default prunUrl;
    property SaveDesktop: Boolean read FSaveDesktop write FSaveDesktop default True;
    property Desktop: TEditorDesktop read FDesktop;
    property Options: TEditorProjectOptions read FOptions write FOptions default nil;
  end;

  { TDebugMarksPart }

  TSynDebugMarksPart = class(TSynGutterPartBase)
  protected
    FEditorFile: TEditorFile;
    procedure Init; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
  published
    property MarkupInfo;
  end;

  TEditorFileMode = (efmUnix, efmWindows, efmMac);

  { TEditorFile }

  TEditorFile = class(TCollectionItem)
  private
    FName: string;
    FSynEdit: TSynEdit;
    FEdited: Boolean;
    FFileAge: integer;
    FGroup: TFileGroup;
    FRelated: string;
    FMode: TEditorFileMode;
    procedure SetEdited(const Value: Boolean);
    procedure SetGroup(const Value: TFileGroup);
    function GetReadonly: Boolean;
    procedure SetReadonly(const Value: Boolean);
    function GetModeAsText: string;
    procedure SetMode(const Value: TEditorFileMode);
  protected
    procedure Edit;
    procedure DoEdit(Sender: TObject);
    procedure DoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure DoGutterClickEvent(Sender: TObject; X, Y, Line: integer; Mark: TSynEditMark);
    procedure DoSpecialLineMarkup(Sender: TObject; Line: integer; var Special: Boolean; Markup: TSynSelectedColor);
    procedure UpdateAge;
    function GetHighlighter: TSynCustomHighlighter; virtual;
    procedure NewSource; virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Load(FileName: string); virtual;
    procedure Save(FileName: string); virtual;
    procedure SaveFile(AsNewFile: Boolean = False); virtual;
    procedure Reload;
    procedure Show;
    procedure Close;
    procedure OpenInclude; virtual;
    function CanOpenInclude: Boolean; virtual;
    function CheckChanged: Boolean;
    //run the file or run the project depend on the project type (perspective)
    function Run: Boolean; virtual;
    property Mode: TEditorFileMode read FMode write SetMode default efmUnix;
    property ModeAsText: string read GetModeAsText;
    property Name: string read FName write FName;
    property Related: string read FRelated write FRelated;
    property SynEdit: TSynEdit read FSynEdit;
    property Edited: Boolean read FEdited write SetEdited;
    property ReadOnly: Boolean read GetReadonly write SetReadonly;
    property Group: TFileGroup read FGroup write SetGroup;
  published
  end;

  { TEditorFiles }

  TEditorFiles = class(TCollection)
  private
    FCheckChanged: Boolean;
    FCurrent: TEditorFile;
    function GetItems(Index: integer): TEditorFile;
    function GetCurrent: TEditorFile;
    procedure SetCurrent(const Value: TEditorFile);
    function InternalOpenFile(FileName: string; AppendToRecent: Boolean): TEditorFile;
  protected
    function SetActiveFile(FileName: string): TEditorFile;
  public
    destructor Destroy; override;
    function FindFile(const vFileName: string): TEditorFile;
    function IsExist(vName: string): Boolean;
    function LoadFile(vFileName: string; AppendToRecent: Boolean = True): TEditorFile;
    function ShowFile(vFileName: string): TEditorFile; overload; //open it without add to recent, for debuging
    function ShowFile(const FileName: string; Line: integer): TEditorFile; overload;
    function OpenFile(vFileName: string): TEditorFile;
    procedure SetCurrentIndex(Index: integer; vRefresh: Boolean);
    function New(vGroupName: string = ''): TEditorFile; overload;
    function New(Category, Name, Related: string; ReadOnly, Executable: Boolean): TEditorFile; overload;
    procedure Open;
    procedure Save;
    procedure SaveAll;
    procedure SaveAs;
    procedure Revert;
    procedure Refresh;
    procedure Next;
    procedure Prior;
    procedure Edited;
    procedure Replace;
    procedure Find;
    procedure FindNext;
    procedure CheckChanged;
    procedure CloseAll;
    function GetEditedCount: integer;
    property Current: TEditorFile read GetCurrent write SetCurrent;
    property Items[Index: integer]: TEditorFile read GetItems; default;
  published
  end;

  TSynBreakPointItem = class(TSynObjectListItem)
  public
    IsBreakPoint: Boolean;
  end;

  TShowFolderFiles = (sffRelated, sffKnown, sffAll);
  TEditorFileClass = class of TEditorFile;

  TOnEngineChanged = procedure of object;

  { TEditorOptions }

  TEditorOptions = class(TmnXMLProfile)
  private
    FFileName: string;
    FShowFolder: Boolean;
    FShowFolderFiles: TShowFolderFiles;
    FWindowMaxmized: Boolean;
    FBoundRect: TRect;
    FSearchHistory: TStringList;
    FProfile: TEditorProfile;
    FCompilerFolder: string;
    FRecentFiles: TStringList;
    FRecentProjects: TStringList;
    FProjects: TStringList;
    FShowMessages: Boolean;
    FCollectAutoComplete: Boolean;
    FCollectTimeout: DWORD;
    FReplaceHistory: TStringList;
    FSendOutputToNewFile: Boolean;
    FShowOutput: Boolean;
    FAutoStartDebugServer: Boolean;
    FOutputHeight: integer;
    FMessagesHeight: integer;
    FFoldersWidth: integer;
    FSearchFolderHistory: TStringList;
    FExtraExtensions: TStringList;
    procedure SetRecentFiles(const Value: TStringList);
    procedure SetRecentProjects(const Value: TStringList);
    procedure SetProjects(const Value: TStringList);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Apply;
    procedure Load(vFileName: string);
    procedure Save;
    procedure Show;
    property FileName: string read FFileName write FFileName;
    property BoundRect: TRect read FBoundRect write FBoundRect; //not saved yet
  published
    property ExtraExtensions: TStringList read FExtraExtensions write FExtraExtensions;
    property CollectAutoComplete: Boolean read FCollectAutoComplete write FCollectAutoComplete default False;
    property CollectTimeout: DWORD read FCollectTimeout write FCollectTimeout default 60;
    property ShowFolder: Boolean read FShowFolder write FShowFolder default True;
    property ShowFolderFiles: TShowFolderFiles read FShowFolderFiles write FShowFolderFiles default sffRelated;
    property ShowMessages: Boolean read FShowMessages write FShowMessages default False;
    property ShowOutput: Boolean read FShowOutput write FShowOutput default False;
    property OutputHeight: integer read FOutputHeight write FOutputHeight default 100;
    property MessagesHeight: integer read FMessagesHeight write FMessagesHeight default 100;
    property FoldersWidth: integer read FFoldersWidth write FFoldersWidth default 180;
    property SendOutputToNewFile: Boolean read FSendOutputToNewFile write FSendOutputToNewFile default False;
    property AutoStartDebugServer: Boolean read FAutoStartDebugServer write FAutoStartDebugServer default False;
    property WindowMaxmized: Boolean read FWindowMaxmized write FWindowMaxmized default False;
    property SearchHistory: TStringList read FSearchHistory;
    property ReplaceHistory: TStringList read FReplaceHistory;
    property SearchFolderHistory: TStringList read FSearchFolderHistory;
    property Profile: TEditorProfile read FProfile;
    property RecentFiles: TStringList read FRecentFiles write SetRecentFiles;
    property RecentProjects: TStringList read FRecentProjects write SetRecentProjects;
    property Projects: TStringList read FProjects write SetProjects;
  end;

  TEditorSessionOptions = class(TmnXMLProfile)
  private
    FDefaultPerspective: string;
    FDefaultSCM: string;
  public
  published
    property DefaultPerspective: string read FDefaultPerspective write FDefaultPerspective;
    property DefaultSCM: string read FDefaultSCM write FDefaultSCM;
  end;

  TFileCategoryKind = (fckPublish);
  TFileCategoryKinds = set of TFileCategoryKind;

  { TFileCategory }

  TFileCategory = class(TEditorElements)
  private
    FName: string;
    FEditorFileClass: TEditorFileClass;
    FHighlighter: TSynCustomHighlighter;
    FKind: TFileCategoryKinds;
    function GetItem(Index: Integer): TFileGroup;
  protected
    FCompletion: TSynCompletion;
    procedure OnExecuteCompletion(Sender: TObject); virtual;
    function CreateHighlighter: TSynCustomHighlighter; virtual;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Name: string read FName write FName;
    function Find(vName: string): TFileGroup;
    function CreateEditorFile(Files: TEditorFiles): TEditorFile; virtual;
    procedure EnumExtensions(vExtensions: TStringList);
    property EditorFileClass: TEditorFileClass read FEditorFileClass;
    property Highlighter: TSynCustomHighlighter read FHighlighter;
    property Completion: TSynCompletion read FCompletion;
    property Kind: TFileCategoryKinds read FKind;
    property Items[Index: Integer]: TFileGroup read GetItem; default;
  end;

  TFileCategoryClass = class of TFileCategory;

  TFileCategories = class(TObjectList)
  private
    function GetItem(Index: integer): TFileCategory;
    procedure SetItem(Index: integer; AObject: TFileCategory);
  public
    function Find(vName: string): TFileCategory;
    procedure Add(const Name: string; EditorFileClass: TEditorFileClass; CategoryClass: TFileCategoryClass; Kind: TFileCategoryKinds = []);
    property Items[Index: integer]: TFileCategory read GetItem write SetItem; default;
  end;

  TFileGroupKind = (
    fgkExecutable,//You can guess what is it :P
    fgkProject,//this can be the main file for project
    fgkMember,//a member of project php, inc are memver, c,h,cpp members, pas,pp, p , inc also members, ini,txt not member of any project
    fgkBrowsable//When open file show it in the extension list
  );
  TFileGroupKinds = set of TFileGroupKind;

  { TFileGroup }

  TFileGroup = class(TEditorElement)
  private
    FExtensions: TStringList;
    FKind: TFileGroupKinds;
    FCategory: TFileCategory;
    procedure SetCategory(AValue: TFileCategory);
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure EnumExtensions(vExtensions: TStringList);
    procedure EnumExtensions(vExtensions: TEditorElements);
    property Category: TFileCategory read FCategory write SetCategory;
    property Extensions: TStringList read FExtensions;
    property Kind: TFileGroupKinds read FKind write FKind;
  end;

  { TFileGroups }

  TFileGroups = class(TEditorElements)
  private
    function GetItem(Index: integer): TFileGroup;
  public
    function Find(vName: string): TFileGroup;
    function Find(vName, vCategory: string): TFileGroup;
    procedure EnumExtensions(vExtensions: TStringList);
    procedure EnumExtensions(vExtensions: TEditorElements);
    function FindExtension(vExtension: string): TFileGroup;
    function CreateFilter(vGroup: TFileGroup = nil): string;
    procedure Add(vGroup: TFileGroup);
    procedure Add(const Name, Title: string; Category: string; Extensions: array of string; Kind: TFileGroupKinds = []);
    property Items[Index: integer]: TFileGroup read GetItem; default;
  end;

  { TPerspectives }

  TPerspectives = class(TEditorElements)
  private
    function GetItem(Index: integer): TEditorPerspective;
  public
    function Find(vName: string): TEditorPerspective;
    procedure Add(vEditorPerspective: TEditorPerspectiveClass);
    procedure Add(vEditorPerspective: TEditorPerspective);
    property Items[Index: integer]: TEditorPerspective read GetItem; default;
  end;

  { TSourceManagements }

  TSourceManagements = class(TEditorElements)
  private
    function GetItem(Index: integer): TEditorSCM;
  public
    function Find(vName: string): TEditorSCM;
    procedure Add(vEditorSCM: TEditorSCMClass);
    property Items[Index: Integer]: TEditorSCM read GetItem; default;
  end;

  { TEditorFormItem }

  TEditorFormItem = class(TObject)
  private
    FObjectClass: TClass;
    FItemClass: TCustomFormClass;
  protected
  public
    property ObjectClass: TClass read FObjectClass;
    property ItemClass: TCustomFormClass read FItemClass;
  end;

  { TEditorFormList }

  TEditorFormList = class(TObjectList)
  private
    function GetItem(Index: integer): TEditorFormItem;
  public
    function Find(ObjectClass: TClass): TEditorFormItem;
    procedure Add(vObjectClass: TClass; vFormClass: TCustomFormClass);
    property Items[Index: integer]: TEditorFormItem read GetItem; default;
  end;

  {
    Session object to manage the current opened project, only one project can open.
  }

  { TEditorSession }

  TEditorSession = class(TObject)
  private
    FOptions: TEditorSessionOptions;
    FProject: TEditorProject;
    procedure SetProject(const Value: TEditorProject);
    function GetIsOpened: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(FileName: string);
    function New: TEditorProject;
    procedure Close;
    procedure Open;
    //Is project opened
    property IsOpened: Boolean read GetIsOpened;
    //Current is the opened project if it is nil it is mean not opened any project.
    property Project: TEditorProject read FProject write SetProject;
    //Session Options is depend on the system used not shared between OSs
    property Options: TEditorSessionOptions read FOptions;
  end;

  TEditorMessagesList = class;

  TEditorMessage = class(TObject)
  private
    FText: string;
  public
    property Text: string read FText write FText;
  end;

  TEditorMessages = class(TObjectList)
  private
    FName: string;
    function GetItem(Index: integer): TEditorMessage;
    procedure SetItem(Index: integer; const Value: TEditorMessage);
  public
    function GetText(Index: integer): string;
    property Name: string read FName write FName;
    property Items[Index: integer]: TEditorMessage read GetItem write SetItem; default;
  end;

  TEditorMessagesList = class(TObjectList)
  private
    function GetItem(Index: integer): TEditorMessages;
    procedure SetItem(Index: integer; const Value: TEditorMessages);
  public
    function Find(Name: string): TEditorMessages;
    function GetMessages(Name: string): TEditorMessages;
    property Items[Index: integer]: TEditorMessages read GetItem write SetItem; default;
  end;

  TOnFoundEvent = procedure(FileName: string; const Line: string; LineNo, Column, FoundLength: integer) of object;
  TOnEditorChangeState = procedure(State: TEditorChangeState) of object;

  { TEditorEngine }

  TEditorEngine = class(TObject)
  private
    //if the project not defined any perspective this is the default one
    FDefaultPerspective: TEditorPerspective;
    FDefaultSCM: TEditorSCM;
    //FInternalPerspective used only there is no any default Perspective defined, it is mean simple editor without any project type
    FInternalPerspective: TDefaultPerspective;
    FForms: TEditorFormList;
    FPerspectives: TPerspectives;
    FSourceManagements: TSourceManagements;
    FUpdateState: TEditorChangeState;
    FUpdateCount: integer;
    FFiles: TEditorFiles;
    FFilesControl: TWinControl;
    FOptions: TEditorOptions;
    FSearchEngine: TSynEditSearch;
    FCategories: TFileCategories;
    FGroups: TFileGroups;
    FExtenstion: string;
    FOnChangedState: TOnEditorChangeState;
    FSession: TEditorSession;
    FMessagesList: TEditorMessagesList;
    FBrowseFolder: string;
    //FMacroRecorder: TSynMacroRecorder;
    FWorkSpace: string;
    FOnReplaceText: TReplaceTextEvent;
    function GetPerspective: TEditorPerspective;
    function GetSCM: TEditorSCM;
    function GetRoot: string;
    function GetUpdating: Boolean;
    procedure SetBrowseFolder(const Value: string);
    function GetWorkSpace: string;
    procedure SetDefaultPerspective(AValue: TEditorPerspective);
    procedure SetDefaultSCM(AValue: TEditorSCM);
  protected
    property SearchEngine: TSynEditSearch read FSearchEngine;
    procedure DoChangedState(State: TEditorChangeState); virtual;
    procedure DoMacroStateChange(Sender: TObject);
    procedure DoReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //I used it for search in files
    function SearchReplace(const FileName: string; const ALines: TStringList; const ASearch, AReplace: string; OnFoundEvent: TOnFoundEvent; AOptions: TSynSearchOptions): integer;
    //Recent
    procedure ProcessRecentFile(const FileName: string);
    procedure RemoveRecentFile(const FileName: string);
    procedure ProcessRecentProject(const FileName: string);
    procedure RemoveRecentProject(const FileName: string);
    procedure ProcessProject(const FileName: string);
    procedure RemoveProject(const FileName: string);

    procedure LoadOptions;
    procedure SaveOptions;
    procedure Shutdown;

    procedure BeginUpdate;
    procedure UpdateState(State: TEditorChangeState);
    property Updating: Boolean read GetUpdating;
    procedure EndUpdate;

    function ExpandFileName(FileName: string): string;
    property Extenstion: string read FExtenstion write FExtenstion;
    property Root: string read GetRoot;
    property WorkSpace: string read GetWorkSpace write FWorkSpace;

    property Categories: TFileCategories read FCategories;
    property Groups: TFileGroups read FGroups;
    property Perspectives: TPerspectives read FPerspectives;
    property SourceManagements: TSourceManagements read FSourceManagements;
    property Forms: TEditorFormList read FForms;
    //
    property Files: TEditorFiles read FFiles;
    property Session: TEditorSession read FSession;
    property Options: TEditorOptions read FOptions;
    property MessagesList: TEditorMessagesList read FMessagesList;
    //FilesControl is a panel or any wincontrol that the editor SynEdit put on it
    property FilesControl: TWinControl read FFilesControl write FFilesControl;
    property BrowseFolder: string read FBrowseFolder write SetBrowseFolder;
    procedure SetDefaultPerspective(vName: string);
    procedure SetDefaultSCM(vName: string);
    property DefaultPerspective: TEditorPerspective read FDefaultPerspective write SetDefaultPerspective;
    property DefaultSCM: TEditorSCM read FDefaultSCM write SetDefaultSCM;
    property Perspective: TEditorPerspective read GetPerspective;
    property SCM: TEditorSCM read GetSCM;
    //property MacroRecorder: TSynMacroRecorder read FMacroRecorder;
    property OnChangedState: TOnEditorChangeState read FOnChangedState write FOnChangedState;
    property OnReplaceText: TReplaceTextEvent read FOnReplaceText write FOnReplaceText;
    //debugger
  published
  end;

function SelectFolder(const Caption: string; const Root: WideString; var Directory: string): Boolean;
procedure SpliteStr(S, Separator: string; var Name, Value: string);
procedure EnumFiles(Folder, Filter: string; FileList: TStringList);
procedure EnumFileList(const Root, Path, Files: string; Strings: TStringList; vMaxCount: integer; Recursive: Boolean);
procedure SaveAsUnix(Strings: TStrings; Stream: TStream);
procedure SaveAsWindows(Strings: TStrings; Stream: TStream);
procedure SaveAsMAC(Strings: TStrings; Stream: TStream);
procedure SaveAsMode(const FileName: string; Mode: TEditorFileMode; Strings: Tstrings);
function DetectFileMode(const Contents: string): TEditorFileMode;
function ChangeTabsToSpace(const Contents: string; TabWidth: integer): string;

function Engine: TEditorEngine;

const
{$ifdef WINDOWS}
  SysPlatform = 'WINDOWS';
{$else}
  SysPlatform = 'LINUX';
{$endif}

implementation

uses
  SynHighlighterApache, SynHighlighterXHTML, SynHighlighterHashEntries, SynGutterCodeFolding,
  Registry, SearchForms, SynEditTextBuffer,
  mneResources;

var
  FEngineShutdown: Boolean  = False;
  FEngine: TEditorEngine = nil;

function Engine: TEditorEngine;
begin
  if FEngineShutdown then
    raise Exception.Create('Engine in shutdown?');
  if FEngine = nil then
    FEngine := TEditorEngine.Create;
  Result := FEngine;
end;

function SelectFolder(const Caption: string; const Root: WideString; var Directory: string): Boolean;
begin
  Result := SelectDirectory(Caption, Root, Directory);
end;

procedure SpliteStr(S, Separator: string; var Name, Value: string);
var
  p: integer;
begin
  p := AnsiPos(Separator, S);
  if P <> 0 then
  begin
    Name := Copy(s, 1, p - 1);
    Value := Copy(s, p + 1, MaxInt);
  end
  else
  begin
    Name := s;
    Value := '';
  end;
end;

procedure SaveAsUnix(Strings: TStrings; Stream: TStream);
var
  i, l: integer;
  S: string;
begin
  l := Strings.Count - 1;
  for i := 0 to l do
  begin
    S := Strings[i];
    if i <> l then
      S := S + #$A;
    Stream.WriteBuffer(Pointer(S)^, Length(S));
  end;
end;

procedure SaveAsWindows(Strings: TStrings; Stream: TStream);
var
  i, l: integer;
  S: string;
begin
  l := Strings.Count - 1;
  for i := 0 to l do
  begin
    S := Strings[i];
    if i <> l then
      S := S + #$D#$A;
    Stream.WriteBuffer(Pointer(S)^, Length(S));
  end;
end;

procedure SaveAsMAC(Strings: TStrings; Stream: TStream);
var
  i, l: integer;
  S: string;
begin
  l := Strings.Count - 1;
  for i := 0 to l do
  begin
    S := Strings[i];
    if i <> l then
      S := S + #$D;
    Stream.WriteBuffer(Pointer(S)^, Length(S));
  end;
end;

{ TEditorSCM }

constructor TEditorSCM.Create;
begin
  inherited Create;
end;

{ TEditorElements }

function TEditorElements.GetItem(Index: integer): TEditorElement;
begin
  Result := inherited Items[Index] as TEditorElement;
end;

function TEditorElements.Find(vName: string): TEditorElement;
var
  i: integer;
begin
  Result := nil;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, vName) then
      begin
        Result := Items[i];
        break;
      end;
    end;
end;

function TEditorElements.IndexOf(vName: string): Integer;
var
  i: integer;
begin
  Result := -1;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, vName) then
      begin
        Result := i;
        break;
      end;
    end;
end;

{ TSourceManagements }

function TSourceManagements.GetItem(Index: integer): TEditorSCM;
begin
  Result := inherited Items[Index] as TEditorSCM;
end;

function TSourceManagements.Find(vName: string): TEditorSCM;
begin
  Result := inherited Find(vName) as TEditorSCM;
end;

procedure TSourceManagements.Add(vEditorSCM: TEditorSCMClass);
var
  aItem: TEditorSCM;
begin
  aItem := vEditorSCM.Create;
  inherited Add(aItem);
end;

{ TEditorElement }

function TEditorElement.GetDescription: string;
begin
  Result := FDescription;
end;

constructor TEditorElement.Create;
begin
  inherited Create;
  FImageIndex := -1;
end;

{ TDefaultPerspective }

procedure TDefaultPerspective.Init;
begin
  FTitle := 'Default';
  FName := 'Default';
  FDescription := 'Default project type';
end;

function TDefaultPerspective.GetGroups: TFileGroups;
begin
  Result := Engine.Groups;
end;

function TDefaultPerspective.GetDefaultGroup: TFileGroup;
begin
  Result := Groups.Find('txt');
end;

{ TEditorFormList }

function TEditorFormList.GetItem(Index: integer): TEditorFormItem;
begin
  Result := inherited Items[Index] as TEditorFormItem;
end;

function TEditorFormList.Find(ObjectClass: TClass): TEditorFormItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if ObjectClass.InheritsFrom(Items[i].ObjectClass) then
    begin
      Result := Items[i] as TEditorFormItem;
      break;
    end;
  end;
end;

procedure TEditorFormList.Add(vObjectClass: TClass; vFormClass: TCustomFormClass);
var
  aItem: TEditorFormItem;
begin
  aItem := TEditorFormItem.Create;
  aItem.FObjectClass := vObjectClass;
  aItem.FItemClass := vFormClass;
  inherited Add(aItem);
end;

{ TEditorPerspective }

function TEditorPerspective.GetGroups: TFileGroups;
begin
  Result := FGroups;
end;

procedure TEditorPerspective.AddGroup(vName, vCategory: string);
var
  G: TFileGroup;
  C: TFileCategory;
begin
  if vCategory = '' then
    C := nil
  else
    C := Engine.Categories.Find(vName);
  if C = nil then
    G := Engine.Groups.Find(vName)
  else
    G := C.Find(vName);

  if G = nil then
    raise Exception.Create(vName + ' file group not found');
  Groups.Add(G);
end;

function TEditorPerspective.CreateDebugger: TEditorDebugger;
begin
  Result := nil;
end;

constructor TEditorPerspective.Create;
begin
  inherited;
  FGroups := TFileGroups.Create(False);//it already owned by Engine.Groups
  FDebug := CreateDebugger;
  Init;
{  if Groups.Count = 0 then
    raise Exception.Create('You must add groups in Init method');}//removed DefaultPerspective has no groups
end;

destructor TEditorPerspective.Destroy;
begin
  FreeAndNil(FDebug);
  FreeAndNil(FGroups);
  inherited;
end;

function TEditorPerspective.FindExtension(vExtension: string): TFileGroup;
begin
  if LeftStr(vExtension, 1) = '.' then
    vExtension := Copy(vExtension, 2, MaxInt);
  Result := Groups.FindExtension(vExtension);
  if Result = nil then
    Result := Engine.Groups.FindExtension(vExtension)
end;

function TEditorPerspective.CreateEditorFile(vGroup: string): TEditorFile;
var
  G: TFileGroup;
begin
  G := Groups.Find(vGroup);
  if G = nil then
    G := Engine.Groups.Find(vGroup);
  Result := CreateEditorFile(G);
end;

function TEditorPerspective.CreateEditorFile(vGroup: TFileGroup): TEditorFile;
begin
  if vGroup <> nil then
    Result := vGroup.Category.CreateEditorFile(Engine.Files)
  else
    Result := TEditorFile.Create(Engine.Files);
  Result.Group := vGroup;
end;

function TEditorPerspective.CreateEditorProject: TEditorProject;
begin
  Result := TEditorProject.Create;
  Result.PerspectiveName := Name;
end;

function TEditorPerspective.GetDefaultGroup: TFileGroup;
begin
  if Groups.Count > 0 then
    Result := Groups[0]
  else
    Result := Engine.Groups[0];//first group in all groups, naah
end;

{ TPerspectives }

function TPerspectives.GetItem(Index: integer): TEditorPerspective;
begin
  Result := inherited Items[Index] as TEditorPerspective;
end;

function TPerspectives.Find(vName: string): TEditorPerspective;
begin
  Result := inherited Find(vName) as TEditorPerspective;
end;

procedure TPerspectives.Add(vEditorPerspective: TEditorPerspectiveClass);
var
  aItem: TEditorPerspective;
begin
  aItem := vEditorPerspective.Create;
  Add(aItem);
end;

procedure TPerspectives.Add(vEditorPerspective: TEditorPerspective);
begin
  inherited Add(vEditorPerspective);
end;

{ TSynDebugMarksPart }

procedure TSynDebugMarksPart.Init;
begin
  inherited;
end;

constructor TSynDebugMarksPart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //  FMouseActions := TSynEditMouseActionsLineNum.Create(self);
  //  FMouseActions.ResetDefaults;
end;

destructor TSynDebugMarksPart.Destroy;
begin
  FreeAndNil(FMouseActions);
  inherited;
end;

function TEditorEngine.SearchReplace(const FileName: string; const ALines: TStringList; const ASearch, AReplace: string; OnFoundEvent: TOnFoundEvent; AOptions: TSynSearchOptions): integer;
var
  i: integer;
  nSearchLen, nReplaceLen, n, nChar: integer;
  nInLine: integer;
  iResultOffset: integer;
  aLine, aReplaceText: string;
  Replaced: Boolean;
begin
  if not Assigned(SearchEngine) then
  begin
    raise ESynEditError.Create('No search engine has been assigned');
  end;

  Result := 0;
  // can't search for or replace an empty string
  if Length(ASearch) = 0 then
    exit;

  i := 0;
  // initialize the search engine
  //SearchEngine.Options := AOptions;
  SearchEngine.Pattern := ASearch;
  // search while the current search position is inside of the search range
  try
    while i < ALines.Count do
    begin
      aLine := ALines[i];
      nInLine := SearchEngine.FindAll(aLine);
      iResultOffset := 0;
      n := 0;
      // Operate on all results in this line.
      Replaced := False;
      while nInLine > 0 do
      begin
        // An occurrence may have been replaced with a text of different length
        nChar := SearchEngine.Results[n] + iResultOffset;
        nSearchLen := SearchEngine.ResultLengths[n];
        Inc(n);
        Dec(nInLine);

        Inc(Result);
        OnFoundEvent(FileName, aLine, i + 1, nChar, nSearchLen);

        if (ssoReplace in AOptions) then
        begin
          //aReplaceText := SearchEngine.Replace(ASearch, AReplace);//need to review
          nReplaceLen := Length(aReplaceText);
          aLine := Copy(aLine, 1, nChar - 1) + aReplaceText + Copy(aLine, nChar + nSearchLen, MaxInt);
          if (nSearchLen <> nReplaceLen) then
          begin
            Inc(iResultOffset, nReplaceLen - nSearchLen);
          end;
          Replaced := True;
        end;
      end;
      if Replaced then
        ALines[i] := aLine;
      // search next / previous line
      Inc(i);
    end;
  finally
  end;
end;

{ TEditorEngine }

procedure TEditorOptions.Apply;
var
  i: integer;
begin
  for i := 0 to Engine.Categories.Count - 1 do
  begin
    //check if Engine.Categories[i].Completion = nil
    //Engine.Categories[i].Completion.Font := Profile.Font;
    //Engine.Categories[i].Completion.Options := Engine.Categories[i].Completion.Options + [scoTitleIsCentered];
    if Engine.Categories[i].Highlighter <> nil then
    begin
      Profile.Highlighters.AssignTo(Engine.Categories[i].Highlighter);
    end;
  end;

  for i := 0 to Engine.Files.Count - 1 do
  begin
    Profile.AssignTo(Engine.Files[i].SynEdit);
  end;
end;

procedure TEditorEngine.BeginUpdate;
begin
  if FUpdateCount = 0 then
  begin
    FUpdateState := [];
  end;
  Inc(FUpdateCount);
end;

procedure TEditorFiles.CheckChanged;
var
  i: integer;
  b: Boolean;
begin
  if not FCheckChanged then
  begin
    Engine.BeginUpdate;
    FCheckChanged := True;
    b := True;
    try
      for i := 0 to Count - 1 do
      begin
        if not b then
          Items[i].UpdateAge
        else
          b := Items[i].CheckChanged;
      end;
    finally
      FCheckChanged := False;
      Engine.EndUpdate;
    end;
  end;
end;

procedure TEditorFiles.CloseAll;
begin
  Engine.BeginUpdate;
  try
    while Engine.Files.Count > 0 do
      Engine.Files[0].Close;
  finally
    Engine.EndUpdate;
  end;
end;

procedure TEditorSession.Close;
begin
  FProject := nil;
  Engine.UpdateState([ecsChanged, ecsState, ecsRefresh, ecsProject]);
end;

constructor TEditorEngine.Create;
begin
  inherited;
  FMessagesList := TEditorMessagesList.Create;
  //FMacroRecorder := TSynMacroRecorder.Create(nil);
  //FMacroRecorder.OnStateChange := DoMacroStateChange;
  FInternalPerspective := TDefaultPerspective.Create;
  FForms := TEditorFormList.Create(True);
  FOptions := TEditorOptions.Create;
  FCategories := TFileCategories.Create(True);
  FGroups := TFileGroups.Create(True);
  FPerspectives := TPerspectives.Create(True);
  FSourceManagements := TSourceManagements.Create(True);
  FSearchEngine := TSynEditSearch.Create;
  FFiles := TEditorFiles.Create(TEditorFile);
  FSession := TEditorSession.Create;
  Extenstion := 'mne-project';
  Perspectives.Add(FInternalPerspective);
end;

destructor TEditorEngine.Destroy;
begin
  if not FEngineShutdown then
    Shutdown;
  FreeAndNil(FFiles);
  FreeAndNil(FSession);
  FreeAndNil(FCategories);
  FreeAndNil(FGroups);
  FreeAndNil(FPerspectives);
  FreeAndNil(FSearchEngine);
  FreeAndNil(FOptions);
  //FreeAndNil(FMacroRecorder);
  FreeAndNil(FMessagesList);
  FOnChangedState := nil;
  FInternalPerspective := nil;
//  FreeAndNil(FInternalPerspective);
  FreeAndNil(FForms);
  inherited;
end;

procedure EnumFiles(Folder, Filter: string; FileList: TStringList);
var
  R: integer;
  SearchRec: TSearchRec;
begin
  Folder := IncludeTrailingPathDelimiter(Folder);
  R := FindFirst(Folder + Filter, faAnyFile, SearchRec);
  while R = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      FileList.Add(SearchRec.Name);
    end;
    R := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

procedure EnumFileList(const Root, Path, Files: string; Strings: TStringList; vMaxCount: integer; Recursive: Boolean);
var
  sr: TSearchRec;

  function FullPath: string;
  begin
    if Root <> '' then
      Result := IncludeTrailingPathDelimiter(Root);
    if Path <> '' then
      Result := IncludeTrailingPathDelimiter(Result + Path);
  end;

begin
  if FindFirst(FullPath + Files, faAnyFile, sr) = 0 then
  begin
    repeat
      Strings.Add(IncludeTrailingPathDelimiter(Path) + sr.Name);
      if Strings.Count > vMaxCount then
        raise Exception.Create('Too many files');
    until (FindNext(sr) <> 0);
  end;

  if Recursive then
    if FindFirst(FullPath + '*.*', faDirectory, sr) = 0 then
    begin
      repeat
        if (sr.Name = '') or (sr.Name[1] = '.') or (sr.Name = '..') or (copy(sr.Name, 1, 5) = '_vti_') or SameText(sr.Name, '.svn') or SameText(sr.Name, '_svn') then
          continue;
        if (sr.Attr and faDirectory) <> 0 then
          EnumFileList(Root, IncludeTrailingPathDelimiter(Path) + sr.Name, Files, Strings, vMaxCount, Recursive)
      until (FindNext(sr) <> 0);
    end;
end;

procedure TEditorFiles.Edited;
begin
  Engine.UpdateState([ecsEdit]);
end;

procedure TEditorEngine.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    if Files.Current <> nil then
      Files.Current.Show;
    if FUpdateState <> [] then
      DoChangedState(FUpdateState);
    FUpdateState := [];
  end;
end;

procedure TEditorFiles.Find;
begin
  if Current <> nil then
    ShowSearchForm(Current.SynEdit, Engine.Options.SearchHistory, Engine.Options.ReplaceHistory, False);
end;

procedure TEditorEngine.DoReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
begin
  if Assigned(FOnReplaceText) then
    FOnReplaceText(Sender, ASearch, AReplace, Line, Column, ReplaceAction);
end;

function TEditorFiles.FindFile(const vFileName: string): TEditorFile;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameFileName(vFileName, Items[i].Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

procedure TEditorFiles.FindNext;
begin
  if Current <> nil then
    NextSearchText(Current.SynEdit);
end;

function TEditorFiles.GetCurrent: TEditorFile;
begin
  Result := FCurrent;
end;

function TEditorFiles.GetEditedCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Edited then
      Result := Result + 1;
  end;
end;

function TEditorEngine.GetRoot: string;
var
  s: string;
begin
  s := ExtractFilePath(Application.ExeName);
  if Session.Project.RootDir = '' then
    Result := s
  else
    Result := ExpandToPath(Session.Project.RootDir, s);
end;

function TEditorEngine.GetSCM: TEditorSCM;
begin
  if (Session <> nil) and (Session.Project <> nil) and (Session.Project.SCM <> nil) then
    Result := Session.Project.SCM
  else if DefaultSCM <> nil then
    Result := DefaultSCM
  else
    Result := nil;
end;

function TEditorEngine.GetPerspective: TEditorPerspective;
begin
  if (Session <> nil) and (Session.Project <> nil) and (Session.Project.Perspective <> nil) then
    Result := Session.Project.Perspective
  else if DefaultPerspective <> nil then
    Result := FDefaultPerspective
  else
    Result := FInternalPerspective;
end;

function TEditorFiles.InternalOpenFile(FileName: string; AppendToRecent: Boolean): TEditorFile;
var
  lFileName: string;
begin
  {$ifdef windows}
  lFileName := ExpandFileName(FileName);
  {$else}
  if ExtractFilePath(FileName) = '' then
    lFileName := IncludeTrailingPathDelimiter(SysUtils.GetCurrentDir()) + FileName
  else lFileName := FileName;
  {$endif}
  Result := FindFile(lFileName);
  if Result = nil then
  begin
    Result := Engine.Perspective.CreateEditorFile(Engine.Perspective.FindExtension(ExtractFileExt(lFileName)));
    Result.Load(lFileName);
  end;
  if AppendToRecent then
    Engine.ProcessRecentFile(lFileName);
end;

procedure TEditorOptions.Load(vFileName: string);
begin
  SafeLoadFromFile(vFileName);
  FileName := vFileName;
  Apply;
end;

procedure TEditorSession.Load(FileName: string);
var
  aProject: TEditorProject;
begin
  Engine.BeginUpdate;
  try
    Close; //must free before load project for save the desktop and sure to save its files
    aProject := New;
    try
      aProject.LoadFromFile(FileName);
      aProject.FileName := FileName;
    except
      aProject.Free;
      raise;
    end;
    Project := aProject;
    Engine.ProcessRecentProject(FileName);
    Engine.UpdateState([ecsChanged, ecsState, ecsRefresh, ecsProject]);
  finally
    Engine.EndUpdate;
  end;
end;

function TEditorFiles.New(vGroupName: string): TEditorFile;
var
  aGroup: TFileGroup;
begin
  BeginUpdate;
  try
    if vGroupName = '' then
      aGroup := Engine.Perspective.GetDefaultGroup
    else
      aGroup := Engine.Groups.Find(vGroupName);
    Result := Engine.Perspective.CreateEditorFile(aGroup);
    Result.NewSource;
    Result.Edit;
    Current := Result;
    Engine.UpdateState([ecsChanged, ecsState, ecsRefresh]);
  finally
    EndUpdate;
  end;
end;

function TEditorFiles.New(Category, Name, Related: string; ReadOnly, Executable: Boolean): TEditorFile;
begin
  Result := Engine.Perspective.CreateEditorFile(Category);
  Result.ReadOnly := ReadOnly;
  Result.Name := Name;
  Result.Related := Related;
  Current := Result;
  Engine.UpdateState([ecsChanged, ecsState, ecsRefresh]);
end;

function TEditorSession.New: TEditorProject;
begin
  Result := Engine.Perspective.CreateEditorProject;
end;

procedure TEditorFiles.Next;
var
  i: integer;
begin
  if Current <> nil then
  begin
    i := Current.Index + 1;
    if i >= Count then
      i := 0;
    SetCurrentIndex(i, True);
  end;
end;

procedure TEditorFiles.Open;
var
  i: integer;
  aFile: TEditorFile;
  aDialog: TOpenDialog;
begin
  aDialog := TOpenDialog.Create(nil);
  try
    aDialog.Title := 'Open file';
    aDialog.Options := aDialog.Options + [ofAllowMultiSelect];
    aDialog.Filter := Engine.Groups.CreateFilter;
    aDialog.InitialDir := Engine.BrowseFolder;
    aDialog.DefaultExt := Engine.Perspective.GetDefaultGroup.Extensions[0];
    aDialog.FileName := '*' + aDialog.DefaultExt;
    if aDialog.Execute then
    begin
      Engine.BeginUpdate;
      try
        aFile := nil;
        for i := 0 to aDialog.Files.Count - 1 do
        begin
          aFile := InternalOpenFile(aDialog.Files[i], True);
        end;
        if aFile <> nil then
          Current := aFile;
        Engine.UpdateState([ecsChanged, ecsState, ecsRefresh]);
      finally
        Engine.EndUpdate;
      end;
    end;
  finally
    aDialog.Free;
  end;
end;

function TEditorFiles.OpenFile(vFileName: string): TEditorFile;
begin
  if SameText(ExtractFileExt(vFileName), '.' + Engine.Extenstion) then
  begin
    Engine.Session.Load(vFileName);
    Result := nil; //it is a project not a file.
  end
  else
  begin
    Result := LoadFile(vFileName);
  end;
end;

procedure TEditorSession.Open;
var
  aDialog: TOpenDialog;
begin
  aDialog := TOpenDialog.Create(nil);
  try
    aDialog.Title := 'Open project';
    aDialog.DefaultExt := Engine.Extenstion;
    aDialog.Filter := 'Project files (*.' + Engine.Extenstion + ')|*.' + Engine.Extenstion + '|All files|*.*';
    aDialog.InitialDir := Engine.BrowseFolder;
    aDialog.FileName := '*' + aDialog.DefaultExt;
    if aDialog.Execute then
    begin
      Load(aDialog.FileName);
    end;
  finally
    aDialog.Free;
  end;
end;

procedure TEditorFiles.Prior;
var
  i: integer;
begin
  if Current <> nil then
  begin
    i := Current.Index - 1;
    if i < 0 then
      i := Count - 1;
    SetCurrentIndex(i, True);
  end;
end;

procedure TEditorEngine.ProcessRecentFile(const FileName: string);
var
  i: integer;
begin
  i := Options.RecentFiles.IndexOf(FileName);
  if i >= 0 then
    Options.RecentFiles.Move(i, 0)
  else
    Options.RecentFiles.Insert(0, FileName);
  while Options.RecentFiles.Count > 50 do
    Options.RecentFiles.Delete(50);
end;

procedure TEditorEngine.ProcessRecentProject(const FileName: string);
var
  i: integer;
begin
  i := Options.RecentProjects.IndexOf(FileName);
  if i >= 0 then
    Options.RecentProjects.Move(i, 0)
  else
    Options.RecentProjects.Insert(0, FileName);
  while Options.RecentProjects.Count > 50 do
    Options.RecentProjects.Delete(50);
end;

procedure TEditorEngine.ProcessProject(const FileName: string);
var
  i: integer;
begin
  i := Options.Projects.IndexOf(FileName);
  if i >= 0 then
    Options.Projects.Move(i, 0)
  else
    Options.Projects.Insert(0, FileName);
end;

procedure TEditorFiles.Save;
begin
  if Current <> nil then
    Current.SaveFile;
end;

procedure TEditorFiles.SaveAll;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].SaveFile;
  end;
end;

procedure TEditorFiles.SaveAs;
begin
  if Current <> nil then
    Current.SaveFile(True);
end;

procedure TEditorOptions.Save;
begin
  if (FileName <> '') and DirectoryExists(ExtractFilePath(FileName)) then
  begin
    SaveToFile(FileName);
    Engine.UpdateState([ecsFolder]);
  end;
end;

procedure TEditorFiles.SetCurrent(const Value: TEditorFile);
begin
  if FCurrent <> Value then
  begin
    FCurrent := Value;
    if not Engine.Updating then
      FCurrent.Show;
  end;
end;

procedure TEditorFiles.SetCurrentIndex(Index: integer; vRefresh: Boolean);
var
  aCurrent: TEditorFile;
begin
  if Count <> 0 then
  begin
    if Index >= Count then
      Index := Count - 1;
    aCurrent := Items[Index];
    if aCurrent <> nil then
    begin
      Current := aCurrent;
    end;
  end;
  if vRefresh then
    Engine.UpdateState([ecsState, ecsRefresh]);
end;

procedure TEditorSession.SetProject(const Value: TEditorProject);
begin
  if FProject <> Value then
  begin
    if FProject <> nil then
      FreeAndNil(FProject);
    FProject := Value;
    Engine.UpdateState([ecsChanged, ecsState, ecsRefresh]);
  end;
end;

procedure TEditorOptions.Show;
var
  i: integer;
  aList: TList;
begin
  with TEditorOptionsForm.Create(Application) do
  begin
    aList := TSynHighlighterList.Create;
    try
      for i := 0 to Engine.Categories.Count - 1 do
      begin
        if Engine.Categories[i].Highlighter <> nil then
          aList.Add(Engine.Categories[i].Highlighter.ClassType);
      end;
      if Execute(Profile, aList) then
      begin
        Apply;
      end;
    finally
      aList.Free;
    end;
    Free;
  end;
end;

procedure TEditorEngine.RemoveProject(const FileName: string);
var
  i: integer;
begin
  i := Options.Projects.IndexOf(FileName);
  if i >= 0 then
    Options.Projects.Delete(i);
end;

procedure TEditorEngine.LoadOptions;
var
  aFile: string;
  i: Integer;
begin
  BeginUpdate;
  try
    Options.Load(Workspace + 'mne-options.xml');
    Session.Options.SafeLoadFromFile(LowerCase(Workspace + 'mne-' + SysPlatform + '-options.xml'));
    for i := 0 to Perspectives.Count - 1 do
    begin
      if Perspectives[i].OSDepended then
        aFile := LowerCase(Workspace + 'mne-' + SysPlatform + '-' + Perspectives[i].Name + '.xml')
      else
        aFile := LowerCase(Workspace + 'mne-' + Perspectives[i].Name + '.xml');
      if FileExists(aFile) then
        XMLReadObjectFile(Perspectives[i], aFile);
    end;
    SetDefaultPerspective(Session.Options.DefaultPerspective);
    SetDefaultSCM(Session.Options.DefaultSCM);
  finally
    EndUpdate;
  end;
end;

procedure TEditorEngine.SaveOptions;
var
  aFile: string;
  i: integer;
begin
  Options.Save;
  Session.Options.SaveToFile(LowerCase(Workspace + 'mne-' + SysPlatform + '-options.xml'));
  for i := 0 to Perspectives.Count - 1 do
  begin
    if Perspectives[i].OSDepended then
      aFile := LowerCase(Workspace + 'mne-' + SysPlatform + '-' + Perspectives[i].Name + '.xml')
    else
      aFile := LowerCase(Workspace + 'mne-' + Perspectives[i].Name + '.xml');
    if FileExists(aFile) then
      XMLWriteObjectFile(Perspectives[i], aFile);
  end;
end;

procedure TEditorEngine.Shutdown;
begin
  FEngineShutdown := True;
  if Perspective.Debug <> nil then
    Perspective.Debug.Stop;
  Files.Clear;
end;

procedure TEditorEngine.RemoveRecentProject(const FileName: string);
var
  i: integer;
begin
  i := Options.RecentProjects.IndexOf(FileName);
  if i >= 0 then
    Options.RecentProjects.Delete(i);
end;

procedure TEditorEngine.RemoveRecentFile(const FileName: string);
var
  i: integer;
begin
  i := Options.RecentFiles.IndexOf(FileName);
  Options.RecentFiles.Delete(i);
end;

function TEditorEngine.GetUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TEditorEngine.ExpandFileName(FileName: string): string;
begin
  if Session.Project <> nil then
    Result := ExpandToPath(FileName, Session.Project.RootDir)
  else if Files.Current <> nil then
    Result := ExpandToPath(FileName, ExtractFilePath(Files.Current.Name))
  else
    Result := FileName;
end;

procedure TEditorEngine.SetDefaultPerspective(vName: string);
var
  P: TEditorPerspective;
begin
  P := Perspectives.Find(vName);
  if P = nil then
    P := FInternalPerspective;
  DefaultPerspective := P;
end;

procedure TEditorEngine.SetDefaultSCM(vName: string);
begin
  DefaultSCM := SourceManagements.Find(vName);
end;

procedure TEditorEngine.DoChangedState(State: TEditorChangeState);
begin
  if Assigned(FOnChangedState) then
    FOnChangedState(State);
end;

procedure TEditorEngine.UpdateState(State: TEditorChangeState);
begin
  if Updating then
    FUpdateState := FUpdateState + State
  else
    DoChangedState(State);
end;

function TEditorFiles.LoadFile(vFileName: string; AppendToRecent: Boolean): TEditorFile;
begin
  Result := InternalOpenFile(vFileName, AppendToRecent);
  Engine.UpdateState([ecsChanged]);
  if Result <> nil then
    Current := Result;
  Engine.UpdateState([ecsState, ecsRefresh]);
end;

procedure TEditorFiles.Replace;
begin
  if Current <> nil then
    ShowSearchForm(Current.SynEdit, Engine.Options.SearchHistory, Engine.Options.ReplaceHistory, True);
end;

procedure TEditorFiles.Revert;
begin
  if Current <> nil then
  begin
    if Application.MessageBox(PChar('Revert file ' + Current.Name), 'Save', MB_YESNO) = mrYes then
      Current.Load(Current.Name);
  end;
end;

procedure TEditorEngine.SetBrowseFolder(const Value: string);
begin
  FBrowseFolder := Value;
  if FBrowseFolder <> '' then
    FBrowseFolder := IncludeTrailingPathDelimiter(FBrowseFolder);
end;

procedure TEditorEngine.DoMacroStateChange(Sender: TObject);
begin
  UpdateState([ecsState]);
end;

function TEditorEngine.GetWorkSpace: string;
begin
  Result := IncludeTrailingPathDelimiter(FWorkSpace);
end;

procedure TEditorEngine.SetDefaultPerspective(AValue: TEditorPerspective);
begin
  if FDefaultPerspective = AValue then
    exit;
  FDefaultPerspective := AValue;
  if FDefaultPerspective <> nil then
    Session.Options.DefaultPerspective := FDefaultPerspective.Name;
  Engine.UpdateState([ecsChanged, ecsProject]);
end;

procedure TEditorEngine.SetDefaultSCM(AValue: TEditorSCM);
begin
  if FDefaultSCM =AValue then
    exit;
  FDefaultSCM :=AValue;
  if FDefaultSCM <> nil then
    Session.Options.DefaultSCM := FDefaultSCM.Name;
  Engine.UpdateState([ecsChanged, ecsProject]);
end;

{ TEditorFiles }

function TEditorFiles.GetItems(Index: integer): TEditorFile;
begin
  Result := inherited Items[Index] as TEditorFile;
end;

function TEditorFiles.IsExist(vName: string): Boolean;
begin
  Result := FindFile(vName) <> nil;
end;

function TEditorFiles.SetActiveFile(FileName: string): TEditorFile;
begin
  Result := FindFile(FileName);
  if Result <> nil then
    Current := Result;
end;

destructor TEditorFiles.Destroy;
begin
  inherited;
end;

function TEditorFiles.ShowFile(vFileName: string): TEditorFile;
begin
  Result := InternalOpenFile(vFileName, False);
  Engine.UpdateState([ecsChanged]);
  if Result <> nil then
    Current := Result;
  Engine.UpdateState([ecsState, ecsRefresh]);
end;

procedure TEditorFiles.Refresh;
begin
  if Current <> nil then
  begin
    Current.SynEdit.Refresh;
  end;
end;

function TEditorFiles.ShowFile(const FileName: string; Line: integer): TEditorFile;
begin
  Result := InternalOpenFile(FileName, False);
  Result.SynEdit.CaretY := Line;
  Result.SynEdit.CaretX := 1;
  Engine.UpdateState([ecsChanged]);
  if Result <> nil then
    Current := Result;
  Engine.UpdateState([ecsState, ecsRefresh]);
end;

{ TEditorFile }

procedure TEditorFile.Edit;
begin
  if not SynEdit.ReadOnly then
    Edited := True;
end;

procedure TEditorFile.Close;
var
  aParent: TEditorEngine;
  i: integer;
  mr: integer;
begin
  if Edited then
  begin
    mr := Application.MessageBox(PChar('Save file ' + Name + ' before close?'), 'Save', MB_YESNOCANCEL);
    if mr = mrCancel then
      Abort
    else if mr = mrYes then
      SaveFile;
  end;

  i := Index;
  aParent := Engine;
  if aParent.Files.FCurrent = self then
    aParent.Files.FCurrent := nil;
  Free;
  aParent.Files.SetCurrentIndex(i, False);
  aParent.UpdateState([ecsChanged, ecsState, ecsRefresh]);
end;

procedure TEditorFile.OpenInclude;
begin

end;

function TEditorFile.CanOpenInclude: Boolean;
begin
  Result := False;
end;

constructor TEditorFile.Create(ACollection: TCollection);
var
  cf: TSynGutterCodeFolding;
begin
  inherited;
  { There is more assigns in SetGroup }
  FSynEdit := TSynEdit.Create(Engine.FilesControl);
  FSynEdit.OnChange := @DoEdit;
  FSynEdit.OnStatusChange := @DoStatusChange;
  FSynEdit.OnGutterClick := @DoGutterClickEvent;
  FSynEdit.OnSpecialLineMarkup := @DoSpecialLineMarkup;
  FSynEdit.BookMarkOptions.BookmarkImages := EditorResource.BookmarkImages;
  FSynEdit.OnReplaceText := @Engine.DoReplaceText;
  //  FSynEdit.Gutter.MarksPart(0).DebugMarksImageIndex := 0;
  //FSynEdit.Gutter.MarksPart.DebugMarksImageIndex := 0;
  //FSynEdit.Gutter.Parts.Add(TSynBreakPointItem.Create(FSynEdit.Gutter.Parts));
  cf := FSynEdit.Gutter.Parts.ByClass[TSynGutterCodeFolding, 0] as TSynGutterCodeFolding;
  if cf <> nil then
  begin
    cf.Visible := False; //I hate code folding
  end;
  FSynEdit.TrimSpaceType := settLeaveLine;
  FSynEdit.BoundsRect := Engine.FilesControl.ClientRect;
  FSynEdit.BorderStyle := bsNone;
  FSynEdit.ShowHint := True;
  FSynEdit.Visible := False;
  FSynEdit.Align := alClient;
  FSynEdit.Realign;
  FSynEdit.WantTabs := True;
  FSynEdit.Parent := Engine.FilesControl;
end;

destructor TEditorFile.Destroy;
begin
  FSynEdit.Free;
  inherited;
end;

procedure TEditorFile.DoEdit(Sender: TObject);
begin
  Edit;
  Engine.Files.Edited;
end;

procedure TEditorFile.Load(FileName: string);
var
  Contents: string;
  Size: integer;
  Stream: TFileStream;
begin
  FileName := ExpandFileName(FileName);
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  SynEdit.BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(Contents, nil, Size);
    Stream.Read(Pointer(Contents)^, Size);
    Mode := DetectFileMode(Contents);
    if eoTabsToSpaces in SynEdit.Options then
    begin
      Contents := ChangeTabsToSpace(Contents, SynEdit.TabWidth);
    end;
    SynEdit.Lines.Text := Contents; //lll
    Name := FileName;
    Edited := False;
    UpdateAge;
  finally
    SynEdit.EndUpdate;
    Stream.Free;
  end;
end;

procedure SaveAsMode(const FileName: string; Mode: TEditorFileMode; Strings: TStrings);
var
  aStream: TFileStream;
begin
  aStream := TFileStream.Create(FileName, fmCreate);
  try
    case Mode of
      efmWindows: SaveAsWindows(Strings, aStream);
      efmMac: SaveAsMac(Strings, aStream);
      else
        SaveAsUnix(Strings, aStream);
    end;
  finally
    aStream.Free;
  end;
end;

procedure TEditorFile.Save(FileName: string);
begin
  SaveAsMode(FileName, Mode, SynEdit.Lines);
  Name := FileName;
  Edited := False;
  Engine.UpdateState([ecsFolder]);
  UpdateAge;
end;

procedure TEditorFile.SetEdited(const Value: Boolean);
begin
  FEdited := Value;
end;

procedure TEditorFile.Show;
begin
  SynEdit.Visible := True;
  SynEdit.Show;
  SynEdit.BringToFront;
  (Engine.FilesControl.Owner as TCustomForm).ActiveControl := SynEdit;
end;

procedure TEditorFile.SaveFile(AsNewFile: Boolean);
var
  aDialog: TSaveDialog;
  DoSave, DoRecent: Boolean;
  aName: string;
begin
  DoRecent := False;
  aName := '';
  if (FName = '') or AsNewFile then
  begin
    aDialog := TSaveDialog.Create(nil);
    aDialog.Title := 'Save file';
    aDialog.Filter := Engine.Groups.CreateFilter;
    aDialog.InitialDir := Engine.BrowseFolder;
    if Group <> nil then
      aDialog.DefaultExt := Group.Extensions[0]
    else
      aDialog.DefaultExt := Engine.Perspective.GetDefaultGroup.Extensions[0];
    aDialog.FileName := '*' + aDialog.DefaultExt;


    DoSave := aDialog.Execute;
    if DoSave then
    begin
      aName := aDialog.FileName;
      DoRecent := True;
    end;
    aDialog.Free;
  end
  else
  begin
    aName := FName;
    DoSave := True;
  end;

  if DoSave then
  begin
    Save(aName);
    FName := aName;
    if DoRecent then
    begin
      Engine.ProcessRecentFile(aName);
      Engine.UpdateState([ecsRefresh, ecsState, ecsChanged]);
    end
    else
      Engine.UpdateState([ecsState, ecsRefresh]);
  end;
end;

function TEditorFile.CheckChanged: Boolean;
var
  mr: integer;
begin
  Result := True;
  if (FileExists(Name)) and (FFileAge <> FileAge(Name)) then
  begin
    mr := Application.MessageBox(PChar(Name + #13' was changed, update it?'), 'Update', MB_YESNOCANCEL);
    if mr = mrYes then
      Reload
    else
      UpdateAge;
    if mr = mrCancel then
      Result := False;
  end;
end;

function TEditorFile.Run: Boolean;
begin
  Result := False;
end;

procedure TEditorFile.UpdateAge;
begin
  FFileAge := FileAge(Name);
  if SynEdit <> nil then
  begin
    SynEdit.Modified := False;
    SynEdit.MarkTextAsSaved;
  end;
end;

procedure TEditorFile.Reload;
begin
  Load(Name);
end;

procedure TEditorFile.SetGroup(const Value: TFileGroup);
begin
  if FGroup <> Value then
  begin
    FGroup := Value;
    if FGroup <> nil then
    begin
      FSynEdit.Highlighter := FGroup.Category.Highlighter;
      FGroup.Category.InitCompletion(FSynEdit);
      if fgkExecutable in FGroup.Kind then
        with TSynDebugMarksPart.Create(FSynEdit.Gutter.Parts) do
        begin
          FEditorFile := Self;
          AutoSize := False;
          Width := EditorResource.DebugImages.Width + DEBUG_IMAGE_MARGINES;
        end;
      FSynEdit.Gutter.SeparatorPart(0).Index := FSynEdit.Gutter.Parts.Count - 1;
      //Engine.MacroRecorder.AddEditor(FSynEdit);
    end;
    Engine.Options.Profile.AssignTo(FSynEdit);
  end;
end;

function TEditorFile.GetHighlighter: TSynCustomHighlighter;
begin
  if Group <> nil then
    Result := Group.Category.Highlighter
  else
    Result := nil;
end;

function TEditorFile.GetReadonly: Boolean;
begin
  Result := SynEdit.ReadOnly;
end;

procedure TEditorFile.SetReadonly(const Value: Boolean);
begin
  SynEdit.ReadOnly := Value;
end;

procedure TEditorFile.NewSource;
begin

end;

function DetectFileMode(const Contents: string): TEditorFileMode;
var
  i: integer;
begin
  Result := efmUnix;
  for i := 1 to Length(Contents) do
  begin
    if Contents[i] = #$D then
    begin
      if (i < Length(Contents) - 1) and (Contents[i + 1] = #$A) then
        Result := efmWindows
      else
        Result := efmMac;
      break;
    end
    else if Contents[i] = #$A then
    begin
      Result := efmUnix;
      break;
    end;
  end;
end;

function ChangeTabsToSpace(const Contents: string; TabWidth: integer): string;
var
  p, l: integer;

  procedure ScanToEOL;
  var
    i: integer;
  begin
    i := p;
    while i <= l do
    begin
      if Contents[i] in [#13, #10] then
        break;
      Inc(i);
    end;
    if ((i + 1) <= l) and (Contents[i + 1] in [#13, #10]) then
      Inc(i);
    Result := Result + Copy(Contents, p, i - p + 1);
    p := i + 1;
  end;

  procedure ScanSpaces;
  var
    i, c: integer;
  begin
    i := p;
    c := 0;
    while i <= l do
    begin
      if Contents[i] = ' ' then
        c := c + 1
      else if Contents[i] = #9 then
        c := c + TabWidth
      else
        break;
      Inc(i);
    end;
    Result := Result + RepeatString(' ', c);
    p := i;
  end;

begin
  p := 1;
  l := Length(Contents);
  while p <= l do
  begin
    ScanSpaces;
    ScanToEOL;
  end;
end;

function TEditorFile.GetModeAsText: string;
begin
  case Mode of
    efmUnix: Result := 'Unix';
    efmWindows: Result := 'Windows';
    efmMac: Result := 'Mac';
  end;
end;

procedure TEditorFile.SetMode(const Value: TEditorFileMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    Edit;
    Engine.UpdateState([ecsState, ecsRefresh]);
  end;
end;

procedure TEditorFile.DoSpecialLineMarkup(Sender: TObject; Line: integer; var Special: Boolean; Markup: TSynSelectedColor);
begin
  if (Engine.Perspective.Debug <> nil) and (Engine.Perspective.Debug.ExecutedEdit = Sender) then
  begin
    if Engine.Perspective.Debug.ExecutedLine = Line then
    begin
      Special := True;
      Markup.Background := clNavy;
      Markup.Foreground := clWhite;
    end;
  end;
end;

procedure TEditorFile.DoGutterClickEvent(Sender: TObject; X, Y, Line: integer; Mark: TSynEditMark);
var
  aLine: integer;
begin
  if fgkExecutable in Group.Kind then
  begin
    aLine := SynEdit.PixelsToRowColumn(Point(X, Y)).y;
    Engine.Perspective.Debug.Lock;
    try
      Engine.Perspective.Debug.Breakpoints.Toggle(Name, aLine);
    finally
      Engine.Perspective.Debug.Unlock;
    end;
    SynEdit.InvalidateLine(aLine);
  end;
end;

procedure TEditorFile.DoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if ([scLeftChar, scTopLine, scSelection] * Changes) <> [] then
    Engine.UpdateState([ecsState]);
end;

{ TEditorOptions }

constructor TEditorOptions.Create;
begin
  inherited Create;
  FSearchHistory := TStringList.Create;
  FReplaceHistory := TStringList.Create;
  FSearchFolderHistory := TStringList.Create;
  FProfile := TEditorProfile.Create(nil);
  FExtraExtensions := TStringList.Create;
  FRecentFiles := TStringList.Create;
  FRecentProjects := TStringList.Create;
  FProjects := TStringList.Create;
  FShowFolder := True;
  FShowMessages := False;
  FCollectTimeout := 60;
  FOutputHeight := 100;
  FMessagesHeight := 100;
  FFoldersWidth := 180;
end;

destructor TEditorOptions.Destroy;
begin
  FSearchHistory.Free;
  FReplaceHistory.Free;
  FSearchFolderHistory.Free;
  FExtraExtensions.Free;
  FProfile.Free;
  FRecentFiles.Free;
  FRecentProjects.Free;
  FProjects.Free;
  inherited;
end;

procedure TEditorOptions.SetProjects(const Value: TStringList);
begin
  if FRecentProjects <> Value then
    FRecentProjects.Assign(Value);
end;

procedure TEditorOptions.SetRecentFiles(const Value: TStringList);
begin
  if FRecentFiles <> Value then
    FRecentFiles.Assign(Value);
end;

procedure TEditorOptions.SetRecentProjects(const Value: TStringList);
begin
  if FRecentProjects <> Value then
    FRecentProjects.Assign(Value);
end;

{ TFileCategories }

procedure TFileCategories.Add(const Name: string; EditorFileClass: TEditorFileClass; CategoryClass: TFileCategoryClass; Kind: TFileCategoryKinds);
var
  aFC: TFileCategory;
begin
  aFC := CategoryClass.Create;
  aFC.FName := Name;
  aFC.FEditorFileClass := EditorFileClass;
  aFC.FKind := Kind;
  inherited Add(aFC);
end;

function TFileGroups.CreateFilter(vGroup: TFileGroup): string;
var
  i, j: integer;
  s: string;
  aSupported: string;
  AExtensions: TStringList;
begin
  aSupported := '';
  AExtensions := TStringList.Create;
  try
    for i := 0 to Count - 1 do
    begin
      if (vGroup = nil) or (vGroup = Items[i]) then
        if fgkBrowsable in Items[i].Kind then
        begin
          if Result <> '' then
            Result := Result + '|';
          s := '';
          AExtensions.Clear;
          Items[i].EnumExtensions(AExtensions);
          for j := 0 to AExtensions.Count - 1 do
          begin
            if s <> '' then
              s := s + ';';
            s := s + '*.' + AExtensions[j];
            if aSupported <> '' then
              aSupported := aSupported + ';';
            aSupported := aSupported + '*.' + AExtensions[j];
          end;
          Result := Result + Items[i].Title + ' (' + s + ')|' + s;
        end;
    end;
  finally
    AExtensions.Free;
  end;

  if Result <> '' then
    Result := 'All files (' + aSupported + ')|' + aSupported + '|' + Result;

  if Result <> '' then
    Result := Result + '|';
  Result := Result + 'Any file (*.*)|*.*';
end;

procedure TFileGroups.Add(vGroup: TFileGroup);
begin
  inherited Add(vGroup);
end;

function TFileCategories.Find(vName: string): TFileCategory;
var
  i: integer;
begin
  Result := nil;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, vName) then
      begin
        Result := Items[i];
        break;
      end;
    end;
end;

function TFileGroups.FindExtension(vExtension: string): TFileGroup;
var
  i, j: integer;
  AExtensions: TStringList;
begin
  Result := nil;
  if LeftStr(vExtension, 1) = '.' then
    vExtension := Copy(vExtension, 2, MaxInt);
  if vExtension <> '' then
  begin
    AExtensions := TStringList.Create;
    try
      for i := 0 to Count - 1 do
      begin
        AExtensions.Clear;
        Items[i].EnumExtensions(AExtensions);
        for j := 0 to AExtensions.Count - 1 do
        begin
          if SameText(AExtensions[j], vExtension) then
          begin
            Result := Items[i];
            break;
          end;
        end;
      end;
    finally
      AExtensions.Free;
    end;
  end;
end;

function TFileCategories.GetItem(Index: integer): TFileCategory;
begin
  Result := inherited Items[Index] as TFileCategory;
end;

procedure TFileCategories.SetItem(Index: integer; AObject: TFileCategory);
begin
  inherited Items[Index] := AObject;
end;

{ TFileCategory }

constructor TFileCategory.Create;
begin
  inherited Create(False);//childs is groups and already added to Groups and freed by it
  FHighlighter := CreateHighlighter;
end;

function TFileCategory.CreateEditorFile(Files: TEditorFiles): TEditorFile;
begin
  Result := FEditorFileClass.Create(Files);
end;

procedure TFileCategory.EnumExtensions(vExtensions: TStringList);
var
  i: Integer;
begin
  for i  := 0 to Count - 1 do
  begin
    Items[i].EnumExtensions(vExtensions);
  end;
end;

function TFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

procedure TFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
end;

destructor TFileCategory.Destroy;
begin
  FreeAndNil(FHighlighter);
  FreeAndNil(FCompletion);
  inherited;
end;

function TFileCategory.Find(vName: string): TFileGroup;
begin
  Result := inherited Find(vName) as TFileGroup;
end;

function TFileCategory.GetItem(Index: Integer): TFileGroup;
begin
  Result := inherited Items[Index] as TFileGroup;
end;

procedure TFileCategory.OnExecuteCompletion(Sender: TObject);
begin
end;

{ TEditorProject }

constructor TEditorProject.Create;
begin
  inherited Create;
  FDesktop := TEditorDesktop.Create;
  FCachedVariables := THashedStringList.Create;
  FCachedIdentifiers := THashedStringList.Create;
  FSaveDesktop := True;
end;

destructor TEditorProject.Destroy;
begin
  if FileName <> '' then
    Save;
  FDesktop.Free;
  FCachedVariables.Free;
  FCachedIdentifiers.Free;
  inherited;
end;

procedure TEditorProject.SetPerspectiveName(AValue: string);
begin
  if FPerspectiveName <> AValue then
  begin
    FPerspectiveName :=AValue;
    FPerspective := nil;
    if FPerspectiveName <> '' then
      FPerspective := Engine.Perspectives.Find(PerspectiveName);
    if FPerspective = nil then
      FPerspective := Engine.DefaultPerspective;
    Engine.UpdateState([ecsChanged, ecsProject]);
  end;
end;

procedure TEditorProject.SetSCM(AValue: TEditorSCM);
begin
  if FSCM =AValue then exit;
  FreeAndNil(FSCM);
  FSCM :=AValue;
  Engine.UpdateState([ecsChanged, ecsProject]);
end;

procedure TEditorProject.Loaded(Failed: Boolean);
begin
  inherited;
  if not Failed and FSaveDesktop then
    Desktop.Load;
end;

function TEditorProject.Save: Boolean;
begin
  if FileName = '' then
    Result := SaveAs
  else
  begin
    SaveToFile(FileName);
    Engine.ProcessRecentProject(FileName);
    Engine.UpdateState([ecsFolder, ecsChanged, ecsState, ecsRefresh]);
    Result := True;
  end;
end;

function TEditorProject.SaveAs: Boolean;
var
  aDialog: TSaveDialog;
begin
  aDialog := TSaveDialog.Create(nil);
  try
    aDialog.Title := 'Save project';
    aDialog.DefaultExt := Engine.Extenstion;
    aDialog.Filter := 'Project files (*.' + Engine.Extenstion + ')|*.' + Engine.Extenstion + '|All files|*.*';
    aDialog.InitialDir := Engine.BrowseFolder;
    aDialog.FileName := Name + aDialog.DefaultExt;
    Result := aDialog.Execute;
    if Result then
    begin
      FileName := aDialog.FileName;
      Save;
    end;
  finally
    aDialog.Free;
  end;
end;

procedure TEditorProject.SetSCMClass(SCMClass: TEditorSCM);
begin
  if (SCMClass = nil) or not((SCM <> nil) and (SCM.ClassType = SCMClass.ClassType)) then
    SCM := nil;
  if (SCMClass <> nil) then
    SCM := TEditorSCMClass(SCMClass.ClassType).Create;
end;

procedure TEditorProject.Saving;
begin
  inherited;
  if FSaveDesktop then
    Desktop.Save;
end;

{ TFileGroup }

procedure TFileGroup.SetCategory(AValue: TFileCategory);
begin
  if FCategory <> AValue then
  begin
    if FCategory <> nil then
      FCategory.Extract(Self);
    FCategory :=AValue;
    if FCategory <> nil then
      FCategory.Add(Self);
  end;
end;

constructor TFileGroup.Create;
begin
  inherited;
  FExtensions := TStringList.Create;
  FKind := [fgkBrowsable];
end;

procedure TFileGroup.EnumExtensions(vExtensions: TStringList);
  procedure AddIt(E: string);
  begin
    if vExtensions.IndexOf(E) < 0 then
      vExtensions.Add(E);
  end;

  procedure AddStrings(E: TStringList);
  var
    i: Integer;
  begin
    for i := 0 to E.Count -1 do
      AddIt(E[i]);
  end;
var
  s: string;
  lStrings:TStringList;
begin
  vExtensions.BeginUpdate;
  try
    AddStrings(Extensions);
    s := Engine.Options.ExtraExtensions.Values[Name];
    if s <> '' then
    begin
      lStrings := TStringList.Create;
      try
        StrToStrings(s, lStrings, [';'], [' ']);
        AddStrings(lStrings);
      finally
        lStrings.Free;
      end;
    end;
  finally
    vExtensions.EndUpdate;
  end;
end;

procedure TFileGroup.EnumExtensions(vExtensions: TEditorElements);
var
  lList:TStringList;
  i: Integer;
  lItem: TEditorElement;
begin
  lList := TStringList.Create;
  try
    EnumExtensions(lList);
    for i := 0 to lList.Count -1 do
    begin
      lItem := TEditorElement.Create;
      lItem.Name := lList[i];
      lItem.Title := lList[i];
      lItem.Description := Title;
      vExtensions.Add(lItem);
    end;
  finally
    lList.Free;
  end;
end;

destructor TFileGroup.Destroy;
begin
  FExtensions.Free;
  inherited;
end;

{ TFileGroups }

procedure TFileGroups.Add(const Name, Title:string; Category: string; Extensions: array of string; Kind: TFileGroupKinds);
var
  aCategory: TFileCategory;
  aGroup: TFileGroup;
  i: integer;
begin
  aCategory := Engine.Categories.Find(Category);
  if aCategory = nil then
    raise Exception.Create('Can not find category ' + Category);
  aGroup:= Find(Name);
  if aGroup <> nil then
    raise Exception.Create(Name + ' already exists');
  aGroup := TFileGroup.Create;
  aGroup.FTitle := Title;
  aGroup.FName := Name;
  aGroup.FKind := Kind;
  for i := 0 to Length(Extensions) - 1 do
    aGroup.Extensions.Add(Extensions[i]);
  aGroup.Category := aCategory;
  inherited Add(aGroup);
end;

function TFileGroups.Find(vName: string): TFileGroup;
begin
  Result := inherited Find(vName) as TFileGroup;
end;

function TFileGroups.Find(vName, vCategory: string): TFileGroup;
var
  i: integer;
begin
  Result := nil;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, vName) and (Items[i].Category.Name = vCategory) then
      begin
        Result := Items[i];
        break;
      end;
    end;
end;

function TFileGroups.GetItem(Index: integer): TFileGroup;
begin
  Result := inherited Items[Index] as TFileGroup;
end;

destructor TEditorSession.Destroy;
begin
  FProject := nil;
  FreeAndNil(FOptions);
  inherited;
end;

function TEditorSession.GetIsOpened: Boolean;
begin
  Result := FProject <> nil;
end;

constructor TEditorSession.Create;
begin
  inherited;
  FOptions := TEditorSessionOptions.Create;
end;

procedure TFileGroups.EnumExtensions(vExtensions: TStringList);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].EnumExtensions(vExtensions);
  end;
end;

procedure TFileGroups.EnumExtensions(vExtensions: TEditorElements);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].EnumExtensions(vExtensions);
  end;
end;

{ TEditorDesktopFiles }

function TEditorDesktopFiles.Add(FileName: string): TEditorDesktopFile;
begin
  Result := inherited Add as TEditorDesktopFile;
  Result.FileName := FileName;
end;

function TEditorDesktopFiles.Find(vName: string): TEditorDesktopFile;
var
  i: integer;
begin
  Result := nil;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].FileName, vName) then
      begin
        Result := Items[i] as TEditorDesktopFile;
        break;
      end;
    end;
end;

function TEditorDesktopFiles.GetItems(Index: integer): TEditorDesktopFile;
begin
  Result := inherited Items[Index] as TEditorDesktopFile;
end;

function TEditorDesktopFiles.IsExist(vName: string): Boolean;
begin
  Result := Find(vName) <> nil;
end;

{ TDebugSupportPlugin }

type
  THackSynEdit = class(TCustomSynEdit);

procedure CenterRect(var R1: TRect; R2: TRect);//from posDraws
begin
  OffsetRect(R1, ((R2.Right - R2.Left) div 2) - ((R1.Right - R1.Left) div 2) + (R2.Left - R1.Left), ((R2.Bottom - R2.Top) div 2) - ((R1.Bottom - R1.Top) div 2) + (R2.Top - R1.Top));
end;

procedure TSynDebugMarksPart.Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
var
  i, x, y, lh, iw, el: integer;
  aLine: integer;
  aRect: TRect;

  procedure DrawIndicator(Line: integer; ImageIndex: integer);
  var
    r: TRect;
  begin
    Line := TSynEdit(SynEdit).RowToScreenRow(Line);
    if (Line >= FirstLine) and (Line <= LastLine) then
    begin
      aRect := AClip;
      aRect.Top := Line * lh;
      aRect.Bottom := aRect.Top + lh;
      r := aRect;
      r.Right := r.Left + iw;
      CenterRect(r, aRect);
      //todo center the rect by image size
      EditorResource.DebugImages.Draw(Canvas, r.Left, r.Top, ImageIndex);
    end;
  end;

begin
  //inherited;
  if Engine.Perspective.Debug <> nil then
  begin
    lh := TSynEdit(SynEdit).LineHeight;
    iw := EditorResource.DebugImages.Width;

    Engine.Perspective.Debug.Lock;
    try
      for i := 0 to Engine.Perspective.Debug.Breakpoints.Count - 1 do
      begin
        if SameText(Engine.Perspective.Debug.Breakpoints[i].FileName, FEditorFile.Name) then//need improve
          DrawIndicator(Engine.Perspective.Debug.Breakpoints[i].Line, DEBUG_IMAGE_BREAKPOINT);
      end;
    finally
      Engine.Perspective.Debug.Unlock;
    end;

    if (Engine.Perspective.Debug.ExecutedEdit = SynEdit) and (Engine.Perspective.Debug.ExecutedLine >= 0) then
      DrawIndicator(Engine.Perspective.Debug.ExecutedLine, DEBUG_IMAGE_EXECUTE);
  end;
end;

{ TEditorDesktop }

constructor TEditorDesktop.Create;
begin
  FFiles := TEditorDesktopFiles.Create(TEditorDesktopFile);
  inherited;
end;

destructor TEditorDesktop.Destroy;
begin
  FFiles.Free;
  inherited;
end;

procedure TEditorDesktop.Load;
var
  i: integer;
  aItem: TEditorDesktopFile;
  aFile: TEditorFile;
begin
  Engine.BeginUpdate;
  try
    if Engine.Perspective.Debug <> nil then
    begin
      Engine.Perspective.Debug.Lock;
      try
  {      Engine.Perspective.Debug.BreakpointsClear;
        for i := 0 to Breakpoints.Count - 1 do
        begin
          Engine.Perspective.Debug.Breakpoints.Add(Breakpoints[i].FileName, Breakpoints[i].Line);
        end;

        Engine.Perspective.Debug.Watches.Clear;
        for i := 0 to Watches.Count - 1 do
        begin
          Engine.Perspective.Debug.Watches.Add(Watches[i].VariableName, Watches[i].Value);
        end;}
      finally
        Engine.Perspective.Debug.Unlock;
      end;
      Engine.UpdateState([ecsDebug]);
    end;

    Engine.Files.CloseAll;
    for i := 0 to Files.Count - 1 do
    begin
      aItem := Files[i];
      if FileExists(aItem.FileName) then
      begin
        aFile := Engine.Files.LoadFile(aItem.FileName, False);
        if aFile <> nil then
        begin
          aFile.SynEdit.CaretX := aItem.CaretX;
          aFile.SynEdit.CaretY := aItem.CaretY;
          aFile.SynEdit.TopLine := aItem.TopLine;
        end;
      end;
    end;
    Engine.Files.SetActiveFile(Files.CurrentFile);
  finally
    Engine.EndUpdate;
    Files.Clear;
  end;
end;

procedure TEditorDesktop.Save;
var
  i: integer;
  aItem: TEditorDesktopFile;
  aFile: TEditorFile;
begin
{  Breakpoints.Clear;
  Watches.Clear;
  Engine.Perspective.Debug.Lock;
  try
    for i := 0 to Engine.Perspective.Debug.Breakpoints.Count - 1 do
    begin
      Breakpoints.Add(Engine.Perspective.Debug.Breakpoints[i].FileName, Engine.Perspective.Debug.Breakpoints[i].Line);
    end;

    for i := 0 to Engine.Perspective.Debug.Watches.Count - 1 do
    begin
      Watches.Add(Engine.Perspective.Debug.Watches[i].VariableName, Engine.Perspective.Debug.Watches[i].Value);
    end;
  finally
    Engine.Perspective.Debug.Unlock;
  end;}

  Files.Clear;
  if Engine.Files.Current <> nil then
    Files.CurrentFile := Engine.Files.Current.Name
  else
    Files.CurrentFile := '';
  for i := 0 to Engine.Files.Count - 1 do
  begin
    aFile := Engine.Files[i];
    aItem := Files.Add(aFile.Name);
    aItem.CaretX := aFile.SynEdit.CaretX;
    aItem.CaretY := aFile.SynEdit.CaretY;
    aItem.TopLine := aFile.SynEdit.TopLine;
  end;
end;

{ TEditorMessages }

function TEditorMessages.GetItem(Index: integer): TEditorMessage;
begin
  Result := inherited Items[Index] as TEditorMessage;
end;

function TEditorMessages.GetText(Index: integer): string;
begin
  if Index < Count then
    Result := Items[Index].Text
  else
    Result := '';
end;

procedure TEditorMessages.SetItem(Index: integer; const Value: TEditorMessage);
begin
  inherited Items[Index] := Value;
end;

{ TEditorMessagesList }

function TEditorMessagesList.GetItem(Index: integer): TEditorMessages;
begin
  Result := inherited Items[Index] as TEditorMessages;
end;

function TEditorMessagesList.GetMessages(Name: string): TEditorMessages;
begin
  Result := Find(Name);
  if Result = nil then
  begin
    Result := TEditorMessages.Create;
    Result.Name := Name;
  end;
  Add(Result);
end;

procedure TEditorMessagesList.SetItem(Index: integer; const Value: TEditorMessages);
begin
  inherited Items[Index] := Value;
end;

function TEditorMessagesList.Find(Name: string): TEditorMessages;
var
  i: integer;
begin
  Result := nil;
  if Name <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, Name) then
      begin
        Result := Items[i];
        break;
      end;
    end;
end;

finalization
  FreeAndNil(FEngine);
end.
