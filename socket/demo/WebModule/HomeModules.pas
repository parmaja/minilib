unit HomeModules;

{$H+}{$M+}
{$ifdef fpc}
{$mode delphi}
{$modeswitch functionreferences}{$modeswitch anonymousfunctions}
{$endif}

interface

uses
  Classes, SysUtils, StrUtils, DateUtils,
  mnUtils, mnStreams, mnModules, mnWebModules, mnMultipartData,
	mnLogs, mnWebElements, mnBootstraps;

type

  { TmySchema }

  TmySchema = class abstract(THTML)
  protected
    procedure DoCompose; override;
  end;

  { TWelcomeSchema }

  TWelcomeSchema = class(TmySchema)
  private
  protected
    Input1: THTML.TInput;
    Input2: THTML.TInput;
    Input3: THTML.TInput;
    procedure DoAccept(const AContext: TmnwContext; var Resume: Boolean); override;
    procedure DoCompose; override;
  public
    class function GetCapabilities: TmnwSchemaCapabilities; override;
  end;

  TWSShema = class(THTML)
  private
  public
  protected
    procedure DoCompose; override;
  public
  end;

  { TLoginSchema }

  TLoginSchema = class(THTML)
  private
  public
  protected
    procedure DoAction(const AContext: TmnwContext; AResponse: TmnwResponse); override;
    procedure DoCompose; override;
  public
  end;

  { TDemoSchema }

  TDemoSchema = class(THTML)
  private
  public
  protected
    procedure DoAction(const AContext: TmnwContext; AResponse: TmnwResponse); override;
    procedure DoCompose; override;
  public
  end;

  { TSimpleSchema }

  TSimpleSchema = class(THTML)
  private
  public
  protected
    procedure DoCompose; override;
  public
  end;

  { TFilesSchema }

  TFilesSchema = class(THTML)
  private
  public
  protected
    procedure Created; override;
    procedure DoCompose; override;
  public
  end;

  TWSEchoGetHomeCommand = class(TwebCommand)
  protected
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  { THomeModule }

  THomeModule = class(TUIWebModule)
  private
  protected
    function CreateRenderer: TmnwRenderer; override;
    procedure InitItems; override;
    procedure Start; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  mnMIME, mnParams;

type

  { TClockComposer }

  TClockCompose = class(THTML.TIntervalCompose)
  public
    procedure InnerCompose(Inner: TmnwElement; AResponse: TmnwResponse); override;
  end;

  TThreadTimer = class(TThread)
  public
  end;

  { TMyAction }

  TMyAction = class(THTML.TAction)
  public
    procedure DoExecute; override;
  end;

  { TMyLink }

  TMyLink = class(THTML.TLink)
  public
    procedure DoAction(const AContext: TmnwContext; AResponse: TmnwResponse); override;
    procedure DoExecute; override;
  end;

  { TMyButton }

  TMyButton = class(THTML.TButton)
  public
    procedure DoExecute; override;
  end;

{ TMyLink }

procedure TMyLink.DoAction(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
  AResponse.Resume := False;
end;

procedure TMyLink.DoExecute;
begin
  inherited;
end;

{ TMyButton }

procedure TMyButton.DoExecute;
begin
  inherited;
  with (Schema as TWelcomeSchema) do
  begin
    log.WriteLn('Clicked')
  end;
end;

{ TMyAction }

procedure TMyAction.DoExecute;
begin
  inherited;
  if Schema <> nil then
    Schema.Attachments.SendMessage('{"type": "text", "element": "input1", "value": "my new value"}');
end;

{ TClockComposer }

procedure TClockCompose.InnerCompose(Inner: TmnwElement; AResponse: TmnwResponse);
begin
  with THTML do
  begin
    TParagraph.Create(Inner, TimeToStr(Now));
    {with TImage.Create(Self) do
    begin
      Name := 'file_logo';
  //          Route := 'logo';
      Source := IncludeURLDelimiter(Module.HomeURL)+'assets/logo.png';
    end;}
  end;
end;

{ TmySchema }

procedure TmySchema.DoCompose;
begin
  inherited;
end;

{ TWellcomeSchema }

procedure TWelcomeSchema.DoAccept(const AContext: TmnwContext;
  var Resume: Boolean);
begin
  Resume := True;
end;

procedure TWelcomeSchema.DoCompose;
begin
  inherited;
  RefreshInterval := 5;
  Interactive := True;
  with Document do
  begin
    Title := 'My Home';
    Direction := dirLeftToRight;
    with Body do
    begin
      //TJSFile.Create(This, [ftResource], 'WebElements_JS', 'WebElements.js');
//      TJSFile.Create(This, [], ExpandFileName(GetCurrentDir + '../../source/mnWebElements.js'));

      Header.NavBar.Title := 'Creative Solutions';
      with Header.NavBar do
      begin
        with TNavItem.Create(This) do
        begin
          Caption := 'Home';

        end;

        with TNavItem.Create(This) do
        begin
          Caption := 'SubMenu';
        end;
      end;

      with TImage.Create(This) do
      begin
        Name := 'image_logo';
        Comment := 'Image from another module';
        Source := '/doc/logo.png';
      end;

      Header.RenderIt := True;
      Toast.RenderIt := True;

      with Main do
      begin
        Name := 'Main';
        with TParagraph.Create(This) do
        begin
          Text := 'Hello Word';
          Name := 'p1';
        end;

        with TMyAction.Create(This) do
        begin
          Route := 'myaction';
        end;

        with TCard.Create(This) do
        begin
          Caption := 'Welcome';
          Name := 'card';

          with TMemoryImage.Create(This) do
          begin
            Name := 'logo';
            Route := 'logo';
            LoadFromFile(IncludePathDelimiter(Schema.GetHomePath) + 'logo.png');
          end;

{          with TImage.Create(This) do
          begin
            Name := 'logo';
  //          Route := 'logo';
              Source := IncludeURLDelimiter(Module.HomeURL)+'assets/logo';
          end;}

          with TRow.Create(This) do
          begin
            Input1 := TInput.Create(This);
            with Input1 do
            begin
              Name := 'Input1';
              id := 'input1';
              Caption := 'Number 1';
            end;

            Input2 := TInput.Create(This);
            with Input2 do
            begin
              Name := 'Input2';
              Caption := 'Number 2';
            end;

            with TMyButton.Create(This) do
            begin
              ID := 'Add';
              Name := 'AddBtn';
              Caption := 'Add';
            end;

            Input3 := TInput.Create(This);
            with Input3 do
            begin
              Name := 'Input3';
              Caption := 'Result';
            end;

          end;

{$ifdef fpc1}
          with TClockCompose.Create(This) do
          begin
          end;
{$else}
          with TIntervalCompose.Create(This) do
          begin
            Route := 'clock';
            OnCompose := procedure(Inner: TmnwElement; AResponse: TmnwResponse)
            begin
              AResponse.Stamp := TimeToStr(Now);
              TParagraph.Create(Inner, TimeToStr(Now));
              {with TImage.Create(Inner) do
              begin
                Name := 'file_logo';
      //          Route := 'logo';
                Source := IncludeURLDelimiter(Module.HomeURL)+'assets/logo.png';
              end;}
            end;
          end;
{$endif}
        end;
      end;
    end;
  end;
end;

class function TWelcomeSchema.GetCapabilities: TmnwSchemaCapabilities;
begin
  //Result := [schemaInteractive] + Inherited GetCapabilities;
  Result := Inherited GetCapabilities;
end;

{ TWSEchoGetHomeCommand }

procedure TWSEchoGetHomeCommand.RespondResult(var Result: TmodRespondResult);
var
  s: string;
begin
  if Request.ConnectionType = ctWebSocket then
  begin
    //Request.Path := DeleteSubPath(Name, Request.Path);
    while Respond.Stream.Connected do
    begin
      if Respond.Stream.ReadUTF8Line(s) then
      begin
        Respond.Stream.WriteUTF8Line(s);
        log(s);
      end;
    end;
  end;
  inherited;
end;

{ TLoginSchema }

procedure TLoginSchema.DoAction(const AContext: TmnwContext; AResponse: TmnwResponse);
var
  aUsername, aPassword: string;
begin
  if AContext.Data <> nil then
  begin
    if SameText(AContext.Data.Values['execute'], 'true') then
    begin
      aUsername := AContext.Data.Values['username'];
      aPassword := AContext.Data.Values['password'];
      AResponse.Session.Value := aUsername +'/'+ aPassword;
      AResponse.Resume := False;
      AResponse.Answer := hrRedirect;
      AResponse.Location := IncludePathDelimiter(AContext.GetPath) + 'dashboard';
    end;
  end;
  inherited;
end;

procedure TLoginSchema.DoCompose;
begin
  inherited;
  with Document do
  begin
    //Name := 'document';
    //Route := 'document';
    Title := 'MyHome';
    Direction := dirLeftToRight;

    with Body do
    begin
      with TComment.Create(This) do
        Comment := 'This is just login page source';

      with Header do
      begin
        RenderIt := True;
//        Fixed := fixedTop;
        with NavBar do
        begin
          Title := 'Creative Solutions';
          with TNavItem.Create(This) do
          begin
            Caption := 'Item1';
          end;

          with TNavItem.Create(This) do
          begin
            Caption := 'Item2';
          end;
        end;
      end;

      with SideBar do
      begin
        RenderIt := True;
        with TLink.Create(This, 'http://www.google.com', 'Google') do
        begin
          ClickType := clickNavigate;
          NoDecoration := True;
        end;

        with TMyLink.Create(This, '', 'Home') do
        begin
          Route := 'my_link';
          Location := GetPath;
          ClickType := clickAction;
          NoDecoration := True;
        end;
      end;

      with Main do
      begin
        Padding := 1;

        {with TNavBar.Create(This) do
        begin
          Caption := 'Nav2';
          //Fixed := fixedTop;
          with TNavItem.Create(This) do
          begin
            Caption := 'Menu 1';
          end;
          with TNavItem.Create(This) do
          begin
            Caption := 'Menu 2';
          end;
        end;}

        //with TRow.Create(This) do
        begin

         // ContentAlign := alignCenter;
          with TCard.Create(This) do
          begin
            Solitary := True;
            Size := szNormal;
            Caption := 'Login';

            with TForm.Create(This) do
            begin
              PostTo.Where := toElement;

              with TInput.Create(This) do
              begin
                ID := 'username';
                Name := 'username';
                Caption := 'Username';
                PlaceHolder := 'Type user name';
              end;

              with TInputPassword.Create(This) do
              begin
                ID := 'password';
                Name := 'password';
                Caption := 'Password';
                HelpText := 'You need to use numbers';
              end;

              TBreak.Create(This);

              Submit.Caption := 'Submit';
              Reset.Caption := 'Reset';

            end;
           end;
        end;
      end;
    end;
  end;
end;

{ TDemoSchema }

procedure TDemoSchema.DoAction(const AContext: TmnwContext; AResponse: TmnwResponse);
var
  aUsername, aPassword: string;
begin
  if AContext.Data <> nil then
  begin
    if SameText(AContext.Data.Values['execute'], 'true') then
    begin
      aUsername := AContext.Data.Values['username'];
      aPassword := AContext.Data.Values['password'];
      AResponse.Session.Value := aUsername +'/'+ aPassword;
      AResponse.Resume := False;
      AResponse.Answer := hrRedirect;
      AResponse.Location := IncludePathDelimiter(AContext.GetPath) + 'dashboard';
    end;
  end;
  inherited;
end;

procedure TDemoSchema.DoCompose;
var
  i: Integer;
begin
  inherited;
  with Document do
  begin
    Title := 'Demo Title';
    Direction := dirLeftToRight;

    with Body do
    begin

      with TComment.Create(This) do
        Comment := 'This is just login page source';

      with Header do
      begin
        RenderIt := True;
//        Fixed := fixedTop;
        with NavBar do
        begin
          Title := 'Creative Solutions';
          with TNavItem.Create(This) do
          begin
            Caption := 'Item1';
          end;

          with TNavItem.Create(This) do
          begin
            Caption := 'Item2';
          end;

          with Buttons do
          begin
            with TThemeModeButton.Create(This) do
            begin
              Caption := 'Mode';
            end;

            with TDropdown.Create(This) do
            begin
              Image.Icon := 'icon mw-font-normal';
              with TZoomButtons.Create(This) do
              begin
                //Caption := 'Font';
              end;
            end;
          end;
        end;
      end;

      with SideBar do
      begin
        RenderIt := True;
        with TAccordion.Create(This) do
        begin
          AlwaysOpen := True;
          with TBar.Create(This) do
          begin
            Padding := 2;
            with TThemeModeButton.Create(This) do
            begin
              Caption := 'Mode';
            end;
          end;

          with TAccordionSection.Create(This) do
          begin
            Caption := 'Accounts';
            Expanded := True;
            //with TAccordionSection.Create(This) do
            with TLink.Create(This, 'http://www.google.com', 'Google') do
            begin
              ClickType := clickNavigate;
            end;

            with TMyLink.Create(This, '', 'Home') do
            begin
              Route := 'my_link';
              Location := GetPath;
              ClickType := clickAction;
            end;
          end;

          with TAccordionSection.Create(This) do
          begin
            Caption := 'Favorites';
            Expanded := True;
            with TLink.Create(This, 'http://www.parmaja.org', 'parmaja') do
            begin
              ClickType := clickNavigate;
            end;

            for i := 0 to 20 do
            begin
	            with TMyLink.Create(This, '', 'Link'+IntToStr(i)) do
              begin
                Location := GetPath;
                ClickType := clickAction;
              end;
            end;
          end;
        end;

      end;

      with Main do
      begin
        Padding := 1;

        {with TNavBar.Create(This) do
        begin
          Caption := 'Nav2';
          //Fixed := fixedTop;
          with TNavItem.Create(This) do
          begin
            Caption := 'Menu 1';
          end;
          with TNavItem.Create(This) do
          begin
            Caption := 'Menu 2';
          end;
        end;}

        //with TRow.Create(This) do
        begin
          with TDropdown.Create(This) do
          begin
            Caption := 'Font';
            with TButton.Create(This) do
            begin
              Caption := 'A';
            end;

            with TButton.Create(This) do
            begin
              Caption := 'B';
            end;
          end;

          with TGroupButtons.Create(This) do
          begin
            with TButton.Create(This) do
            begin
              Caption := 'A';
            end;

            with TButton.Create(This) do
            begin
              Caption := 'B';
            end;
          end;

          with TToolbar.Create(This) do
          begin
            with TButton.Create(This) do
            begin
              Caption := 'A';
            end;

            with TButton.Create(This) do
            begin
              Caption := 'B';
            end;
          end;

          with This.Add<TColumn> do
          begin
            Size := 8;
            with TCard.Create(This) do
            begin
              Collapse := True;
              Size := szVeryLarge;
              Caption := 'Empty';
              //Solitary := True;
            end;
          end;

         // ContentAlign := alignCenter;
          with TCard.Create(This) do
          begin
            Collapse := True;
            Size := szNormal;
            Caption := 'Login';
            Solitary := True;

            with TForm.Create(This) do
            begin
              PostTo.Where := toElement;

              with TInput.Create(This) do
              begin
                ID := 'username';
                Name := 'username';
                Caption := 'Username';
                PlaceHolder := 'Type user name';
              end;

              with TInputPassword.Create(This) do
              begin
                ID := 'password';
                Name := 'password';
                Caption := 'Password';
                HelpText := 'You need to use numbers';
              end;

              TBreak.Create(This);

              Submit.Caption := 'Submit';
              Reset.Caption := 'Reset';

            end;
           end;

          with TCard.Create(This) do
          begin
            Size := szVeryLarge;
            Caption := 'Task';
            Solitary := True;
            Collapse := True;

            with TCollapseCaption.Create(This) do
            begin
              Caption := 'You must go!';
              TParagraph.Create(This, 'Yes you must go to the police to tell them the full story!');
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ TSimpleSchema }

procedure TSimpleSchema.DoCompose;
begin
  inherited;
end;

{ TFilesSchema }

procedure TFilesSchema.Created;
begin
  inherited;
end;

procedure TFilesSchema.DoCompose;
begin
  inherited;
  ServeFiles := [serveEnabled, serveSmart, serveDefault, serveIndex];
  HomePath := IncludePathDelimiter(App.HomePath) + 'files';
  with TFolder.Create(This) do
  begin
    ServeFiles := [serveEnabled, serveSmart, serveDefault, serveIndex];
    Route := 'folder';
    HomePath := ExpandFileName(App.HomePath + 'smilies');
  end;
end;

{ TWSShema }

procedure TWSShema.DoCompose;
begin
  inherited;
  Name := 'ws';
  Route := 'ws';
  with TFile.Create(This) do
  begin
    Route := 'echo';
    FileName := IncludePathDelimiter(App.HomePath) + 'ws.html';
  end;
end;

{ THomeModule }

function THomeModule.CreateRenderer: TmnwRenderer;
begin
  Result := TmnwBootstrapRenderer.Create(Self, WebApp.IsLocal);
end;

destructor THomeModule.Destroy;
begin
  inherited;
end;

procedure THomeModule.InitItems;
begin
  inherited;
  WebApp.RegisterSchema('', TWelcomeSchema);
  WebApp.RegisterSchema('login', TLoginSchema);
  WebApp.RegisterSchema('demo', TDemoSchema);
  WebApp.RegisterSchema('simple', TSimpleSchema);
  WebApp.RegisterSchema('files', TFilesSchema);
  WebApp.RegisterSchema('ws', TWSShema);
  RegisterCommand('.ws', TWSEchoGetHomeCommand, False);
end;

procedure THomeModule.Start;
begin
  inherited;
  with WebApp.Assets do
  begin
    if Logo.Data.Size = 0 then
      Logo.LoadFromFile(HomePath + 'logo.png');
      //Logo.LoadFromFile(HomePath + 'logo.png');
    with thtml.TFile.Create(This) do
    begin
      Name := 'jquery';
      Route := 'jquery';
      FileName := IncludePathDelimiter(HomePath) + 'jquery-3.7.1.min.js';
    end;
  end;
end;

end.

