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
    procedure DoCompose(const AContext: TmnwContext); override;
  end;

  THomeSchema = class(TmySchema)
  private
    procedure DoRespondHeader(const AContext: TmnwContext);
  public
  protected
    procedure DoCompose(const AContext: TmnwContext); override;
  public
  end;
  
  { TWelcomeSchema }

  TDemo1Schema = class(TmySchema)
  private
  protected
    Input1: THTML.TInput;
    Input2: THTML.TInput;
    Input3: THTML.TInput;
     
    procedure DoPrepare; override;    
    procedure DoAccept(var AContext: TmnwContext; var Resume: Boolean); override;
    procedure DoCompose(const AContext: TmnwContext); override;
    procedure AttachedMessage(const s: string); override; 
  public    
  end;

  TWSShema = class(THTML)
  private
  public
  protected
    procedure DoCompose(const AContext: TmnwContext); override;
  public
  end;

  { TLoginSchema }

  TLoginSchema = class(THTML)
  private
  public
  protected
    procedure DoRespondHeader(const AContext: TmnwContext); override;
    procedure DoCompose(const AContext: TmnwContext); override;
  public
  end;

  { TDemo2Schema }

  TDemo2Schema = class(THTML)
  private
  public
  protected
    procedure DoRespondHeader(const AContext: TmnwContext); override;
    procedure DoCompose(const AContext: TmnwContext); override;
  public
  end;

  { TInfoSchema }

  TInfoSchema = class(THTML)
  private
  public
  protected     
    procedure DoCompose(const AContext: TmnwContext); override;
  public
    class function GetCapabilities: TmnwSchemaCapabilities; override;
  end;

  { TFilesSchema }

  TFilesSchema = class(THTML)
  private
  public
  protected
    procedure Created; override;
    procedure DoCompose(const AContext: TmnwContext); override;
  public
  end;

  TWSEchoGetHomeCommand = class(TwebCommand)
  protected
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  { THomeModule }

  THomeModule = class(TmnwWebModule)
  private
  protected
    procedure InitItems; override;
    procedure Start; override;
  public    
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
    procedure DoRespondHeader(const AContext: TmnwContext); override;
    procedure DoExecute; override;
  end;

  { TMyButton }

  TMyButton = class(THTML.TButton)
  public
    procedure DoExecute; override;
  end;

{ TMyLink }

procedure TMyLink.DoRespondHeader(const AContext: TmnwContext);
begin
  inherited;
  AContext.Response.Responded;
end;

procedure TMyLink.DoExecute;
begin
  inherited;
end;

{ TMyButton }

procedure TMyButton.DoExecute;
begin
  inherited;
  with (Schema as TDemo1Schema) do
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

procedure TmySchema.DoCompose(const AContext: TmnwContext);
begin
  inherited;
end;

procedure TDemo1Schema.AttachedMessage(const s: string);
begin
  inherited;
  Attachments.SendMessage('ECHO: '+s);
end;

{ TWellcomeSchema }

procedure TDemo1Schema.DoAccept(var AContext: TmnwContext; var Resume: Boolean);
begin
  Resume := True;
end;

procedure TDemo1Schema.DoCompose(const AContext: TmnwContext);
begin
  inherited;
  RefreshInterval := 5;
  Interactive := True;
  with Document do
  begin
    Title := 'My Home';
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

          with TImageMemory.Create(This) do
          begin
            Name := 'logo';
            Route := 'logo';
            LoadFromFile(IncludePathDelimiter(Schema.GetPublicPath) + 'logo.png');
          end;

{          with TImage.Create(This) do
          begin
            Name := 'logo';
  //          Route := 'logo';
              Source := IncludeURLDelimiter(Module.HomeURL)+'assets/logo';
          end;}

          with TColumn.Create(This) do
          begin
            Input1 := TInput.Create(This);
            with Input1 do
            begin
              Name := 'Input1';
              id := 'input1';
              Caption := 'Number 1';
            end;
            TBreak.Create(This);
            Input2 := TInput.Create(This);
            with Input2 do
            begin
              Name := 'Input2';
              Caption := 'Number 2';
            end;
            TBreak.Create(This);
            with TMyButton.Create(This) do
            begin
              ID := 'Add';
              Name := 'AddBtn';
              Caption := 'Add';
            end;
            TBreak.Create(This);
            Input3 := TInput.Create(This);
            with Input3 do
            begin
              Name := 'Input3';
              Caption := 'Result';
            end;
            TBreak.Create(This);
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

procedure TDemo1Schema.DoPrepare;
begin
  inherited;

end;

{ TWSEchoGetHomeCommand }

procedure TWSEchoGetHomeCommand.RespondResult(var Result: TmodRespondResult);
var
  s: string;
begin
  if Request.RequestType = rtWebSocket then
  begin
    //Request.Path := DeleteSubPath(Name, Request.Path);
    while Response.Stream.Connected do
    begin
      if Response.Stream.ReadUTF8Line(s) then
      begin
        Response.Stream.WriteUTF8Line(s);
        log(s);
      end;
    end;
  end;
  inherited;
end;

{ TLoginSchema }

procedure TLoginSchema.DoRespondHeader(const AContext: TmnwContext);
var
  aUsername, aPassword: string;
begin
  if AContext.Data <> nil then
  begin
    if AContext.Data['execute'].AsBoolean then
    begin
      aUsername := AContext.Data['username'].AsString;
      aPassword := AContext.Data['password'].AsString;
      AContext.Session.ID := aUsername +'/'+ aPassword;
      AContext.Response.RespondRedirectTo(IncludePathDelimiter(AContext.GetPath) + 'dashboard');
    end;
  end;
  inherited;
end;

procedure TLoginSchema.DoCompose(const AContext: TmnwContext);
begin
  inherited;
  with Document do
  begin
    //Name := 'document';
    //Route := 'document';
    Title := 'MyHome';    

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
            Caption := 'Login';

            with TForm.Create(This) do
            begin
              Endpoint.Where := toElement;

              with TInput.Create(This) do
              begin
                ID := 'username';
                Name := 'username';
                Caption := 'Username';
                PlaceHolder := 'Type user name';
              end;

              with TPassword.Create(This) do
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

{ TDemo2Schema }

procedure TDemo2Schema.DoRespondHeader(const AContext: TmnwContext);
var
  aUsername, aPassword: string;
begin
  if AContext.Data <> nil then
  begin
    if AContext.Data['execute'].AsBoolean then
    begin
      aUsername := AContext.Data['username'].AsString;
      aPassword := AContext.Data['password'].AsString;
      AContext.Session.ID := aUsername +'/'+ aPassword;
      AContext.Response.RespondRedirectTo(IncludePathDelimiter(AContext.GetPath) + 'dashboard');
    end;
  end;
  inherited;
end;

procedure TDemo2Schema.DoCompose(const AContext: TmnwContext);
var
  i: Integer;
begin
  inherited;
  with Document do
  begin
    Title := 'Demo Title';    

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

          with Tools do
          begin
            TThemeButton.Create(This);

            with TDropdown.Create(This) do
            begin
              Image.Symbol := 'icon mw-font-normal';
              Options := Options + [dropEnd];
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
            TThemeButton.Create(This);
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
            end;
          end;

          with TCard.Create(This) do
          begin
            Collapse := True;
            Caption := 'Login';
            Solitary := True;

            with TForm.Create(This) do
            begin
              Endpoint.Where := toElement;

              with TInput.Create(This) do
              begin
                ID := 'username';
                Name := 'username';
                Caption := 'Username';
                PlaceHolder := 'Type user name';
              end;

              with TPassword.Create(This) do
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

{ TInfoSchema }

// http://localhost:8080/home/info/panel1/test1/test2
procedure TInfoSchema.DoCompose(const AContext: TmnwContext);
var
  aPanel: TPanel;
begin
  inherited;
  with Document.Body.Main do
  begin            
//    Route := 'main';
    with TPanel.Create(this) do    
    begin
      Route := 'panel1';
      TCode.Create(This, 'Context.CurrentPath: ' + AContext.CurrentPath);
      TBreak.Create(This);
      TCode.Create(This, 'Context.Request.Path: ' + AContext.Request.Path);
      TBreak.Create(This);
      TCode.Create(This, 'Context.Request.CurrentPath: ' + AContext.Request.CurrentPath);
      TBreak.Create(This);
      TCode.Create(This, 'Context.Request.BasePath: ' + AContext.Request.BasePath);
      TBreak.Create(This);
      TCode.Create(This, 'Context.Request.NameSpace: ' + AContext.Request.NameSpace);
      TBreak.Create(This);
      TCode.Create(This, 'Context.Request.NameSpace: ' + AContext.Request.NameSpace);
      TBreak.Create(This);
      TCode.Create(This, 'e.GetPath: ' + This.GetPath);
      TBreak.Create(This);
      TCode.Create(This, 'Context.GetRelativePath: ' + AContext.GetRelativePath(This));
      TBreak.Create(This);
      TBreak.Create(This);
      TCode.Create(This, 'Context.GetDefaultPath: ' + AContext.GetDefaultPath);
      TBreak.Create(This);
      TCode.Create(This, 'Context.GetPath(e): ' + AContext.GetPath(This));
      TBreak.Create(This);
      TCode.Create(This, 'Context.GetURL: ' + AContext.GetURL);
      TBreak.Create(This);
      TCode.Create(This, 'Context.GetURL(e): ' + AContext.GetURL(this));
      TBreak.Create(This);
      TCode.Create(This, 'Context.GetHomeURL: ' + AContext.GetHomeURL);
    end;
    
    aPanel := TPanel.Create(this);
    with aPanel do    
    begin
      Route := 'panel2'; 
      OnRespond := procedure (const AContext: TmnwContext)
      begin
        AContext.Response.RespondText('Hello World'+#13 + AContext.CurrentPath);
      end;
    end;
  end;
    
end;

class function TInfoSchema.GetCapabilities: TmnwSchemaCapabilities;
begin
  Result := (inherited GetCapabilities) + [schemaDynamic];
end;

{ TFilesSchema }

procedure TFilesSchema.Created;
begin
  inherited;
end;

procedure TFilesSchema.DoCompose(const AContext: TmnwContext);
begin
  inherited;
  ServeFiles := [serveEnabled, serveSmart, serveDefault, serveIndex];
  PublicPath := IncludePathDelimiter(Web.PublicPath) + 'files';
  with TFolder.Create(This) do
  begin
    ServeFiles := [serveEnabled, serveSmart, serveDefault, serveIndex];
    Route := 'Dir';
    PublicPath := ExpandFileName(Web.PublicPath+ 'smilies');
  end;
end;

{ TWSShema }

procedure TWSShema.DoCompose(const AContext: TmnwContext);
begin
  inherited;
  Name := 'ws';
  Route := 'ws';
  with TFile.Create(This) do
  begin
    Route := 'echo';
    FileName := IncludePathDelimiter(Web.PublicPath) + 'ws.html';
  end;
end;

{ THomeModule }

procedure THomeModule.InitItems;
begin
  inherited;
  Web.RegisterSchema('', THomeSchema);
  Web.RegisterSchema('login', TLoginSchema);
  Web.RegisterSchema('demo1', TDemo1Schema);
  Web.RegisterSchema('demo2', TDemo2Schema);
  Web.RegisterSchema('info', TInfoSchema);
  Web.RegisterSchema('files', TFilesSchema);
  Web.RegisterSchema('ws', TWSShema);
  RegisterCommand('.ws', TWSEchoGetHomeCommand, False);
end;

procedure THomeModule.Start;
begin
  inherited;
  with Web.Assets do
  begin
    //Web.OnlineFiles:= olfSmart;
    LogoFile := PublicPath + 'logo.png';
  end;
end;

{ THomeSchema }

procedure THomeSchema.DoCompose(const AContext: TmnwContext);
begin
  inherited;
  with Document.Body.Main do
  begin
    with TCard.Create(This) do
    begin
      TLink.Create(This, AContext.GetHomeURL + '/login', 'login');
      TLink.Create(This, AContext.GetHomeURL + '/info', 'info');
      TLink.Create(This, AContext.GetHomeURL + '/demo1', 'demo1');
      TLink.Create(This, AContext.GetHomeURL + '/demo2', 'demo2');
      TLink.Create(This, AContext.GetHomeURL + '/files', 'files');
      TLink.Create(This, AContext.GetHomeURL + '/info/sub1', 'test params info/sub1');
      TLink.Create(This, AContext.GetHomeURL + '/info/sub2/', 'test params info/sub2/');
      TLink.Create(This, AContext.GetHomeURL + '/info/sub1/sub2/', 'test params info/sub1/sub2');
      TLink.Create(This, AContext.GetHomeURL + '/files/sub1', 'test redirect files/sub1');
      TLink.Create(This, AContext.GetHomeURL + '/files/sub1/sub2', 'test redirect files/sub1/sub2');
    end;
  end;
end;

procedure THomeSchema.DoRespondHeader(const AContext: TmnwContext);
begin
  inherited;

end;

initialization  
end.

