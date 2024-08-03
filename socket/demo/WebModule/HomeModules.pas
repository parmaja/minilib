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
  { TWelcomeSchema }

  TWelcomeSchema = class(TUIWebSchema)
  private
  protected
    Input1: THTML.TInput;
    Input2: THTML.TInput;
    Input3: THTML.TInput;
    procedure DoCompose(const AContext: TmnwContext); override;
  public
    class function GetCapabilities: TmnwSchemaCapabilities; override;
  end;

  TWSShema = class(TUIWebSchema)
  private
  public
  protected
    procedure DoCompose(const AContext: TmnwContext); override;
  public
  end;

  { TLoginSchema }

  TLoginSchema = class(TUIWebSchema)
  private
  public
  protected
    procedure DoAction(const AContext: TmnwContext; var AReturn: TmnwReturn); override;
    procedure DoCompose(const AContext: TmnwContext); override;
  public
  end;

  TWSEchoGetHomeCommand = class(TmodHttpCommand)
  protected
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  { THomeModule }

  THomeModule = class(TUIWebModule)
  private
  protected
    function CreateRenderer: TmnwRenderer; override;
    procedure CreateItems; override;
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
    procedure InnerCompose(Inner: TmnwElement); override;
  end;

  TThreadTimer = class(TThread)
  public
  end;

  { TMyAction }

  TMyAction = class(THTML.TAction)
  public
    procedure DoExecute; override;
  end;

  { TMyButton }

  TMyButton = class(THTML.TButton)
  public
    procedure DoExecute; override;
  end;

{ TMyButton }

procedure TMyButton.DoExecute;
begin
  inherited;
  with (Schema as TWelcomeSchema) do
  begin
	  Input3.Value := IntToStr(StrToIntDef(Input1.Value, 0) + StrToIntDef(Input2.Value, 0));
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

procedure TClockCompose.InnerCompose(Inner: TmnwElement);
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

{ TWellcomeSchema }

procedure TWelcomeSchema.DoCompose(const AContext: TmnwContext);
begin
  inherited;
  Name := 'welcome';
  Route := 'welcome';

  with TDocument.Create(This) do
  begin
    Name := 'document';
    //Route := 'document';
    Title := 'MyHome';
    Direction := dirLTR;
    with Body do
    begin
      //TJSFile.Create(This, [ftResource], 'mnWebElements.js');
      TJSFile.Create(This, [], ExpandFileName(Module.AppPath + '../../source/mnWebElements.js'));

      Header.Caption := 'Creative Solutions';
      with NavBar do
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
      Footer.RenderIt := True;

      with Container do
      begin
        Name := 'Container';
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
            LoadFromFile(IncludePathDelimiter(Module.HomePath) + 'logo.png');
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

{$ifdef fpc}
{          with TClockCompose.Create(This) do
          begin
          end;}
{$else}
          with TIntervalCompose.Create(This) do
          begin
            Route := 'clock';
            OnCompose := procedure(Inner: TmnwElement)
            begin
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
  Result := [schemaInteractive] + Inherited GetCapabilities;
  //Result := Inherited GetCapabilities;
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

procedure TLoginSchema.DoAction(const AContext: TmnwContext; var AReturn: TmnwReturn);
var
  aUsername, aPassword: string;
begin
  if AContext.Data <> nil then
  begin
    if SameText(AContext.Data.Values['execute'], 'true') then
    begin
      aUsername := AContext.Data.Values['username'];
      aPassword := AContext.Data.Values['password'];
      AReturn.SessionID := aUsername +'/'+ aPassword;
      AReturn.Resume := False;
      AReturn.Respond.HttpResult := hrRedirect;
      AReturn.Location := IncludePathDelimiter(AContext.Renderer.GetHomeURL) + 'dashboard';
    end;
  end;
  inherited;
end;

procedure TLoginSchema.DoCompose(const AContext: TmnwContext);
begin
  inherited;
  Name := 'login';
  Route := 'login';
  with TDocument.Create(This) do
  begin
    //Name := 'document';
    //Route := 'document';
    Title := 'MyHome';
    Direction := dirLTR;

    with Body do
    begin
      with TComment.Create(This) do
        Comment := 'This is just login page source';

      with Header do
      begin
        RenderIt := True;
        Fixed := fixedTop;
        Header.Caption := 'Creative Solutions';
        with TNavItem.Create(This) do
        begin
          Caption := 'Item1';
        end;
        with TNavItem.Create(This) do
        begin
          Caption := 'Item2';
        end;
      end;

      Footer.RenderIt := True;

      with Container do
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
            Style := 'center';
            Size := szSmall;
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

{ TWSShema }

procedure TWSShema.DoCompose(const AContext: TmnwContext);
begin
  inherited;
  Name := 'ws';
  Route := 'ws';
  with TFile.Create(This) do
  begin
    Route := 'echo';
    FileName := IncludePathDelimiter(Module.HomePath) + 'ws.html';
  end;
end;

{ THomeModule }

function THomeModule.CreateRenderer: TmnwRenderer;
begin
  Result := TmnwBootstrapRenderer.Create(Self, True);
end;

destructor THomeModule.Destroy;
begin
  inherited;
end;

procedure THomeModule.CreateItems;
begin
  inherited;
  WebApp.RegisterSchema('welcome', TWelcomeSchema);
  WebApp.RegisterSchema('assets', TAssetsSchema);
  WebApp.RegisterSchema('login', TLoginSchema);
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
    with TFile.Create(This) do
    begin
      Name := 'jquery';
      Route := 'jquery';
      FileName := IncludePathDelimiter(Module.HomePath) + 'jquery-3.7.1.min.js';
    end;
  end;
end;

end.

