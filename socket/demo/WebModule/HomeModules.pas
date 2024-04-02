unit HomeModules;

{$H+}{$M+}
{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils,
  mnUtils, mnStreams, mnModules, mnWebModules, mnMultipartData,
	mnWebElements, mnBootstraps;

type
  THomeModule = class;

  TCustomHomeSchema = class(TmnwBootstrapHTML)
  public
    Module: THomeModule;
  end;

  { TAssetsSchema }

  TAssetsSchema = class(TCustomHomeSchema)
  private
  public
  protected
    procedure DoCompose; override;
  public
  end;

  TWelcomeSchema = class(TCustomHomeSchema)
  private
  public
  protected
    procedure DoCompose; override;
  public
  end;

  TLoginSchema = class(TCustomHomeSchema)
  private
  public
  protected
    procedure DoCompose; override;
  public
  end;

  { TbsHttpGetHomeCommand }

  TbsHttpGetHomeCommand = class(TmodHttpCommand)
  protected
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  TbsWSGetHomeCommand = class(TmodWebSocketCommand)
  protected
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
  end;

  THomeSchemas = class(TmnwSchemas)
  protected
    Module: THomeModule;
    procedure SchemaCreated(Schema: TmnwSchema); override;
  end;

  { THomeModule }

  THomeModule = class(TmodWebModule)
  protected
    procedure DoRegisterCommands; override;
    procedure Start; override;
    procedure Created; override;

    procedure DoMatch(const ARequest: TmodRequest; var vMatch: Boolean); override;
    procedure DoPrepareRequest(ARequest: TmodRequest); override;

  public
    Schemas: THomeSchemas;
    //LoginSchema: TLoginSchema;
    destructor Destroy; override;
  end;

implementation

{ TWellcomeSchema }

procedure TWelcomeSchema.DoCompose;
begin
  inherited;
  Name := 'welcome';
  Route := 'welcome';
  with This.Add<TDocument> do
  begin
    Name := 'document';
    //Route := 'document';
    Title := 'MyHome';
    Direction := dirLTR;

    Header.Text := 'Creative Solutions';
    with Header.Add<TImage> do
    begin
      Name := 'image_logo';
      Comment := 'Image from another module';
      Source := IncludeURLDelimiter(Module.HostURL)+'doc/logo.png';
    end;
    Header.Use := True;
    Footer.Use := True;

    with Container do
    begin
      Name := 'Container';
      with This.Add<TParagraph> do
      begin
        Text := 'Hello Word';
        Name := 'p1';
      end;

      with This.Add<TCard>() do
      begin
        Caption := 'Welcome';
        Name := 'card';

        with This.Add<TMemoryImage> do
        begin
          Name := 'logo';
          Route := 'logo';
          LoadFromFile(IncludePathDelimiter(Module.HomePath) + 'logo.png');
        end;

        with This.Add<TImage> do
        begin
          Name := 'logo';
//          Route := 'logo';
            Source := IncludeURLDelimiter(Module.HomeURL)+'assets/logo';
        end;

        with This.Add<TImage> do
        begin
          Name := 'file_logo';
//          Route := 'logo';
          Source := IncludeURLDelimiter(Module.HomeURL)+'assets/logo.png';
        end;
      end;

    end;
  end;
end;

{ TbsHttpGetHomeCommand }

procedure TbsHttpGetHomeCommand.RespondResult(var Result: TmodRespondResult);
var
  Renderer : TmnwBootstrapRenderer;
begin
  inherited;
  Respond.PutHeader('Content-Type', DocumentToContentType('html'));
  Respond.HttpResult := hrOK;
//  Respond.SendHeader;

  Renderer := TmnwBootstrapRenderer.Create;
  try
    Renderer.HomeUrl := (Module as THomeModule).HomeUrl;
    (Module as THomeModule).Schemas.Respond(DeleteSubPath('', Request.Path), Renderer, Self, Respond.Stream);
  finally
    Renderer.Free;
  end;
end;

{ THomeModule }

procedure THomeModule.DoMatch(const ARequest: TmodRequest; var vMatch: Boolean);
begin
  inherited;
end;

procedure THomeModule.DoPrepareRequest(ARequest: TmodRequest);
begin
  inherited;
  ARequest.Command := ARequest.Route[1];
  //ARequest.Path := DeleteSubPath(ARequest.Command, ARequest.Path);
end;

procedure THomeModule.DoRegisterCommands;
begin
  inherited;
  RegisterCommand('Page', TbsHttpGetHomeCommand, true);
  RegisterCommand('WS', TbsWSGetHomeCommand, false);
end;

procedure THomeModule.Created;
begin
  inherited;
end;

procedure THomeModule.Start;
begin
  inherited;
  Schemas := THomeSchemas.Create;
  Schemas.Module := Self;
  Schemas.RegisterSchema('welcome', TWelcomeSchema);
  Schemas.RegisterSchema('assets', TAssetsSchema);
  Schemas.RegisterSchema('login', TLoginSchema);
end;

destructor THomeModule.Destroy;
begin
  FreeAndNil(Schemas);
  inherited;
end;

{ TAssetsSchema }

procedure TAssetsSchema.DoCompose;
begin
  inherited;
  Name := 'Assets';
  Route := 'assets';
  with This.Add<TAssets> do
  begin
    HomePath := Module.HomePath;
    Kind := Kind + [elFallback];
    //Name := 'document';
//    Route := '';
    with This.Add<TDirectFile> do
    begin
      Name := 'jquery';
      Route := 'jquery';
      FileName := IncludePathDelimiter(Module.HomePath) + 'jquery-3.7.1.min.js';
    end;

    with This.Add<TDirectFile> do
    begin
      Name := 'logo';
      Route := 'logo';
      FileName := IncludePathDelimiter(Module.HomePath) + 'logo.png';
    end;
  end;
end;

{ TbsWSGetHomeCommand }

procedure TbsWSGetHomeCommand.RespondResult(var Result: TmodRespondResult);
begin
  Request.Path := DeleteSubPath(Name, Request.Path);
  inherited;
end;

{ TLoginSchema }

procedure TLoginSchema.DoCompose;
begin
  inherited;
  Name := 'welcome';
  Route := 'welcome';
  with This.Add<TDocument> do
  begin
    //Name := 'document';
    Route := 'document';
    Title := 'MyHome';
    Direction := dirLTR;

    Header.Text := 'Creative Solutions';
    with Header.Add<TImage> do
    begin
      Comment := 'Image from another module';
      Source := IncludeURLDelimiter(Module.HostURL)+'doc/logo.png';
    end;
    Header.Use := True;
    Footer.Use := True;

    with Container do
    begin
      with This.Add<TParagraph> do
      begin
        Text := 'Hello Word';
      end;

      with This.Add<TCard>() do
      begin
        Caption := 'Login';

        with This.Add<TForm>() do
        begin
          with This.Add<TInput>('username') do
          begin
            Caption := 'Username';
            PlaceHolder := 'Type user name';
          end;

          with This.Add<TInputPassword>('password') do
          begin
            Caption := 'Password';
          end;

          This.Add<TBreak>;

        end;
      end;
    end;
  end;
end;

{ THomeSchemas }

procedure THomeSchemas.SchemaCreated(Schema: TmnwSchema);
begin
  inherited;
  if Schema is TCustomHomeSchema then
    (Schema as TCustomHomeSchema).Module := Module;
end;

end.

