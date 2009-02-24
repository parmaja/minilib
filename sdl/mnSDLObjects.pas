unit mnSDLObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  LCLType,
  SDL, sdl_ttf;

type
  TmnSDL = class;

  { TmnSDLCustomSurface }

  TmnSDLCustomSurface = class(TObject)
  private
    FSDL: TmnSDL;
    FSDL_Surface: PSDL_Surface;
  //
    FColorDepth: Integer;
    FHeight: Integer;
    FWidth: Integer;
    procedure SetColorDepth(const AValue: Integer);
    procedure SetHeight(const AValue: Integer);
    function GetPixels(y, x: SInt16): Word;
    procedure SetPixels(y, x: SInt16; const AValue: Word);
    procedure SetWidth(const AValue: Integer);
  public
    constructor Create(ASDL: TmnSDL);
    procedure Init; virtual;
    procedure Finish; virtual;
    procedure Update;
    procedure UpdateRect(ARect: TSDL_Rect);
    procedure UpdateRect(x, y: SInt16; w, h: UInt16);
    procedure Blit(Surface: TmnSDLCustomSurface; x, y, w, h: Integer);
    property Pixels[y,x:SInt16] :Word read GetPixels write SetPixels;
    property SDL_Surface: PSDL_Surface read FSDL_Surface;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property ColorDepth: Integer read FColorDepth write SetColorDepth;
  end;

  TmnSDLSurface= class(TmnSDLCustomSurface)
  end;

  TmnSDLScreen = class(TmnSDLCustomSurface)
  end;

  TmnSDL = class(TObject)
  private
    FScreen: TmnSDLScreen;
    FInit: Boolean;
    FTitle: string;
    procedure SetTitle(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    procedure Finish;
    procedure Start;
    procedure Stop;
    procedure Delay(Value:UInt32);
    procedure WaitAnyKey;
    property Title: string read FTitle write SetTitle;
    property Screen: TmnSDLScreen read FScreen;
  end;

  { TSDLTTF }

  TSDLTTF = class(TObject)
  private
    FSDL: TmnSDL;
    FFont: PTTF_font;
  public
    constructor Create(ASDL: TmnSDL);
    destructor Destroy; override;
    procedure Open(Name: string; Size: Integer);
    function Render(Text: string; Color: TColor):TmnSDLCustomSurface;
    function RenderUnicode(Text: widestring; Color: TColor):TmnSDLCustomSurface;
    procedure Close;
  end;

function ColorToSDLColor(Color:TColor):TSDL_Color;

implementation

function ColorToSDLColor(Color:TColor):TSDL_Color;
begin
  Color:=ColorToRGB(Color);
  Result.r := Red(Color);
  Result.g := Green(Color);
  Result.b := Blue(Color);
end;

{ TmnSDLCustomSurface }

procedure TmnSDLCustomSurface.SetHeight(const AValue: Integer);
begin
  if FHeight <> AValue then
  begin
    FHeight := AValue;
  end;
end;

procedure TmnSDLCustomSurface.SetPixels(y, x: SInt16; const AValue: Word);
var
  aPixel: PWord;
begin
  if FSDL_Surface <> nil then
  begin
    aPixel := FSDL_Surface^.pixels;
    Inc(aPixel, (y * Width) + x);
    aPixel^ := AValue;
  end;
end;

procedure TmnSDLCustomSurface.SetColorDepth(const AValue: Integer);
begin
  if FColorDepth <> AValue then
  begin
    FColorDepth :=AValue;
  end;
end;

function TmnSDLCustomSurface.GetPixels(y, x: SInt16): Word;
var
  aPixel: PWord;
begin
  if FSDL_Surface <> nil then
  begin
    aPixel := FSDL_Surface^.pixels;
    Inc(aPixel, (y * Width) + x);
    Result := aPixel^;
  end
  else
    Result := 0;//must raise exception
end;

procedure TmnSDLCustomSurface.SetWidth(const AValue: Integer);
begin
  if FWidth <> AValue then
  begin
    FWidth := AValue;
  end;
end;

constructor TmnSDLCustomSurface.Create(ASDL: TmnSDL);
begin
  inherited Create;
  FSDL :=  ASDL;
  FWidth := 320;
  FHeight := 240;
  FColorDepth := 16;
end;

procedure TmnSDLCustomSurface.Init;
begin
  FSDL_Surface := SDL_SetVideoMode (width, height, colordepth, SDL_SWSURFACE) ;
  if FSDL_Surface = nil then
  begin
     raise Exception.Create('Couldn''t initialize video mode at ' + IntToStr(Width) + 'x' +
              IntToStr(Height) + 'x' + IntToStr(ColorDepth) + 'bpp') ;
     Halt(1)
  end ;
end;

procedure TmnSDLCustomSurface.Finish;
begin
  SDL_FreeSurface(FSDL_Surface);
end;

procedure TmnSDLCustomSurface.Update;
begin
  UpdateRect(0, 0, 0, 0);
end;

procedure TmnSDLCustomSurface.UpdateRect(ARect: TSDL_Rect);
begin
  UpdateRect(ARect.x, ARect.y, ARect.w, ARect.h) ;
end;

procedure TmnSDLCustomSurface.UpdateRect(x, y: SInt16; w, h: UInt16);
begin
  SDL_UpdateRect(FSDL_Surface, x, y, w, h) ;
end;

procedure TmnSDLCustomSurface.Blit(Surface: TmnSDLCustomSurface; x, y, w, h: Integer);
var
  R: TSDL_Rect;
begin
  R.x := x;
  R.y := y;
  R.w := w;
  R.h := h;
  SDL_BlitSurface(Surface.FSDL_Surface, nil, FSDL_Surface, @R);
end;

{ TmnSDL }

procedure TmnSDL.SetTitle(const AValue: string);
begin
  if FTitle <> AValue then
  begin
    FTitle :=AValue;
    if FInit then
      SDL_WM_SetCaption(PChar(FTitle), nil);
  end;
end;

constructor TmnSDL.Create;
begin
  FScreen := TmnSDLScreen.Create(Self);
end;

destructor TmnSDL.Destroy;
begin
  inherited;
  FScreen.Free;
end;

procedure TmnSDL.Init;
begin
  SDL_Init (SDL_INIT_VIDEO) ;
  FScreen.Init;
  SDL_WM_SetCaption(PChar(FTitle), nil);
  FInit := True;
end;

procedure TmnSDL.Finish;
begin
  FInit := False;
end;

procedure TmnSDL.Start;
begin
//   WriteLn('hello world');
end;

procedure TmnSDL.Stop;
begin
   FScreen.Finish;
   SDL_Quit;
end;

procedure TmnSDL.Delay(Value: UInt32);
begin
  SDL_Delay(Value);
end;

procedure TmnSDL.WaitAnyKey;
var
  event : TSDL_Event;
  done: Boolean;
begin
  { Wait for a keystroke, and blit text on mouse press }
  done := false;
  while ( not done ) do
  begin
    while ( SDL_PollEvent( @event ) <> 0 ) do
    begin
      case (event.type_) of
        SDL_KEYDOWN, SDL_QUITEV:
          begin
            Done := true;
          end;
      end;
    end;
  end;
end;

{ TSDLTTF }

constructor TSDLTTF.Create(ASDL: TmnSDL);
begin
  inherited Create;
  FSDL :=  ASDL;
  TTF_Init();
end;

destructor TSDLTTF.Destroy;
begin
  if FFont <> nil then
    Close;
  inherited Destroy;
end;

procedure TSDLTTF.Open(Name: string; Size: Integer);
begin
  if FFont <> nil then
    raise Exception.Create('Already opened');
  FFont := TTF_OpenFont(PChar(Name), Size);
  //FName := Name;
end;

function TSDLTTF.Render(Text: string; Color: TColor): TmnSDLCustomSurface;
var
  aSDL_Surface: PSDL_Surface;
  SDLColor: TSDL_Color;
  SDLColor2: TSDL_Color;
begin
  SDLColor := ColorToSDLColor(Color);
  SDLColor2 := ColorToSDLColor(clRed);
  //aSDL_Surface := TTF_Render_Shaded(FFont, PChar(Text), SDLColor, SDLColor2);
  aSDL_Surface := TTF_RenderText_Blended(FFont, PChar(Text), SDLColor);
  if aSDL_Surface <> nil then
  begin
    Result := TmnSDLSurface.Create(FSDL);
    Result.FSDL_Surface := aSDL_Surface;
  end
  else
    Result := nil;
end;

function TSDLTTF.RenderUnicode(Text: widestring; Color: TColor): TmnSDLCustomSurface;
var
  aSDL_Surface: PSDL_Surface;
  SDLColor: TSDL_Color;
  SDLColor2: TSDL_Color;
  p: Pointer;
begin
  SDLColor := ColorToSDLColor(Color);
  SDLColor2 := ColorToSDLColor(clRed);
  //aSDL_Surface := TTF_RenderText_Shaded(FFont, PChar(Text), SDLColor, SDLColor2);
  p := PWideChar(Text);
  aSDL_Surface := TTF_RenderUNICODE_Blended(FFont, p, SDLColor);
  if aSDL_Surface <> nil then
  begin
    Result := TmnSDLSurface.Create(FSDL);
    Result.FSDL_Surface := aSDL_Surface;
  end
  else
    Result := nil;
end;

procedure TSDLTTF.Close;
begin
  TTF_CloseFont(FFont);
  FFont := nil;
end;

end.

