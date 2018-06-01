unit mnSynHighlighterXHTML;
{$mode objfpc}{$H+}
{**
 *
 *  This file is part of the "Mini Library"
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  SynEdit, SynEditTypes, SynEditHighlighter, mnSynHighlighterMultiProc;

type

  { TSynXHTMLSyn }

  TSynXHTMLSyn = class(TSynMultiProcSyn)
  private
  protected
    function GetSampleSource: string; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitProcessors; override;
  published
  end;

const
  SYNS_LangXHTML = 'HTML/PHP';
  SYNS_FilterXHTML = 'HTML/PHP Files (*.php;*.html;*.phtml;*.inc)|*.php;*.html;*.phtml;*.inc';

implementation

uses
  PHPProcessor, HTMLProcessor;

constructor TSynXHTMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFilter := SYNS_FilterXHTML;
end;

procedure TSynXHTMLSyn.InitProcessors;
begin
  inherited;
  Processors.Add(THTMLProcessor.Create(Self, 'html'));
  Processors.Add(TPHPProcessor.Create(Self, 'php'));
  Processors.Add(TPHPProcessor.Create(Self, 'hh')); //<-- same php temporary ^.^
  Processors.Add(TPlainProcessor.Create(Self, ''));

  Processors.MainProcessor := 'html';
  Processors.DefaultProcessor := 'php';
end;

class function TSynXHTMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangXHTML;
end;

function TSynXHTMLSyn.GetSampleSource: string;
begin
  Result := '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'#13#10 +
    '<html dir="ltr">'#13#10 +
    '  <body class="normal">'#13#10 +
    '  HTML and PHP syntax editor'#13#10 +
    '<?php'#13#10 +
    '// Syntax highlighting'#13#10 +
    '/**'#13#10 +
    ' It is a Documentation comments'#13#10 +
    '*/'#13#10 +
    '  function printNumber()'#13#10 +
    '  {'#13#10 +
    '    $number = 1234;'#13#10 +
    '    print "The number is $number";'#13#10 +
    '    /* '#13#10 +
    '    Multi line comment '#13#10 +
    '    */ '#13#10 +
    '    for ($i = 0; $i <= $number; $i++)'#13#10 +
    '    {'#13#10 +
    '      $x++;'#13#10 +
    '      $x--;'#13#10 +
    '      $x += 1.0;'#13#10 +
    '    }'#13#10 +
    ' }'#13#10 +
    '?>'#13#10 +
    '  </body>'#13#10 +
    '</html>'#13#10;
end;

end.

