unit mnFields;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *
 *  TODO  http://docwiki.embarcadero.com/RADStudio/Rio/en/Supporting_Properties_and_Methods_in_Custom_Variants
 *}

 {

1
	n: v
2
	n: v; n1=v1; n2=v2
3
	n: v,x,y; n1=l1,l2; n1=v3


1
v := h[n].value; //v

3
v := h[n].text; //v,x,y; n1=l1,l2; n1=v3
v := h[n].value; //v,x,y
v := h[n].list[1]; //x

v := h[n].items[n1].list[1]; //l2

 }

{$IFDEF fpc}
{$MODE delphi}{$H+}
{$ELSE}
{$DEFINE WINDOWS}
{$M+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DateUtils, Variants, Contnrs,
  {$ifndef FPC} Types,{$endif}
  mnClasses, mnUtils;

implementation


end.
