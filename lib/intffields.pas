unit intffields;

{$mode delphi}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils; 

type
  IField = interface
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsCurrency: Currency;
    procedure SetAsCurrency(const Value: Currency);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const Value: TDateTime);
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
  end;

implementation

end.

