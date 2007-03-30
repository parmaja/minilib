unit RegMiniSocket;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  SysUtils, Classes, mnCommandServers, mnCommandClients, mnHttpServer;

procedure Register;

implementation

{$R mnHttpServer.dcr}

procedure Register;
begin
  RegisterComponents('Mini Socket', [TmnCommandServer, TmnCommandClient, TmnHttpServer]);
end;

end.
