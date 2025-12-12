unit ugpsd;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, IdTCPClient, IdIOHandler;

type
  { TGpsThread }
  TGpsThread = class(TThread)
  protected
    procedure Execute; override;
  public
  end;

implementation

{ TGpsThread }

procedure TGpsThread.Execute;
var Client: TIdTCPClient;
    Response: String;
begin
  Client := TIdTCPClient.Create(nil);
  try
    Client.Host := '127.0.0.1';
    Client.Port := 2947;
    Client.Connect;
    Response := Client.IOHandler.ReadLn;
  finally
    Client.Free;
  end;
end;

end.
