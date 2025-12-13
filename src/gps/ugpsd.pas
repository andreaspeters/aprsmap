unit ugpsd;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, IdTCPClient, IdIOHandler, utypes;

type
  { TGpsThread }
  TGpsThread = class(TThread)
  private
    FConfig: PAPRSConfig;
  protected
    procedure Execute; override;
  public
    Latitude: Double;
    Longitude: Double;
    Altitude: Double;
    Speed: Double;
    Climb: Double;
    constructor Create(Config: PAPRSConfig);
  end;

implementation

{ TGpsThread }

constructor TGpsThread.Create(Config: PAPRSConfig);
begin
  inherited Create(True);
  FConfig := Config;
end;

procedure TGpsThread.Execute;
var Client: TIdTCPClient;
    Response: String;
    JsonObj  : TJSONObject;
begin
  if not FConfig^.GPSdEnabled then
    Exit;

  Client := TIdTCPClient.Create(nil);
  try
    Client.Host := FConfig^.GPSdHost;
    Client.Port := FConfig^.GPSdPort;
    Client.Connect;
    Client.IOHandler.WriteLn('?WATCH={"json":true,"enable":true}');
  except
    on E: Exception do
    begin
      Client.Free;
      Client := Nil;
      Exit;
    end;
  end;

  while not Terminated do
  begin
    try
      Client.IOHandler.ReadTimeout := 500;
      Response := Client.IOHandler.ReadLn;

      if Length(Response) > 0 then
      begin
        if GetJSON(Response).JSONType = jtObject then
        begin
          JsonObj := TJSONObject(GetJSON(Response));
          if Assigned(JsonObj) and (JsonObj.Find('class') <> nil) then
            if JsonObj.Find('class').AsString = 'TPV' then
            begin
              JsonObj := TJSONObject(GetJSON(Response));

              if JsonObj.Find('lat') <> nil then
                Latitude := JsonObj.Find('lat').AsFloat;

              if JsonObj.Find('lon') <> nil then
                Longitude := JsonObj.Find('lon').AsFloat;

              if JsonObj.Find('alt') <> nil then
                Altitude := JsonObj.Find('alt').AsFloat;

              if JsonObj.Find('speed') <> nil then
                Speed := JsonObj.Find('speed').AsFloat;

              if JsonObj.Find('climb') <> nil then
                Climb := JsonObj.Find('climb').AsFloat;
            end;
        end;
      end;
    except
      on E: Exception do
      begin
        writeln('Error GPSd Response Loop:', E.Message);
      end;
    end;

    Sleep(200);
  end;
  JsonObj.Free;
  Client.Free;
end;

end.
