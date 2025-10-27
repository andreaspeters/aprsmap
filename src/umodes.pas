unit umodes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, utypes, RegExpr, Contnrs, fphttpclient;

type
  PTAPRSConfig = ^TAPRSConfig;

  TAircraftData = record
    // {"hex":"3c55c3", "flight":"", "lat":53.589772, "lon":9.904902, "altitude":6850, "track":219, "speed":201}

    Hex: String;
    Altitude: Double;
    Latitude: Double;
    Longitude: Double;
    Track: Integer;
    Speed: Integer;
    Flight: String;
  end;
  PTAircraftData = ^TAircraftData;


  { TModeSThread }
  TModeSThread = class(TThread)
  private
    FConfig: PTAPRSConfig;
  protected
    procedure Execute; override;
  public
    ModeSMessageList: TFPHashList;
    procedure LoadAircraftsFromDump1090;
    constructor Create(Config: PTAPRSConfig);
  end;


implementation

{ TModeSThread }

constructor TModeSThread.Create(Config: PTAPRSConfig);
begin
  inherited Create(True);
  FConfig := Config;
  FreeOnTerminate := True;
  ModeSMessageList := TFPHashList.Create;
  Start;
end;

procedure TModeSThread.Execute;
begin
  while not Terminated do
  begin
    LoadAircraftsFromDump1090;
    sleep(1000);
  end;
end;

procedure TModeSThread.LoadAircraftsFromDump1090;
var Response: String;
    Obj: TJSONObject;
    Items: TJSONArray;
    Aircraft, tmp: PTAircraftData;
    i: Integer;

begin
  // Simuliere das Empfangen einer JSON-Zeichenkette (in der Realität würde
  // diese Zeichenkette von einem Netzwerk kommen)
  Response := '[{"hex":"3c55c3", "flight":"TESTFLUG", "lat":53.589772, "lon":9.904902, "altitude":6850, "track":219, "speed":201}]';

  Response := TFPHTTPClient.SimpleGet('http://localhost:8888/data.json');

  if Length(Response) > 0 then
  begin
    try
      Items := TJSONArray(GetJSON(Response));
      if Items.Count <= 0 then
        Exit;

      for i := 0 to Items.Count - 1 do
      begin
        Obj := Items.Objects[i];

        New(AirCraft);
        if Obj.Find('hex') <> nil then
          AirCraft^.Hex := Obj.Strings['hex'];

        if Obj.Find('flight') <> nil then
          AirCraft^.Flight := StringReplace(Obj.Strings['flight'], ' ', '', [rfReplaceAll]);

        if Obj.Find('lat') <> nil then
          AirCraft^.Latitude := Obj.Floats['lat'];

        if Obj.Find('lon') <> nil then
          AirCraft^.Longitude := Obj.Floats['lon'];

        if Obj.Find('altitude') <> nil then
          AirCraft^.Altitude := Obj.Integers['altitude'];

        if Obj.Find('speed') <> nil then
          AirCraft^.Speed := Obj.Integers['speed'];

        if Obj.Find('track') <> nil then
          AirCraft^.Track := Obj.Integers['track'];

        tmp := ModeSMessageList.Find(AirCraft^.Flight);
        if (not Assigned(tmp)) and (Length(AirCraft^.Flight) > 0) then
          ModeSMessageList.Add(AirCraft^.Flight, Aircraft)
      end;
    except
      on E: Exception do
      begin
      {$IFDEF UNIX}
        writeln('JSON Error: ' + E.Message);
      {$ENDIF}
      end;
    end;
  end;
end;

end.

