unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process, mvGpsObj, Forms, Controls, ComCtrls,
  Generics.Collections;

type
  { Icon Primary meaning
    --------------------
    TABLE    RESULT
     &       RESERVED for possible AUXILLIARY tables (Aug 09)
     /       Primary   symbol Table  (Mostly stations)
     \       Alternate symbol table  (Mostly Objects)
     0-9     Alternate OVERLAY symbols with 0-9 overlayed
     A-Z     Alternate OVERLAY symbols with A-Z overlayed
  }

  TDoubleList = specialize TList<Double>;

  TRS41SGPData = record
    Enabled: Boolean;
    clb: TDoubleList;   // Climprate m/s
    o3: TDoubleList;    // Ozone concentration in hPa
    t: TDoubleList;     // Air Temperature
    ti: TDoubleList;    // Temperature internal °C
    pump: TDoubleList;  // How much A the o3 pumb consume in mA
    batt: TDoubleList;  // Batterie State in V
    dist: TDoubleList;  // Distance in km (I still don't know to what)
    rssi: TDoubleList;  // Receive in dB
    p: TDoubleList;     // Air Pressure in hPa
    h: TDoubleList;     // Humidity in %
    sats: TDoubleList;  // Number of GPS Sats
  end;

  TDevices = Record
    RS41: TRS41SGPData
  end;

  TAPRSMessage = record
    FromCall: String;
    ToCall: String;
    Path: String;
    DataType: String;
    IconPrimary: String;
    Icon: String;
    Longitude: Double;
    Latitude: Double;
    Altitude: TDoubleList;
    Course: Double;
    Speed: TDoubleList;
    PHGPower: Double;
    PHGHeight: Double;
    PHGGain: Double;
    PHGDirectivity: String;
    DFSStrength: Double;
    DFSHeight: Double;
    DFSGain: Double;
    DFSDirectivity: String;
    RNGRange: Double;
    WXDirection: TDoubleList;
    WXSpeed: TDoubleList;
    WXGust: TDoubleList;
    WXTemperature: TDoubleList;
    WXPressure: TDoubleList;
    WXLum: TDoubleList;
    WXSnowFall: TDoubleList;
    WXRainCount: TDoubleList;
    WXRainFall1h: TDoubleList;
    WXRainFall24h: TDoubleList;
    WXRainFallToday: TDoubleList;
    WXHumidity: TDoubleList;
    Message: String;
    ID: Integer;
    Time: TTime;
    Track: TGPSTrack;
    TrackID: Integer;
    EnableTrack: Boolean;
    ImageIndex: Integer;
    ImageDescription: String;
    ModeS: Boolean;
    Distance: Double;
    RAWMessages: TStringList;
    Count: Integer;
    Visible: Boolean;
    ActiveTabSheet: TTabSheet;
    Devices: TDevices;
    Checksum: String;
  end;

  TAPRSConfig = record
    MAPCache: String;
    MAPProvider: String;
    LocalTilesDirectory: String;
    IGateEnabled: Boolean;
    IGateServer: String;
    IGatePort: Integer;
    IGatePassword: String;
    IGateFilter: String;
    Callsign: String;
    AprsSymbol: Integer;
    Latitude: Double;
    Longitude: Double;
    CleanupTime: Integer;
    ModeSEnabled: Boolean;
    ModeSServer: String;
    ModeSPort: Integer;
    ModeSExecutable: String;
    MainPosX: Integer;
    MainPosY: Integer;
    MainWidth: Integer;
    MainHeight: Integer;
    LastSeenPosX: Integer;
    LastSeenPosY: Integer;
    LastSeenWidth: Integer;
    LastSeenHeight: Integer;
    LastSeenVisible: Boolean;
    RawMessagePosX: Integer;
    RawMessagePosY: Integer;
    RawMessageWidth: Integer;
    RawMessageHeight: Integer;
    RawMessageVisible: Boolean;
    GPSdEnabled: Boolean;
    GPSdHost: String;
    GPSdPort: Integer;
  end;

  PTAPRSConfig = ^TAPRSConfig;

  TAPRSSymbol = record
    SymbolChar: Char;      // Zeichen des Symbols
    Description: String;   // Beschreibung des Symbols
  end;

  TPHGCode = record
    Code: Byte;
    Value: String;
  end;

  ArrayOfPHGCode = array[0..9] of TPHGCode;
  PAPRSMessage = ^TAPRSMessage;
  PAPRSConfig = ^TAPRSConfig;

const
  PHGPowerCodeTable: ArrayOfPHGCode = (
    (Code: 0; Value: '0'),
    (Code: 1; Value: '1'),
    (Code: 2; Value: '4'),
    (Code: 3; Value: '9'),
    (Code: 4; Value: '16'),
    (Code: 5; Value: '25'),
    (Code: 6; Value: '36'),
    (Code: 7; Value: '49'),
    (Code: 8; Value: '64'),
    (Code: 9; Value: '81')
  );

  PHGHeightCodeTable: ArrayOfPHGCode = (
    (Code: 0; Value: '10'),
    (Code: 1; Value: '20'),
    (Code: 2; Value: '40'),
    (Code: 3; Value: '80'),
    (Code: 4; Value: '160'),
    (Code: 5; Value: '320'),
    (Code: 6; Value: '640'),
    (Code: 7; Value: '1280'),
    (Code: 8; Value: '2560'),
    (Code: 9; Value: '5120')
  );

  PHGGainCodeTable: ArrayOfPHGCode = (
    (Code: 0; Value: '0'),
    (Code: 1; Value: '1'),
    (Code: 2; Value: '2'),
    (Code: 3; Value: '3'),
    (Code: 4; Value: '4'),
    (Code: 5; Value: '5'),
    (Code: 6; Value: '6'),
    (Code: 7; Value: '7'),
    (Code: 8; Value: '8'),
    (Code: 9; Value: '9')
  );

  PHGDirectivityCodeTable: ArrayOfPHGCode = (
    (Code: 0; Value: 'omni'),
    (Code: 1; Value: '45 NE'),
    (Code: 2; Value: '90 E'),
    (Code: 3; Value: '135 SE'),
    (Code: 4; Value: '180 S'),
    (Code: 5; Value: '225 SW'),
    (Code: 6; Value: '270 W'),
    (Code: 7; Value: '315 NW'),
    (Code: 8; Value: '360 N'),
    (Code: 8; Value: '')
  );

  DFSStrengthCodeTable: ArrayOfPHGCode = (
    (Code: 0; Value: '0'),
    (Code: 1; Value: '1'),
    (Code: 2; Value: '2'),
    (Code: 3; Value: '3'),
    (Code: 4; Value: '4'),
    (Code: 5; Value: '5'),
    (Code: 6; Value: '6'),
    (Code: 7; Value: '7'),
    (Code: 8; Value: '8'),
    (Code: 9; Value: '9')
  );

  APRSPrimarySymbolTable: array[1..94] of TAPRSSymbol = (
    (SymbolChar: '!'; Description: 'Police, Sheriff, Law Enforcement'),
    (SymbolChar: '"'; Description: 'No Symbol'),
    (SymbolChar: '#'; Description: 'Digi'),
    (SymbolChar: '$'; Description: 'Phone'),
    (SymbolChar: '%'; Description: 'DX Cluster'),
    (SymbolChar: '&'; Description: 'HF Gateway'),
    (SymbolChar: ''''; Description: 'Small Aircraft'),
    (SymbolChar: '('; Description: 'Mobile Satellite Station'),
    (SymbolChar: ')'; Description: 'Wheelchair (Handicapped)'),
    (SymbolChar: '*'; Description: 'Snowmobile'),
    (SymbolChar: '+'; Description: 'Red Cross'),
    (SymbolChar: ','; Description: 'Boy Scouts'),
    (SymbolChar: '-'; Description: 'House (QTH)'),
    (SymbolChar: '.'; Description: 'X (cross, mark)'),
    (SymbolChar: '/'; Description: 'Red Dot'),
    (SymbolChar: '0'; Description: 'Circle (0)'),
    (SymbolChar: '1'; Description: 'Circle (1)'),
    (SymbolChar: '2'; Description: 'Circle (2)'),
    (SymbolChar: '3'; Description: 'Circle (3)'),
    (SymbolChar: '4'; Description: 'Circle (4)'),
    (SymbolChar: '5'; Description: 'Circle (5)'),
    (SymbolChar: '6'; Description: 'Circle (6)'),
    (SymbolChar: '7'; Description: 'Circle (7)'),
    (SymbolChar: '8'; Description: 'Circle (8)'),
    (SymbolChar: '9'; Description: 'Circle (9)'),
    (SymbolChar: ':'; Description: 'Fire'),
    (SymbolChar: ';'; Description: 'Campground'),
    (SymbolChar: '<'; Description: 'Motorcycle'),
    (SymbolChar: '='; Description: 'Railroad Engine'),
    (SymbolChar: '>'; Description: 'Car'),
    (SymbolChar: '?'; Description: 'File Server'),
    (SymbolChar: '@'; Description: 'HC Future'),
    (SymbolChar: 'A'; Description: 'Aid Station'),
    (SymbolChar: 'B'; Description: 'BBS'),
    (SymbolChar: 'C'; Description: 'Canoe'),
    (SymbolChar: 'D'; Description: 'No Symbol'),
    (SymbolChar: 'E'; Description: 'Eyeball'),
    (SymbolChar: 'F'; Description: 'Tractor'),
    (SymbolChar: 'G'; Description: 'Grid Square'),
    (SymbolChar: 'H'; Description: 'Hotel'),
    (SymbolChar: 'I'; Description: 'TCP/IP'),
    (SymbolChar: 'J'; Description: 'No Symbol'),
    (SymbolChar: 'K'; Description: 'School'),
    (SymbolChar: 'L'; Description: 'User Log-ON'),
    (SymbolChar: 'M'; Description: 'MacAPRS'),
    (SymbolChar: 'N'; Description: 'NTS Station'),
    (SymbolChar: 'O'; Description: 'Balloon'),
    (SymbolChar: 'P'; Description: 'Police'),
    (SymbolChar: 'Q'; Description: 'TBD'),
    (SymbolChar: 'R'; Description: 'Rec Vehicle'),
    (SymbolChar: 'S'; Description: 'Shuttle'),
    (SymbolChar: 'T'; Description: 'SSTV'),
    (SymbolChar: 'U'; Description: 'Bus'),
    (SymbolChar: 'V'; Description: 'ATV'),
    (SymbolChar: 'W'; Description: 'WX Service'),
    (SymbolChar: 'X'; Description: 'Helicopter'),
    (SymbolChar: 'Y'; Description: 'Yacht'),
    (SymbolChar: 'Z'; Description: 'WinAPRS'),
    (SymbolChar: '['; Description: 'Jogger'),
    (SymbolChar: '\'; Description: 'Triangle'),
    (SymbolChar: ']'; Description: 'Jobber'),
    (SymbolChar: '^'; Description: 'Large Aircraft'),
    (SymbolChar: '_'; Description: 'WX Station'),
    (SymbolChar: '`'; Description: 'Dish Antenna'),
    (SymbolChar: 'a'; Description: 'Ambulance'),
    (SymbolChar: 'b'; Description: 'Bike'),
    (SymbolChar: 'c'; Description: 'ICP'),
    (SymbolChar: 'd'; Description: 'Fire Station'),
    (SymbolChar: 'e'; Description: 'Horse'),
    (SymbolChar: 'f'; Description: 'Fire Truck'),
    (SymbolChar: 'g'; Description: 'Glider'),
    (SymbolChar: 'h'; Description: 'Hospital'),
    (SymbolChar: 'i'; Description: 'IOTA'),
    (SymbolChar: 'j'; Description: 'Jeep'),
    (SymbolChar: 'k'; Description: 'Truck'),
    (SymbolChar: 'l'; Description: 'Laptop'),
    (SymbolChar: 'm'; Description: 'Mic-E Repeater'),
    (SymbolChar: 'n'; Description: 'Node'),
    (SymbolChar: 'o'; Description: 'EOC'),
    (SymbolChar: 'p'; Description: 'Rover'),
    (SymbolChar: 'q'; Description: 'Grid Square'),
    (SymbolChar: 'r'; Description: 'Antenna'),
    (SymbolChar: 's'; Description: 'Power Boat'),
    (SymbolChar: 't'; Description: 'Truck Stop'),
    (SymbolChar: 'u'; Description: 'Truck 18wh'),
    (SymbolChar: 'v'; Description: 'Van'),
    (SymbolChar: 'w'; Description: 'Water Station'),
    (SymbolChar: 'x'; Description: 'XAPRS'),
    (SymbolChar: 'y'; Description: 'Yagi'),
    (SymbolChar: 'z'; Description: 'Shelter'),
    (SymbolChar: '{'; Description: 'No Symbol'),
    (SymbolChar: '|'; Description: 'TNC Stream Sw'),
    (SymbolChar: '}'; Description: 'No Symbol'),
    (SymbolChar: '~'; Description: 'TNC Stream Sw')
  );

  APRSAlternateSymbolTable: array[1..94] of TAPRSSymbol = (
    (SymbolChar: '!'; Description: 'Emergency'),
    (SymbolChar: '"'; Description: 'No Symbol'),
    (SymbolChar: '#'; Description: 'No. Digi'),
    (SymbolChar: '$'; Description: 'Bank'),
    (SymbolChar: '%'; Description: 'No Symbol'),
    (SymbolChar: '&'; Description: 'No. Diamond'),
    (SymbolChar: ''''; Description: 'Crash site'),
    (SymbolChar: '('; Description: 'Cloudy'),
    (SymbolChar: ')'; Description: 'MEO'),
    (SymbolChar: '*'; Description: 'Snow'),
    (SymbolChar: '+'; Description: 'Church'),
    (SymbolChar: ','; Description: 'Girl Scouts'),
    (SymbolChar: '-'; Description: 'House (HF)'),
    (SymbolChar: '.'; Description: 'Unknown Position'),
    (SymbolChar: '/'; Description: 'Destination'),
    (SymbolChar: '0'; Description: 'No Symbol'),
    (SymbolChar: '1'; Description: 'No Symbol'),
    (SymbolChar: '2'; Description: 'No Symbol'),
    (SymbolChar: '3'; Description: 'No Symbol'),
    (SymbolChar: '4'; Description: 'No Symbol'),
    (SymbolChar: '5'; Description: 'No Symbol'),
    (SymbolChar: '6'; Description: 'No Symbol'),
    (SymbolChar: '7'; Description: 'No Symbol'),
    (SymbolChar: '8'; Description: 'No Symbol'),
    (SymbolChar: '9'; Description: 'Petrol Station'),
    (SymbolChar: ':'; Description: 'Hail'),
    (SymbolChar: ';'; Description: 'Park'),
    (SymbolChar: '<'; Description: 'Gale F1'),
    (SymbolChar: '='; Description: 'No Symbol'),
    (SymbolChar: '>'; Description: 'No. Car'),
    (SymbolChar: '?'; Description: 'Info Kiosk'),
    (SymbolChar: '@'; Description: 'Hurricane'),
    (SymbolChar: 'A'; Description: 'No. Box'),
    (SymbolChar: 'B'; Description: 'Snow Blowing'),
    (SymbolChar: 'C'; Description: 'Coast Guard'),
    (SymbolChar: 'D'; Description: 'Drizzle'),
    (SymbolChar: 'E'; Description: 'Smoke'),
    (SymbolChar: 'F'; Description: 'Frozen Rain'),
    (SymbolChar: 'G'; Description: 'Snow Shower'),
    (SymbolChar: 'H'; Description: 'Haze'),
    (SymbolChar: 'I'; Description: 'Rain Shower'),
    (SymbolChar: 'J'; Description: 'Lightning'),
    (SymbolChar: 'K'; Description: 'Kenwood'),
    (SymbolChar: 'L'; Description: 'Lighthouse'),
    (SymbolChar: 'M'; Description: 'No Symbol'),
    (SymbolChar: 'N'; Description: 'Nav Buoy'),
    (SymbolChar: 'O'; Description: 'Rocket'),
    (SymbolChar: 'P'; Description: 'Parking'),
    (SymbolChar: 'Q'; Description: 'Quake'),
    (SymbolChar: 'R'; Description: 'Restaurant'),
    (SymbolChar: 'S'; Description: 'Sat/Pacsat'),
    (SymbolChar: 'T'; Description: 'Thunderstorm'),
    (SymbolChar: 'U'; Description: 'Sunny'),
    (SymbolChar: 'V'; Description: 'VORTAC'),
    (SymbolChar: 'W'; Description: 'No. WXS'),
    (SymbolChar: 'X'; Description: 'Pharmavy'),
    (SymbolChar: 'Y'; Description: 'No Symbol'),
    (SymbolChar: 'Z'; Description: 'No Symbol'),
    (SymbolChar: '['; Description: 'Wall Cloud'),
    (SymbolChar: '\'; Description: 'No Symbol'),
    (SymbolChar: ']'; Description: 'No Symbol'),
    (SymbolChar: '^'; Description: 'No. Plane'),
    (SymbolChar: '_'; Description: 'No WX Station'),
    (SymbolChar: '`'; Description: 'Rain'),
    (SymbolChar: 'a'; Description: 'No. Diamond'),
    (SymbolChar: 'b'; Description: 'Dust Blowing'),
    (SymbolChar: 'c'; Description: 'No. CivDef'),
    (SymbolChar: 'd'; Description: 'DX Spot'),
    (SymbolChar: 'e'; Description: 'Sleet'),
    (SymbolChar: 'f'; Description: 'Funnel Cloud'),
    (SymbolChar: 'g'; Description: 'Gale'),
    (SymbolChar: 'h'; Description: 'HAM Store'),
    (SymbolChar: 'i'; Description: 'No. Black Box'),
    (SymbolChar: 'j'; Description: 'Work Zone'),
    (SymbolChar: 'k'; Description: 'SUV'),
    (SymbolChar: 'l'; Description: 'Area Locations'),
    (SymbolChar: 'm'; Description: 'Milepost'),
    (SymbolChar: 'n'; Description: 'No. Triangle'),
    (SymbolChar: 'o'; Description: 'Circle sm'),
    (SymbolChar: 'p'; Description: 'Part Cloid'),
    (SymbolChar: 'q'; Description: 'No Symbol'),
    (SymbolChar: 'r'; Description: 'Restrooms'),
    (SymbolChar: 's'; Description: 'No. Boat'),
    (SymbolChar: 't'; Description: 'Tornado'),
    (SymbolChar: 'u'; Description: 'No. Truck'),
    (SymbolChar: 'v'; Description: 'No. Van'),
    (SymbolChar: 'w'; Description: 'Flooding'),
    (SymbolChar: 'x'; Description: 'No Symbol'),
    (SymbolChar: 'y'; Description: 'Sky Warm'),
    (SymbolChar: 'z'; Description: 'No. Shelter'),
    (SymbolChar: '{'; Description: 'Fog'),
    (SymbolChar: '|'; Description: 'TNC Stream Sw'),
    (SymbolChar: '}'; Description: 'No Symbol'),
    (SymbolChar: '~'; Description: 'TNC Stream Sw')
  );


procedure RestartApplication;
procedure PrependDoubleList(Dest: TDoubleList; const Src: TDoubleList);
function FahrenheitToCelsius(F: Double): Double;
function FahrenheitToCelsius(F: String): String;
function NormalizeString(Data: AnsiString): AnsiString;

implementation

procedure RestartApplication;
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := ParamStr(0);
    Process.Execute;
    Halt(0);
  finally
    Process.Free;
  end;
end;


procedure PrependDoubleList(Dest: TDoubleList; const Src: TDoubleList);
var
  i: Integer;
begin
  try
    if not Assigned(Dest) or not Assigned(Src) then
      Exit;

    if Src.Count > 0 then
      for i := Src.Count - 1 downto 0 do
        Dest.Insert(0, Src[i]);
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln(Format('PrependDoubleList Error: %s', [E.Message]));
      {$ENDIF}
    end;
  end;
end;

function FahrenheitToCelsius(F: Double): Double;
begin
  Result := Round((F - 32.0) * 5.0 / 9.0);
end;

function FahrenheitToCelsius(F: String): String;
begin
  Result := '';

  try
    Result := FloatToStrF((StrToFloat(F) - 32.0) * 5.0 / 9.0, ffFixed, 8, 0);
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln(Format('FahrenheitToCelsius Error: %s', [E.Message]));
      {$ENDIF}
    end;
  end;
end;

function NormalizeString(Data: AnsiString): AnsiString;
begin
  // Normalize > CRLF (#13#10)
  Data   := StringReplace(Data, #13#10, #10, [rfReplaceAll]); // CRLF -> LF
  Data   := StringReplace(Data, #13, #10, [rfReplaceAll]);    // CR -> LF
  Result := StringReplace(Data, #10, #13#10, [rfReplaceAll]); // LF -> CRLF
end;

end.

