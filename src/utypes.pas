unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process;

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

  TAPRSMessage = record
    FromCall: String;
    ToCall: String;
    Path: String;
    DataType: String;
    IconPrimary: String;
    Icon: String;
    Longitude: Double;
    Latitude: Double;
    Altitude: Integer;
    Course: Integer;
    Speed: Integer;
    PHGPower: Byte;
    PHGHeight: Integer;
    PHGGain: Byte;
    PHGDirectivity: String;
    DFSStrength: Byte;
    DFSHeight: Integer;
    DFSGain: Byte;
    DFSDirectivity: String;
    RNGRange: Integer;
    WXDirection: Integer;
    WXSpeed: Integer;
    WXGust: Integer;
    WXTemperature: Integer;
    WXPressure: Double;
    WXLum: Integer;
    WXSnowFall: Integer;
    WXRainCount: Integer;
    WXRainFall1h: Integer;
    WXRainFall24h: Integer;
    WXRainFallToday: Integer;
    WXHumidity: Byte;
    Message: String;
    ID: Integer;
    Time: TTime;
    Track: Boolean;
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
    Latitude: Double;
    Longitude: Double;
    CleanupTime: Integer;
  end;

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

  APRSPrimarySymbolTable: array[1..90] of TAPRSSymbol = (
    (SymbolChar: '!'; Description: 'Police, Sheriff, Law Enforcement'),
    (SymbolChar: '"'; Description: 'Reserved (obsolete)'),
    (SymbolChar: '#'; Description: 'No Symbol, Overlay is a Number'),
    (SymbolChar: '$'; Description: 'Reserved (obsolete)'),
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
    (SymbolChar: '/'; Description: 'Reserved (Do not use)'),
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
    (SymbolChar: ':'; Description: 'Fire Station'),
    (SymbolChar: ';'; Description: 'Campground'),
    (SymbolChar: '<'; Description: 'Restrooms or Toilet'),
    (SymbolChar: '='; Description: 'Railroad Engine'),
    (SymbolChar: '>'; Description: 'Car'),
    (SymbolChar: '?'; Description: 'Information Kiosk'),
    (SymbolChar: '@'; Description: 'Hurricane or Weather Station'),
    (SymbolChar: 'A'; Description: 'Ambulance'),
    (SymbolChar: 'B'; Description: 'Bike'),
    (SymbolChar: 'C'; Description: 'Incident Command Post'),
    (SymbolChar: 'D'; Description: 'Fire Department'),
    (SymbolChar: 'E'; Description: 'Horse'),
    (SymbolChar: 'F'; Description: 'Dog'),
    (SymbolChar: 'G'; Description: 'Grid Square'),
    (SymbolChar: 'H'; Description: 'Hotel'),
    (SymbolChar: 'I'; Description: 'TCP/IP'),
    (SymbolChar: 'J'; Description: 'School'),
    (SymbolChar: 'K'; Description: 'Kennel (Dog)'),
    (SymbolChar: 'L'; Description: 'Library'),
    (SymbolChar: 'M'; Description: 'Mic-Repeater Input'),
    (SymbolChar: 'N'; Description: 'Node (Internet/Network)'),
    (SymbolChar: 'O'; Description: 'EOC'),
    (SymbolChar: 'P'; Description: 'Parking'),
    (SymbolChar: 'Q'; Description: 'Bus'),
    (SymbolChar: 'R'; Description: 'Restaurant'),
    (SymbolChar: 'S'; Description: 'Satellite Ground Station'),
    (SymbolChar: 'T'; Description: 'Tree'),
    (SymbolChar: 'U'; Description: 'Truck'),
    (SymbolChar: 'V'; Description: 'Van'),
    (SymbolChar: 'W'; Description: 'Watercraft'),
    (SymbolChar: 'X'; Description: 'Wreck or Obstruction'),
    (SymbolChar: 'Y'; Description: 'Yacht'),
    (SymbolChar: 'Z'; Description: 'Reserved (obsolete)'),
    (SymbolChar: '['; Description: 'Shelter'),
    (SymbolChar: '\'; Description: 'Reserved (obsolete)'),
    (SymbolChar: ']'; Description: 'Reserved (obsolete)'),
    (SymbolChar: '^'; Description: 'Large Aircraft'),
    (SymbolChar: '_'; Description: 'Weather Station'),
    (SymbolChar: '`'; Description: 'Dish Antenna'),
    (SymbolChar: 'a'; Description: 'Aid Station'),
    (SymbolChar: 'b'; Description: 'Black Spot (Danger)'),
    (SymbolChar: 'c'; Description: 'Church'),
    (SymbolChar: 'd'; Description: 'digi (Digital Repeater)'),
    (SymbolChar: 'e'; Description: 'Eyeball (Meeting)'),
    (SymbolChar: 'f'; Description: 'Farm Vehicle'),
    (SymbolChar: 'g'; Description: 'Grid Square'),
    (SymbolChar: 'h'; Description: 'Hospital'),
    (SymbolChar: 'i'; Description: 'Information Source'),
    (SymbolChar: 'j'; Description: 'Police or Sheriff'),
    (SymbolChar: 'k'; Description: 'Kenwood Radio'),
    (SymbolChar: 'l'; Description: 'Lighthouse'),
    (SymbolChar: 'm'; Description: 'Mac-APRS'),
    (SymbolChar: 'n'; Description: 'Network Node'),
    (SymbolChar: 'o'; Description: 'Outpost or Tent'),
    (SymbolChar: 'p'; Description: 'Parking'),
    (SymbolChar: 'q'; Description: 'Quad'),
    (SymbolChar: 'r'; Description: 'Restaurant'),
    (SymbolChar: 's'; Description: 'Server or SysOp'),
    (SymbolChar: 't'; Description: 'Truck Stop'),
    (SymbolChar: 'u'; Description: 'UFO'),
    (SymbolChar: 'v'; Description: 'Van'),
    (SymbolChar: 'w'; Description: 'Weather Balloon'),
    (SymbolChar: 'x'; Description: 'Crossroad'),
    (SymbolChar: 'y'; Description: 'Yacht'),
    (SymbolChar: 'z'; Description: 'Reserved (obsolete)')
  );


procedure RestartApplication;

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

end.

