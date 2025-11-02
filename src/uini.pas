unit uini;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, inifiles, utypes;

type
  PTAPRSConfig = ^TAPRSConfig;

procedure SaveConfigToFile(Config: PTAPRSConfig);
procedure LoadConfigFromFile(Config: PTAPRSConfig);

implementation

var
  HomeDir, CacheDir, TilesDir: String;

procedure SaveConfigToFile(Config: PTAPRSConfig);
var
  ini : TIniFile;
begin
  // Load config file
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/aprsmap/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'/aprsmap/';
  {$ENDIF}

  ini := TIniFile.Create(HomeDir+'/aprsmap.ini');

  ini.WriteString('MAP', 'provider', Config^.MAPProvider);
  ini.WriteString('MAP', 'LocalTilesDirectory', Config^.LocalTilesDirectory);
  ini.WriteString('APRS', 'mapcache', Config^.MAPCache);
  ini.WriteString('APRS', 'callsign', Config^.Callsign);
  ini.WriteFloat('APRS', 'latitude', Config^.Latitude);
  ini.WriteFloat('APRS', 'longitude', Config^.Longitude);
  ini.WriteInteger('APRS', 'cleanuptime', Config^.CleanupTime);
  ini.WriteInteger('APRS', 'symbol', Config^.AprsSymbol);
  ini.WriteBool('IGATE', 'enable', Config^.IGateEnabled);
  ini.WriteString('IGATE', 'server', Config^.IGateServer);
  ini.WriteInteger('IGATE', 'port', Config^.IGatePort);
  ini.WriteString('IGATE', 'password', Config^.IGatePassword);
  ini.WriteString('IGATE', 'filter', Config^.IGateFilter);
  ini.WriteString('MODES', 'server', Config^.ModeSServer);
  ini.WriteInteger('MODES', 'port', Config^.ModeSPort);
  ini.WriteString('MODES', 'executable', Config^.ModeSExecutable);
  ini.WriteInteger('MAIN', 'posx', Config^.MainPosX);
  ini.WriteInteger('MAIN', 'posy', Config^.MainPosY);
  ini.WriteInteger('MAIN', 'width', Config^.MainWidth);
  ini.WriteInteger('MAIN', 'height', Config^.MainHeight);
end;

procedure LoadConfigFromFile(Config: PTAPRSConfig);
var
  ini : TIniFile;
begin
  // Load config file
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/aprsmap/';
  CacheDir := GetEnvironmentVariable('HOME')+'/.cache/aprsmap/cache/';
  TilesDir := GetEnvironmentVariable('HOME')+'/.cache/aprsmap/tiles/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'/aprsmap/';
  CacheDir := GetEnvironmentVariable('USERPROFILE')+'/aprsmap/cache';
  TilesDir := GetEnvironmentVariable('USERPROFILE')+'/aprsmap/tiles';
  {$ENDIF}

  // create directory structure if it does not exist
  ForceDirectories(HomeDir);
  ForceDirectories(CacheDir);
  ForceDirectories(TilesDir);

  ini := TIniFile.Create(HomeDir+'/aprsmap.ini');
  Config^.MAPProvider := ini.ReadString('MAP', 'provider', 'OpenStreetMap Standard');
  Config^.LocalTilesDirectory := ini.ReadString('MAP', 'LocalTilesDirectory', TilesDir );
  Config^.MAPCache := ini.ReadString('APRS', 'mapcache', CacheDir);
  Config^.Callsign := ini.ReadString('APRS', 'callsign', 'NOCALL');
  Config^.Latitude := ini.ReadFloat('APRS', 'latitude', 34.509410);
  Config^.Longitude := ini.ReadFloat('APRS', 'longitude', -32.892659);
  Config^.CleanupTime := ini.ReadInteger('APRS', 'cleanuptime', 5);
  Config^.AprsSymbol := ini.ReadInteger('APRS', 'symbol', 9);
  Config^.IGateEnabled := ini.ReadBool('IGATE', 'enable', False);
  Config^.IGateServer := ini.ReadString('IGATE', 'server', 'rotate.aprs.net');
  Config^.IGatePort := ini.ReadInteger('IGATE', 'port', 14580);
  Config^.IGatePassword := ini.ReadString('IGATE', 'password', '');
  Config^.IGateFilter := ini.ReadString('IGATE', 'filter', 'r/<LAT>/<LON>/200');
  Config^.ModeSServer := ini.ReadString('MODES', 'server', 'localhost');
  Config^.ModeSPort := ini.ReadInteger('MODES', 'port', 8080);
  Config^.ModeSExecutable := ini.ReadString('MODES', 'executable', '');
  Config^.MainPosX := ini.ReadInteger('MAIN', 'posx', 0);
  Config^.MainPosY := ini.ReadInteger('MAIN', 'posy', 0);
  Config^.MainWidth := ini.ReadInteger('MAIN', 'width', 1574);
  Config^.MainHeight := ini.ReadInteger('MAIN', 'height', 1035);
end;


end.

