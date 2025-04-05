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
  HomeDir, CacheDir: String;

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
  ini.WriteString('APRS', 'mapcache', Config^.MAPCache);
  ini.WriteString('APRS', 'callsign', Config^.Callsign);
  ini.WriteFloat('APRS', 'latitude', Config^.Latitude);
  ini.WriteFloat('APRS', 'longitude', Config^.Longitude);
  ini.WriteInteger('APRS', 'cleanuptime', Config^.CleanupTime);
  ini.WriteBool('IGATE', 'enable', Config^.IGateEnabled);
  ini.WriteString('IGATE', 'server', Config^.IGateServer);
  ini.WriteInteger('IGATE', 'port', Config^.IGatePort);
  ini.WriteString('IGATE', 'password', Config^.IGatePassword);
  ini.WriteString('IGATE', 'filter', Config^.IGateFilter);
end;

procedure LoadConfigFromFile(Config: PTAPRSConfig);
var
  ini : TIniFile;
begin
  // Load config file
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/aprsmap/';
  CacheDir := GetEnvironmentVariable('HOME')+'/.cache/aprsmap/map/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'/aprsmap/';
  CacheDir := GetEnvironmentVariable('USERPROFILE')+'/aprsmap/cache';
  {$ENDIF}

  // create directory structure if it does not exist
  ForceDirectories(HomeDir);
  ForceDirectories(CacheDir);

  ini := TIniFile.Create(HomeDir+'/aprsmap.ini');
  Config^.MAPProvider := ini.ReadString('MAP', 'provider', 'OpenStreetMap Standard');
  Config^.MAPCache := ini.ReadString('APRS', 'mapcache', CacheDir);
  Config^.Callsign := ini.ReadString('APRS', 'callsign', 'NOCALL');
  Config^.Latitude := ini.ReadFloat('APRS', 'latitude', 34.509410);
  Config^.Longitude := ini.ReadFloat('APRS', 'longitude', -32.892659);
  Config^.CleanupTime := ini.ReadInteger('APRS', 'cleanuptime', 5);
  Config^.IGateEnabled := ini.ReadBool('IGATE', 'enable', False);
  Config^.IGateServer := ini.ReadString('IGATE', 'server', 'rotate.aprs.net');
  Config^.IGatePort := ini.ReadInteger('IGATE', 'port', 14580);
  Config^.IGatePassword := ini.ReadString('IGATE', 'password', '');
  Config^.IGateFilter := ini.ReadString('IGATE', 'filter', 'r/<LAT>/<LON>/200');
end;


end.

