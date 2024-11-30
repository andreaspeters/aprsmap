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

  ini.WriteString('APRS', 'mapcache', Config^.MAPCache);
end;

procedure LoadConfigFromFile(Config: PTAPRSConfig);
var
  ini : TIniFile;
  EXE: string;
begin
  EXE := '';

  // Load config file
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/aprsmap/';
  CacheDir := GetEnvironmentVariable('HOME')+'/.cache/aprsmap/map/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'/aprsmap/';
  CacheDir := GetEnvironmentVariable('USERPROFILE')+'/aprsmap/cache';
  EXE := '.exe';
  {$ENDIF}

  // create directory structure if it does not exist
  ForceDirectories(HomeDir);
  ForceDirectories(CacheDir);

  ini := TIniFile.Create(HomeDir+'/aprsmap.ini');
  Config^.MAPCache := ini.ReadString('APRS', 'mapcache', CacheDir);
end;


end.
