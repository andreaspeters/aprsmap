unit uaeromuxdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAeromuxAircraft = record
    Found: Boolean;
    ICAO, Registration, Country, SerialNumber, TypeCode, TypeDescription,
    TypeClass, WTC, Manufacturer, OperatorName, OperatorIATA,
    OperatorCountry, OperatorCallsign, YearOfManufacture, Model: String;
    Military, FAAPIA, FAALADD: Integer;
  end;

function EnsureAeromuxDatabase: Boolean;
function LookupAeromuxAircraft(const ICAO: String; out Aircraft: TAeromuxAircraft): Boolean;
function AeromuxDatabasePath: String;
function AeromuxDatabaseStatus: String;

implementation

uses
  fphttpclient, fpjson, jsonparser, sqlite3dyn, ctypes;

const
  LatestReleaseURL = 'https://api.github.com/repos/aeromux/aeromux-db/releases/latest';
  ConfigDirectory = '.config/aprsmap';

function AeromuxDatabasePath: String;
begin
  Result := GetEnvironmentVariable('HOME');
  if Result = '' then Result := ExpandFileName('~');
  Result := IncludeTrailingPathDelimiter(Result) + ConfigDirectory +
    DirectorySeparator + 'aeromux-db.sqlite';
end;

function AeromuxDatabaseStatus: String;
begin
  if FileExists(AeromuxDatabasePath) then
    Result := 'Aeromux DB: ' + AeromuxDatabasePath
  else
    Result := 'Aeromux DB not installed';
end;

function DownloadURL(out URL: String): Boolean;
var
  Client: TFPHTTPClient;
  Root: TJSONData;
  Obj, Asset: TJSONObject;
  Assets: TJSONArray;
  Response: String;
  I: Integer;
begin
  Result := False;
  URL := '';
  Client := TFPHTTPClient.Create(nil);
  Root := nil;
  try
    Client.AddHeader('User-Agent', 'APRSMap');
    Client.AllowRedirect := True;
    Response := Client.Get(LatestReleaseURL);
    Root := GetJSON(Response);
    if not (Root is TJSONObject) then Exit;
    Obj := TJSONObject(Root);
    if not (Obj.Find('assets') is TJSONArray) then Exit;
    Assets := TJSONArray(Obj.Find('assets'));
    for I := 0 to Assets.Count - 1 do
    begin
      if not (Assets.Items[I] is TJSONObject) then Continue;
      Asset := TJSONObject(Assets.Items[I]);
      if (Pos('.sqlite', LowerCase(Asset.Get('name', ''))) > 0) then
      begin
        URL := Asset.Get('browser_download_url', '');
        Break;
      end;
    end;
    Result := URL <> '';
  finally
    Root.Free;
    Client.Free;
  end;
end;

function DownloadDatabase(const Target: String): Boolean;
var
  URL, TempPath: String;
  Client: TFPHTTPClient;
  Stream: TFileStream;
  DownloadSize: Int64;
begin
  Result := False;
  if not DownloadURL(URL) then Exit;
  TempPath := Target + '.download';
  Client := TFPHTTPClient.Create(nil);
  Stream := nil;
  try
    Client.AddHeader('User-Agent', 'APRSMap');
    Client.AllowRedirect := True;
    Stream := TFileStream.Create(TempPath, fmCreate);
    Client.Get(URL, Stream);
    DownloadSize := Stream.Size;
    Stream.Free;
    Stream := nil;
    if DownloadSize < 1024 then Exit;
    Result := RenameFile(TempPath, Target);
  finally
    Stream.Free;
    Client.Free;
    if FileExists(TempPath) and not Result then DeleteFile(TempPath);
  end;
end;

function EnsureAeromuxDatabase: Boolean;
begin
  Result := FileExists(AeromuxDatabasePath);
  if Result then Exit;
  ForceDirectories(ExtractFileDir(AeromuxDatabasePath));
  try
    Result := DownloadDatabase(AeromuxDatabasePath);
  except
    Result := False;
  end;
end;

function SQLiteText(Statement: psqlite3_stmt; Column: Integer): String;
var
  Value: PAnsiChar;
begin
  Value := sqlite3_column_text(Statement, Column);
  if Value = nil then Result := '' else Result := String(Value);
end;

function LookupAeromuxAircraft(const ICAO: String; out Aircraft: TAeromuxAircraft): Boolean;
var
  DB: psqlite3;
  Statement: psqlite3_stmt;
  Key: AnsiString;
  Code: cint;
begin
  FillChar(Aircraft, SizeOf(Aircraft), 0);
  Result := False;
  if (Length(ICAO) <> 6) or not TryStrToInt('$' + ICAO, Code) then Exit;
  if not EnsureAeromuxDatabase then Exit;
  if InitialiseSQLite('libsqlite3.so.0') < 0 then Exit;
  DB := nil;
  Statement := nil;
  try
    if sqlite3_open_v2(PAnsiChar(AnsiString(AeromuxDatabasePath)), @DB,
      SQLITE_OPEN_READONLY, nil) <> SQLITE_OK then Exit;
    if sqlite3_prepare_v2(DB,
      'SELECT aircraft_icao_address, aircraft_registration, aircraft_country, '
      + 'aircraft_serial_number, aircraft_type_code, type_description, '
      + 'type_icao_class, type_wtc, manufacturer_name, operator_name, '
      + 'operator_iata, operator_country, operator_callsign, year, model, '
      + 'faa_pia, faa_ladd, military FROM aircraft_view '
      + 'WHERE aircraft_icao_address = ?1', -1, @Statement, nil) <> SQLITE_OK then Exit;
    Key := AnsiString(UpperCase(ICAO));
    if sqlite3_bind_text(Statement, 1, PAnsiChar(Key), Length(Key),
      sqlite3_destructor_type(SQLITE_TRANSIENT)) <> SQLITE_OK then Exit;
    if sqlite3_step(Statement) <> SQLITE_ROW then Exit;
    Aircraft.Found := True;
    Aircraft.ICAO := SQLiteText(Statement, 0);
    Aircraft.Registration := SQLiteText(Statement, 1);
    Aircraft.Country := SQLiteText(Statement, 2);
    Aircraft.SerialNumber := SQLiteText(Statement, 3);
    Aircraft.TypeCode := SQLiteText(Statement, 4);
    Aircraft.TypeDescription := SQLiteText(Statement, 5);
    Aircraft.TypeClass := SQLiteText(Statement, 6);
    Aircraft.WTC := SQLiteText(Statement, 7);
    Aircraft.Manufacturer := SQLiteText(Statement, 8);
    Aircraft.OperatorName := SQLiteText(Statement, 9);
    Aircraft.OperatorIATA := SQLiteText(Statement, 10);
    Aircraft.OperatorCountry := SQLiteText(Statement, 11);
    Aircraft.OperatorCallsign := SQLiteText(Statement, 12);
    Aircraft.YearOfManufacture := SQLiteText(Statement, 13);
    Aircraft.Model := SQLiteText(Statement, 14);
    Aircraft.FAAPIA := sqlite3_column_int(Statement, 15);
    Aircraft.FAALADD := sqlite3_column_int(Statement, 16);
    Aircraft.Military := sqlite3_column_int(Statement, 17);
    Result := True;
  finally
    if Statement <> nil then sqlite3_finalize(Statement);
    if DB <> nil then sqlite3_close(DB);
  end;
end;

end.
