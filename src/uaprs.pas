unit uaprs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, utypes, SysUtils, ExtCtrls, Forms, Controls, Graphics, Dialogs,
  mvGPSObj, Contnrs, mvMapViewer, mvTypes, RegExpr, Math;

procedure DelPoI(Layer: TMapLayer; const Call: String);
procedure SetPoi(Layer: TMapLayer; Message: PAPRSMessage; List: TGPSObjectList);
procedure SetPoI(Layer: TMapLayer; const Latitude, Longitude: Double; const Text: String; const visibility: Boolean; const ImageIndex: Integer; List: TGPSObjectList);
procedure ConvertNMEAToLatLong(const NMEALat, NMEALon: string; out Latitude, Longitude: Double; const divider: Integer);
function GetImageIndex(const Symbol, IconPrimary: String):Byte;
function LatLonToLocator(const Latitude, Longitude: Double): string;
function FindGPSItem(Layer: TMapLayer; const Call: String):TGPSObj;
function FindGPSItem(Layer: TMapLayer; const x, y: Integer):TMapPointOfInterest;
function GetAltitude(const Text: String):Integer;
function GetCourse(const Text: String):Integer;
function GetSpeed(const Text: String):Integer;
function GetDFSStrength(const Text: String):Integer;
function GetDFSHeight(const Text: String):Integer;
function GetDFSGain(const Text: String):Integer;
function GetDFSDirectivity(const Text: String):String;
function GetPHGPower(const Text: String):Integer;
function GetPHGHeight(const Text: String):Integer;
function GetPHGGain(const Text: String):Integer;
function GetPHGDirectivity(const Text: String):String;
function GetAPRSDataExtension(const Text, Search: String; const MatchIndex: Byte; const Table: ArrayOfPHGCode):String;
function GetRNG(const Text: String):Integer;
function GetWX(const Text, Search: String):String;
function GetAPRSMessageObject(const Data, DataType, DataMessage: String; const RegExString: String): TAPRSMessage;

var
  APRSMessageList: TFPHashList;

implementation

uses
  umain;

function LatLonToLocator(const Latitude, Longitude: Double): string;
var
  FieldLon, FieldLat: Char;
  SquareLon, SquareLat: Integer;
  SubLon, SubLat: Char;
begin
  FieldLon := Chr(Ord('A') + Trunc((Longitude + 180) / 20));
  FieldLat := Chr(Ord('A') + Trunc((Latitude + 90) / 10));

  SquareLon := Trunc((Longitude + 180) / 2) mod 10;
  SquareLat := Trunc((Latitude + 90)) mod 10;

  SubLon := Chr(Ord('a') + Trunc(((Longitude + 180) mod 2) * 12));
  SubLat := Chr(Ord('a') + Trunc(((Latitude + 90) mod 1) * 24));

  Result := FieldLon + FieldLat + IntToStr(SquareLat) + IntToStr(SquareLon) + SubLon + SubLat;
end;


function FindGPSItem(Layer: TMapLayer; const x, y: Integer):TMapPointOfInterest;
var
  p: TRealPoint;
  i: Integer;
  poi: TMapPointOfInterest;
  Tolerance: Double;
begin
  p := Layer.View.ScreenToLatLon(Point(x, y));
  Tolerance := 0.0;

  if Layer.View.Zoom < 20 then
    Tolerance := 0.0005;
  if Layer.View.Zoom < 15 then
    Tolerance := 0.0010;
  if Layer.View.Zoom < 13 then
    Tolerance := 0.0050;
  if Layer.View.Zoom < 10 then
    Tolerance := 0.0500;
  if Layer.View.Zoom < 7 then
    Tolerance := 0.1000;
  if Layer.View.Zoom < 5 then
    Tolerance := 0.3000;

  for i := 0 to Layer.PointsOfInterest.Count - 1 do
  begin
    poi := Layer.PointsOfInterest[i];

    if (Abs(poi.Longitude - p.Lon) <= Tolerance) and
       (Abs(poi.Latitude - p.Lat) <= Tolerance) then
    begin
      Result := poi;
      Exit;
    end;
  end;
  Result := nil;
end;

function FindGPSItem(Layer: TMapLayer; const Call: String):TGPSObj;
var i: Integer;
begin
  for i := 0 to Layer.PointsOfInterest.Count - 1 do
  begin
    if UpperCase(Trim(Layer.PointsOfInterest[i].Caption)) = UpperCase(Trim(Call)) then
    begin
      Result := Layer.PointsOfInterest[i].GPSObj;
      Exit;
    end;
  end;
  Result := nil;
end;


procedure SetPoi(Layer: TMapLayer; Message: PAPRSMessage; List: TGPSObjectList);
var poi: TMapPointOfInterest;
begin
  poi := Layer.PointsOfInterest.Add as TMapPointOfInterest;
  poi.Longitude := Message^.Longitude;
  poi.Latitude := Message^.Latitude;
  poi.Caption := Message^.FromCall;
  poi.ImageIndex := GetImageIndex(Message^.Icon, Message^.IconPrimary);
  APRSMessageList.Add(Message^.FromCall, Message);
end;

procedure SetPoI(Layer: TMapLayer; const Latitude, Longitude: Double; const Text: String; const visibility: Boolean; const ImageIndex: Integer; List: TGPSObjectList);
var poi: TMapPointOfInterest;
begin
  poi := Layer.PointsOfInterest.Add as TMapPointOfInterest;
  poi.Longitude := Longitude;
  poi.Latitude := Latitude;
  poi.Caption := Text;
  poi.ImageIndex := ImageIndex;
end;

procedure DelPoI(Layer: TMapLayer; const Call: String);
var i: Integer;
    msg: PAPRSMessage;
begin
  // Do not check position 0 because it's ourself.
  for i := 1 to Layer.PointsOfInterest.Count - 1 do
  begin
    msg := APRSMessageList.Find(Call);
    if msg <> nil then
    begin
      APRSMessageList.Remove(msg);
      try
        Layer.PointsOfInterest.Delete(i);
      except
      end;
      Exit;
    end;
  end;
end;

function GetImageIndex(const Symbol, IconPrimary: String):Byte;
var i: Byte;
    count: Integer;
begin
  count := Length(APRSPrimarySymbolTable);
  Result := 0;
  for i := 1 to count do
  begin
    if APRSPrimarySymbolTable[i].SymbolChar = Symbol then
    begin
      Result := i;
      { Icon Primary meaning
        --------------------
        TABLE    RESULT
         &       RESERVED for possible AUXILLIARY tables (Aug 09)
         /       Primary   symbol Table  (Mostly stations)
         \       Alternate symbol table  (Mostly Objects)
         0-9     Alternate OVERLAY symbols with 0-9 overlayed
         A-Z     Alternate OVERLAY symbols with A-Z overlayed
      }

      if IconPrimary = '\' then
        Result := Result+94;
      Exit;
    end;
  end;
end;

function GetAltitude(const Text: String):Integer;
var Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^.*A=(\d{6}).*$';
    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
      Result := Round(StrToInt(Regex.Match[1])*0.3048);
      Exit;
    end;
  except
  end;
  Result := 0;
end;

function GetCourse(const Text: String):Integer;
var Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^(\d{3})\/(\d{3}).*$';
    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
      Result := StrToInt(Regex.Match[1]);
      Exit;
    end;
  finally
    Regex.Free;
  end;
  Result := 0;
end;

function GetSpeed(const Text: String):Integer;
var Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^(\d{3})\/(\d{3}).*$';
    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
      Result := Round(StrToInt(Regex.Match[2])*1.85);
      Exit;
    end;
  finally
    Regex.Free;
  end;
  Result := 0;
end;

function GetDFSStrength(const Text: String):Integer;
var res: String;
begin
  Result := 0;
  res := GetAPRSDataExtension(Text, 'DFS', 1, PHGPowerCodeTable);
  if Length(res) > 0 then
    Result := StrToInt(res);
end;

function GetDFSHeight(const Text: String):Integer;
var res: String;
begin
  Result := 0;
  res := GetAPRSDataExtension(Text, 'DFS', 2, PHGHeightCodeTable);
  if Length(res) > 0 then
    Result := Round(StrToInt(res)*0.3048);
end;

function GetDFSGain(const Text: String):Integer;
var res: String;
begin
  Result := 0;
  res := GetAPRSDataExtension(Text, 'DFS', 3, PHGGainCodeTable);
  if Length(res) > 0 then
    Result := StrToInt(res);
end;

function GetDFSDirectivity(const Text: String):String;
var res: String;
begin
  Result := '';
  res := GetAPRSDataExtension(Text, 'DFS', 4, PHGDirectivityCodeTable);
  if Length(res) > 0 then
    Result := res;
end;

function GetPHGPower(const Text: String):Integer;
var res: String;
begin
  Result := 0;
  res := GetAPRSDataExtension(Text, 'PHG', 1, PHGPowerCodeTable);
  if Length(res) > 0 then
    Result := StrToInt(res);
end;

function GetPHGHeight(const Text: String):Integer;
var res: String;
begin
  Result := 0;
  res := GetAPRSDataExtension(Text, 'PHG', 2, PHGHeightCodeTable);
  if Length(res) > 0 then
    Result := Round(StrToInt(res)*0.3048);
end;

function GetPHGGain(const Text: String):Integer;
var res: String;
begin
  Result := 0;
  res := GetAPRSDataExtension(Text, 'PHG', 3, PHGGainCodeTable);
  if Length(res) > 0 then
    Result := StrToInt(res);
end;

function GetPHGDirectivity(const Text: String):String;
var res: String;
begin
  Result := '';
  res := GetAPRSDataExtension(Text, 'PHG', 4, PHGDirectivityCodeTable);
  if Length(res) > 0 then
    Result := res;
end;

function GetRNG(const Text: String):Integer;
var Regex: TRegExpr;
begin
  Result := 0;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^RNG(\d{4}).*$';
    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
       Result := Round(StrToInt(Regex.Match[1])*1.85);
       Exit;
    end;
  except
  end;
end;

function GetWX(const Text, Search: String):String;
var Regex: TRegExpr;
begin
  Result := '0';
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^.*'+Search+'(\d{3}).*$';
    if Search = 'h' then
      Regex.Expression := '^.*'+Search+'(\d{2}).*$';
    if Search = 'b' then
      Regex.Expression := '^.*'+Search+'(\d{5}).*$';

    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
       Result := Regex.Match[1];
       Exit;
    end;
  finally
    Regex.Free;
  end;
end;

function GetAPRSDataExtension(const Text, Search: String; const MatchIndex: Byte; const Table: ArrayOfPHGCode):String;
var Regex: TRegExpr;
    count: Integer;
    i: Byte;
begin
  count := Length(Table);
  Result := '';
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^'+Search+'(\d)(\d)(\d)(\d).*$';
    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
      for i := 0 to count - 1 do
       begin
         if Table[i].Code = StrToInt(Regex.Match[MatchIndex]) then
         begin
           Result := Table[i].Value;
           Exit;
         end;
       end;
    end;
  finally
    Regex.Free;
  end;
end;

procedure ConvertNMEAToLatLong(const NMEALat, NMEALon: string; out Latitude, Longitude: Double; const divider: Integer);
var
  Degrees, Minutes: Double;
  Direction: Char;
begin
  // Latitude
  if Length(NMEALat) < 5 then
    Exit;

  Degrees := StrToFloat(Copy(NMEALat, 1, 2));
  Minutes := StrToFloat(Copy(NMEALat, 3, Length(NMEALat) - 4));
  Direction := NMEALat[Length(NMEALat)];

  Latitude := Degrees + (Minutes / 60.0);
  if Direction = 'S' then
    Latitude := -Latitude;

  // Longitude
  if Length(NMEALon) < 6 then
    Exit;

  Degrees := StrToFloat(Copy(NMEALon, 1, 3));
  Minutes := StrToFloat(Copy(NMEALon, 4, Length(NMEALon) - 5));
  Direction := NMEALon[Length(NMEALon)];

  Longitude := (Degrees + (Minutes / 60.0)) / divider;
  if Direction = 'W' then
    Longitude := -Longitude;
end;

function GetAPRSMessageObject(const Data, DataType, DataMessage: String; const RegExString: String): TAPRSMessage;
var Regex: TRegExpr;
    Lat, Lon: Double;
    APRSMessageObject: TAPRSMessage;
const
    // with position
    WX = '!=/@;';
    WXRaw = '!#$*';
    ItemObject = ');';
    // without position
    WXPositionless = '_';
    Messages = ':';
    StatusReport = '>';
    Telemetry = 'T#';
begin
  Result := Default(TAPRSMessage);
  Regex := TRegExpr.Create;
  try
    // check type
    if Length(Data) <= 0 then
      Exit;

      //Regex.Expression := '^.*?Fm ([A-Z0-9]{1,6}(?:-[0-9]{1,2})?) To ([A-Z0-9]{1,6})(?: Via ([A-Z0-9,-]+))? .*?>\[(\d{2}:\d{2}:\d{2})\].?\s*(.+)$';
      Regex.Expression := RegExString;
      Regex.ModifierI := False;
      if Regex.Exec(Data) then
      begin
        // FromCall: String;
        // ToCall: String;
        // Path: String;
        // Longitude: Double;
        // Latitude: Double;
        // Message: String;
        // Time: String
        APRSMessageObject.FromCall := Trim(Regex.Match[1]);
        APRSMessageObject.ToCall := Trim(Regex.Match[2]);

        if (Pos(DataType, WX) > 0) or (Pos(DataType, WXRaw) > 0) or (Pos(DataType, ItemObject) > 0) then
        begin
          ConvertNMEAToLatLong(Regex.Match[4], Regex.Match[6], Lat, Lon, 1);
          APRSMessageObject.Latitude := Lat;
          APRSMessageObject.Longitude := Lon;
          APRSMessageObject.IconPrimary := Regex.Match[5];
          APRSMessageObject.Icon := Regex.Match[7];
          APRSMessageObject.Message := Regex.Match[8];
          APRSMessageObject.Altitude := GetAltitude(APRSMessageObject.Message);
          APRSMessageObject.Course := GetCourse(APRSMessageObject.Message);
          APRSMessageObject.Speed := GetSpeed(APRSMessageObject.Message);
          APRSMessageObject.PHGPower := GetPHGPower(APRSMessageObject.Message);
          APRSMessageObject.PHGHeight := GetPHGHeight(APRSMessageObject.Message);
          APRSMessageObject.PHGGain := GetPHGGain(APRSMessageObject.Message);
          APRSMessageObject.PHGDirectivity := GetPHGDirectivity(APRSMessageObject.Message);
          APRSMessageObject.DFSStrength := GetDFSStrength(APRSMessageObject.Message);
          APRSMessageObject.DFSHeight := GetDFSHeight(APRSMessageObject.Message);
          APRSMessageObject.DFSGain := GetDFSGain(APRSMessageObject.Message);
          APRSMessageObject.DFSDirectivity := GetDFSDirectivity(APRSMessageObject.Message);
          APRSMessageObject.RNGRange := GetRNG(APRSMessageObject.Message);

          APRSMessageObject.WXDirection := 0;
          APRSMessageObject.WXSpeed := 0;
          APRSMessageObject.WXGust := 0;
          APRSMessageObject.WXTemperature := 0;
          APRSMessageObject.WXRainFall1h := 0;
          APRSMessageObject.WXRainFall24h := 0;
          APRSMessageObject.WXRainFallToday := 0;
          APRSMessageObject.WXHumidity := 0;
          APRSMessageObject.WXPressure := 0;
          APRSMessageObject.WXLum := 0;
          APRSMessageObject.WXSnowFall := 0;
          APRSMessageObject.WXRainCount := 0;

          if (APRSMessageObject.Icon = '_') or (APRSMessageObject.Icon = '@') or (APRSMessageObject.Icon = 'w') then
          begin
            APRSMessageObject.WXDirection := StrToInt(GetWX(APRSMessageObject.Message,'c'));
            APRSMessageObject.WXSpeed := Round(StrToInt(GetWX(APRSMessageObject.Message,'s'))*1.85);
            APRSMessageObject.WXGust := Round(StrToInt(GetWX(APRSMessageObject.Message,'g'))*1.85);
            APRSMessageObject.WXTemperature := Round((StrToInt(GetWX(APRSMessageObject.Message,'t')) - 32)*5/9);
            APRSMessageObject.WXRainFall1h := Round(StrToInt(GetWX(APRSMessageObject.Message,'r'))*25.4);
            APRSMessageObject.WXRainFall24h := Round(StrToInt(GetWX(APRSMessageObject.Message,'p'))*25.4);
            APRSMessageObject.WXRainFallToday := Round(StrToInt(GetWX(APRSMessageObject.Message,'P'))*25.4);
            APRSMessageObject.WXHumidity := StrToInt(GetWX(APRSMessageObject.Message,'h'));
            APRSMessageObject.WXPressure := Round(StrToInt(GetWX(APRSMessageObject.Message,'b'))/10);
            APRSMessageObject.WXLum := StrToInt(GetWX(APRSMessageObject.Message,'L'));
            APRSMessageObject.WXSnowFall := StrToInt(GetWX(APRSMessageObject.Message,'s'));
            APRSMessageObject.WXRainCount := StrToInt(GetWX(APRSMessageObject.Message,'#'));
          end;

          APRSMessageObject.Time := now();
          APRSMessageObject.Track := False;
        end;
      end;

      // text, bulletins, announcement and some telemetry messages
      // shares the same datatype
      if (Pos(DataType, Messages) > 0) then
      begin
        // check telemetry message
        Regex.Expression := '^.*((UNIT|PARM|EQNS|BITS))(.*)$';
        Regex.ModifierI := False;
        if Regex.Exec(DataMessage) then
        begin
          // todo telemetry messages
          APRSMessageObject.Message := APRSMessageObject.Message + Data;
        end;

        // check bulletin message
        Regex.Expression := '^.*:(BLN)(\d{1})(\w{5}):(.*)$';
        Regex.ModifierI := False;
        if Regex.Exec(DataMessage) then
        begin
          // todo bulletin message
          {$IFDEF UNIX}
          writeln(data);
          {$ENDIF}
          APRSMessageObject.Message := APRSMessageObject.Message + Data;
        end;

        // check National Weather Service bulletin
        Regex.Expression := '^.*:(NWS)-(\w{5}):(.*)$';
        Regex.ModifierI := False;
        if Regex.Exec(DataMessage) then
        begin
          // todo nws message
          APRSMessageObject.Message := APRSMessageObject.Message + Data;
        end
      end;

      // check status report
      if (Pos(DataType, StatusReport) > 0) then
      begin
        APRSMessageObject.Message := APRSMessageObject.Message + Data;
      end;

      // check telemetry
      if (Pos(DataType, Telemetry) > 0) then
      begin
        // todo telemetry messages
        APRSMessageObject.Message := APRSMessageObject.Message + Data;
      end;
      Result := APRSMessageObject;
  finally
    Regex.Free;
  end;
end;


end.

