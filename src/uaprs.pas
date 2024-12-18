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
function FindGPSItem(Layer: TMapLayer; const x, y: Integer):TPointOfInterest;
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

var
  APRSMessageList: TFPHashList;

implementation


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


function FindGPSItem(Layer: TMapLayer; const x, y: Integer):TPointOfInterest;
var
  p: TRealPoint;
  i, count: Integer;
  poi: TPointOfInterest;
  Tolerance: Double;
begin
  count := Layer.PointsOfInterest.Count;
  p := Layer.View.ScreenToLatLon(Point(x, y));

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

  for i := 0 to count - 1 do
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
var i, count: Integer;
begin
  count := Layer.PointsOfInterest.Count;
  for i := 0 to count - 1 do
  begin
    if Layer.PointsOfInterest[i].Caption = Call then
    begin
      Result := Layer.PointsOfInterest[i].GPSObj;
      Exit;
    end;
  end;
  Result := nil;
end;


procedure SetPoi(Layer: TMapLayer; Message: PAPRSMessage; List: TGPSObjectList);
var poi: TPointOfInterest;
begin
  poi := Layer.PointsOfInterest.Add as TPointOfInterest;
  poi.Longitude := Message^.Longitude;
  poi.Latitude := Message^.Latitude;
  poi.Caption := Message^.FromCall;
  poi.ImageIndex := GetImageIndex(Message^.Icon, Message^.IconPrimary);
  APRSMessageList.Add(Message^.FromCall, Message);
end;

procedure SetPoI(Layer: TMapLayer; const Latitude, Longitude: Double; const Text: String; const visibility: Boolean; const ImageIndex: Integer; List: TGPSObjectList);
var poi: TPointOfInterest;
begin
  poi := Layer.PointsOfInterest.Add as TPointOfInterest;
  poi.Longitude := Longitude;
  poi.Latitude := Latitude;
  poi.Caption := Text;
  poi.ImageIndex := ImageIndex;
end;

procedure DelPoI(Layer: TMapLayer; const Call: String);
var i, count: Integer;
    msg: PAPRSMessage;
begin
  count := Layer.PointsOfInterest.Count;
  for i := 1 to count - 1 do
  begin
    msg := APRSMessageList.Find(Call);
    if msg <> nil then
    begin
      APRSMessageList.Remove(msg);
      Layer.PointsOfInterest.Delete(i);
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
  except
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
  except
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
  except
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
  except
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


end.

