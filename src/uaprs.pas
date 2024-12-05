unit uaprs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, utypes, SysUtils, ExtCtrls, Forms, Controls, Graphics, Dialogs,
  mvGPSObj, Contnrs, mvMapViewer, mvTypes;

procedure DelPoI(Layer: TMapLayer; const Call: String);
procedure SetPoi(Layer: TMapLayer; Message: PAPRSMessage; List: TGPSObjectList);
procedure SetPoI(Layer: TMapLayer; const Latitude, Longitude: Double; const Text: String; const visibility: Boolean; const ImageIndex: Integer; List: TGPSObjectList);
procedure ConvertNMEAToLatLong(const NMEALat, NMEALon: string; out Latitude, Longitude: Double; const divider: Integer);
function GetImageIndex(const Symbol, IconPrimary: String):Byte;
function FindGPSItem(Layer: TMapLayer; const Call: String):TGPSObj;
function FindGPSItem(Layer: TMapLayer; const x, y: Integer):TPointOfInterest;

var
  APRSMessageList: TFPHashList;

implementation

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
begin
  Result := 0;
  for i := 0 to Length(APRSPrimarySymbolTable) do
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

