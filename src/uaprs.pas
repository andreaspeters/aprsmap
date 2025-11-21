unit uaprs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, utypes, SysUtils, ExtCtrls, Forms, Controls, Graphics, Dialogs,
  mvGPSObj, Contnrs, mvMapViewer, mvTypes, RegExpr, Math,
  FPImage, IntfGraphics, GraphType,
  u_rs41sg
  ;

procedure DelPoI(Layer: TMapLayer; const Call: String);
procedure SetPoI(Layer: TMapLayer; Message: PAPRSMessage; const visibility: Boolean);
procedure SetPoI(Layer: TMapLayer; const Latitude, Longitude: Double; const Text: String; const visibility: Boolean; const ImageIndex: Integer; List: TGPSObjectList);
procedure ConvertNMEAToLatLong(const NMEALat, NMEALon: string; out Latitude, Longitude: Double; const divider: Integer);
function GetImageIndex(const Symbol, IconPrimary: String):Byte;
function GetImageDescription(const Symbol, IconPrimary: String):String;
function LatLonToLocator(const Latitude, Longitude: Double): string;
function FindGPSItem(Layer: TMapLayer; const Call: String):TGPSObj;
function FindGPSItem(Layer: TMapLayer; const x, y: Integer):TMapPointOfInterest;
function FindPoI(Layer: TMapLayer; const Call: String): TMapPointOfInterest;
function GetAltitude(const Text: String):Double;
function GetCourse(const Text: String):Double;
function GetSpeed(const Text: String):Double;
function GetDFSStrength(const Text: String):Double;
function GetDFSHeight(const Text: String):Double;
function GetDFSGain(const Text: String):Double;
function GetDFSDirectivity(const Text: String):String;
function GetPHGPower(const Text: String):Double;
function GetPHGHeight(const Text: String):Double;
function GetPHGGain(const Text: String):Double;
function GetPHGDirectivity(const Text: String):String;
function GetAPRSDataExtension(const Text, Search: String; const MatchIndex: Byte; const Table: ArrayOfPHGCode):String;
function GetRNG(const Text: String):Double;
function GetWX(const Text, Search: String):Double;
function GetAPRSMessageObject(const Data, DataType, DataMessage: String): TAPRSMessage;
function CreateOverlay(ImageList: TImageList; IndexBase, IndexOverlay: Integer): Integer;

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

  Result := FieldLon + FieldLat + FloatToStr(SquareLat) + FloatToStr(SquareLon) + SubLon + SubLat;
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



procedure SetPoi(Layer: TMapLayer; Message: PAPRSMessage; const visibility: Boolean);
var poi: TMapPointOfInterest;
begin
  if (Message^.Longitude <= 0) or (Message^.Latitude <= 0) then
    Exit;

  poi := FindPoI(Layer, Message^.FromCall);

  if poi <> nil then
    DelPoI(Layer, Message^.FromCall);

  poi := Layer.PointsOfInterest.Add as TMapPointOfInterest;
  poi.Longitude := Message^.Longitude;
  poi.Latitude := Message^.Latitude;
  poi.Caption := Message^.FromCall;
  poi.ImageIndex := Message^.ImageIndex;
  poi.Visible := visibility;
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
  poi.Visible := visibility;

  if FMain.CBEFilter.ItemIndex > 0 then
    poi.Visible := False;
end;

function FindPoI(Layer: TMapLayer; const Call: String): TMapPointOfInterest;
var i: Integer;
    msg: PAPRSMessage;
begin
  Result := nil;
  // Do not check position 0 because it's ourself.
  for i := 1 to Layer.PointsOfInterest.Count - 1 do
  begin
    msg := APRSMessageList.Find(Call);
    if msg <> nil then
    begin
       Result := Layer.PointsOfInterest[i];
    end;
  end;
end;


procedure DelPoI(Layer: TMapLayer; const Call: String);
var i: Integer;
begin
  if Layer.PointsOfInterest.Count <= 0 then
    Exit;

  try
   // Do not check position 0 because it's ourself.
   i := 1;
   while i <= Layer.PointsOfInterest.Count - 1 do
   begin
     if SameText(Layer.PointsOfInterest[i].Caption, Call) then
     begin
       Layer.PointsOfInterest.Delete(i);
       dec(i);
       if i < 1 then
         Exit;
     end;
     inc(i);
   end;
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('Error DelPoi: ', E.Message);
      {$ENDIF}
    end;
  end;
end;

function GetImageDescription(const Symbol, IconPrimary: String):String;
var i: Byte;
    count: Integer;
begin
  Result := '';
  if (Length(Symbol) <= 0) or (Length(IconPrimary) <= 0) then
    Exit;

  // Primary Symbols
  if IconPrimary = '/' then
  begin
    count := Length(APRSPrimarySymbolTable);

    for i := 1 to count do
    begin
      if APRSPrimarySymbolTable[i].SymbolChar = Symbol then
      begin
        Result := APRSPrimarySymbolTable[i].Description;
        Exit;
      end;
    end;
  end;

  // Alternate Symbols
  if IconPrimary = '\' then
  begin
    count := Length(APRSAlternateSymbolTable);

    for i := 1 to count do
    begin
      if APRSAlternateSymbolTable[i].SymbolChar = Symbol then
      begin
        Result := APRSAlternateSymbolTable[i].Description;
        Exit;
      end;
    end;
  end;
end;

function GetImageIndex(const Symbol, IconPrimary: String):Byte;
var i, x: Byte;
    count: Integer;
    overlay: String;
begin
  Result := 0;
  if (Length(Symbol) <> 1) or (Length(IconPrimary) <> 1) then
    Exit;

  count := Length(APRSPrimarySymbolTable);

  for i := 1 to count do
  begin
    if APRSPrimarySymbolTable[i].SymbolChar = Symbol then
    begin
      Result := i;
      x := 0;
      { Icon Primary meaning
        --------------------
        TABLE    RESULT
         &       RESERVED for possible AUXILLIARY tables (Aug 09)
         /       Primary   symbol Table  (Mostly stations)
         \       Alternate symbol table  (Mostly Objects)
         0-9     Alternate OVERLAY symbols with 0-9 overlayed
         a-j     Alternate OVERLAY symbols with 0-9 overlayed
         A-Z     Alternate OVERLAY symbols with A-Z overlayed
      }

      // Primary Symbols
      if IconPrimary = '/' then
      begin
        Result := i;
        Exit;
      end;

      // Alternate Symbols
      if IconPrimary = '\' then
      begin
        Result := i+96;
        Exit;
      end;

      if Length(IconPrimary) > 0 then
      begin
        try
          // Primary Symbols with Overlay
          if IconPrimary = '&' then
            x := 0;

          // Alternate Symbols with Overlay
          if (IconPrimary[1] in ['A'..'Z', 'a'..'z', '0'..'9']) then
            x := 96;

          Overlay := 'ABCEFGHIJKLMNOPQRSTUVWXYZ';
          if Pos(IconPrimary[1], Overlay) > 0 then
          begin
            Result := CreateOverlay(FMain.ImageList1, i + x, Pos(IconPrimary[1], Overlay)+195+1);
            Exit;
          end;

          Overlay := 'abcdefghij';
          if Pos(IconPrimary[1], Overlay) > 0 then
          begin
            Result := CreateOverlay(FMain.ImageList1, i + x, Pos(IconPrimary[1], Overlay)+221+1);
            Exit;
          end;

          overlay := '0123456789';
          if Pos(IconPrimary[1], Overlay) > 0 then
          begin
            Result := CreateOverlay(FMain.ImageList1, i + x, Pos(IconPrimary[1], Overlay)+221);
            Exit;
          end;
        except
          on E: Exception do
          begin
            {$IFDEF UNIX}
            writeln('Error Create Overlay Image: ', E.Message);
            {$ENDIF}
            Result := i;
          end;
        end;
      end;
      Exit;
    end;
  end;
end;

function GetAltitude(const Text: String):Double;
var Regex: TRegExpr;
begin
  Result := -999999;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^.*A=(\d{6}).*$';
    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
      Result := Round(StrToFloat(Regex.Match[1])*0.3048);
      Exit;
    end;
  except
  end;
end;

function GetCourse(const Text: String):Double;
var Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^(\d{3})\/(\d{3}).*$';
    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
      Result := StrToFloat(Regex.Match[1]);
      Exit;
    end;
  finally
    Regex.Free;
  end;
  Result := 0;
end;

function GetSpeed(const Text: String):Double;
var Regex: TRegExpr;
begin
  Result := -999999;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^(\d{3})\/(\d{3}).*$';
    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
      Result := Round(StrToFloat(Regex.Match[2])*1.85);
      Exit;
    end;
  finally
    Regex.Free;
  end;
end;

function GetDFSStrength(const Text: String):Double;
var res: String;
begin
  Result := 0;
  res := GetAPRSDataExtension(Text, 'DFS', 1, PHGPowerCodeTable);
  if Length(res) > 0 then
    Result := StrToFloat(res);
end;

function GetDFSHeight(const Text: String):Double;
var res: String;
begin
  Result := 0;
  res := GetAPRSDataExtension(Text, 'DFS', 2, PHGHeightCodeTable);
  if Length(res) > 0 then
    Result := Round(StrToFloat(res)*0.3048);
end;

function GetDFSGain(const Text: String):Double;
var res: String;
begin
  Result := 0;
  res := GetAPRSDataExtension(Text, 'DFS', 3, PHGGainCodeTable);
  if Length(res) > 0 then
    Result := StrToFloat(res);
end;

function GetDFSDirectivity(const Text: String):String;
var res: String;
begin
  Result := '';
  res := GetAPRSDataExtension(Text, 'DFS', 4, PHGDirectivityCodeTable);
  if Length(res) > 0 then
    Result := res;
end;

function GetPHGPower(const Text: String):Double;
var res: String;
begin
  Result := 0;
  res := GetAPRSDataExtension(Text, 'PHG', 1, PHGPowerCodeTable);
  if Length(res) > 0 then
    Result := StrToFloat(res);
end;

function GetPHGHeight(const Text: String):Double;
var res: String;
begin
  Result := 0;
  res := GetAPRSDataExtension(Text, 'PHG', 2, PHGHeightCodeTable);
  if Length(res) > 0 then
    Result := Round(StrToFloat(res)*0.3048);
end;

function GetPHGGain(const Text: String):Double;
var res: String;
begin
  Result := 0;
  res := GetAPRSDataExtension(Text, 'PHG', 3, PHGGainCodeTable);
  if Length(res) > 0 then
    Result := StrToFloat(res);
end;

function GetPHGDirectivity(const Text: String):String;
var res: String;
begin
  Result := '';
  res := GetAPRSDataExtension(Text, 'PHG', 4, PHGDirectivityCodeTable);
  if Length(res) > 0 then
    Result := res;
end;

function GetRNG(const Text: String):Double;
var Regex: TRegExpr;
begin
  Result := 0;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^RNG(\d{4}).*$';
    Regex.ModifierI := True;
    if Regex.Exec(Text) then
    begin
       Result := Round(StrToFloat(Regex.Match[1])*1.85);
       Exit;
    end;
  except
  end;
end;

function GetWX(const Text, Search: String):Double;
var Regex: TRegExpr;
begin
  Result := -999999;
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^.*'+Search+'(\d{3}).*$';
    if Search = 'h' then
      Regex.Expression := '^.*'+Search+'(\d{2,3}).*$';
    if Search = 'b' then
      Regex.Expression := '^.*'+Search+'(\d{4,5}).*$';

    Regex.ModifierI := False;
    if Regex.Exec(Text) then
    begin
       Result := StrToFloat(Regex.Match[1]);
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

function GetAPRSMessageObject(const Data, DataType, DataMessage: String): TAPRSMessage;
var Regex, OrRegex: TRegExpr;
    Lat, Lon: Double;
    APRSMessageObject: TAPRSMessage;
    Message: Array of String;
const
    // with position
    WX = '!=/@;';
    WXRaw = '!#$*';
    ItemObject = ');';
    // without position
    // WXPositionless = '_';
    Messages = ':';
    StatusReport = '>';
    Telemetry = 'T#';
    ObjectReport = ';';
begin
  Result := Default(TAPRSMessage);
  Regex := TRegExpr.Create;
  try
    // check type
    if Length(Data) <= 0 then
      Exit;

    Message := Data.Split('|');
    if Length(Message) < 4 then
      Exit;

    //, '^.*?Fm\s(\S+)\sTo\s(\S+)\s(?:Via\s(\S+))? .*UI(?:[v]{0,1})\spid(?:[=|\s]{0,1})F0.*([!=\/@zh]{1})(\d{4}\.\d{2}[N|S])(.)(\d{5}\.\d{2}[E|W])(.)(.+)$'
    Regex.Expression := '.*([!=\/@zh]{1})(\d{4}\.\d{2}[N|S])(.)(\d{5}\.\d{2}[E|W])(.)(.+)$';
    Regex.ModifierI := False;

    if Regex.Exec(Message[3]) then
    begin
      // FromCall: String;
      // ToCall: String;
      // Path: String;
      // Longitude: Double;
      // Latitude: Double;
      // Message: String;
      // Time: String
      APRSMessageObject.FromCall := Trim(Message[0]);
      APRSMessageObject.ToCall := Trim(Message[1]);
      APRSMessageObject.EnableTrack := False;
      APRSMessageObject.Track := TGPSTrack.Create;
      APRSMessageObject.Track.Visible := False;
      APRSMessageObject.Track.LineWidth := 1;
      APRSMessageObject.ModeS := False;
      APRSMessageObject.RAWMessages := TStringList.Create;
      APRSMessageObject.RAWMessages.Add(Format('%-10s > %s', [APRSMessageObject.FromCall, Data]));
      APRSMessageObject.Count := 0;
      APRSMessageObject.Visible := True;

      // In a ObjectReport, the Callsign could be inside the PayLoad
      if DataType = ObjectReport then
      begin
        OrRegex.Expression := '^;([A-Z0-9\-]{1,9})\s';
        OrRegex.ModifierI := False;
        if OrRegex.Exec(Message[3]) then
          if OrRegex.SubExprMatchCount >= 1 then
            APRSMessageObject.FromCall := Trim(OrRegex.Match[1]);
      end;


      if (Pos(DataType, WX) > 0) or (Pos(DataType, WXRaw) > 0) or (Pos(DataType, ItemObject) > 0) then
      begin
        if Regex.SubExprMatchCount < 6 then
          Exit;
        ConvertNMEAToLatLong(Regex.Match[2], Regex.Match[4], Lat, Lon, 1);
        APRSMessageObject.Latitude := Lat;
        APRSMessageObject.Longitude := Lon;
        APRSMessageObject.IconPrimary := Regex.Match[3];
        APRSMessageObject.Icon := Regex.Match[5];
        APRSMessageObject.ImageIndex := GetImageIndex(Regex.Match[5], Regex.Match[3]);
        APRSMessageObject.ImageDescription := GetImageDescription(Regex.Match[5], Regex.Match[3]);
        APRSMessageObject.Message := Regex.Match[6];
        APRSMessageObject.Course := GetCourse(APRSMessageObject.Message);
        APRSMessageObject.PHGPower := GetPHGPower(APRSMessageObject.Message);
        APRSMessageObject.PHGHeight := GetPHGHeight(APRSMessageObject.Message);
        APRSMessageObject.PHGGain := GetPHGGain(APRSMessageObject.Message);
        APRSMessageObject.PHGDirectivity := GetPHGDirectivity(APRSMessageObject.Message);
        APRSMessageObject.DFSStrength := GetDFSStrength(APRSMessageObject.Message);
        APRSMessageObject.DFSHeight := GetDFSHeight(APRSMessageObject.Message);
        APRSMessageObject.DFSGain := GetDFSGain(APRSMessageObject.Message);
        APRSMessageObject.DFSDirectivity := GetDFSDirectivity(APRSMessageObject.Message);
        APRSMessageObject.RNGRange := GetRNG(APRSMessageObject.Message);

        APRSMessageObject.Altitude := TDoubleList.Create;
        APRSMessageObject.Speed := TDoubleList.Create;

        if GetAltitude(APRSMessageObject.Message) <> -999999 then
          APRSMessageObject.Altitude.Add(GetAltitude(APRSMessageObject.Message));
        if GetSpeed(APRSMessageObject.Message) <> -999999 then
          APRSMessageObject.Speed.Add(GetSpeed(APRSMessageObject.Message));

        APRSMessageObject.WXTemperature := TDoubleList.Create;
        APRSMessageObject.WXPressure := TDoubleList.Create;
        APRSMessageObject.WXHumidity := TDoubleList.Create;
        APRSMessageObject.WXDirection := TDoubleList.Create;
        APRSMessageObject.WXSpeed := TDoubleList.Create;
        APRSMessageObject.WXGust := TDoubleList.Create;
        APRSMessageObject.WXRainFall1h := TDoubleList.Create;
        APRSMessageObject.WXRainFall24h := TDoubleList.Create;
        APRSMessageObject.WXRainFallToday := TDoubleList.Create;
        APRSMessageObject.WXLum := TDoubleList.Create;
        APRSMessageObject.WXSnowFall := TDoubleList.Create;
        APRSMessageObject.WXRainCount := TDoubleList.Create;

        if (APRSMessageObject.Icon = '_') or (APRSMessageObject.Icon = '@') or (APRSMessageObject.Icon = 'w') then
        begin
          if GetWX(APRSMessageObject.Message,'c') <> -999999 then
            APRSMessageObject.WXDirection.Add(GetWX(APRSMessageObject.Message,'c'));
          if GetWX(APRSMessageObject.Message,'s') <> -999999 then
            APRSMessageObject.WXSpeed.Add(GetWX(APRSMessageObject.Message,'s')*1.85);
          if GetWX(APRSMessageObject.Message,'g') <> -999999 then
            APRSMessageObject.WXGust.Add(GetWX(APRSMessageObject.Message,'g')*1.85);
          if GetWX(APRSMessageObject.Message,'r') <> -999999 then
            APRSMessageObject.WXRainFall1h.Add(GetWX(APRSMessageObject.Message,'r')*25.4);
          if GetWX(APRSMessageObject.Message,'p') <> -999999 then
            APRSMessageObject.WXRainFall24h.Add(GetWX(APRSMessageObject.Message,'p')*25.4);
          if GetWX(APRSMessageObject.Message,'P') <> -999999 then
            APRSMessageObject.WXRainFallToday.Add(GetWX(APRSMessageObject.Message,'P')*25.4);
          if GetWX(APRSMessageObject.Message, 't') <> -999999 then
            APRSMessageObject.WXTemperature.Add(FahrenheitToCelsius(GetWX(APRSMessageObject.Message, 't')));
          if GetWX(APRSMessageObject.Message,'h') <> -999999 then
            APRSMessageObject.WXHumidity.Add(GetWX(APRSMessageObject.Message,'h'));
          if GetWX(APRSMessageObject.Message, 'b') <> -999999 then
            APRSMessageObject.WXPressure.Add(GetWX(APRSMessageObject.Message, 'b')/ 10);
          if GetWX(APRSMessageObject.Message,'L') <> -999999 then
            APRSMessageObject.WXLum.Add(GetWX(APRSMessageObject.Message,'L'));
          if GetWX(APRSMessageObject.Message,'s') <> -999999 then
            APRSMessageObject.WXSnowFall.Add(GetWX(APRSMessageObject.Message,'s'));
          if GetWX(APRSMessageObject.Message,'#') <> -999999 then
            APRSMessageObject.WXRainCount.Add(GetWX(APRSMessageObject.Message,'#'));
        end;

        APRSMessageObject.Time := now();
      end;


      RS41SGP(@APRSMessageObject);
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
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('Error GetAPRSMessageObject: ', E.Message)
      {$ENDIF}
    end;
  end;
  Regex.Free;
  OrRegex.Free;
end;

function BearingFromTo(Lat1, Lon1, Lat2, Lon2: Double): Double;
var dLon, y, x: Double;
begin
  // Umrechnung in Radiant
  Lat1 := DegToRad(Lat1);
  Lat2 := DegToRad(Lat2);
  dLon := DegToRad(Lon2 - Lon1);

  y := Sin(dLon) * Cos(Lat2);
  x := Cos(Lat1) * Sin(Lat2) - Sin(Lat1) * Cos(Lat2) * Cos(dLon);

  // Winkel in Grad (0–360)
  Result := RadToDeg(ArcTan2(y, x));
  if Result < 0 then
    Result := Result + 360;
end;

function CreateOverlay(ImageList: TImageList; IndexBase, IndexOverlay: Integer): Integer;
var bmpBase, bmpOverlay, bmpMerged: TBitmap;
begin
  Result := 0;

  bmpBase := TBitmap.Create;
  bmpOverlay := TBitmap.Create;
  bmpMerged := TBitmap.Create;
  try
    // set Size
    bmpMerged.SetSize(ImageList.Width, ImageList.Height);

    ImageList.GetBitmap(IndexBase, bmpBase);
    ImageList.GetBitmap(IndexOverlay, bmpOverlay);

    bmpMerged.PixelFormat := bmpBase.PixelFormat;
    bmpMerged.Transparent := True;
    bmpMerged.TransparentColor := clWhite;

    bmpMerged.Canvas.Draw(0, 0, bmpBase);
    bmpMerged.Canvas.Draw(0, 0, bmpOverlay);

    // add new image to imagelist
    Result := ImageList.Add(bmpMerged, nil);
  finally
    bmpBase.Free;
    bmpOverlay.Free;
    bmpMerged.Free;
  end;
end;

end.

