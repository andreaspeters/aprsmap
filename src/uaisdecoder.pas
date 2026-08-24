unit uaisdecoder;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

type
  TAISMessage = record
    MessageType: Integer;
    MMSI: Cardinal;
    Latitude: Double;
    Longitude: Double;
    SOG: Double;
    COG: Double;
    Heading: Integer;
    ShipName: String;
    HasPosition: Boolean;
    HasName: Boolean;
  end;

function DecodeAISPayload(const Payload: String; out Msg: TAISMessage): Boolean;
function DecodeAISBits(const Bits: array of Byte; BitCount: Integer;
  out Msg: TAISMessage): Boolean;

implementation

function GetBits(const Bits: array of Byte; FirstBit, Count: Integer;
  out Value: Int64): Boolean;
var
  I: Integer;
begin
  Value := 0;
  Result := (FirstBit >= 0) and (Count > 0) and
    (FirstBit + Count <= Length(Bits));
  if not Result then Exit;
  for I := 0 to Count - 1 do
    Value := (Value shl 1) or (Bits[FirstBit + I] and 1);
end;

function SignedBits(const Bits: array of Byte; FirstBit, Count: Integer;
  out Value: Int64): Boolean;
var
  Raw, SignBit, Limit: Int64;
begin
  Result := GetBits(Bits, FirstBit, Count, Raw);
  if not Result then Exit;
  SignBit := Int64(1) shl (Count - 1);
  Limit := Int64(1) shl Count;
  if (Raw and SignBit) <> 0 then
    Value := Raw - Limit
  else
    Value := Raw;
end;

function AISChar(Value: Integer): Char;
begin
  if Value < 32 then
    Result := Chr(Value + 64)
  else
    Result := Chr(Value + 32);
end;

function DecodeText(const Bits: array of Byte; FirstBit, Count: Integer): String;
var
  I: Integer;
  V: Int64;
begin
  Result := '';
  for I := 0 to (Count div 6) - 1 do
    if GetBits(Bits, FirstBit + I * 6, 6, V) then
      Result := Result + AISChar(V);
  Result := Trim(StringReplace(Result, '@', ' ', [rfReplaceAll]));
end;

function DecodeAISBits(const Bits: array of Byte; BitCount: Integer;
  out Msg: TAISMessage): Boolean;
var
  V, LatRaw, LonRaw, SOGRaw, COGRaw, HeadingRaw: Int64;
  Type18: Boolean;
begin
  FillChar(Msg, SizeOf(Msg), 0);
  Result := False;
  if (BitCount < 38) or (Length(Bits) < BitCount) then Exit;
  if not GetBits(Bits, 0, 6, V) then Exit;
  Msg.MessageType := V;
  if Msg.MessageType in [1, 2, 3] then
  begin
    if BitCount < 137 then Exit;
    Type18 := False;
  end
  else if Msg.MessageType = 18 then
  begin
    if BitCount < 133 then Exit;
    Type18 := True;
  end
  else if Msg.MessageType = 5 then
  begin
    if BitCount < 232 then Exit;
    if not GetBits(Bits, 8, 30, V) then Exit;
    Msg.MMSI := V;
    Msg.ShipName := DecodeText(Bits, 112, 120);
    Msg.HasName := Msg.ShipName <> '';
    Exit(True);
  end
  else
    Exit;

  if not GetBits(Bits, 8, 30, V) then Exit;
  Msg.MMSI := V;
  if Type18 then
  begin
    if not GetBits(Bits, 46, 10, SOGRaw) then Exit;
    if not SignedBits(Bits, 57, 28, LonRaw) then Exit;
    if not SignedBits(Bits, 85, 27, LatRaw) then Exit;
    if not GetBits(Bits, 112, 12, COGRaw) then Exit;
    if not GetBits(Bits, 124, 9, HeadingRaw) then Exit;
  end
  else
  begin
    if not GetBits(Bits, 50, 10, SOGRaw) then Exit;
    if not SignedBits(Bits, 61, 28, LonRaw) then Exit;
    if not SignedBits(Bits, 89, 27, LatRaw) then Exit;
    if not GetBits(Bits, 116, 12, COGRaw) then Exit;
    if not GetBits(Bits, 128, 9, HeadingRaw) then Exit;
  end;
  Msg.SOG := SOGRaw / 10.0;
  Msg.COG := COGRaw / 10.0;
  Msg.Heading := HeadingRaw;
  Msg.Longitude := LonRaw / 600000.0;
  Msg.Latitude := LatRaw / 600000.0;
  Msg.HasPosition := (Abs(Msg.Latitude) <= 90) and (Abs(Msg.Longitude) <= 180) and
    (LatRaw <> 91000000) and (LonRaw <> 181000000);
  Result := True;
end;

function DecodeAISPayload(const Payload: String; out Msg: TAISMessage): Boolean;
var
  Bits: array of Byte;
  I, V: Integer;
  C: Char;
begin
  SetLength(Bits, Length(Payload) * 6);
  for I := 1 to Length(Payload) do
  begin
    C := Payload[I];
    if (C < '0') or (C > 'w') then
    begin
      FillChar(Msg, SizeOf(Msg), 0);
      Exit(False);
    end;
    if C <= 'W' then V := Ord(C) - Ord('0')
    else V := Ord(C) - 56;
    if (V < 0) or (V > 63) then
    begin
      FillChar(Msg, SizeOf(Msg), 0);
      Exit(False);
    end;
    Bits[(I - 1) * 6] := (V shr 5) and 1;
    Bits[(I - 1) * 6 + 1] := (V shr 4) and 1;
    Bits[(I - 1) * 6 + 2] := (V shr 3) and 1;
    Bits[(I - 1) * 6 + 3] := (V shr 2) and 1;
    Bits[(I - 1) * 6 + 4] := (V shr 1) and 1;
    Bits[(I - 1) * 6 + 5] := V and 1;
  end;
  Result := DecodeAISBits(Bits, Length(Bits), Msg);
end;

end.
