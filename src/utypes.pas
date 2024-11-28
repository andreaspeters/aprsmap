unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  TAPRSMessage = record
    FromCall: String;
    ToCall: String;
    Path: String;
    Longitude: Double;
    Latitude: Double;
    Message: String;
    Time: String
  end;

  PAPRSMessage = ^TAPRSMessage;

implementation

end.

