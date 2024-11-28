unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mvMapViewer, mvDLEFpc, mvDE_BGRA, mvDE_RGBGraphics, Forms,
  Controls, Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, uresize,
  utypes, ureadpipe, RegExpr;

type

  { TFMain }

  TFMain = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    MAPRSMonitor: TMemo;
    MVMap: TMapView;
    MvBGRADrawingEngine1: TMvBGRADrawingEngine;
    MVDEFPC1: TMVDEFPC;
    TBZoomMap: TTrackBar;
    TMainLoop: TTimer;
    procedure FMainInit(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TBZoomMapChange(Sender: TObject);
    procedure DecodeAPRSMessage(const Data: String);
    procedure TMainLoopTimer(Sender: TObject);
  private

  public

  end;

var
  FMain: TFMain;
  OrigWidth, OrigHeight: Integer;
  APRSMessageObject: PAPRSMessage;
  ReadPipe : TReadPipeThread;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.FMainInit(Sender: TObject);
begin
  OrigWidth := Self.Width;
  OrigHeight := Self.Height;

  TBZoomMap.Position := MVMap.Zoom;
  StoreOriginalSizes(Self);

  ReadPipe := TReadPipeThread.Create('flexpacketaprspipe');
end;

procedure TFMain.FormResize(Sender: TObject);
var
  scaleFactorWidth, scaleFactorHeight, scaleFactor: Double;
  i: Integer;
begin
  scaleFactorWidth := Width / OrigWidth;
  scaleFactorHeight := Height / OrigHeight;
  scaleFactor := Min(scaleFactorWidth, scaleFactorHeight);

  for i := 0 to ControlCount - 1 do
    ResizeControl(Controls[i], scaleFactorWidth, scaleFactorHeight, scaleFactor);

end;

procedure TFMain.TBZoomMapChange(Sender: TObject);
begin
  MVMap.Zoom := TBZoomMap.Position;
end;

procedure TFMain.DecodeAPRSMessage(const Data: String);
var Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
   try
     Regex.Expression := '^.*?Fm ([A-Z0-9]{1,6}(?:-[0-9]{1,2})?) To ([A-Z0-9]{1,6})(?: Via ([A-Z0-9,-]+))? .*?>\[(\d{2}:\d{2}:\d{2})\].?\s*(.+)$';
     if Regex.Exec(Data) then
     begin
       WriteLn('Source: ', Regex.Match[1]);
       WriteLn('Destination: ', Regex.Match[2]);
       WriteLn('Path: ', Regex.Match[3]);
       WriteLn('Time: ', Regex.Match[4]);
       WriteLn('Payload: ', Regex.Match[5]);

       // FromCall: String;
       // ToCall: String;
       // Path: String;
       // Longitude: Double;
       // Latitude: Double;
       // Message: String;
       // Time: String

       New(APRSMessageObject);
       APRSMessageObject^.FromCall := Regex.Match[1];
       APRSMessageObject^.ToCall := Regex.Match[2];
       APRSMessageObject^.Path := Regex.Match[3];


       Regex.Expression := '^!(\d{4})\.(\d{2})(\w)\/(\d{5})\.(\d{2})(\w)(\w)(.+)$';
       if Regex.Exec(Regex.Match[5]) then
       begin
         WriteLn('Latitude: ', Regex.Match[1]);
         WriteLn('Longitude: ', Regex.Match[4]);
         WriteLn('Type/Icon: ', Regex.Match[7]);
         WriteLn('Message: ', Regex.Match[8]);

       end;
     end;
   finally
     Regex.Free;
   end;
end;

procedure TFMain.TMainLoopTimer(Sender: TObject);
begin
  DecodeAPRSMessage(ReadPipe.PipeData);
end;

end.

