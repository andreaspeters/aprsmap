program aprsmap;

{$APPTYPE GUI}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, indylaz, lazcontrols, tachartlazaruspkg, umain, uresize, ureadpipe,
  utypes, uaprs, uini, uigate, usettings, uinfo, umodes, ulastseen, urawmessage,
  u_rs41sg, ugps, ugpsd;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='APRSMap';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TFSettings, FSettings);
  Application.CreateForm(TFInfo, FInfo);
  Application.CreateForm(TFLastSeen, FLastSeen);
  Application.CreateForm(TFRawMessage, FRawMessage);
  Application.CreateForm(TFGPS, FGPS);
  Application.Run;
end.

