program DebugTool;

uses
  FastMM4 in 'Lib\FastMM4.pas',
  FastMM4Messages in 'Lib\FastMM4Messages.pas',
  Vcl.Forms,
  OracleCore,
  PostgresCore,
  fMain in 'Source\GUI\fMain.pas' {FormMain},
  GUIPrinters in 'Source\GUI\GUIPrinters.pas',
  frSettings in 'Source\GUI\frSettings.pas' {FrameSettings: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

