program Formatter;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {FormMain},
  Streams in 'Streams.pas',
  Tokens in 'Tokens.pas',
  Printer in 'Printer.pas',
  Tokenizer in 'Tokenizer.pas',
  Parser in 'Parser.pas',
  TreePrinter in 'TreePrinter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

