program DebugTool;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {FormMain},
  Streams in 'Streams.pas',
  Tokens in 'Tokens.pas',
  Tokenizer in 'Tokenizer.pas',
  Parser in 'Parser.pas',
  Printers_ in 'Printers_.pas',
  PLSQL in 'PLSQL.pas',
  Expressions in 'Expressions.pas',
  DML in 'DML.pas',
  Statements in 'Statements.pas',
  DDL in 'DDL.pas',
  Attributes in 'Attributes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
