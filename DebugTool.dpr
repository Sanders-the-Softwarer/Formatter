program DebugTool;

uses
  Vcl.Forms,
  fMain in 'Source\fMain.pas' {FormMain},
  Streams in 'Source\Streams.pas',
  Tokens in 'Source\Tokens.pas',
  Tokenizer in 'Source\Tokenizer.pas',
  Parser in 'Source\Parser.pas',
  Printers_ in 'Source\Printers_.pas',
  PLSQL in 'Source\PLSQL.pas',
  Expressions in 'Source\Expressions.pas',
  DML in 'Source\DML.pas',
  Statements in 'Source\Statements.pas',
  DDL in 'Source\DDL.pas',
  Attributes in 'Source\Attributes.pas',
  SQLPlus in 'Source\SQLPlus.pas',
  Utils in 'Source\Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

