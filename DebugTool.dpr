program DebugTool;

uses
  Vcl.Forms,
  fMain in 'Source\GUI\fMain.pas' {FormMain},
  Streams in 'Source\Streams.pas',
  Tokens in 'Source\Tokens.pas',
  Tokenizer in 'Source\Tokenizer.pas',
  Parser in 'Source\Parser.pas',
  Printer in 'Source\Printer.pas',
  PLSQL in 'Source\PLSQL.pas',
  Expressions in 'Source\Expressions.pas',
  DML in 'Source\DML.pas',
  Statements in 'Source\Statements.pas',
  DDL in 'Source\DDL.pas',
  SQLPlus in 'Source\SQLPlus.pas',
  Utils in 'Source\Utils.pas',
  Controller in 'Source\Controller.pas',
  GUIPrinters in 'Source\GUI\GUIPrinters.pas',
  FormatterPrinter in 'Source\FormatterPrinter.pas',
  Commons in 'Source\Commons.pas',
  BasePrinter in 'Source\BasePrinter.pas',
  TextBuilder in 'Source\TextBuilder.pas',
  Rulers in 'Source\Rulers.pas',
  Trigger in 'Source\PLSQL\Trigger.pas',
  Keywords in 'Source\Keywords.pas',
  Role in 'Source\DDL\Role.pas',
  Sequence in 'Source\DDL\Sequence.pas',
  Alter in 'Source\DDL\Alter.pas',
  Synonym in 'Source\DDL\Synonym.pas',
  Create in 'Source\DDL\Create.pas',
  DatabaseLink in 'Source\DDL\DatabaseLink.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

