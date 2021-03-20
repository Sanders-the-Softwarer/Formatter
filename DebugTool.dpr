program DebugTool;

uses
  Vcl.Forms,
  fMain in 'Source\GUI\fMain.pas' {FormMain},
  Streams in 'Source\Core\Streams.pas',
  Tokens in 'Source\Core\Tokens.pas',
  Tokenizer in 'Source\Core\Tokenizer.pas',
  Parser in 'Source\Core\Parser.pas',
  Printer in 'Source\Core\Printer.pas',
  PLSQL in 'Source\PLSQL.pas',
  Expressions in 'Source\Core\Expressions.pas',
  DML in 'Source\DML.pas',
  DDL in 'Source\DDL.pas',
  SQLPlus in 'Source\SQLPlus.pas',
  Utils in 'Source\Core\Utils.pas',
  Controller in 'Source\Core\Controller.pas',
  GUIPrinters in 'Source\GUI\GUIPrinters.pas',
  FormatterPrinter in 'Source\Core\FormatterPrinter.pas',
  Commons in 'Source\Core\Commons.pas',
  BasePrinter in 'Source\Core\BasePrinter.pas',
  TextBuilder in 'Source\Core\TextBuilder.pas',
  Rulers in 'Source\Core\Rulers.pas',
  Trigger in 'Source\PLSQL\Trigger.pas',
  Keywords in 'Source\Core\Keywords.pas',
  Role in 'Source\DDL\Role.pas',
  Sequence in 'Source\DDL\Sequence.pas',
  Alter in 'Source\DDL\Alter.pas',
  Synonym in 'Source\DDL\Synonym.pas',
  Create in 'Source\DDL\Create.pas',
  DatabaseLink in 'Source\DDL\DatabaseLink.pas',
  Select in 'Source\DML\Select.pas',
  Statements in 'Source\Core\Statements.pas',
  Set_ in 'Source\DDL\Set_.pas',
  Insert in 'Source\DML\Insert.pas',
  Session in 'Source\DDL\Session.pas',
  Set_SQLPlus in 'Source\SQL_Plus\Set_SQLPlus.pas',
  AlterPackageProcedureFunction in 'Source\DDL\AlterPackageProcedureFunction.pas',
  Exit_SQLPlus in 'Source\SQL_Plus\Exit_SQLPlus.pas',
  Grant in 'Source\DDL\Grant.pas',
  Drop in 'Source\DDL\Drop.pas',
  Label_ in 'Source\PLSQL\Label_.pas',
  Goto_ in 'Source\PLSQL\Goto_.pas',
  Exit_ in 'Source\PLSQL\Exit_.pas',
  OpenFor in 'Source\PLSQL\OpenFor.pas',
  ForAll in 'Source\PLSQL\ForAll.pas',
  Assignment in 'Source\PLSQL\Assignment.pas',
  Intervals in 'Source\Core\Intervals.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

