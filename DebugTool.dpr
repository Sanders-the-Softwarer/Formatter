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
  AlterPackageProcedureFunction in 'Source\DDL\AlterPackageProcedureFunction.pas',
  Grant in 'Source\DDL\Grant.pas',
  Drop in 'Source\DDL\Drop.pas',
  Label_ in 'Source\PLSQL\Label_.pas',
  Goto_ in 'Source\PLSQL\Goto_.pas',
  Exit_ in 'Source\PLSQL\Exit_.pas',
  OpenFor in 'Source\PLSQL\OpenFor.pas',
  ForAll in 'Source\PLSQL\ForAll.pas',
  Assignment in 'Source\PLSQL\Assignment.pas',
  Intervals in 'Source\Core\Intervals.pas',
  Update in 'Source\DML\Update.pas',
  DML_Commons in 'Source\DML\DML_Commons.pas',
  Call in 'Source\DML\Call.pas',
  Accept in 'Source\SQL_Plus\Accept.pas',
  At in 'Source\SQL_Plus\At.pas',
  Clear in 'Source\SQL_Plus\Clear.pas',
  Define in 'Source\SQL_Plus\Define.pas',
  Execute in 'Source\SQL_Plus\Execute.pas',
  Exit_SQLPlus in 'Source\SQL_Plus\Exit_SQLPlus.pas',
  Host in 'Source\SQL_Plus\Host.pas',
  Set_SQLPlus in 'Source\SQL_Plus\Set_SQLPlus.pas',
  Slash in 'Source\SQL_Plus\Slash.pas',
  Variable in 'Source\SQL_Plus\Variable.pas',
  Undefine in 'Source\SQL_Plus\Undefine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

