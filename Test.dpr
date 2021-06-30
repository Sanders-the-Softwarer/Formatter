program Test;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  DUnitTestRunner,
  TestFramework,
  TestDML in 'Test\TestDML.pas',
  FileBasedTest in 'Test\FileBasedTest.pas',
  TestExpressions in 'Test\TestExpressions.pas',
  TestDDL in 'Test\TestDDL.pas',
  TestPLSQL in 'Test\TestPLSQL.pas',
  TestBugs in 'Test\TestBugs.pas',
  TestSQLPLUS in 'Test\TestSQLPLUS.pas',
  BasePrinter in 'Source\Core\BasePrinter.pas',
  Commons in 'Source\Core\Commons.pas',
  Controller in 'Source\Core\Controller.pas',
  DDL in 'Source\DDL.pas',
  DML in 'Source\DML.pas',
  Expressions in 'Source\Core\Expressions.pas',
  FormatterPrinter in 'Source\Core\FormatterPrinter.pas',
  Keywords in 'Source\Core\Keywords.pas',
  Parser in 'Source\Core\Parser.pas',
  PLSQL in 'Source\PLSQL.pas',
  Printer in 'Source\Core\Printer.pas',
  Rulers in 'Source\Core\Rulers.pas',
  SQLPlus in 'Source\SQLPlus.pas',
  Statements in 'Source\Core\Statements.pas',
  Streams in 'Source\Core\Streams.pas',
  TextBuilder in 'Source\Core\TextBuilder.pas',
  Tokenizer in 'Source\Core\Tokenizer.pas',
  Tokens in 'Source\Core\Tokens.pas',
  Utils in 'Source\Core\Utils.pas',
  UnderConstruction in 'Test\UnderConstruction.pas',
  TestSettings in 'Test\TestSettings.pas',
  Insert in 'Source\DML\Insert.pas',
  Select in 'Source\DML\Select.pas',
  Alter in 'Source\DDL\Alter.pas',
  AlterPackageProcedureFunction in 'Source\DDL\AlterPackageProcedureFunction.pas',
  Create in 'Source\DDL\Create.pas',
  DatabaseLink in 'Source\DDL\DatabaseLink.pas',
  Drop in 'Source\DDL\Drop.pas',
  Grant in 'Source\DDL\Grant.pas',
  Role in 'Source\DDL\Role.pas',
  Sequence in 'Source\DDL\Sequence.pas',
  Session in 'Source\DDL\Session.pas',
  Set_ in 'Source\DDL\Set_.pas',
  Synonym in 'Source\DDL\Synonym.pas',
  Assignment in 'Source\PLSQL\Assignment.pas',
  Exit_ in 'Source\PLSQL\Exit_.pas',
  ForAll in 'Source\PLSQL\ForAll.pas',
  Goto_ in 'Source\PLSQL\Goto_.pas',
  Label_ in 'Source\PLSQL\Label_.pas',
  OpenFor in 'Source\PLSQL\OpenFor.pas',
  Trigger in 'Source\PLSQL\Trigger.pas',
  Exit_SQLPlus in 'Source\SQL_Plus\Exit_SQLPlus.pas',
  Set_SQLPlus in 'Source\SQL_Plus\Set_SQLPlus.pas',
  Intervals in 'Source\Core\Intervals.pas',
  DML_Commons in 'Source\DML\DML_Commons.pas',
  Update in 'Source\DML\Update.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.


