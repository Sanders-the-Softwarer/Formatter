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
  Trigger in 'Source\PLSQL\Trigger.pas',
  TestExpressions in 'Test\TestExpressions.pas',
  TestDDL in 'Test\TestDDL.pas',
  TestPLSQL in 'Test\TestPLSQL.pas',
  TestBugs in 'Test\TestBugs.pas',
  TestSQLPLUS in 'Test\TestSQLPLUS.pas',
  Select in 'Source\DML\Select.pas',
  Alter in 'Source\DDL\Alter.pas',
  Create in 'Source\DDL\Create.pas',
  DatabaseLink in 'Source\DDL\DatabaseLink.pas',
  Role in 'Source\DDL\Role.pas',
  Sequence in 'Source\DDL\Sequence.pas',
  Synonym in 'Source\DDL\Synonym.pas',
  BasePrinter in 'Source\BasePrinter.pas',
  Commons in 'Source\Commons.pas',
  Controller in 'Source\Controller.pas',
  DDL in 'Source\DDL.pas',
  DML in 'Source\DML.pas',
  Expressions in 'Source\Expressions.pas',
  FormatterPrinter in 'Source\FormatterPrinter.pas',
  Keywords in 'Source\Keywords.pas',
  Parser in 'Source\Parser.pas',
  PLSQL in 'Source\PLSQL.pas',
  Printer in 'Source\Printer.pas',
  Rulers in 'Source\Rulers.pas',
  SQLPlus in 'Source\SQLPlus.pas',
  Statements in 'Source\Statements.pas',
  Streams in 'Source\Streams.pas',
  TextBuilder in 'Source\TextBuilder.pas',
  Tokenizer in 'Source\Tokenizer.pas',
  Tokens in 'Source\Tokens.pas',
  Utils in 'Source\Utils.pas';

{$R *.RES}

type
  _UnderConstruction = class(TTestCase)
  protected
    procedure PostponeTill(AYear, AMonth, ADay: integer);
  published
    procedure Нужно_Написать_Тесты_На_Команды_SQLPLUS;
    procedure Нужно_Написать_Тесты_На_Объектные_Типы;
    procedure Нужно_Написать_Тесты_На_Create_Trigger;
    procedure Нужно_Написать_Тесты_На_Невыравнивание_Везде_Где_Есть_На_Выравнивание;
    procedure Нужно_Написать_Тесты_На_DeclarationSingleLineParamLimit;
    procedure Нужно_Написать_Тесты_На_ArgumentSingleLineParamLimit;
    procedure Нужно_Написать_Тесты_На_PreferredExpressionLength;
    procedure Нужно_Написать_Тесты_На_MatchParamLimit;
    procedure Нужно_Написать_Тесты_На_ReplaceDefault;
    procedure Нужно_Написать_Тесты_На_ReplaceAsIs;
    procedure Нужно_Добавить_Group_By_Rollup_Cube_Grouping_Sets;
  end;

{ _UnderConstruction }

procedure _UnderConstruction.Нужно_Написать_Тесты_На_ArgumentSingleLineParamLimit;
begin
  PostponeTill(2020, 4, 30);
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_Create_Trigger;
begin
  PostponeTill(2020, 4, 30);
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_DeclarationSingleLineParamLimit;
begin
  PostponeTill(2020, 4, 30);
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_MatchParamLimit;
begin
  PostponeTill(2020, 4, 30);
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_PreferredExpressionLength;
begin
  PostponeTill(2020, 4, 30);
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_ReplaceAsIs;
begin
  PostponeTill(2020, 4, 30);
end;

procedure _UnderConstruction.Нужно_Добавить_Group_By_Rollup_Cube_Grouping_Sets;
begin
  PostponeTill(2020, 4, 30);
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_ReplaceDefault;
begin
  PostponeTill(2020, 4, 30);
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_Команды_SQLPLUS;
begin
  PostponeTill(2020, 4, 30);
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_Невыравнивание_Везде_Где_Есть_На_Выравнивание;
begin
  PostponeTill(2020, 4, 30);
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_Объектные_Типы;
begin
  PostponeTill(2020, 4, 30);
end;

procedure _UnderConstruction.PostponeTill(AYear, AMonth, ADay: integer);
begin
  Check(Now < EncodeDate(AYear, AMonth, ADay), 'Пока не сделано!');
end;

begin
  RegisterTest(_UnderConstruction.Suite);
  DUnitTestRunner.RunRegisteredTests;
end.


