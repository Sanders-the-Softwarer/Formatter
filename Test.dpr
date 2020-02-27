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
  Attributes in 'Source\Attributes.pas',
  DDL in 'Source\DDL.pas',
  DML in 'Source\DML.pas',
  Expressions in 'Source\Expressions.pas',
  fMain in 'Source\fMain.pas' {FormMain},
  Parser in 'Source\Parser.pas',
  PLSQL in 'Source\PLSQL.pas',
  Printers_ in 'Source\Printers_.pas',
  SQLPlus in 'Source\SQLPlus.pas',
  Statements in 'Source\Statements.pas',
  Streams in 'Source\Streams.pas',
  Tokenizer in 'Source\Tokenizer.pas',
  Tokens in 'Source\Tokens.pas',
  TestExpressions in 'Test\TestExpressions.pas',
  TestDDL in 'Test\TestDDL.pas',
  TestPLSQL in 'Test\TestPLSQL.pas',
  TestRulers in 'Test\TestRulers.pas',
  Utils in 'Source\Utils.pas',
  Controller in 'Source\Controller.pas',
  TestBugs in 'Test\TestBugs.pas',
  TestSQLPLUS in 'Test\TestSQLPLUS.pas';

{$R *.RES}

type
  _UnderConstruction = class(TTestCase)
  public
    { отложим }
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
  Fail('Пока что не сделано');
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_Create_Trigger;
begin
  Fail('Пока что не сделано');
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_DeclarationSingleLineParamLimit;
begin
  Fail('Пока что не сделано');
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_MatchParamLimit;
begin
  Fail('Пока что не сделано');
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_PreferredExpressionLength;
begin
  Fail('Пока что не сделано');
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_ReplaceAsIs;
begin
  Fail('Пока что не сделано');
end;

procedure _UnderConstruction.Нужно_Добавить_Group_By_Rollup_Cube_Grouping_Sets;
begin
  Fail('Пока что не сделано');
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_ReplaceDefault;
begin
  Fail('Пока что не сделано');
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_Команды_SQLPLUS;
begin
  Fail('Пока что не сделано');
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_Невыравнивание_Везде_Где_Есть_На_Выравнивание;
begin
  Fail('Пока что не сделано');
end;

procedure _UnderConstruction.Нужно_Написать_Тесты_На_Объектные_Типы;
begin
  Fail('Пока что не сделано');
end;

begin
  RegisterTest(_UnderConstruction.Suite);
  DUnitTestRunner.RunRegisteredTests;
end.


