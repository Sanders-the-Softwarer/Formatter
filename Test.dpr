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
  TestPLSQL in 'Test\TestPLSQL.pas';

{$R *.RES}

type
  _UnderConstruction = class(TTestCase)
  published
    procedure �����_��������_�����_��_�������_SQLPLUS;
    procedure �����_��������_�����_��_���������_����;
    procedure �����_��������_�����_��_Create_Trigger;
  end;

{ _UnderConstruction }

procedure _UnderConstruction.�����_��������_�����_��_Create_Trigger;
begin
  Fail('���� ��� �� �������');
end;

procedure _UnderConstruction.�����_��������_�����_��_�������_SQLPLUS;
begin
  Fail('���� ��� �� �������');
end;

procedure _UnderConstruction.�����_��������_�����_��_���������_����;
begin
  Fail('���� ��� �� �������');
end;

begin
  RegisterTest(_UnderConstruction.Suite);
  DUnitTestRunner.RunRegisteredTests;
end.

