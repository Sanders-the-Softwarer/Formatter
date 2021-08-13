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
  FastMM4 in 'Lib\FastMM4.pas',
  FastMM4Messages in 'Lib\FastMM4Messages.pas',
  SysUtils,
  DUnitTestRunner,
  TestFramework,
  OracleCore,
  TestDML in 'Test\TestDML.pas',
  FileBasedTest in 'Test\FileBasedTest.pas',
  TestExpressions in 'Test\TestExpressions.pas',
  TestDDL in 'Test\TestDDL.pas',
  TestPLSQL in 'Test\TestPLSQL.pas',
  TestBugs in 'Test\TestBugs.pas',
  TestSQLPLUS in 'Test\TestSQLPLUS.pas',
  UnderConstruction in 'Test\UnderConstruction.pas',
  TestSettings in 'Test\TestSettings.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.


