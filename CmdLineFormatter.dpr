program CmdLineFormatter;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  SysUtils,
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
  Alter in 'Source\DDL\Alter.pas',
  Create in 'Source\DDL\Create.pas',
  DatabaseLink in 'Source\DDL\DatabaseLink.pas',
  Role in 'Source\DDL\Role.pas',
  Sequence in 'Source\DDL\Sequence.pas',
  Synonym in 'Source\DDL\Synonym.pas',
  Trigger in 'Source\PLSQL\Trigger.pas',
  Set_ in 'Source\DDL\Set_.pas',
  Stats in 'Source\Core\Stats.pas';

var
  InFileName, OutFileName, InText, OutText: string;
  InFile, OutFile: TStringList;

begin
  try
    { ���� ����� ��� ����������, �������� }
    if ParamCount < 1 then
    begin
      WriteLn('Usage: CmdLineFormatter[.exe] in-file-name [out-file-name]');
      Halt(1);
    end;
    { �������� ������� �������� ����� }
    InFileName := ParamStr(1);
    if not FileExists(InFileName) then
    begin
      WriteLn('File ' + InFileName + ' does not exists');
      Halt(2);
    end;
    { ��������� ��� }
    InFile := nil;
    try
      InFile := TStringList.Create;
      try
        InFile.LoadFromFile(InFileName);
        InText := InFile.Text;
      except
        on E: Exception do
        begin
          WriteLn('Error reading file ' + InFileName);
          WriteLn(E.Message);
          Halt(3);
        end;
      end;
    finally
      FreeAndNil(InFile);
    end;
    { ���������� ��� ��������� ����� }
    if ParamCount > 1
      then OutFileName := ParamStr(2)
      else OutFileName := InFileName + '.formatted';
    if FileExists(OutFileName) then
    begin
      WriteLn('File ' + OutFileName + ' already exists');
      Halt(6);
    end;
    { ������������� }
    Controller.MakeFormatted(InText, TFormatSettings.Default, OutText);
    { � �������� }
    OutFile := nil;
    try
      OutFile := TStringList.Create;
      OutFile.Text := OutText;
      try
        OutFile.SaveToFile(OutFileName);
      except
        on E: Exception do
        begin
          WriteLn('Error writing file ' + OutFileName);
          WriteLn(E.Message);
          Halt(4);
        end;
      end;
    finally
      FreeAndNil(OutFile);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

