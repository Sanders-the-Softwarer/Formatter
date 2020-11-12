program CmdLineFormatter;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  SysUtils,
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
  Utils in 'Source\Utils.pas',
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
    { Если вызов без параметров, ругнёмся }
    if ParamCount < 1 then
    begin
      WriteLn('Usage: CmdLineFormatter[.exe] in-file-name [out-file-name]');
      Halt(1);
    end;
    { Проверим наличие входного файла }
    InFileName := ParamStr(1);
    if not FileExists(InFileName) then
    begin
      WriteLn('File ' + InFileName + ' does not exists');
      Halt(2);
    end;
    { Прочитаем его }
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
    { Сформируем имя выходного файла }
    if ParamCount > 1
      then OutFileName := ParamStr(2)
      else OutFileName := InFileName + '.formatted';
    if FileExists(OutFileName) then
    begin
      WriteLn('File ' + OutFileName + ' already exists');
      Halt(6);
    end;
    { Отформатируем }
    Controller.MakeFormatted(InText, TFormatSettings.Default, OutText);
    { И сохраним }
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

