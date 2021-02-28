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
  Set_SQLPlus in 'Source\SQL_Plus\Set_SQLPlus.pas';

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

