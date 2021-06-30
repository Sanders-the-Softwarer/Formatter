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
  Set_SQLPlus in 'Source\SQL_Plus\Set_SQLPlus.pas',
  Intervals in 'Source\Core\Intervals.pas',
  GpCommandLineParser in 'Source\Core\GpCommandLineParser.pas',
  DML_Commons in 'Source\DML\DML_Commons.pas',
  Update in 'Source\DML\Update.pas';

type
  TCommandLine = class
  strict private
    FInFile, FOutFile, FSettingsFile: string;
  public
    [CLPName('i'), CLPLongName('InFile'), CLPDescription('Input file name', '<file-name>'), CLPRequired]
    property InFile: string read FInFile write FInFile;
    [CLPName('o'), CLPLongName('OutFile'), CLPDescription('Output file name', '<file-name>')]
    property OutFile: string read FOutFile write FOutFile;
    [CLPName('s'), CLPLongName('SettingsFile'), CLPDescription('Settings file name', '<file-name>')]
    property SettingsFile: string read FSettingsFile write FSettingsFile;
  end;

var
  InText, OutText: string;
  InFile, OutFile, SettingsFile: TStringList;
  Parser: IGpCommandLineParser;
  CmdLine: TCommandLine;
  Settings: TFormatSettings;
  S: string;

begin

  try
    { Если вызов без параметров или с неверными параметрами - ругнёмся }
    Parser := CommandLineParser;
    CmdLine := TCommandLine.Create;
    if (ParamCount < 1) or not Parser.Parse(CmdLine) then
    begin
      WriteLn('Usage:');
      WriteLn;
      for S in Parser.Usage do WriteLn(S);
      Halt(1);
    end;
    { Проверим наличие входного файла }
    if not FileExists(CmdLine.InFile) then
    begin
      WriteLn('File ' + CmdLine.InFile + ' does not exists');
      Halt(2);
    end;
    { Прочитаем его }
    InFile := nil;
    try
      InFile := TStringList.Create;
      try
        InFile.LoadFromFile(CmdLine.InFile);
        InText := InFile.Text;
      except
        on E: Exception do
        begin
          WriteLn('Error reading file ' + CmdLine.InFile);
          WriteLn(E.Message);
          Halt(3);
        end;
      end;
    finally
      FreeAndNil(InFile);
    end;
    { Сформируем имя выходного файла }
    if CmdLine.OutFile = '' then CmdLine.OutFile := CmdLine.InFile + '.formatted';
    if FileExists(CmdLine.OutFile) then
    begin
      WriteLn('File ' + CmdLine.OutFile + ' already exists');
      Halt(4);
    end;
    { Соберём настройки }
    Settings := TFormatSettings.Default;
    if CmdLine.SettingsFile <> '' then
    begin
      if not FileExists(CmdLine.SettingsFile) then
      begin
        WriteLn('File ' + CmdLine.SettingsFile + ' already exists');
        Halt(5);
      end;
      try
        SettingsFile := TStringList.Create;
        SettingsFile.LoadFromFile(CmdLine.SettingsFile);
        Settings.Load(SettingsFile);
        FreeAndNil(SettingsFile);
      except
        WriteLn('Error reading file ' + CmdLine.SettingsFile);
        Halt(6);
      end;
    end;
    { Отформатируем }
    Controller.MakeFormatted(InText, Settings, OutText);
    { И сохраним }
    OutFile := nil;
    try
      OutFile := TStringList.Create;
      OutFile.Text := OutText;
      try
        OutFile.SaveToFile(CmdLine.OutFile);
      except
        on E: Exception do
        begin
          WriteLn('Error writing file ' + CmdLine.OutFile);
          WriteLn(E.Message);
          Halt(7);
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

