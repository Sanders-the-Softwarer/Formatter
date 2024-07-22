program CmdLineFormatter;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  SysUtils,
  Printer,
  Controller,
  OracleCore,
  PostgresCore,
  GpCommandLineParser in 'Source\Core\GpCommandLineParser.pas';

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

