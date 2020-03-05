program CmdLineFormatter;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes,
  SysUtils,
  Attributes in 'Source\Attributes.pas',
  Controller in 'Source\Controller.pas',
  DDL in 'Source\DDL.pas',
  DML in 'Source\DML.pas',
  Expressions in 'Source\Expressions.pas',
  Parser in 'Source\Parser.pas',
  PLSQL in 'Source\PLSQL.pas',
  Printers_ in 'Source\Printers_.pas',
  SQLPlus in 'Source\SQLPlus.pas',
  Statements in 'Source\Statements.pas',
  Streams in 'Source\Streams.pas',
  Tokenizer in 'Source\Tokenizer.pas',
  Tokens in 'Source\Tokens.pas',
  Utils in 'Source\Utils.pas',
  FormatterPrinter in 'Source\FormatterPrinter.pas';

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
