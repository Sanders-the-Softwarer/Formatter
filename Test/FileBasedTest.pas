////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Базовый класс  автотестов                         //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit FileBasedTest;

{ ----- Примечания -------------------------------------------------------------

  Как логично для форматизатора исходников, автотесты построены на сравнении
  результата форматирования той или иной конструкции с ожидаемым вариантом.
  Так как в Delphi неудобно задавать текстовые константы с SQL-кодом, данные
  тестов размещены в отдельном каталоге в виде независимых файлов, а класс
  теста считывает эти файлы и проверяет, что результат обработки входного
  совпадает с выходным

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, TestFramework, Printers_, Parser, Tokenizer, Streams;

type
  { Класс автотестов, проверяющих совпадение форматирования файла с результатом }
  TFileBasedTest = class(TTestCase)
  private
    { Чтение файла в строку }
    function LoadFile(const AFileName: string): string;
    { Приведение строки к наглядному для сравнения виду }
    function Beautify(const S: string): string;
    { Сравнение форматированного текста с ожидаемым }
    procedure Check(AText, AExpected: string);
  protected
    { Загрузка файлов и проверка форматирования }
    procedure CheckFile(const AFileName: string);
    { Запуск тестового метода }
    procedure Invoke(AMethod: TTestMethod); override;
  end;

implementation

{ Загрузка файлов и проверка форматирования }
procedure TFileBasedTest.CheckFile(const AFileName: string);
begin
  Check(LoadFile(AFileName + '.in'), LoadFile(AFileName + '.out'));
end;

{ Запуск тестового метода }
procedure TFileBasedTest.Invoke(AMethod: TTestMethod);
begin
  { От метода нам нужно только имя, по нему считываем файлы }
  CheckFile(GetName);
end;

{ Чтение файла в строку }
function TFileBasedTest.LoadFile(const AFileName: string): string;
begin
  with TStringList.Create do
  try
    LoadFromFile('.\Тестовые Данные\' + AFileName);
    Result := TrimRight(Text);
  finally
    Free;
  end;
end;

{ Приведение строки к наглядному для сравнения виду }
function TFileBasedTest.Beautify(const S: string): string;
const
  Liner = #13'---------->>----------'#13;
begin
  Result := Liner + StringReplace(S, ' ', #183, [rfReplaceAll]) + Liner;
end;

{ Сравнение форматированного текста с ожидаемым }
procedure TFileBasedTest.Check(AText, AExpected: string);
var
  Settings: TFormatSettings;
  Parser: TParser;
  Printer: TPrinter;
begin
  try
    Settings := TFormatSettings.Default;
    Parser := TParser.Create(
                TCommentProcessor.Create(
                  TMerger.Create(
                    TWhitespaceSkipper.Create(
                      TTokenizer.Create(
                        TPositionStream.Create(
                          TStringStream.Create(AText)))))), Settings);
    Printer := TPrinter.CreateFormatterPrinter(nil);
    Printer.Settings := Settings;
    Parser.PrintAll(Printer);
    CheckEquals(Beautify(AExpected), Beautify(Printer.GetText));
  finally
    FreeAndNil(Settings);
    FreeAndNil(Parser);
    FreeAndNil(Printer);
  end;
end;

end.

