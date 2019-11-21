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
  совпадает с выходным.

  Имена файлов берутся из названия теста. Тело метода теста может быть пустым
  (в этом случае просто обрабатываются файлы), но выполняется - поэтому в нём
  можно сделать, например, настройку опций форматирования для тех тестов,
  которые проверяют именно их.

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
    Settings: TFormatSettings;
  protected
    { Загрузка файлов и проверка форматирования }
    procedure CheckFile(const AFileName: string);
    { Запуск тестового метода }
    procedure Invoke(AMethod: TTestMethod); override;
  public
    { Подготовка к выполнению теста }
    procedure SetUp; override;
    { Уборка после выполненного теста }
    procedure TearDown; override;
  end;

implementation

{ Подготовка к выполнению теста }
procedure TFileBasedTest.SetUp;
begin
  { По умолчанию отключим то, что приводит к усложнению результата }
  Settings := TFormatSettings.Default;
  Settings.AlignVariables   := false;
  Settings.AlignFields      := false;
  Settings.AlignExpressions := false;
  Settings.DeclarationSingleLineParamLimit := 99;
  Settings.ArgumentSingleLineParamLimit := 99;
  Settings.PreferredExpressionLength := 9999;
  Settings.MatchParamLimit  := 99999;
  Settings.ReplaceDefault   := false;
  Settings.ReplaceAsIs      := false;
end;

{ Уборка после выполненного теста }
procedure TFileBasedTest.TearDown;
begin
  FreeAndNil(Settings);
end;

{ Загрузка файлов и проверка форматирования }
procedure TFileBasedTest.CheckFile(const AFileName: string);
begin
  Check(LoadFile(AFileName + '.in'), LoadFile(AFileName + '.out'));
end;

{ Запуск тестового метода }
procedure TFileBasedTest.Invoke(AMethod: TTestMethod);
begin
  AMethod; { само тело теста нужно только для настройки опций, если требуется }
  CheckFile(GetName); { а теперь по имени теста сравним файлы }
end;

{ Чтение файла в строку }
function TFileBasedTest.LoadFile(const AFileName: string): string;
var i: integer;
begin
  with TStringList.Create do
  try
    LoadFromFile('.\Тестовые Данные\' + AFileName);
    for i := 0 to Count - 1 do Strings[i] := TrimRight(Strings[i]);
    { С вероятностью 1/2 поставим либо не поставим на входе последний перевод
      строки, результат на выходе не должен от этого зависеть }
    if (Random >= 0.5) and AFileName.EndsWith('.in')
      then Result := Text
      else Result := TrimRight(Text);
  finally
    Free;
  end;
end;

{ Приведение строки к наглядному для сравнения виду }
function TFileBasedTest.Beautify(const S: string): string;
const
  Liner = #13#10'---------->>----------'#13#10;
begin
  Result := Liner + StringReplace(S, ' ', #183, [rfReplaceAll]) + Liner;
end;

{ Сравнение форматированного текста с ожидаемым }
procedure TFileBasedTest.Check(AText, AExpected: string);
var
  Parser: TParser;
  Printer: TPrinter;
  Expected, Actual: string;
begin
  try
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
    Expected := Beautify(AExpected);
    Actual   := Beautify(Printer.GetText);
    CheckEquals(Expected, Actual);
  finally
    FreeAndNil(Parser);
    FreeAndNil(Printer);
  end;
end;

end.

