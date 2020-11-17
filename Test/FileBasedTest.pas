////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Базовый класс  автотестов                         //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
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

uses Classes, SysUtils, StrUtils, TestFramework, Printer, Controller;

type
  { Класс автотестов, проверяющих совпадение форматирования файла с результатом }
  TFileBasedTest = class(TTestCase)
  protected
    Settings: TFormatSettings;
    Skip: boolean;
  protected
    procedure UnderConstruction;
    { Настройки каталога размещения и расширений входного-выходного файлов }
    function GetDir: string; virtual;
    function GetExtIn: string; virtual;
    function GetExtOut: string; virtual;
    function GetFileNameToSave(const AFileName: string): string; virtual;
  protected
    { Загрузка файлов и проверка форматирования }
    procedure CheckFile(AFileName: string);
    { Запуск тестового метода }
    procedure Invoke(AMethod: TTestMethod); override;
    { Чтение файла в строку }
    function LoadFile(const AFileName: string): string;
    { Приведение строки к наглядному для сравнения виду }
    function Beautify(const S: string): string;
    { Сравнение форматированного текста с ожидаемым }
    procedure Check(AText, AExpected, ASaveToFile: string);
    { Отложить тест до указанной даты }
    procedure PostponeTill(AYear, AMonth, ADay: integer);
  protected
    { Подготовка к выполнению теста }
    procedure SetUp; override;
    { Уборка после выполненного теста }
    procedure TearDown; override;
  end;

implementation

{ Подготовка к выполнению теста }
procedure TFileBasedTest.SetUp;
begin
  Settings := TFormatSettings.ForTest; { По умолчанию отключаем то, что приводит к усложнению результата }
end;

{ Уборка после выполненного теста }
procedure TFileBasedTest.TearDown;
begin
  FreeAndNil(Settings);
end;

{ Оповещение о том, что тест не реализован }
procedure TFileBasedTest.UnderConstruction;
begin
  if Now < EncodeDate(2020, 11, 21)
    then Skip := true
    else Fail('Пока не сделано!');
end;

{ Настройки каталога размещения и расширений входного-выходного файлов }

function TFileBasedTest.GetDir: string;
begin
  Result := '..\Фичи\';
end;

function TFileBasedTest.GetExtIn: string;
begin
  Result := '.in';
end;

function TFileBasedTest.GetExtOut: string;
begin
  Result := '.out';
end;

function TFileBasedTest.GetFileNameToSave(const AFileName: string): string;
begin
  Result := '';
end;

{ Загрузка файлов и проверка форматирования }
procedure TFileBasedTest.CheckFile(AFileName: string);
var Dir, FileNameIn, FileNameOut, FileNameSave, TextIn, TextOut: string;
begin
  if AFileName.StartsWith('_') then AFileName := AFileName.Substring(1);
  Dir := IncludeTrailingPathDelimiter(GetDir);
  FileNameIn  := Dir + AFileName + GetExtIn;
  FileNameOut := Dir + AFileName + GetExtOut;
  FileNameSave := GetFileNameToSave(Dir + AFileName);
  TextIn      := LoadFile(FileNameIn);
  if FileExists(FileNameOut)
    then TextOut := LoadFile(FileNameOut)
    else TextOut := TextIn;
  Check(TextIn, TextOut, FileNameSave);
end;

{ Запуск тестового метода }
procedure TFileBasedTest.Invoke(AMethod: TTestMethod);
begin
  Skip := false;
  AMethod; { само тело теста нужно только для настройки опций, если требуется }
  if not Skip then CheckFile(GetName); { а теперь по имени теста сравним файлы }
end;

{ Чтение файла в строку }
function TFileBasedTest.LoadFile(const AFileName: string): string;
var i: integer;
begin
  with TStringList.Create do
  try
    LoadFromFile(AFileName);
    for i := 0 to Count - 1 do Strings[i] := TrimRight(Strings[i]);
    Result := TrimRight(Text);
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
procedure TFileBasedTest.Check(AText, AExpected, ASaveToFile: string);
var Actual: string;
begin
  { С вероятностью 1/2 поставим либо не поставим на входе последний перевод
    строки, результат на выходе не должен от этого зависеть }
  if Random >= 0.5
    then AText := TrimRight(AText) + #13
    else AText := TrimRight(AText);
  Controller.MakeFormatted(AText, Settings, Actual);
  if ASaveToFile <> '' then
    with TStringList.Create do
    try
      Text := Actual;
      SaveToFile(ASaveToFile);
    finally
      Free;
    end;
  CheckEquals(Beautify(AExpected), Beautify(Actual));
end;

{ Отложить тест до указанной даты }
procedure TFileBasedTest.PostponeTill(AYear, AMonth, ADay: integer);
begin
  Skip := Date < EncodeDate(AYear, AMonth, ADay);
end;

end.

