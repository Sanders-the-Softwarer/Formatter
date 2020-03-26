////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                       Вызовы основной функциональности                     //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Controller;

{ ----- Примечания -------------------------------------------------------------

  По сути, этот модуль - интерфейс для различных клиентов функциональности
  (консольный форматизатор, плагин к PL/SQL Developer, тестовое приложение,
  отладочное приложение и т. п.), скрывающий от них детали внутренней
  реализации (потоки и т. п., о чём им думать решительно незачем).

  Процедура MakeFormatted проходит все шаги преобразований и формирует из
  строки на входе строку на выходе. Функция MakeMinimalTokenStream формирует
  входной поток лексем с расставленными координатами и выброшенными пробелами.

------------------------------------------------------------------------------ }

interface

uses SysUtils, PrinterIntf, FormatterPrinter, Streams, Tokens, Statements, Parser;

{ Форматирование текста, полученного в виде строки, с возвратом результата в строку }
procedure MakeFormatted(const AText: string; ASettings: TFormatSettings; out AResult: string);

{ Создание минимально обработанного потока лексем из текста, полученного в виде строки }
function MakeMinimalTokenStream(const AText: string): TBufferedStream<TToken>;

{ Создание дополнительно обработанного потока лексем из минимально обработанного }
function MakeAdvancedTokenStream(ATokenStream: TBufferedStream<TToken>): TBufferedStream<TToken>;

{ Создание потока синтаксических конструкций из потока лексем }
function MakeStatementStream(ATokenStream: TBufferedStream<TToken>; ASettings: TFormatSettings): TBufferedStream<TStatement>;

implementation

uses Tokenizer;

{ Форматирование текста, полученного в виде строки, с возвратом результата в строку }
procedure MakeFormatted(const AText: string; ASettings: TFormatSettings; out AResult: string);
var
  Parser: TBufferedStream<TStatement>;
  Printer: TPrinter;
  Settings: TFormatSettings;
begin
  try
    { Если настроек не передано, используем стандартные }
    if Assigned(ASettings) then
      Settings := nil
    else
      begin
        Settings := TFormatSettings.Default;
        ASettings := Settings;
      end;
    { Создадим парсер входного текста }
    Parser := MakeStatementStream(MakeAdvancedTokenStream(MakeMinimalTokenStream(AText)), ASettings);
    { И выведем его результат на принтер }
    Printer := TFormatterPrinter.Create(ASettings);
    Parser.PrintAll(Printer);
    AResult := Printer.GetText;
  finally
    FreeAndNil(Parser);
    FreeAndNil(Printer);
    FreeAndNil(Settings);
  end;
end;

{ Создание минимально обработанного потока лексем из текста, полученного в виде строки }
function MakeMinimalTokenStream(const AText: string): TBufferedStream<TToken>;
begin
  Result := TSkipSpecCommentProcessor.Create(TWhitespaceSkipper.Create(TTokenizer.Create(TPositionStream.Create(TStringStream.Create(AText)))));
end;

{ Создание потока лексем из текста, полученного в виде строки }
function MakeAdvancedTokenStream(ATokenStream: TBufferedStream<TToken>): TBufferedStream<TToken>;
begin
  Assert(ATokenStream <> nil);
  Result := TMerger.Create(TCommentProcessor.Create(ATokenStream));
end;

{ Создание потока синтаксических конструкций из потока лексем }
function MakeStatementStream(ATokenStream: TBufferedStream<TToken>; ASettings: TFormatSettings): TBufferedStream<TStatement>;
begin
  Result := TSameTypeLinker.Create(TParser.Create(ATokenStream, ASettings));
end;

end.
