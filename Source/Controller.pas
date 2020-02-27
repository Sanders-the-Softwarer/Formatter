////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                       Вызовы основной функциональности                     //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Controller;

{ ----- Примечания -------------------------------------------------------------

  По сути, этот модуль - интерфейс для различных клиентов функциональности
  (консольный форматизатор, плагин к PL/SQL Developer, тестовое приложение,
  отладочное приложение и т. п.), скрывающий от них детали внутренней
  реализации (потоки и т. п., о чём им думать решительно незачем)

------------------------------------------------------------------------------ }

interface

uses SysUtils, Printers_, Streams, Tokens, Statements, Parser;

{ Форматирование текста, полученного в виде строки, с возвратом результата в строку }
procedure MakeFormatted(const AText: string; ASettings: TFormatSettings; out AResult: string);

{ Создание потока лексем из текста, полученного в виде строки }
function MakeTokenStream(const AText: string): TBufferedStream<TToken>;

{ Создание потока синтаксических конструкций из потока лексем }
function MakeStatementStream(ATokenStream: TBufferedStream<TToken>; ASettings: TFormatSettings): TParser;

implementation

uses Tokenizer;

{ Форматирование текста, полученного в виде строки, с возвратом результата в строку }
procedure MakeFormatted(const AText: string; ASettings: TFormatSettings; out AResult: string);
var
  Parser: TParser;
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
    Parser := MakeStatementStream(MakeTokenStream(AText), ASettings);
    { И выведем его результат на принтер }
    Printer := TPrinter.CreateFormatterPrinter;
    Printer.Settings := ASettings;
    Parser.PrintAll(Printer);
    AResult := Printer.GetText;
  finally
    FreeAndNil(Parser);
    FreeAndNil(Printer);
    FreeAndNil(Settings);
  end;
end;

{ Создание потока лексем из текста, полученного в виде строки }
function MakeTokenStream(const AText: string): TBufferedStream<TToken>;
begin
  Result := TMerger.Create(TCommentProcessor.Create(TSkipSpecCommentProcessor.Create(TWhitespaceSkipper.Create(TTokenizer.Create(TPositionStream.Create(TStringStream.Create(AText)))))));
end;

{ Создание потока синтаксических конструкций из потока лексем }
function MakeStatementStream(ATokenStream: TBufferedStream<TToken>; ASettings: TFormatSettings): TParser;
begin
  Result := TParser.Create(ATokenStream, ASettings);
end;

end.
