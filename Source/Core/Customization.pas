﻿////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                Средства кастомизации универсальных парсеров                //
//                                                                            //
//               Copyright(c) 2019-2024 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Customization;

{ ----- Примечания -------------------------------------------------------------

  С появлением поддержки второго языка возникает желание использовать общий
  код для работы со схожими синтаксическими конструкциями - подпрограммами,
  списками параметров, блоками операторов и т. д. Для того, чтобы это стало
  возможным, необходимо кастомизировать ряд мест общего кода умением разобрать
  конструкцию конкретного языка - например, общий код разбора подпрограммы
  должен уметь разобрать Оракл- либо Постгрес-специфичный синтаксис параметров.

  В принципе, для этого нужно нечто вроде фабрики классов, которая возвращала
  бы реализации для конкретного места и случая. Но у нас в приложении уже
  реализована иерархия парсеров, что позволяет просто и красиво добавить такой
  функционал без новых связей и более того, вывести кастомизацию на уровень
  выбора главного парсера, определяющего, с каким языком мы вообще работаем.

  В принципе, содержимое этого модуля можно рассматривать как часть модуля
  Parser, но на текущий момент мне кажется красивее и правильнее вынести
  его отдельно.

------------------------------------------------------------------------------ }

interface

uses SysUtils, Parser;

{ Выбор языка программирования, с которым работает форматизатор }
procedure SetLanguage(const ALanguage: string);

{ Возврат парсера выбранного языка программирования }
function GetLanguageParser: TParserInfo;

{ Возврат парсера для разделителя синтаксических конструкций }
function GetSeparatorParser: TParserInfo;

implementation

var
  Language: string = 'Oracle';

{ Выбор языка программирования, с которым работает форматизатор }
procedure SetLanguage(const ALanguage: string);
begin
  Language := ALanguage;
end;

{ Возврат парсера выбранного языка программирования }
function GetLanguageParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor(Language);
  if not Assigned(Result) then raise Exception.CreateFmt('Language [%s] does not supported', [Language]);
end;

{ Возврат парсера для разделителя синтаксических конструкций }
function GetSeparatorParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor(Language + '.Separator');
  if not Assigned(Result) then raise Exception.CreateFmt('Separator for [%s] does not registered', [Language]);
end;

end.
