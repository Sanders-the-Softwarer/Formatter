////////////////////////////////////////////////////////////////////////////////
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

{ Возврат парсера для списка деклараций }
function GetDeclarationsParser: TParserInfo;

{ Возврат парсера для одиночной декларации }
function GetDeclarationParser: TParserInfo;

{ Возврат парсера для операторов }
function GetStatementParser: TParserInfo;

{ Проверка наличия парсера для конструкции после end }
function HasAfterEndParser: boolean;

{ Возврат парсера для конструкции после end }
function GetAfterEndParser: TParserInfo;

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

{ Возврат парсера для списка деклараций }
function GetDeclarationsParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor(Language + '.Declarations');
  if not Assigned(Result) then raise Exception.CreateFmt('Declaration list for [%s] does not registered', [Language]);
end;

{ Возврат парсера для одиночной декларации }
function GetDeclarationParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor(Language + '.Declaration');
  if not Assigned(Result) then raise Exception.CreateFmt('Declarations for [%s] does not registered', [Language]);
end;

{ Возврат парсера для операторов }
function GetStatementParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor(Language + '.Statement');
  if not Assigned(Result) then raise Exception.CreateFmt('Statements for [%s] does not registered', [Language]);
end;

{ Проверка наличия парсера для конструкции после end }
function HasAfterEndParser: boolean;
var PI: TParserInfo;
begin
  PI := TParserInfo.InstanceFor(Language + '.AfterEnd');
  Result := Assigned(PI);
end;

{ Возврат парсера для конструкции после end }
function GetAfterEndParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor(Language + '.AfterEnd');
  if not Assigned(Result) then raise Exception.CreateFmt('After end statement for [%s] does not registered', [Language]);
end;

end.
