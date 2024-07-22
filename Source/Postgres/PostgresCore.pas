////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                    Заглавный модуль Postgres-синтаксиса                    //
//                                                                            //
//               Copyright(c) 2019-2024 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit PostgresCore;

interface

uses SysUtils, Parser;

{ Парсер для Postgres синтаксиса }
function PostgresParser: TParserInfo;

implementation

uses Statements;

{ Парсер для Postgres синтаксиса }
function PostgresParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor('Postgres');
end;

initialization
  { Зарегистрируем разделитель для Постгреса }
  TParserInfo.InstanceFor('Postgres.Separator').Add(TSeparator);
  { Регистрация синтаксических конструкций }
  with PostgresParser do
  begin
//    Add(TPostgresCreate);
  end;

end.
