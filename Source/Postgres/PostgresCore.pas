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

uses Statements, ProgramBlock;

{ Парсер для Postgres синтаксиса }
function PostgresParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor('Postgres');
end;

initialization
  { Зарегистрируем реализации для Постгреса }
  TParserInfo.InstanceFor('Postgres.Separator').Add(TSeparator);
  TParserInfo.InstanceFor('Postgres.Declarations').Add(TDeclarations);
  { Регистрация синтаксических конструкций }
  with PostgresParser do
  begin
//    Add(TPostgresCreate);
  end;

end.
