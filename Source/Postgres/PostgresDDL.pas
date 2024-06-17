////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                   Синтаксические конструкции Postgres DDL                  //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit PostgresDDL;

interface

uses Parser;

{ Парсер для DDL }
function PostgresDDLParser: TParserInfo;

implementation

uses Keywords, PostgresCore, PostgresCreate;

{ Парсер для DDL }
function PostgresDDLParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor('Postgres.DDL');
end;

initialization
  { Зарегистрируем конструкции DDL }
  with PostgresDDLParser do
  begin
    Add(TPostgresCreate);
  end;
  { И добавим их в общий postgres синтаксис }
  PostgresParser.Add(PostgresDDLParser);

end.

