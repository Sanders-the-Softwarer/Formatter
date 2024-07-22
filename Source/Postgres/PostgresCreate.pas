////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                     Команда CREATE с postgres спецификой                   //
//                                                                            //
//               Copyright(c) 2019-2024 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit PostgresCreate;

interface

uses Parser, Create;

type
  { Команда create [or replace] }
  TPostgresCreate = class(TCreate)
  strict protected
    function WhatParser: TParserInfo; override;
  end;

{ Список конструкций для команды CREATE }
function PostgresCreateParser: TParserInfo;

implementation

{ Список конструкций для команды CREATE }
function PostgresCreateParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor('Postgres.Create');
end;

{ TPostgresCreate }

function TPostgresCreate.WhatParser: TParserInfo;
begin
  Result := PostgresCreateParser;
end;

initialization
  with PostgresCreateParser do
  begin
  end;

end.
