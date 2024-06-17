﻿////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                     Заглавный модуль Oracle-синтаксиса                     //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit OracleCore;

{ ----- Примечания -------------------------------------------------------------

  Этот модуль выделен для того, чтобы избавить рабочие приложения проекта от
  необходимости подключать многочисленные оракловые модули. Идея в том, чтобы
  каждому из приложений было достаточно сделать uses OracleCore для того, чтобы
  всё необходимое было доступно, правильно инициализировано и готово к
  использованию. Таким образом мы как минимум избавляемся от необходимости
  делать одни и те же изменения в трёх местах (а в дальнейшем, вероятно - и
  более)

------------------------------------------------------------------------------ }

interface

uses Parser;

{ Парсер для ораклового синтаксиса }
function OracleParser: TParserInfo;

implementation

uses PLSQL, DML, DDL, SQLPlus;

{ Парсер для ораклового синтаксиса }
function OracleParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor('Oracle');
end;

end.
