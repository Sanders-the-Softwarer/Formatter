////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                    Команда CREATE с оракловой спецификой                   //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit OracleCreate;

interface

uses Statements, Tokens, Printer, Parser, Commons, Create;

type
  { Команда create [or replace] }
  TOracleCreate = class(TCreate)
  strict private
    _Editionable, _Force: TEpithet;
  strict protected
    procedure AdditionalParse; override;
    procedure AdditionalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses DDL, PLSQL, Sequence, Trigger, Role, Synonym, DatabaseLink, View, Context,
  Package, TypeBody, Subroutine, OracleCore;

{ TOracleCreate }

procedure TOracleCreate.AdditionalParse;
begin
  { Проверим наличие editionable }
  _Editionable := Keyword(['editionable', 'noneditionable']);
  { Проверим наличие force }
  _Force := Keyword('force');
end;

procedure TOracleCreate.AdditionalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Editionable, _Force]);
end;

end.
