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
    _Slash: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function WhatParser: TParserInfo; override;
    function AdditionalParse: boolean; override;
    procedure AdditionalPrintSelf(APrinter: TPrinter); override;
  end;

{ Список конструкций для команды CREATE }
function OracleCreateParser: TParserInfo;

implementation

uses DDL, PLSQL, Sequence, Trigger, Role, Synonym, DatabaseLink, View, Context;

{ Список конструкций для команды CREATE }
function OracleCreateParser: TParserInfo;
begin
  Result := TParserInfo.InstanceFor('Oracle.DDL.Create');
end;

{ TOracleCreate }

function TOracleCreate.InternalParse: boolean;
begin
  Result := inherited;
  _Slash := Terminal('/');
end;

procedure TOracleCreate.InternalPrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.NextLineIf([_Slash]);
end;

function TOracleCreate.WhatParser: TParserInfo;
begin
  Result := OracleCreateParser;
end;

function TOracleCreate.AdditionalParse: boolean;
begin
  Result := true;
  { Проверим наличие editionable }
  _Editionable := Keyword(['editionable', 'noneditionable']);
  { Проверим наличие force }
  _Force := Keyword('force');
end;

procedure TOracleCreate.AdditionalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Editionable, _Force]);
end;

initialization
  with OracleCreateParser do
  begin
    Add(TTable);
    Add(TIndex);
    Add(TPackage);
    Add(TSubroutine);
    Add(TTypeBody);
    Add(TType);
    Add(TSequence);
    Add(TTrigger);
    Add(TSynonym);
    Add(TUser);
    Add(TRole);
    Add(TDatabaseLink);
  end;

end.
