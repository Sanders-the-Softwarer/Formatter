////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                                Команда CREATE                              //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Create;

interface

uses Commons, Statements, Tokens, Printer, Parser, Keywords;

type
  { Команда create [or replace] }
  TCreate = class(TTopStatement)
  strict private
    _Create, _Or, _Replace: TEpithet;
    _What: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  strict protected
    function WhatParser: TParserInfo; virtual; abstract;
    function AdditionalParse: boolean; virtual;
    procedure AdditionalPrintSelf(APrinter: TPrinter); virtual;
  public
    function StatementName: string; override;
    function Grouping: TStatementClass; override;
    function SameTypeAligned: TAlignMode; override;
  end;

implementation

{ TCreate }

function TCreate.InternalParse: boolean;
begin
  Result := true;
  { Если распознали слово create, то распознали конструкцию }
  _Create := Keyword('create');
  if not Assigned(_Create) then exit(false);
  { Проверим наличие or replace }
  _Or := Keyword('or');
  if Assigned(_Or) then _Replace := Keyword('replace');
  if Assigned(_Or) and not Assigned(_Replace) then exit(true);
  { Место для подключения дополнительных конструкций в наследниках }
  if not AdditionalParse then exit(false);
  { И, наконец, распознаем, что же мы создаём }
  if TParser.Parse(Source, Settings, WhatParser, Self, _What) or
     TUnexpectedToken.Parse(Self, Source, _What) then inherited;
end;

function TCreate.AdditionalParse: boolean;
begin
  Result := true;
end;

procedure TCreate.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Create, _Or, _Replace]);
  AdditionalPrintSelf(APrinter);
  APrinter.PrintItems([_What]);
  inherited;
end;

procedure TCreate.AdditionalPrintSelf(APrinter: TPrinter);
begin
  { ничего не делаем }
end;

function TCreate.StatementName: string;
begin
  Result := Concat([_Create, _Or, _Replace, _What]);
end;

function TCreate.Grouping: TStatementClass;
begin
  if Assigned(_What)
    then Result := _What.Grouping
    else Result := nil;
end;

function TCreate.SameTypeAligned: TAlignMode;
begin
  if Assigned(_What)
    then Result := _What.SameTypeAligned
    else Result := amNever;
end;

end.
