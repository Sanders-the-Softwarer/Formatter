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

uses Statements, Tokens, Printer;

type
  { Команда create [or replace] }
  TCreate = class(TSemicolonStatement)
  strict private
    _Create, _Or, _Replace, _Editionable, _Force: TEpithet;
    _What: TStatement;
    _Slash: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    function Grouping: TStatementClass; override;
  end;

implementation

uses DDL, PLSQL, Sequence, Trigger, Role, Synonym, DatabaseLink;

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
  { Проверим наличие editionable }
  _Editionable := Keyword(['editionable', 'noneditionable']);
  { Проверим наличие force }
  _Force := Keyword('force');
  { И, наконец, распознаем, что же мы создаём }
  if TTable.Parse(Self, Source, _What) or
     TView.Parse(Self, Source, _What) or
     TIndex.Parse(Self, Source, _What) or
     TPackage.Parse(Self, Source, _What) or
     TSubroutine.Parse(Self, Source, _What) or
     TTypeBody.Parse(Self, Source, _What) or
     TType.Parse(Self, Source, _What) or
     TSequence.Parse(Self, Source, _What) or
     TTrigger.Parse(Self, Source, _What) or
     TSynonym.Parse(Self, Source, _What) or
     TUser.Parse(Self, Source, _What) or
     TRole.Parse(Self, Source, _What) or
     TDatabaseLink.Parse(Self, Source, _What) or
     TUnexpectedToken.Parse(Self, Source, _What) then inherited;
  { Завершающий слеш }
  _Slash := Terminal('/');
end;

procedure TCreate.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Create, _Or, _Replace, _Editionable, _Force, _What]);
  inherited;
  APrinter.NextLineIf([_Slash]);
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

end.
