////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                       Синтаксические конструкции DDL                       //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit DDL;

interface

uses Tokens, Statements, Printers_;

type

  { Команда create [or replace] }
  TCreate = class(TStatement)
  strict private
    _Create, _Or, _Replace, _Force: TKeyword;
    _What: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

implementation

uses Parser;

{ TCreateStatement }

function TCreate.InternalParse: boolean;
begin
  { Если распознали слово create, то распознали конструкцию }
  _Create := Keyword('create');
  if not Assigned(_Create) then exit(false);
  { Проверим наличие or replace }
  _Or := Keyword('or');
  if Assigned(_Or) then _Replace := Keyword('replace');
  if Assigned(_Or) and not Assigned(_Replace) then exit(true);
  { Проверим наличие force }
  _Force := Keyword('force');
  { И, наконец, распознаем, что же мы создаём }
  TParser.ParseCreation(Self, Source, _What);
  Result := true;
end;

procedure TCreate.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Create, _Or, _Replace, _What]);
end;

function TCreate.StatementName: string;
begin
  Result := _What.StatementName;
  if Result <> '' then Result := 'create ' + Result;
end;

end.

