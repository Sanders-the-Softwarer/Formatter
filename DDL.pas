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

  { Объект view }
  TView = class(TStatement)
  strict private
    _View: TKeyword;
    _ViewName: TIdent;
    _As: TKeyword;
    _Select: TStatement;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function StatementName: string; override;
  end;

implementation

uses Parser, DML;

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
  APrinter.PrintItems([_Create, _Or, _Replace, _Force, _What]);
end;

function TCreate.StatementName: string;
begin
  Result := '';
  if Assigned(_What) then Result := _What.StatementName;
  if Result <> '' then Result := 'create ' + Result;
end;

{ TView }

function TView.InternalParse: boolean;
begin
  _View := Keyword('view');
  if not Assigned(_View) then exit(false);
  _ViewName := Identifier;
  _As := Keyword('as');
  TSelect.Parse(Self, Source, _Select);
  Result := true;
end;

procedure TView.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_View, _ViewName, _As]);
  APrinter.PrintIndented(_Select);
end;

function TView.StatementName: string;
begin
  if Assigned(_ViewName)
    then Result := 'view ' + _ViewName.Value
    else Result := '';
end;

end.

