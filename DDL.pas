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

uses Tokens, Statements, Printers_, Attributes;

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

  { Команда comment }
  TComment = class(TSemicolonStatement)
  strict private
    _Comment, _On, _TableOrColumn: TKeyword;
    _Name: TStatement;
    _Is: TKeyword;
    _Text: TLiteral;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Группа комментариев }
  [Aligned]
  TComments = class(TStatementList<TComment>)
  strict private
    function ParseBreak: boolean; override;
  end;

implementation

uses Parser, DML, Expressions;

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

{ TComment }

function TComment.InternalParse: boolean;
begin
  _Comment := Keyword('comment');
  if not Assigned(_Comment) then exit(false);
  _On := Keyword('on');
  _TableOrColumn := Keyword(['table', 'column']);
  TQualifiedIdent.Parse(Self, Source, _Name);
  _Is := Keyword('is');
  _Text := Literal;
  inherited;
  Result := true;
end;

procedure TComment.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Comment, _On, _TableOrColumn, _Name]);
  APrinter.Ruler('is', Settings.AlignTableColumnComments);
  APrinter.PrintItems([_Is, _Text]);
  inherited;
end;

{ TComments }

function TComments.ParseBreak: boolean;
begin
  Result := true; { последовательность комментариев прерывает всё что угодно }
end;

end.

