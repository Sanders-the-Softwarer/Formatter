////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                    Описание VIEW для команд CREATE/ALTER                   //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit View;

interface

uses Tokens, Statements, Printer;

type

  { Общий класс описания view }
  TView = class(TStatement)
  strict private
    _View, _As: TEpithet;
    _ViewName, _Columns, _Select: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { view для команды create }
  TViewCreate = class(TView)
  strict private
    _As: TEpithet;
    _Columns, _Select: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { view для команды alter }
  TViewAlter = class(TView)
  strict private
    _Add, _Modify, _Constraint, _ConstraintName, _Rely, _Drop, _Primary, _Key,
      _Unique, _Compile, _Read, _Only, _Editionable: TEpithet;
    _ConstraintDesc, _UniqueFields: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses DDL, Commons, Select;

{ TView }

function TView.InternalParse: boolean;
begin
  _View := Keyword('view');
  Result := Assigned(_View);
  if Result then TQualifiedIdent.Parse(Self, Source, _ViewName);
end;

procedure TView.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_View, _ViewName]);
end;

function TView.StatementName: string;
begin
  Result := Concat([_View, _ViewName]);
end;

{ TViewCreate }

function TViewCreate.InternalParse: boolean;
begin
  Result := inherited;
  if not Result then exit;
  TBracketedStatement<TIdentFields>.Parse(Self, Source, _Columns);
  _As := Keyword('as');
  TSelect.Parse(Self, Source, _Select);
end;

procedure TViewCreate.InternalPrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.NextLineIf([_IndentNextLine, _Columns, _UndentNextLine]);
  APrinter.PrintItem(_As);
  APrinter.PrintIndented(_Select);
end;

{ TViewAlter }

function TViewAlter.InternalParse: boolean;
begin
  Result := inherited;
  if not Result then exit;
  { editionable }
  _Editionable := Keyword(['editionable', 'noneditionable']);
  if Assigned(_Editionable) then exit;
  { read only/write }
  _Read := Keyword('read');
  if Assigned(_Read) then _Only := Keyword(['only', 'write']);
  if Assigned(_Read) then exit;
  { compile }
  _Compile := Keyword('compile');
  if Assigned(_Compile) then exit;
  { drop }
  _Drop := Keyword('drop');
  if Assigned(_Drop) then
  begin
    _Constraint := Keyword('constraint');
    if Assigned(_Constraint) then _ConstraintName := Identifier;
    _Primary := Keyword('primary');
    if Assigned(_Primary) then _Key := Keyword('key');
    _Unique := Keyword('unique');
    if Assigned(_Unique) then TSingleLine<TBracketedStatement<TIdentFields>>.Parse(Self, Source, _UniqueFields);
    exit;
  end;
  { modify }
  _Modify := Keyword('modify');
  if Assigned(_Modify) then
  begin
    _Constraint := Keyword('constraint');
    _ConstraintName := Identifier;
    _Rely := Keyword(['rely', 'norely']);
    exit;
  end;
  { add }
  _Add := Keyword('add');
  if Assigned(_Add) then TConstraint.Parse(Self, Source, _ConstraintDesc);
end;

procedure TViewAlter.InternalPrintSelf(APrinter: TPrinter);
begin
  inherited;
  APrinter.PrintItems([_Compile, _Read, _Only, _Editionable, _Add, _Modify, _Drop, _Constraint, _ConstraintName, _ConstraintDesc, _Primary, _Key, _Unique, _UniqueFields, _Rely]);
end;

end.
