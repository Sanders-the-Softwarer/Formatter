////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                                Команда UPDATE                              //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Update;

interface

uses Tokens, Statements, Printer, DML;

type
  { Оператор update }
  TUpdate = class(TDML)
  strict private
    _Update: TEpithet;
    _Table: TStatement;
    _Set: TEpithet;
    _Assignments: TStatement;
    _Where: TStatement;
    _Returning: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Commons, Expressions, DML_Commons;

type

  { Присвоение в update }
  TUpdateAssignment = class(TStatement)
  strict private
    _Target: TStatement;
    _Assignment: TTerminal;
    _Value: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Список присвоений в update }
  TUpdateAssignments = class(TCommaList<TUpdateAssignment>)
  strict protected
    function ParseBreak: boolean; override;
    function Aligned: TAlignMode; override;
  end;

{ TUpdate }

function TUpdate.InternalParse: boolean;
begin
  _Update := Keyword('update');
  if not Assigned(_Update) then exit(false);
  TTableRef.Parse(Self, Source, _Table);
  _Set := Keyword('set');
  TUpdateAssignments.Parse(Self, Source, _Assignments);
  TWhere.Parse(Self, Source, _Where);
  TReturning.Parse(Self, Source, _Returning);
  inherited;
  Result := true;
end;

procedure TUpdate.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Update, _IndentNextLine,
                                _Table,          _UndentNextLine,
                       _Set,    _IndentNextLine,
                                _Assignments,    _UndentNextLine,
                       _Where,  _NextLine,
                       _Returning]);
  inherited;
end;

{ TUpdateAssignment }

function TUpdateAssignment.InternalParse: boolean;
begin
  if not TBracketedStatement<TIdentFields>.Parse(Self, Source, _Target) then TQualifiedIdent.Parse(Self, Source, _Target);
  _Assignment := Terminal('=');
  Result := Assigned(_Target) and Assigned(_Assignment);
  if Result then TExpression.Parse(Self, Source, _Value);
end;

function TUpdateAssignment.StatementName: string;
begin
  if Assigned(_Target)
    then Result := _Target.StatementName
    else Result := '';
end;

procedure TUpdateAssignment.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('target', [_Target]);
  APrinter.PrintRulerItems('assignment', [_Assignment]);
  APrinter.PrintRulerItems('value', [_Value]);
end;

{ TUpdateAssignments }

function TUpdateAssignments.ParseBreak: boolean;
begin
  Result := true;
end;

function TUpdateAssignments.Aligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignFields);
end;

initialization
  DMLParser.Add(TUpdate);

end.
