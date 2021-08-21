////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                                Команда INSERT                              //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Insert;

interface

uses Tokens, Statements, Printer, DML;

type

  { Оператор insert }
  TInsert = class(TDML)
  strict private
    _Insert, _Into, _Values: TEpithet;
    _Table, _Fields, _ValueList, _Returning: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure MatchChildren; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Expressions, Select, Commons;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                               Оператор INSERT                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TInsert.InternalParse: boolean;
begin
  Result := true;
  _Insert := Keyword('insert');
  if not Assigned(_Insert) then exit(false);
  _Into := Keyword('into');
  TTableRef.Parse(Self, Source, _Table);
  TBracketedStatement<TIdentFields>.Parse(Self, Source, _Fields);
  _Values := Keyword('values');
  if not Assigned(_Values) then TSelect.Parse(Self, Source, _ValueList);
  if not Assigned(_ValueList) then TBracketedStatement<TExpressionFields>.Parse(Self, Source, _ValueList);
  if not Assigned(_ValueList) then TExpression.Parse(Self, Source, _ValueList);
  TReturning.Parse(Self, Source, _Returning);
  inherited;
end;

procedure TInsert.MatchChildren;
begin
  Match(_ValueList, _Fields);
end;

procedure TInsert.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Insert, _NextLine,
                       _Into,   _IndentNextLine,
                                _Table, _NextLine,
                                _Fields, _UndentNextLine,
                       _Values, _IndentNextLine]);
  if not Assigned(_Values) then APrinter.Undent;
  APrinter.PrintItem(_ValueList);
  if Assigned(_Values) then APrinter.Undent;
  APrinter.PrintItems([_NextLine, _Returning]);
  inherited;
end;

initialization
  DMLParser.Add(TInsert);

end.
