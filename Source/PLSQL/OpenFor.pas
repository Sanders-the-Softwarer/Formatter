////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                              Оператор OPEN FOR                             //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit OpenFor;

interface

uses Statements, Tokens, Printer, PLSQL;

type
  { Оператор open for }
  TOpenFor = class(TPLSQLStatement)
  strict private
    _Open, _For: TEpithet;
    _Cursor, _Select, _Using: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Commons, Expressions, Select;

{ TOpenFor }

function TOpenFor.InternalParse: boolean;
begin
  _Open := Keyword('open');
  if not Assigned(_Open) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Cursor);
  _For := Keyword('for');
  if not TSelect.Parse(Self, Source, _Select) then TExpression.Parse(Self, Source, _Select);
  TUsing.Parse(Self, Source, _Using);
  inherited;
  Result := true;
end;

procedure TOpenFor.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Open,  _IndentNextLine,
                               _Cursor, _UndentNextLine,
                       _For,   _IndentNextLine,
                               _Select, _UndentNextLine,
                       _Using]);
  inherited;
end;

end.
