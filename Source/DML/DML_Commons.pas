////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                           Общие средства для DML                           //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit DML_Commons;

interface

uses Statements, Tokens, Printer, Commons;

type

  { Конструкция where }
  TWhere = class(TStatement)
  strict private
    _Where: TEpithet;
    _Condition: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Идентификатор как поле в insert, select into итп }
  TIdentField = class(TStatement)
  strict private
    _FieldName: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

  { Список идентификаторов }
  TIdentFields = class(TCommaList<TIdentField>)
  strict protected
    function ParseBreak: boolean; override;
  end;

implementation

uses Expressions;

{ TWhere }

function TWhere.InternalParse: boolean;
begin
  _Where := Keyword('where');
  Result := Assigned(_Where);
  if Result then TExpression.Parse(Self, Source, _Condition);
  if _Condition is TExpression then TExpression(_Condition).IsWhereExpression := true;
end;

procedure TWhere.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Where, _IndentNextLine, _Condition, _Undent]);
end;

{ TIdentField }

function TIdentField.InternalParse: boolean;
begin
  Result := TQualifiedIndexedIdent.Parse(Self, Source, _FieldName);
end;

function TIdentField.StatementName: string;
begin
  Result := _FieldName.StatementName;
end;

procedure TIdentField.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_FieldName);
end;

{ TIdentFields }

function TIdentFields.ParseBreak: boolean;
begin
  Result := Any([Terminal([')', ';']), Keyword('*')]);
end;

end.
