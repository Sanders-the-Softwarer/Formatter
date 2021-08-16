////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                               Функция XMLTABLE                             //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit XmlTable;

interface

uses Tokens, Statements, Printer, Commons;

type
  { Специальный синтаксис функции xmltable }
  TXmlTable = class(TSpecialFunction)
  strict private
    _Namespace, _Passing, _Returning, _Columns: TStatement;
    _XQuery: TLiteral;
    _Comma: TTerminal;
  strict protected
    function ParseFunction: TEpithet; override;
    function InternalParse2: boolean; override;
    procedure InternalPrintSelf2(APrinter: TPrinter); override;
  end;

implementation

uses DML, Expressions;

type

  { Конструкция XMLNAMESPACES }
  TXmlNamespaces = class(TStatement)
  strict private
    _XmlNamespaces: TEpithet;
    _Namespaces: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Элемент конструкции XMLNAMESPACES }
  TXmlNamespace = class(TStatement)
  strict private
    _Default, _As, _Identifier: TEpithet;
    _String: TLiteral;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция PASSING }
  TPassing = class(TStatement)
  strict private
    _Passing, _By, _Value: TEpithet;
    _Values: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function Aligned: TAlignMode; override;
  end;

  { Значение в конструкции PASSING }
  TPassingValue = class(TStatement)
  strict private
    _Expr: TStatement;
    _As, _Identifier: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция RETURNING }
  TReturning = class(TStatement)
  strict private
    _Returning, _Sequence, _By, _Ref: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция COLUMNS }
  TColumns = class(TStatement)
  strict private
    _Columns: TEpithet;
    _List: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
    function Aligned: TAlignMode; override;
  end;

  { Значение в конструкции COLUMNS }
  TColumn = class(TStatement)
  strict private
    _Column, _For, _Ordinality, _Sequence, _By, _Ref, _Path, _Default: TEpithet;
    _OpenBracket, _CloseBracket: TTerminal;
    _PathValue: TLiteral;
    _Type, _Expr: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

{ TXmlTable }

function TXmlTable.ParseFunction: TEpithet;
begin
  Result := Keyword('xmltable');
end;

function TXmlTable.InternalParse2: boolean;
begin
  Result := true;
  TXmlNamespaces.Parse(Self, Source, _Namespace);
  if Assigned(_Namespace) then _Comma := Terminal(',');
  _XQuery := Literal;
  TPassing.Parse(Self, Source, _Passing);
  TReturning.Parse(Self, Source, _Returning);
  TColumns.Parse(Self, Source, _Columns);
end;

procedure TXmlTable.InternalPrintSelf2(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Namespace, _Comma]);
  APrinter.NextLineIf(_XQuery);
  APrinter.NextLineIf(_Passing);
  APrinter.NextLineIf(_Returning);
  APrinter.NextLineIf(_Columns);
end;

{ TXmlNamespaces }

function TXmlNamespaces.InternalParse: boolean;
begin
  Result := true;
  _XmlNamespaces := Keyword('xmlnamespaces');
  if not Assigned(_XmlNamespaces) then exit(false);
  TBracketedStatement<TCommaList<TXmlNamespace>>.Parse(Self, Source, _Namespaces);
end;

procedure TXmlNamespaces.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_XmlNamespaces, _NextLine, _Namespaces]);
end;

{ TXmlNamespace }

function TXmlNamespace.InternalParse: boolean;
begin
  _Default := Keyword('default');
  _String  := Literal;
  _As      := Keyword('as');
  _Identifier := Identifier;
  Result := Assigned(_Default) or Assigned(_String);
end;

procedure TXmlNamespace.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Default, _String, _As, _Identifier]);
end;

{ TPassing }

function TPassing.InternalParse: boolean;
begin
  _Passing := Keyword('passing');
  _By := Keyword('by');
  _Value := Keyword('value');
  Result := Assigned(_Passing) or Assigned(_By) or Assigned(_Value);
  if Result then TCommaList<TPassingValue>.Parse(Self, Source, _Values);
end;

procedure TPassing.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Passing, _By, _Value, _IndentNextLine, _Values, _Undent]);
end;

function TPassing.Aligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignFields);
end;

{ TPassingValue }

function TPassingValue.InternalParse: boolean;
begin
  Result := true;
  TExpression.Parse(Self, Source, _Expr);
  if not Assigned(_Expr) then exit(false);
  _As := Keyword('as');
  if Assigned(_As) then _Identifier := Identifier;
end;

procedure TPassingValue.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('expr', [_Expr]);
  APrinter.PrintRulerItems('as', [_As]);
  APrinter.PrintRulerItems('id', [_Identifier]);
end;

{ TReturning }

function TReturning.InternalParse: boolean;
begin
  _Returning := Keyword('returning');
  _Sequence  := Keyword('sequence');
  _By        := Keyword('by');
  _Ref       := Keyword('ref');
  Result := Assigned(_Returning) or Assigned(_Sequence) or Assigned(_By) or Assigned(_Ref);
end;

procedure TReturning.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Returning, _Sequence, _By, _Ref]);
end;

{ TColumns }

function TColumns.InternalParse: boolean;
begin
  _Columns := Keyword('columns');
  Result := Assigned(_Columns);
  if Result then TCommaList<TColumn>.Parse(Self, Source, _List);
end;

procedure TColumns.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Columns, _IndentNextLine, _List, _Undent]);
end;

function TColumns.Aligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignFields);
end;

{ TColumn }

function TColumn.InternalParse: boolean;
begin
  Result := true;
  _Column := Identifier;
  if not Assigned(_Column) then exit(false);
  _For := Keyword('for');
  _Ordinality := Keyword('ordinality');
  if Assigned(_For) or Assigned(_Ordinality) then exit(true);
  if not TTypeRef.Parse(Self, Source, _Type) then exit(false);
  _OpenBracket := Terminal('(');
  _Sequence := Keyword('sequence');
  _CloseBracket := Terminal(')');
  _By := Keyword('by');
  _Ref := Keyword('ref');
  _Path := Keyword('path');
  if Assigned(_Path) then _PathValue := Literal;
  _Default := Keyword('default');
  if Assigned(_Default) then TExpression.Parse(Self, Source, _Expr);
end;

procedure TColumn.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('column', [_Column]);
  APrinter.PrintRulerItems('datatype', [_For, _Ordinality, _Type, _OpenBracket, _Sequence, _CloseBracket, _By, _Ref]);
  APrinter.PrintRulerItems('path', [_Path, _PathValue]);
  APrinter.PrintRulerItems('default', [_Default, _Expr]);
end;

initialization
  FromSpecFunctionParser.Add(TXmlTable);

end.

