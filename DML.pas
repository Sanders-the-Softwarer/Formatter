﻿////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                       Синтаксические конструкции DML                       //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit DML;

{ ----- Примечание -------------------------------------------------------------

  В настоящей версии dml-операторы реализованы как список произвольных лексем,
  начинающихся с select/insert/update/delete/merge и заканчивающихся точкой с
  запятой. Это позволяет обработать их, встречая в PL/SQL коде и отложить их
  детальную спецификацию на будущее

------------------------------------------------------------------------------ }

interface

uses Math, Tokens, Statements, PLSQL, Printers_;

type
  { Оператор insert }
  TInsert = class(TOperator)
  strict private
    _Insert: TKeyword;
    _Into: TKeyword;
    _TableName: TIdent;
    _OpenBracket: TTerminal;
    _Fields: TStatement;
    _CloseBracket: TTerminal;
    _Values: TKeyword;
    _OpenBracket2: TTerminal;
    _Expressions: TStatement;
    _CloseBracket2: TTerminal;
  strict protected
    function InternalParse: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  { Список выражений через запятую }
  TFieldList = class(TStatementList)
  strict protected
    function ParseStatement(out  AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  public
    function Name: string; override;
    function IdentMaxLen: integer;
  end;

  { Выражение для списка }
  TField = class(TStatement)
  strict private
    _Expression: TStatement;
    _As: TKeyword;
    _Alias: TIdent;
    _Comma: TTerminal;
    _Match: TField;
  strict protected
    function InternalParse: boolean; override;
  public
    property Match: TField read _Match write _Match;
    function Name: string; override;
    function Ident: string;
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TDML = class(TStatementList)
  strict private
    _Keyword: TKeyword;
  strict protected
    function InternalParse: boolean; override;
    function ParseStatement(out AResult: TStatement): boolean; override;
    function ParseBreak: boolean; override;
  public
    procedure PrintSelf(APrinter: TPrinter); override;
    function Name: string; override;
  end;

implementation

uses Expressions;

{ TDML }

function TDML.InternalParse: boolean;
begin
  _Keyword := Keyword(['select', 'update', 'delete', 'merge']);
  Result := Assigned(_Keyword);
  inherited;
end;

function TDML.Name: string;
begin
  Result := '< ' + _Keyword.Value + ' >';
end;

function TDML.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := false;
end;

function TDML.ParseBreak: boolean;
begin
  Result := Any([Terminal(';')]);
end;

procedure TDML.PrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Keyword);
  APrinter.NextLine;
  APrinter.Indent;
  inherited;
  APrinter.Undent;
  APrinter.NextLine;
end;

{ TInsert }

function TInsert.InternalParse: boolean;
begin
  Result := true;
  _Insert := Keyword('insert');
  if not Assigned(_Insert) then exit(false);
  _Into := Keyword('into');
  _TableName := Identifier;
  _OpenBracket := Terminal('(');
  TFieldList.Parse(Self, Source, _Fields);
  _CloseBracket := Terminal(')');
  _Values := Keyword('values');
  _OpenBracket2 := Terminal('(');
  TFieldList.Parse(Self, Source, _Expressions);
  _CloseBracket2 := Terminal(')');
  inherited;
end;

procedure TInsert.PrintSelf(APrinter: TPrinter);
var i: integer;
begin
  if Settings.CommentInsert and Assigned(_Expressions) and Assigned(_Fields) then
    for i := 0 to TStatementList(_Fields).Count - 1 do
      if TStatementList(_Expressions).Count > i then
        (TStatementList(_Expressions).Item(i) as TField).Match := TStatementList(_Fields).Item(i) as TField;
  APrinter.PrintItem(_Insert);
  APrinter.NextLine;
  APrinter.PrintItem(_Into);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_TableName);
  APrinter.NextLine;
  APrinter.PrintItem(_OpenBracket);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Fields);
  APrinter.Undent;
  APrinter.NextLine;
  APrinter.PrintItem(_CloseBracket);
  APrinter.Undent;
  APrinter.NextLine;
  APrinter.PrintItem(_Values);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_OpenBracket2);
  APrinter.NextLine;
  APrinter.Indent;
  APrinter.PrintItem(_Expressions);
  APrinter.NextLine;
  APrinter.Undent;
  APrinter.PrintItem(_CloseBracket2);
  inherited;
  APrinter.Undent;
end;

{ TFieldList }

function TFieldList.ParseStatement(out AResult: TStatement): boolean;
begin
  Result := TField.Parse(Self, Source, AResult);
end;

function TFieldList.Name: string;
begin
  Result := '< fields >';
end;

function TFieldList.IdentMaxLen: integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Item(i) is TField then
      Result := Math.Max(Result, Length(TField(Item(i)).Ident));
end;

function TFieldList.ParseBreak: boolean;
begin
  Result := Any([Terminal([')', ';']), Keyword(['from'])]);
end;

{ TField }

function TField.InternalParse: boolean;
begin
  Result := true;
  if not TExpression.Parse(Self, Source, _Expression) then exit(false);
  _As := Keyword('as');
  _Alias := Identifier;
  _Comma := Terminal(',');
end;

function TField.Name: string;
begin
  if Assigned(_Alias)
    then Result := _Alias.Value
    else Result := '< field >';
end;

function TField.Ident: string;
begin
  if _Expression is TExpression
    then Result := TExpression(_Expression).Ident
    else Result := '';
end;

procedure TField.PrintSelf(APrinter: TPrinter);
var C: string;
begin
  if Settings.AlignCommentInsert then APrinter.PaddingFrom;
  APrinter.PrintItem(_Expression);
  APrinter.Space;
  APrinter.PrintItem(_As);
  APrinter.Space;
  APrinter.PrintItem(_Alias);
  APrinter.SupressSpace;
  APrinter.PrintItem(_Comma);
  APrinter.Space;
  if _Match is TField then
  begin
    if Settings.AlignCommentInsert then APrinter.PaddingTo(TFieldList(Parent).IdentMaxLen + 2);
    C := TField(_Match).Ident;
    if Settings.AlignCommentInsert then C := C + StringOfChar(' ', TFieldList(TField(_Match).Parent).IdentMaxLen - Length(C));
    if C <> '' then APrinter.PrintSpecialComment('=> ' + C);
  end;
  APrinter.NextLine;
end;

end.
