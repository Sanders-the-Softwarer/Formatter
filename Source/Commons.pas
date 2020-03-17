////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                  Общеупотребимые синтаксические конструкции                //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Commons;

interface

uses SysUtils, Tokens, Statements, PrinterIntf;

type

  { Квалифицированный идентификатор }
  TQualifiedIdent = class(TStatement)
  strict private
    _ColonOrAmpersand, _Dot: TTerminal;
    _Name: TToken;
    _Next: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    function IsSimpleIdent: boolean;
  end;

  { Индексированный квалифицированный идентификатор }
  TQualifiedIndexedIdent = class(TStatement)
  strict private
    _Indexes: TStatement;
    _Dot: TTerminal;
    _Ident: TStatement;
    _Next: TStatement;
  strict protected
    function TopStatement: boolean;
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    function IsSimpleIdent: boolean;
  end;

  { Аргумент вызова подпрограммы }
  TArgument = class(TStatement)
  strict private
    _Ident: TEpithet;
    _Assignment: TTerminal;
    _Expression: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function IsNamedNotation: boolean;
  end;

  { Аргументы вызова подпрограммы }
  TArguments = class(TCommaList<TArgument>)
  strict protected
    function ParseBreak: boolean; override;
  public
    function OnePerLine: boolean; override;
    function IsNamedNotation: boolean;
    function Aligned: boolean; override;
  end;

  { Список аргументов в скобках }
  TBracketedArguments = class(TOptionalBracketedStatement<TArguments>)
  public
    function MultiLine: boolean; override;
  end;

  { Тип данных }
  TTypeRef = class(TStatement)
  strict private
    _Ident: TStatement;
    _Type, _OpenBracket, _Comma, _CloseBracket: TTerminal;
    _Size, _Precision: TNumber;
    _Unit, _WithTimeZone: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function IsSimpleIdent: boolean;
  end;

implementation

uses Parser;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                       Квалифицированный идентификатор                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TQualifiedIdent.InternalParse: boolean;
begin
  if Parent is TQualifiedIdent then _Dot := Terminal(['.', '@']) else _ColonOrAmpersand := Terminal([':', '&']);
  _Name := Identifier;
  if not Assigned(_Name) and Assigned(_ColonOrAmpersand) then _Name := Number;
  Result := Assigned(_Name) and (Assigned(_Dot) = (Parent is TQualifiedIdent));
  if Result then TQualifiedIdent.Parse(Self, Source, _Next);
end;

procedure TQualifiedIdent.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_ColonOrAmpersand, _Dot, _Name, _Next]);
end;

function TQualifiedIdent.StatementName: string;
begin
  Result := StringReplace(Concat([_ColonOrAmpersand, _Dot, _Name, _Next]), ' ', '', [rfReplaceAll]);
end;

function TQualifiedIdent.IsSimpleIdent: boolean;
begin
  Result := not Assigned(_Next);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//               Индексированный квалифицированный идентификатор              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TQualifiedIndexedIdent.TopStatement: boolean;
begin
  Result := not (Parent is TQualifiedIndexedIdent);
end;

function TQualifiedIndexedIdent.InternalParse: boolean;
begin
  if not TopStatement then _Dot := Terminal('.');
  if TopStatement or Assigned(_Dot) then TQualifiedIdent.Parse(Self, Source, _Ident);
  if not TopStatement or Assigned(_Ident) then TBracketedArguments.Parse(Self, Source, _Indexes);
  if Assigned(_Indexes) or Assigned(_Ident) then TQualifiedIndexedIdent.Parse(Self, Source, _Next);
  Result := Assigned(_Indexes) or Assigned(_Ident);
end;

procedure TQualifiedIndexedIdent.InternalPrintSelf(APrinter: TPrinter);
var MultiLineIndexes: boolean;
begin
  MultiLineIndexes := (_Indexes is TBracketedArguments) and TBracketedArguments(_Indexes).MultiLine;
  APrinter.PrintItems([_Dot, _Ident]);
  if Settings.IndentBrackets then APrinter.Indent;
  if MultiLineIndexes then APrinter.PrintItem(_NextLine);
  APrinter.PrintItem(_Indexes);
  if Settings.IndentBrackets then APrinter.Undent;
  if MultiLineIndexes then APrinter.PrintItem(_NextLine);
  APrinter.PrintItem(_Next);
end;

function TQualifiedIndexedIdent.StatementName: string;
begin
  Result := Concat([_Dot, _Ident, _Next]);
end;

function TQualifiedIndexedIdent.IsSimpleIdent: boolean;
begin
  Result := Assigned(_Ident) and not Assigned(_Next) and (_Ident as TQualifiedIdent).IsSimpleIdent;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                       Аргумент в вызове подпрограммы                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TArgument.InternalParse: boolean;
var P: integer;
begin
  P := Source.Mark;
  _Ident := Identifier;
  _Assignment := Terminal('=>');
  if not Assigned(_Ident) or not Assigned(_Assignment) then
  begin
    _Ident := nil;
    _Assignment := nil;
    Source.Restore(P);
  end;
  Result := TParser.ParseExpression(Self, Source, _Expression);
end;

procedure TArgument.InternalPrintSelf(APrinter: TPrinter);
begin
  if IsNamedNotation then
    begin
      APrinter.StartRuler(Settings.AlignVariables);
      APrinter.PrintRulerItem('ident', _Ident);
      APrinter.PrintRulerItem('argument', _Assignment);
      APrinter.PrintRulerItem('expression', _Expression);
    end
  else
    APrinter.PrintItem(_Expression);
end;

function TArgument.IsNamedNotation: boolean;
begin
  Result := Assigned(_Assignment);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                       Аргументы в вызове подпрограммы                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TArguments.ParseBreak: boolean;
begin
  Result := Any([Terminal([';', ')'])]);
end;

function TArguments.OnePerLine: boolean;
begin
  Result := IsNamedNotation and (Self.Count > Settings.NamedArgumentSingleLineParamLimit)
            or (Self.Count > Settings.PositionalArgumentSingleLineParamLimit);
end;

function TArguments.Aligned: boolean;
begin
  Result := true;
end;

function TArguments.IsNamedNotation: boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to Count - 1 do
    if (Item(i) is TArgument) and (TArgument(Item(i)).IsNamedNotation) then
      exit(true);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                  Скобки с аргументами в вызове подпрограммы                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TBracketedArguments.MultiLine: boolean;
begin
  Result := (InnerStatement is TArguments) and TArguments(InnerStatement).OnePerLine;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                                 Тип данных                                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TTypeRef.InternalParse: boolean;
begin
  { Если распознали идентификатор, тип данных распознан }
  TQualifiedIdent.Parse(Self, Source, _Ident);
  if not Assigned(_Ident) then exit(false);
  { Проверим %[row]type }
  _Type := Terminal(['%type', '%rowtype']);
  { Проверим указание размера }
  _OpenBracket := Terminal('(');
  if Assigned(_OpenBracket) then
  begin
    _Size := Number;
    _Unit := Keyword(['char', 'byte']);
    if not Assigned(_Unit) then _Comma := Terminal(',');
    if Assigned(_Comma) then
    begin
      _Comma.IntoNumber := true;
      _Precision := Number;
    end;
    _CloseBracket := Terminal(')');
  end;
  { Проверим with time zone }
  _WithTimeZone := Keyword('with time zone');
  Result := true;
end;

procedure TTypeRef.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Ident, _Type, _OpenBracket, _Size, _Comma, _Precision, _Unit, _CloseBracket, _WithTimeZone]);
end;

function TTypeRef.IsSimpleIdent: boolean;
begin
  Result := not Assigned(_Type) and not Assigned(_OpenBracket);
end;

end.

