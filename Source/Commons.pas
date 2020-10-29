////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                  ��������������� �������������� �����������                //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Commons;

interface

uses SysUtils, Tokens, Statements, Printer;

type

  { ����������������� ������������� }
  TQualifiedIdent = class(TStatement)
  strict private
    _Prefix, _Infix, _Suffix: TTerminal;
    _Name: TToken; { ����� ��������� identifier ��� number }
    _Next: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    function IsSimpleIdent: boolean;
  end;

  { ��������������� ����������������� ������������� }
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

  { ������� ����� ��� ������ ���������� �����������, ���������� �������� }
  TCommaList<S: TStatement> = class(TStatementList<S>)
  strict protected
    function ParseDelimiter(out AResult: TObject): boolean; override;
    function ParseBreak: boolean; override;
  public
    function Transparent: boolean; override;
  end;

  { �������� ������ ������������ }
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
    function IsMultiLine: boolean;
    function StatementName: string; override;
  end;

  { ��������� ������ ������������ }
  TArguments = class(TCommaList<TArgument>)
  strict protected
    function ParseBreak: boolean; override;
    function HasMultiLineArgument: boolean;
    function Aligned: boolean; override;
  public
    function OnePerLine: boolean; override;
    function IsNamedNotation: boolean;
    function Transparent: boolean; override;
  end;

  { ������ ���������� � ������� }
  TBracketedArguments = class(TBracketedStatement<TArguments>)
  public
    function MultiLine: boolean; override;
    function AllowEmpty: boolean; override;
  end;

  { ��� ������ }
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

  { ������ ��� ��� = �������� }

  TNameValuePair = class(TStatement)
  strict private
    _Eq: TTerminal;
  strict protected
    _Name, _Value: TObject;
    function ParseName: boolean; virtual;
    function ParseValue: boolean; virtual;
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  TEqList<S: TNameValuePair> = class(TStatementList<S>)
  strict protected
    function AllowUnexpected: boolean; override;
  end;

  TNameValueList = class(TEqList<TNameValuePair>);

implementation

uses Parser, Expressions;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                       ����������������� �������������                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TQualifiedIdent.InternalParse: boolean;
begin
  Result := true;
  { ���������� ����� ���������� � ��������� (bind) ��� ���������� (macro) �
    ����������� �� ����� ������� (����������������� �����) ���� �������� (�������)}
  if Parent is TQualifiedIdent
    then _Infix := Terminal(['.', '@', '..'])
    else _Prefix := Terminal([':', '&', '&&']);
  if Assigned(_Infix) <> (Parent is TQualifiedIdent) then exit(false);
  if Assigned(_Infix) then TTerminal(_Infix).WithoutSpace := true; { ����� .. ����� �������� }
  { ������� ����� ���������� - ��� (� ������ bind ����� ���� �����)}
  _Name := Identifier;
  if not Assigned(_Name) and Assigned(_Prefix) then _Name := Number;
  if not Assigned(_Name) then exit(false);
  { ������ ������� ��������� ����������� ���������� �������������� }
  if TQualifiedIdent.Parse(Self, Source, _Next) then exit;
  { ���������������� ����� ������������� ������ }
  if not Assigned(_Prefix) then exit;
  if (_Prefix.Value = '&') or (_Prefix.Value = '&&') then _Suffix := Terminal('.');
  { � ����� �������, ��� ����� ����� ����� ���� ����������� }
  TQualifiedIdent.Parse(Self, Source, _Next);
end;

procedure TQualifiedIdent.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Prefix, _Infix, _Name, _Suffix, _Next]);
end;

function TQualifiedIdent.StatementName: string;
begin
  Result := StringReplace(Concat([_Prefix, _Infix, _Name, _Suffix, _Next]), ' ', '', [rfReplaceAll]);
end;

function TQualifiedIdent.IsSimpleIdent: boolean;
begin
  Result := not Assigned(_Next);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//               ��������������� ����������������� �������������              //
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
  if Assigned(_Indexes) then
  begin
    if MultiLineIndexes then APrinter.PrintItem(_IndentNextLine);
    APrinter.PrintItem(_Indexes);
    if MultiLineIndexes then APrinter.PrintItem(_Undent);
  end;
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
//              ������ ���������� ���������, ���������� ��������             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TCommaList<S>.ParseDelimiter(out AResult: TObject): boolean;
begin
  AResult := Terminal(',');
  Result  := Assigned(AResult);
end;

function TCommaList<S>.ParseBreak: boolean;
begin
  Result := true;
end;

function TCommaList<S>.Transparent: boolean;
begin
  Result := true;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                       �������� � ������ ������������                       //
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
  Result := TExpression.Parse(Self, Source, _Expression);
end;

procedure TArgument.InternalPrintSelf(APrinter: TPrinter);
begin
  if IsNamedNotation then
    begin
      APrinter.PrintRulerItems('ident', [_Ident]);
      APrinter.PrintRulerItems('argument', [_Assignment]);
      APrinter.PushIndent;
      APrinter.PrintRulerItems('expression', [_Expression]);
      APrinter.PopIndent;
    end
  else
    APrinter.PrintItem(_Expression);
end;

function TArgument.IsNamedNotation: boolean;
begin
  Result := Assigned(_Assignment);
end;

function TArgument.IsMultiLine: boolean;
begin
  Result := (_Expression is TExpression) and TExpression(_Expression).IsMultiLine;
end;

function TArgument.StatementName: string;
begin
  Result := Concat([_Ident]);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                       ��������� � ������ ������������                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TArguments.ParseBreak: boolean;
begin
  Result := Any([Terminal([';', ')'])]);
end;

function TArguments.OnePerLine: boolean;
begin
  Result := HasMultiLineArgument or
            IsNamedNotation and (Self.Count > Settings.NamedArgumentSingleLineParamLimit) or
            (Self.Count > Settings.PositionalArgumentSingleLineParamLimit);
end;

function TArguments.Aligned: boolean;
begin
  Result := Settings.AlignVariables;
end;

function TArguments.Transparent: boolean;
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

function TArguments.HasMultiLineArgument: boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to Count - 1 do
    if (Item(i) is TArgument) and (TArgument(Item(i)).IsMultiLine) then
      exit(true);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                  ������ � ����������� � ������ ������������                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TBracketedArguments.MultiLine: boolean;
begin
  Result := (InnerStatement is TArguments) and TArguments(InnerStatement).OnePerLine;
end;

function TBracketedArguments.AllowEmpty: boolean;
begin
  Result := true;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                                 ��� ������                                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TTypeRef.InternalParse: boolean;
begin
  { ���� ���������� �������������, ��� ������ ��������� }
  TQualifiedIdent.Parse(Self, Source, _Ident);
  if not Assigned(_Ident) then exit(false);
  { �������� %[row]type }
  _Type := Terminal(['%type', '%rowtype']);
  { �������� �������� ������� }
  _OpenBracket := Terminal('(');
  if Assigned(_OpenBracket) then
  begin
    _Size := Number;
    _Unit := Keyword(['char', 'byte']);
    if not Assigned(_Unit) then _Comma := Terminal(',');
    if Assigned(_Comma) then
    begin
      _Comma.WithoutSpace := true;
      _Precision := Number;
    end;
    _CloseBracket := Terminal(')');
  end;
  { �������� with time zone }
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

{ TNameValuePair }

function TNameValuePair.InternalParse: boolean;
begin
  Result := true;
  if not ParseName then exit(false);
  _Eq := Terminal('=');
  if not Assigned(_Eq) then exit(false);
  if not ParseValue then exit(false);
end;

procedure TNameValuePair.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Name, _Eq, _Value]);
end;

function TNameValuePair.ParseName: boolean;
begin
  _Name := Identifier;
  Result := Assigned(_Name);
end;

function TNameValuePair.ParseValue: boolean;
begin
  Result := TExpression.Parse(Self, Source, TStatement(_Value));
end;

{ TEqList<S> }

function TEqList<S>.AllowUnexpected: boolean;
begin
  Result := false;
end;

end.

