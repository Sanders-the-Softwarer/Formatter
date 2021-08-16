////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                             Функция XMLSERIALIZE                           //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit XMLSerialize;

interface

uses Tokens, Statements, Printer, Commons;

type
  { Специальный синтаксис функции xmlserialize }
  TXmlSerialize = class(TSpecialFunction)
  strict private
    _ValueType, _As, _Encoding, _Version, _No, _Ident, _Size, _Hide, _Defaults: TEpithet;
    _Eq: TTerminal;
    _SizeValue: TNumber;
    _VersionValue: TLiteral;
    _Value, _DataType, _EncodingValue: TStatement;
  strict protected
    function ParseFunction: TEpithet; override;
    function InternalParse2: boolean; override;
    procedure InternalPrintSelf2(APrinter: TPrinter); override;
  end;

implementation

uses DML, Expressions;

{ TXmlSerialize }

function TXmlSerialize.ParseFunction: TEpithet;
begin
  Result := Keyword('xmlserialize');
end;

function TXmlSerialize.InternalParse2: boolean;
begin
  Result := true;
  _ValueType := Keyword(['document', 'content']);
  TExpression.Parse(Self, Source, _Value);
  _As := Keyword('as');
  if Assigned(_As) then TTypeRef.Parse(Self, Source, _DataType);
  _Encoding := Keyword('encoding');
  if Assigned(_Encoding) then TExpression.Parse(Self, Source, _EncodingValue);
  _Version := Keyword('version');
  if Assigned(_Version) then _VersionValue := Literal;
  _No := Keyword('no');
  _Ident := Keyword('ident');
  if Assigned(_Ident) then
  begin
    _Size := Keyword('size');
    _Eq := Terminal('=');
    _SizeValue := Number;
  end;
  _Hide := Keyword(['hide', 'show']);
  _Defaults := Keyword('defaults');
end;

procedure TXmlSerialize.InternalPrintSelf2(APrinter: TPrinter);
begin
  APrinter.PrintItems([_ValueType, _Value, _As, _DataType]);
  APrinter.NextLineIf([_Encoding, _EncodingValue]);
  APrinter.NextLineIf([_Version, _VersionValue]);
  APrinter.NextLineIf([_No, _Ident, _Size, _Eq, _SizeValue]);
  APrinter.NextLineIf([_Hide, _Defaults]);
end;

initialization
  SelectSpecFunctionParser.Add(TXmlSerialize);

end.
