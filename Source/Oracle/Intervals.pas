////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                             Синтаксис интервалов                           //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Intervals;

interface

uses Tokens, Statements, Printer;

type

  { Составляющая интервала }
  TIntervalPart = class(TStatement)
  strict private
    _Volume: TEpithet;
    _OpenBracket, _CloseBracket: TTerminal;
    _Precision: TNumber;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Интервальный суффикс }
  TIntervalSuffix = class(TStatement)
  strict private
    _Part1, _Part2: TStatement;
    _To: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Интервальный тип }
  TIntervalType = class(TStatement)
  strict private
    _Interval: TEpithet;
    _Suffix: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Интервальный литерал }
  TIntervalLiteral = class(TStatement)
  strict private
    _Interval: TEpithet;
    _Value: TLiteral;
    _Suffix: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Интервальное выражение }
  TIntervalExpression = class(TStatement)
  strict private
    _Expression, _Suffix: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Expressions;

{ TIntervalPart }

function TIntervalPart.InternalParse: boolean;
begin
  Result := true;
  _Volume := Keyword(['year', 'month', 'day', 'hour', 'minute', 'second']);
  if not Assigned(_Volume) then exit(false);
  _OpenBracket := Terminal('(');
  if Assigned(_OpenBracket) then _OpenBracket.WithoutSpace := true;
  _Precision   := Number;
  _CloseBracket := Terminal(')');
end;

procedure TIntervalPart.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Volume, _OpenBracket, _Precision, _CloseBracket]);
end;

{ TIntervalType }

function TIntervalType.InternalParse: boolean;
begin
  _Interval := Keyword('interval');
  Result := Assigned(_Interval) and TIntervalSuffix.Parse(Self, Source, _Suffix);
end;

procedure TIntervalType.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Interval, _Suffix]);
end;

{ TIntervalLiteral }

function TIntervalLiteral.InternalParse: boolean;
begin
  _Interval := Keyword('interval');
  _Value := Literal;
  Result := Assigned(_Interval) and TIntervalSuffix.Parse(Self, Source, _Suffix);
end;

procedure TIntervalLiteral.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Interval, _Value, _Suffix]);
end;

{ TIntervalSuffix }

function TIntervalSuffix.InternalParse: boolean;
begin
  Result := true;
  TIntervalPart.Parse(Self, Source, _Part1);
  if not Assigned(_Part1) then exit(false);
  _To := Keyword('to');
  if Assigned(_To) then TIntervalPart.Parse(Self, Source, _Part2);
end;

procedure TIntervalSuffix.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Part1, _To, _Part2]);
end;

{ TIntervalExpression }

function TIntervalExpression.InternalParse: boolean;
begin
  Result := TBracketedExpression.Parse(Self, Source, _Expression) and
            TIntervalSuffix.Parse(Self, Source, _Suffix);
end;

procedure TIntervalExpression.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Expression, _Suffix]);
end;

end.
