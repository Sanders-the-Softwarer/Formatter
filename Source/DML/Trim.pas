////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                                 Функция TRIM                               //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Trim;

interface

uses Tokens, Statements, Printer, Commons;

type
  { Специальный синтаксис функции trim }
  TTrim = class(TSpecialFunction)
  strict private
    _Sides, _From: TEpithet;
    _TrimCharacter, _TrimSource: TStatement;
  strict protected
    function ParseFunction: TEpithet; override;
    function InternalParse2: boolean; override;
    procedure InternalPrintSelf2(APrinter: TPrinter); override;
  end;

implementation

uses DML, Expressions;

{ TTrim }

function TTrim.ParseFunction: TEpithet;
begin
  Result := Keyword('trim');
end;

function TTrim.InternalParse2: boolean;
begin
  _Sides := Keyword(['leading', 'trailing', 'both']);
  TExpression.Parse(Self, Source, _TrimCharacter);
  _From := Keyword('from');
  TExpression.Parse(Self, Source, _TrimSource);
  Result := Assigned(_Sides) or Assigned(_From);
end;

procedure TTrim.InternalPrintSelf2(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Sides, _TrimCharacter, _From, _TrimSource]);
end;

initialization
  SelectSpecFunctionParser.Add(TTrim);

end.
