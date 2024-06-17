////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Команда ACCEPT (SQL*Plus)                         //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Accept;

interface

uses Tokens, Statements, Printer, SQLPlus;

type

  { Команда accept }
  TAccept = class(TSQLPlusStatement)
  strict private
    _Accept, _Type, _Format, _Default, _Prompt, _Hide: TEpithet;
    _Variable, _DefaultValue: TStatement;
    _FormatValue, _PromptValue: TLiteral;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Expressions, Commons;

{ TAccept }

function TAccept.InternalParse: boolean;
begin
  Result := true;
  _Accept := Keyword('acc[ept]');
  if not Assigned(_Accept) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Variable);
  _Type := Keyword(['num[ber]', 'char', 'date', 'binary_float', 'binary_double']);
  _Format := Keyword('for[mat]');
  if Assigned(_Format) then _FormatValue := Literal;
  _Default := Keyword('def[ault]');
  if Assigned(_Default) then TExpression.Parse(Self, Source, _DefaultValue);
  _Prompt := Keyword(['prompt', 'nopr[ompt]']);
  _PromptValue := Literal;
  _Hide := Keyword('hide');
end;

procedure TAccept.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('cmd', [_Accept]);
  APrinter.PrintRulerItems('var', [_Variable]);
  APrinter.PrintRulerItems('rest', [_Type, _Format, _FormatValue, _Default, _DefaultValue, _Prompt, _PromptValue, _Hide]);
end;

end.
