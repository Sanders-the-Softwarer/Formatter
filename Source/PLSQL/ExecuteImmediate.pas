////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Оператор EXECUTE IMMEDIATE                        //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit ExecuteImmediate;

interface

implementation

uses Statements, PLSQL, Tokens, Printer, Expressions, Commons;

type

  { Оператор execute immediate }
  TExecuteImmediate = class(TPLSQLStatement)
  strict private
    _Execute, _Immediate: TEpithet;
    _Command, _Into, _Using, _Returning: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция into }
  TInto = class(TStatement)
  strict private
    _Into: TEpithet;
    _Fields: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Конструкция returning }
  TReturningInto = class(TStatement)
  strict private
    _Returning: TEpithet;
    _Into: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

{ TExecuteImmediate }

function TExecuteImmediate.InternalParse: boolean;
begin
  Result := true;
  _Execute := Keyword('execute');
  _Immediate := Keyword('immediate');
  if not Assigned(_Execute) or not Assigned(_Immediate) then exit(false);
  TExpression.Parse(Self, Source, _Command);
  TInto.Parse(Self, Source, _Into);
  TUsing.Parse(Self, Source, _Using);
  TReturningInto.Parse(Self, Source, _Returning);
  inherited;
end;

procedure TExecuteImmediate.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Execute, _Immediate,  _IndentNextLine,
                                 _Command,    _UndentNextLine,
                       _Into,    _NextLine,
                       _Using,   _NextLine,
                       _Returning]);
  inherited;
end;

{ TInto }

function TInto.InternalParse: boolean;
begin
  Result := true;
  _Into := Keyword(['into', 'bulk collect into']);
  if not Assigned(_Into) then exit(false);
  TIdentFields.Parse(Self, Source, _Fields);
end;

procedure TInto.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Into, _IndentNextLine, _Fields, _Undent]);
end;

{ TReturningInto }

function TReturningInto.InternalParse: boolean;
begin
  Result := true;
  _Returning := Keyword(['returns', 'returning']);
  if not Assigned(_Returning) then exit(false);
  TInto.Parse(Self, Source, _Into);
end;

procedure TReturningInto.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Returning, _Into]);
end;

initialization
  PLSQLParser.Add(TExecuteImmediate);

end.
