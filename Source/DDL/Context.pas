////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                            Синтаксис контекстов                            //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Context;

interface

uses Tokens, Statements, Printer;

type
  { Контекст }
  TContext = class(TStatement)
  strict private
    _Context, _Name, _Using, _Initialized, _Externally: TEpithet;
    _Package: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

implementation

uses Commons, OracleCreate;

{ TContext }

function TContext.InternalParse: boolean;
begin
  Result := true;
  _Context := Keyword('context');
  if not Assigned(_Context) then exit(false);
  _Name := Identifier;
  _Using := Keyword('using');
  TQualifiedIdent.Parse(Self, Source, _Package);
  _Initialized := Keyword(['initialized', 'accessed']);
  _Externally  := Keyword(['externally', 'globally']);
end;

procedure TContext.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Context, _Name, _Using, _Package, _Initialized, _Externally]);
end;

function TContext.Grouping: TStatementClass;
begin
  Result := TContext;
end;

initialization
  OracleCreateParser.Add(TContext);

end.
