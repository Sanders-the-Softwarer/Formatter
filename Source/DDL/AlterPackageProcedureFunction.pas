////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                 Команда [alter] package/procedure/function                 //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit AlterPackageProcedureFunction;

interface

uses Commons, Tokens, Statements, Printer;

type
  TAlterPackageProcedureFunction = class(TStatement)
  strict private
    _Unit, _Editionable, _Compile, _Debug, _Part, _Reuse, _Settings: TEpithet;
    _Name, _Params: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    function Grouping: TStatementClass; override;
  end;

implementation

{ TAlterPackageProcedureFunction }

function TAlterPackageProcedureFunction.InternalParse: boolean;
begin
  Result := true;
  _Unit := Keyword(['package', 'procedure', 'function']);
  if not Assigned(_Unit) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Name);
  _Editionable := Keyword(['editionable', 'noneditionable']);
  _Compile := Keyword('compile');
  _Debug := Keyword('debug');
  _Part := Keyword(['package', 'specification', 'body']);
  TNameValueList.Parse(Self, Source, _Params);
  _Reuse := Keyword('reuse');
  _Settings := Keyword('settings');
end;

procedure TAlterPackageProcedureFunction.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Unit, _Name, _Editionable, _Compile, _Debug, _Part,
    _Params, _Reuse, _Settings]);
end;

function TAlterPackageProcedureFunction.StatementName: string;
begin
  if Assigned(_Unit)
    then Result := Concat([_Unit])
    else Result := inherited;
end;

function TAlterPackageProcedureFunction.Grouping: TStatementClass;
begin
  Result := TAlterPackageProcedureFunction;
end;

end.
