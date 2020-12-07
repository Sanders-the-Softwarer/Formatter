////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                                 Команда DROP                               //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Drop;

interface

uses Statements, Tokens, Printer, Commons;

type
  { Команда drop }
  TDrop = class(TSemicolonStatement)
  strict private
    _Drop, _Type, _Force, _CascadeConstraints: TEpithet;
    _Name: TStatement;
    _Slash: TTerminal;
    function IsTable: boolean;
    function IsType: boolean;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
    function Grouping: TStatementClass; override;
  end;

implementation

{ TDrop }

function TDrop.InternalParse: boolean;
begin
  _Drop := Keyword('drop');
  if not Assigned(_Drop) then exit(false);
  _Type := Keyword(['table', 'procedure', 'function', 'package', 'package body',
                    'view', 'index', 'type', 'type body', 'sequence', 'trigger',
                    'synonym', 'public synonym', 'role', 'database link',
                    'public database link']);
  TQualifiedIdent.Parse(Self, Source, _Name);
  if IsTable then _CascadeConstraints := Keyword('cascade constraints');
  if IsType then _Force := Keyword('force');
  Result := inherited or Assigned(_Type);
  if Result then _Slash := Terminal('/');
end;

procedure TDrop.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Drop, _Type, _Name, _Force, _CascadeConstraints]);
  inherited;
  APrinter.NextLineIf([_Slash]);
end;

function TDrop.StatementName: string;
begin
  Result := Concat([_Drop, _Type, _Name]);
end;

function TDrop.Grouping: TStatementClass;
begin
  Result := TDrop;
end;

function TDrop.IsTable: boolean;
begin
  Result := Assigned(_Type) and (_Type.Value = 'table');
end;

function TDrop.IsType: boolean;
begin
  Result := Assigned(_Type) and (_Type.Value = 'type');
end;

end.
