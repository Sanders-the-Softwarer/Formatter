////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                               Синтаксис ролей                              //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Role;

interface

uses Tokens, Statements, Printer;

type
  { Роль }
  TRole = class(TStatement)
  strict private
    _Role, _Identified, _Container, _ContainerName: TEpithet;
    _Name, _Identifier: TStatement;
    _Eq: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

implementation

uses Commons, OracleCore;

{ TRole }

function TRole.InternalParse: boolean;
begin
  Result := true;
  _Role := Keyword('role');
  if not Assigned(_Role) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Name);
  _Identified := Keyword(['not identified', 'identified by', 'identified using',
                          'identified externally', 'identified globally']);
  if Assigned(_Identified) and ((_Identified.Value = 'identified by') or
                                (_Identified.Value = 'identified using'))
    then TQualifiedIdent.Parse(Self, Source, _Identifier);
  _Container := Keyword('container');
  if Assigned(_Container) then _Eq := Terminal('=');
  if Assigned(_Container) then _ContainerName := Keyword(['current', 'all']);
end;

procedure TRole.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Role, _Name, _Identified, _Identifier, _Container, _Eq, _ContainerName]);
end;

function TRole.Grouping: TStatementClass;
begin
  Result := TRole;
end;

initialization
  OracleCreateParser.Add(TRole);

end.
