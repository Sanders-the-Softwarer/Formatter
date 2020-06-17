////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                              Команда SET (DDL)                             //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Set_;

interface

uses Tokens, Statements, Printer;

type
  { Команда set }
  TSet = class(TSemicolonStatement)
  strict private
    _Set: TEpithet;
    _What: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

implementation

uses Commons;

type

  { Синтаксис ролей для команды set }
  TRole = class(TStatement)
  strict private
    _Role, _None, _All, _Except: TEpithet;
    _Roles: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

  { Идентифицируемая роль }
  TRoleRef = class(TStatement)
  strict private
    _Role, _IdentifiedBy, _Password: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

{ TSet }

function TSet.InternalParse: boolean;
begin
  Result := true;
  _Set := Keyword('set');
  if not Assigned(_Set) then exit(false);
  { Если мы не распознали аргумент, значит это не ddl-команда, а sql*plus }
  if not TRole.Parse(Self, Source, _What) then exit(false);
  inherited;
end;

procedure TSet.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Set, _What]);
  inherited;
end;

function TSet.Grouping: TStatementClass;
begin
  if Assigned(_What)
    then Result := _What.Grouping
    else Result := nil;
end;

{ TRoleRef }

function TRoleRef.InternalParse: boolean;
begin
  Result := true;
  _Role := Identifier;
  if not Assigned(_Role) then exit(false);
  _IdentifiedBy := Keyword('identified by');
  if Assigned(_IdentifiedBy) then _Password := Identifier;
end;

procedure TRoleRef.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Role, _IdentifiedBy, _Password]);
end;

{ TRole }

function TRole.InternalParse: boolean;
begin
  Result := true;
  _Role := Keyword('role');
  if not Assigned(_Role) then exit(false);
  _None := Keyword('none');
  _All  := Keyword('all');
  _Except := Keyword('except');
  TSingleLine<TCommaList<TRoleRef>>.Parse(Self, Source, _Roles);
end;

procedure TRole.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Role, _None, _All, _Except, _Roles]);
end;

function TRole.Grouping: TStatementClass;
begin
  Result := TRole;
end;

end.
