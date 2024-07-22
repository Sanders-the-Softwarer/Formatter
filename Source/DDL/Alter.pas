////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                                Команда ALTER                               //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Alter;

interface

uses Tokens, Statements, Printer, Commons;

type
  { Команда alter }
  TAlter = class(TTopStatement)
  strict private
    _Alter: TEpithet;
    _What: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
    function StatementName: string; override;
  end;

implementation

uses Role, Sequence, DatabaseLink, Session, AlterPackageProcedureFunction,
  View;

{ TAlter }

function TAlter.InternalParse: boolean;
begin
  Result := true;
  _Alter := Keyword('alter');
  if not Assigned(_Alter) then exit(false);
  if not TSequence.Parse(Self, Source, _What) and
     not TRole.Parse(Self, Source, _What) and
     not TDatabaseLink.Parse(Self, Source, _What) and
     not TSession.Parse(Self, Source, _What) and
     not TViewAlter.Parse(Self, Source, _What) and
     not TAlterPackageProcedureFunction.Parse(Self, Source, _What)
     then;
  inherited;
end;

procedure TAlter.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Alter, _What]);
  inherited;
end;

function TAlter.StatementName: string;
begin
  Result := Concat([_Alter, _What]);
end;

function TAlter.Grouping: TStatementClass;
begin
  if Assigned(_What)
    then Result := _What.Grouping
    else Result := nil;
end;

end.
