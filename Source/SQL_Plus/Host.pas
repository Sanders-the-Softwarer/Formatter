////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                           Команда HOST (SQL*Plus)                          //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Host;

interface

uses Tokens, Statements, Printer, SQLPlus;

type

  { Команда host }
  THost = class(TSQLPlusStatement)
  strict private
    _Host: TEpithet;
    _Cmd: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Expressions, Commons;

{ THost }

function THost.InternalParse: boolean;
begin
  Result := true;
  _Host := Keyword('ho[st]');
  if not Assigned(_Host) then exit(false);
  _Cmd := SqlPlusString;
end;

procedure THost.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('host', [_Host]);
  APrinter.PrintRulerItems('cmd', [_Cmd]);
end;

end.

