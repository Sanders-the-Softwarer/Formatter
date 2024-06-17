////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                             Команда @ (SQL*Plus)                           //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit At;

interface

uses Tokens, Statements, Printer, SQLPlus;

type

  { Команда @ }
  TAt = class(TSQLPlusStatement)
  strict private
    _At, _FileName: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

{ TAt }

function TAt.InternalParse: boolean;
begin
  _At := Terminal(['@', '@@']);
  Result := Assigned(_At);
  if Result then _FileName := SqlPlusString;
  inherited;
end;

procedure TAt.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_At, _FileName]);
  inherited;
end;

end.
