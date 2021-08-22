////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                             Команда / (SQL*Plus)                           //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Slash;

interface

uses Tokens, Statements, Printer, SQLPlus;

type

  { Команда / }
  TSlash = class(TSQLPlusStatement)
  strict private
    _Slash: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

{ TSlash }

function TSlash.InternalParse: boolean;
begin
  _Slash := Terminal('/');
  Result := Assigned(_Slash);
end;

procedure TSlash.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_NextLine, _Slash]);
end;

end.
