////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Команда EXECUTE (SQL*Plus)                        //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Execute;

interface

uses SQLPlus, Tokens, Statements, Printer;

type

  { Команда execute }
  TExecute = class(TSQLPlusStatement)
  strict private
    _Cmd: TEpithet;
    _What: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses PLSQL;

{ TExecute }

function TExecute.InternalParse: boolean;
begin
  _Cmd := Keyword(['exec[ute]']);
  Result := Assigned(_Cmd);
  if Result then PLSQLParser.Parse(Self, Source, _What);
  Result := Result and inherited;
end;

procedure TExecute.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Cmd, _What]);
  inherited;
end;

end.
