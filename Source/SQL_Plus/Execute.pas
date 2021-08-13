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
  public
    class function Priority: integer; override;
  end;

implementation

uses Parser, PLSQL;

{ TExecute }

class function TExecute.Priority: integer;
begin
  Result := LOWER_PRIORITY;
end;

function TExecute.InternalParse: boolean;
begin
  _Cmd := Keyword(['exec[ute]']);
  Result := Assigned(_Cmd);
  if Result then TParser.Parse(Source, Settings, PLSQLParser, Self, _What);
  inherited;
end;

procedure TExecute.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintRulerItems('cmd', [_Cmd]);
  APrinter.PrintRulerItems('what', [_What]);
end;

end.
