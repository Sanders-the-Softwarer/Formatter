////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                                Оператор GOTO                               //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Goto_;

interface

uses Statements, Tokens, Printer, PLSQL;

type
  { Оператор goto }
  TGoto = class(TPLSQLStatement)
  strict private
    _Goto, _Address: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

{ TGoto }

function TGoto.InternalParse: boolean;
begin
  _Goto := Keyword('goto');
  Result := Assigned(_Goto);
  if Result then _Address := Identifier;
  inherited;
end;

procedure TGoto.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Goto, _Address]);
  inherited;
end;

end.
