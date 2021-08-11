////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                                Оператор CALL                               //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Call;

interface

uses DML, Tokens, Statements, Printer;

type

  { Оператор call }
  TCall = class(TDML)
  strict private
    _Call, _Into, _Indicator: TEpithet;
    _What, _IntoVar, _IndicatorVar: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Commons;

{ TCall }

function TCall.InternalParse: boolean;
begin
  Result := true;
  _Call := Keyword('call');
  if not Assigned(_Call) then exit(false);
  TQualifiedIndexedIdent.Parse(Self, Source, _What);
  _Into := Keyword('into');
  if Assigned(_Into) then TQualifiedIdent.Parse(Self, Source, _IntoVar);
  _Indicator := Keyword('indicator');
  if Assigned(_Indicator) then TQualifiedIdent.Parse(Self, Source, _IndicatorVar);
end;

procedure TCall.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Call, _What, _Into, _IntoVar, _Indicator, _IndicatorVar]);
end;

end.
