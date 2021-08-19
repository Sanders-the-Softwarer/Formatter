////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                               Оператор FOR ALL                             //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit ForAll;

interface

uses Statements, Tokens, Printer, PLSQL;

type
  { Оператор forall }
  TForAll = class(TPLSQLStatement)
  strict private
    _ForAll: TEpithet;
    _Variable: TEpithet;
    _In: TEpithet;
    _Low: TStatement;
    _To: TTerminal;
    _High: TStatement;
    _IndicesOrValues: TEpithet;
    _Of: TEpithet;
    _TableName: TStatement;
    _Save: TEpithet;
    _Exceptions: TEpithet;
    _DML: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

uses Parser, Commons, Expressions, DML;

{ TForAll }

function TForAll.InternalParse: boolean;
begin
  _ForAll := Keyword('forall');
  if not Assigned(_ForAll) then exit(false);
  _Variable := Identifier;
  _In := Keyword('in');
  _IndicesOrValues := Keyword(['indices', 'values']);
  if Assigned(_IndicesOrValues) then
    begin
      _Of := Keyword('of');
      TQualifiedIndexedIdent.Parse(Self, Source, _TableName);
    end
  else
    begin
      TExpression.Parse(Self, Source, _Low);
      _To := Terminal('..');
      TExpression.Parse(Self, Source, _High);
    end;
  _Save := Keyword('save');
  _Exceptions := Keyword('exceptions');
  if not TParser.Parse(Source, Settings, DMLParser, Self, _DML) then TParser.Parse(Source, Settings, PLSQLParser, Self, _DML);
  inherited;
  Result := true;
end;

procedure TForAll.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_ForAll, _Variable, _In, _IndicesOrValues, _Low, _Of, _To, _TableName, _High, _Save, _Exceptions]);
  APrinter.PrintIndented(_DML);
  inherited;
end;

end.
