////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                             Синтаксис синонимов                            //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Synonym;

interface

uses Statements, Tokens, Printer;

type
  { Синоним }
  TSynonym = class(TStatement)
  strict private
    _Synonym, _For: TEpithet;
    _Name, _Sharing, _Object: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

implementation

uses Commons, DDL;

{ TSynonym }

function TSynonym.InternalParse: boolean;
begin
  _Synonym := Keyword(['synonym', 'public synonym']);
  if not Assigned(_Synonym) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Name);
  TSharing.Parse(Self, Source, _Sharing);
  _For := Keyword('for');
  if Assigned(_For) then TQualifiedIdent.Parse(Self, Source, _Object);
  Result := true;
end;

procedure TSynonym.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignSQLPLUS);
  APrinter.PrintRulerItems('synonym', [_Synonym]);
  APrinter.PrintRulerItems('name',    [_Name, _Sharing]);
  APrinter.PrintRulerItems('for',     [ _For, _Object]);
end;

function TSynonym.Grouping: TStatementClass;
begin
  Result := TSynonym;
end;

end.
