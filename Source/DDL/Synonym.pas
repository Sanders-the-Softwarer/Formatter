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
    function StatementName: string; override;
    function Grouping: TStatementClass; override;
    function SameTypeAligned: TAlignMode; override;
  end;

implementation

uses Commons, DDL;

{ TSynonym }

function TSynonym.StatementName: string;
begin
  Result := Concat([_Synonym, _Name]);
end;

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
  APrinter.PrintRulerItems('synonym', [_Synonym]);
  APrinter.PrintRulerItems('name', [_Name]);
  APrinter.PrintRulerItems('sharing', [_Sharing]);
  APrinter.PrintRulerItems('for', [_For]);
  APrinter.PrintRulerItems('object', [_Object]);
end;

function TSynonym.Grouping: TStatementClass;
begin
  Result := TSynonym;
end;

function TSynonym.SameTypeAligned: TAlignMode;
begin
  Result := AlignMode(Settings.AlignCommands);
end;

end.
