////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                        Синтаксис последовательностей                       //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Sequence;

interface

uses Tokens, Statements, Printer;

type

  { Объект sequence }
  TSequence = class(TStatement)
  strict private
    _Sequence: TEpithet;
    _Name, _Sharing, _Parts: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Части декларации sequence-а }
  TSequencePart = class(TStatement)
  strict private
    _Name: TEpithet;
    _Value: TNumber;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function StatementName: string; override;
  end;

implementation

uses Commons, DDL, OracleCore;

{ TSequence }

function TSequence.InternalParse: boolean;
begin
  Result := true;
  _Sequence := Keyword('sequence');
  if not Assigned(_Sequence) then exit(false);
  TQualifiedIdent.Parse(Self, Source, _Name);
  TSharing.Parse(Self, Source, _Sharing);
  TStrictStatementList<TSequencePart>.Parse(Self, Source, _Parts);
end;

procedure TSequence.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Sequence, _Name, _IndentNextLine, _Sharing, _Parts, _Undent]);
end;

{ TSequencePart }

function TSequencePart.InternalParse: boolean;
begin
  _Name  := Keyword(['increment by', 'start with', 'maxvalue', 'nomaxvalue',
                     'minvalue', 'nominvalue', 'cycle', 'nocycle',
                     'cache', 'nocache', 'order', 'noorder', 'keep', 'nokeep',
                     'session', 'global']);
  _Value := Number;
  Result := Assigned(_Name);
end;

procedure TSequencePart.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Name, _Value]);
end;

function TSequencePart.StatementName: string;
begin
  Result := Concat([_Name, _Value]);
end;

initialization
  OracleCreateParser.Add(TSequence);

end.
