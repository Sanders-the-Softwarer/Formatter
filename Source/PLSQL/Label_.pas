////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                               Синтаксис метки                              //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Label_;

interface

uses Statements, Printer, Tokens;

type
  { Метка }
  TLabel = class(TStatement)
  strict private
    _Name: TToken;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

{ TLabel }

function TLabel.InternalParse: boolean;
begin
  Source.Mark;
  _Name := NextToken;
  Result := _Name is Tokens.TLabel;
  if not Result then Source.Restore;
end;

procedure TLabel.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Name);
end;

end.
