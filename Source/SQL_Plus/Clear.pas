////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                           Команда CLEAR (SQL*Plus)                         //
//                                                                            //
//               Copyright(c) 2019-2021 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Clear;

interface

uses SQLPlus, Tokens, Printer, Statements;

type
  { Команда clear }
  TClear = class(TSQLPlusStatement)
  strict private
    _Clear: TEpithet;
    _Options: TStatement;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

implementation

type

  { Опции команды clear }
  TOption = class(TSQLPlusStatement)
  strict private
    _Option: TEpithet;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  end;

  { Список опций }
  TOptionList = class(TStatementList<TOption>)
  strict protected
    function ParseBreak: boolean; override;
    function OnePerLine: boolean; override;
  end;

{ TClear }

function TClear.InternalParse: boolean;
begin
  _Clear := Keyword('cl[ear]');
  TOptionList.Parse(Self, Source, _Options);
  Result := Assigned(_Clear);
  inherited;
end;

procedure TClear.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItems([_Clear, _Options]);
  inherited;
end;

{ TOption }

function TOption.InternalParse: boolean;
begin
  _Option := Keyword(['bre[aks]', 'buff[er]', 'col[umns]', 'comp[utes]', 'scr[een]', 'sql', 'tim[ing]']);
  Result := Assigned(_Option);
end;

procedure TOption.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.PrintItem(_Option);
end;

{ TOptionList }

function TOptionList.OnePerLine: boolean;
begin
  Result := false;
end;

function TOptionList.ParseBreak: boolean;
var _Next: TStatement;
begin
  Result := not TOption.Parse(Self, Source, _Next);
end;

end.
