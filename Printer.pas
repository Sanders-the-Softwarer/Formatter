////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                      Интерфейс форматированного вывода                     //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Printer;

{ ----- Примечания -------------------------------------------------------------

  Когда наступает пора вывода сформированного текста, классы просто печатают
  себя в абстрактный "принтер", поддерживающий функционал отступов и умеющий
  передавать сформированный текст на реальный вывод.

------------------------------------------------------------------------------ }

interface

uses SysUtils;

type

  TPrinter = class
  private
    FData: string;
  public
    procedure NextLine;
    procedure Write(const Msg: string); overload;
    procedure Write(const Msg: string; Params: array of const); overload;
    procedure WriteLn(const Msg: string); overload;
    procedure WriteLn(const Msg: string; Params: array of const); overload;
    property Data: string read FData;
  end;

implementation

{ TPrinter }

procedure TPrinter.NextLine;
begin
  FData := FData + #13#10;
end;

procedure TPrinter.Write(const Msg: string);
begin
  FData := FData + Msg;
end;

procedure TPrinter.Write(const Msg: string; Params: array of const);
begin
  Write(Format(Msg, Params));
end;

procedure TPrinter.WriteLn(const Msg: string);
begin
  Write(Msg);
  NextLine;
end;

procedure TPrinter.WriteLn(const Msg: string; Params: array of const);
begin
  WriteLn(Format(Msg, Params));
end;

end.


