////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                         Базовая реализация принтера                        //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit BasePrinter;

{ ----- Примечания -------------------------------------------------------------

  Этот модуль необходимо отделить от Printers_ для того, чтобы разрушить
  циклическую ссылку между ним и Statements.

------------------------------------------------------------------------------ }

interface

uses SysUtils, Printer, Statements, Tokens, System.Generics.Collections;

type
  { Базовая (пустая) реализация принтера }
  TBasePrinter = class(TPrinter)
  public
    procedure BeginPrint; override;
    procedure Clear; override;
    procedure PrintItem(AItem: TObject); override;
    procedure PrintToken(AToken: TToken); virtual;
    procedure PrintStatement(AStatement: TStatement); virtual;
    procedure EndPrint; override;
    procedure Indent; override;
    procedure Undent; override;
    procedure PushIndent; override;
    procedure PopIndent; override;
    procedure NextLine; override;
    procedure CancelNextLine; override;
    procedure SupressNextLine(ASupress: boolean); override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure BeforePrintDelimiter; override;
    procedure AfterPrintDelimiter; override;
  public
    procedure ControlChanged; override;
    procedure SyncNotification(AObject: TObject; ALine, ACol, ALen: integer); override;
    function  GetText: string; override;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                   Дефолтовая (пустая) реализация принтера                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TBasePrinter.BeginPrint;
begin
  { Вызываем Clear и больше ничего не делаем }
  Clear;
end;

procedure TBasePrinter.Clear;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.EndPrint;
begin
  { ничего не делаем }
end;

{ Разбиваем PrintItem на PrintToken и PrintStatement }
procedure TBasePrinter.PrintItem(AItem: TObject);
begin
  if AItem is TToken then
    PrintToken(AItem as TToken)
  else if AItem is TStatement then
    PrintStatement(AItem as TStatement);
end;

procedure TBasePrinter.PrintToken(AToken: TToken);
begin
  { ничего не делаем }
end;

procedure TBasePrinter.PrintSpecialComment(AValue: string);
begin
  { ничего не делаем }
end;

procedure TBasePrinter.BeforePrintDelimiter;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.AfterPrintDelimiter;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.ControlChanged;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.SyncNotification(AObject: TObject; ALine, ACol, ALen: integer);
begin
  { ничего не делаем }
end;

procedure TBasePrinter.PrintStatement(AStatement: TStatement);
begin
  AStatement.PrintSelf(Self);
end;

procedure TBasePrinter.Indent;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.Undent;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.PushIndent;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.PopIndent;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.NextLine;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.CancelNextLine;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.SupressNextLine;
begin
  { ничего не делаем }
end;

function TBasePrinter.GetText: string;
begin
  Result := '';
end;

end.
