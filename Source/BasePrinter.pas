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

uses SysUtils, PrinterIntf, Statements, Tokens, System.Generics.Collections;

type
  { Базовая (пустая) реализация принтера }
  TBasePrinter = class(TPrinter)
  public
    procedure BeginPrint; override;
    procedure PrintItem(AItem: TObject); override;
    procedure PrintToken(AToken: TToken); virtual;
    procedure PrintStatement(AStatement: TStatement); virtual;
    procedure EndPrint; override;
    procedure Indent; override;
    procedure Undent; override;
    procedure NextLine; override;
    procedure CancelNextLine; override;
    procedure SupressNextLine(ASupress: boolean); override;
    procedure SupressSpaces(ASupress: boolean); override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure StartRuler(Enabled: boolean); override;
  protected
    procedure Ruler(const ARuler: string); override;
  public
    function  MakeDraftPrinter: TPrinter; override;
    function  CurrentCol: integer; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
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

procedure TBasePrinter.StartRuler;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.Ruler(const ARuler: string);
begin
  { ничего не делаем }
end;

function TBasePrinter.MakeDraftPrinter: TPrinter;
begin
  raise Exception.Create('This printer has not a draft one');
end;

function TBasePrinter.CurrentCol: integer;
begin
  Result := -1;
end;

procedure TBasePrinter.ControlChanged;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
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

procedure TBasePrinter.SupressSpaces(ASupress: boolean);
begin
  { ничего не делаем }
end;

function TBasePrinter.GetText: string;
begin
  Result := '';
end;

end.
