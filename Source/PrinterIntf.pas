////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                       ����� ���������� � ������ �����                      //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit PrinterIntf;

{ ----- ���������� -------------------------------------------------------------

  ��� ������ ����������� ������� �������������� ������� �������� ���� �
  ������������ �� "�������". ������ ��������� ��������, ����������
  ����������������� �����, ���������� ������������ �������������� ���
  ������ ���������� ���������� - ���� ������� � ���, ��� ��� �����������
  ��������� ���������, ��� �������� ������� ����������� ���������������
  ��������. ������������ ������� ��������� ������������, ��� ������� ������,
  ��������, ��������� ������ ��������, ���� �� ����� ��� ����� ������������,
  ��� � ����� ��������������� �����������.

  ����� TPrinter - ���, �� ����, ��������� ������ �� �������. ������ ����������
  �������� � ������ TBasePrinter; �� �� ������� ����������, ��������� �����
  ��������� ����������� ������ ����� �������� Printers, Tokens � Statements.

  � ���� ������ ���������, ��� ����� ����������� ��������������� �������
  ����������� � ������� ������� ���������� �����-������ ������ ������� � ���
  ���������. ��� ����� ���������� ���������, ��� ��� ��� �� ������ �����
  ��������������� ���, �� � ����� �������� ������ ������������, �������
  ���-�� ������������ � ���, ��� �� ������������. ������� ����� ������������
  ������������ ����������� �������, ������ �������� - ����������� �����������
  ������� � ���� � ��� � �������. ����� ��� ��������� ������� ������������
  �������, �������� ������ ����� � ��������������� ���������������
  �������������.

  � ���������� ������ ������������ ����������� ������������ �������� ����������
  ����������� �� ������. ��� ����� � �������� ������������ ����������� �����
  ��������, ����� �������� ����� ��������, �� �������� �������� ���������
  ��������������, �������� �������, ��� ����� ��������� ����������� �����
  � ���������� ����������� ��� ������������ ����� ���������� ��������.

------------------------------------------------------------------------------ }

interface

uses
  Classes, SysUtils, Math, System.Generics.Collections, Tokens, Utils;

type

  { ��������� ������ }
  TFormatSettings = class
  public
    DeclarationSingleLineParamLimit: integer;
    NamedArgumentSingleLineParamLimit: integer;
    PositionalArgumentSingleLineParamLimit: integer;
    MatchParamLimit: integer;
    AlignVariables: boolean;
    AlignFields: boolean;
    AlignColumns: boolean;
    AlignExpressions: boolean;
    AlignTableColumnComments: boolean;
    AlignSpecialComments: boolean;
    AlignSQLPLUS: boolean;
    ReplaceDefault: boolean;
    ReplaceAsIs: boolean;
    AddInAccessSpecificator: boolean;
    PreferredExpressionLength: integer;
  public
    constructor Default;
    constructor ForTest;
  end;

  { ��������� ������ �� ������� }
  TPrinter = class
  private
    FSettings: TFormatSettings;
    function HasItems(AItems: array of TObject): boolean;
  public
    procedure BeginPrint; virtual; abstract;
    procedure PrintItem(AItem: TObject); virtual; abstract;
    procedure EndPrint; virtual; abstract;
    procedure Indent; virtual; abstract;
    procedure Undent; virtual; abstract;
    procedure PushIndent; virtual; abstract;
    procedure PopIndent; virtual; abstract;
    procedure NextLine; virtual; abstract;
    procedure CancelNextLine; virtual; abstract;
    procedure SupressNextLine(ASupress: boolean); virtual; abstract;
    procedure SupressSpaces(ASupress: boolean); virtual; abstract;
    procedure PrintSpecialComment(AValue: string); virtual; abstract;
    procedure StartRuler(Enabled: boolean); virtual; abstract;
  protected
    procedure Ruler(const ARuler: string); virtual; abstract;
  public
    function  MakeDraftPrinter: TPrinter; virtual; abstract;
    function  CurrentCol: integer; virtual; abstract;
    procedure ControlChanged; virtual; abstract;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); virtual; abstract;
    function  GetText: string; virtual; abstract;
  public
    procedure PrintItems(AItems: array of TObject);
    procedure PrintRulerItem(const ARuler: string; AItem: TObject); virtual;
    procedure PrintRulerItems(const ARuler: string; AItems: array of TObject); virtual;
    procedure PrintIndented(AItem: TObject); overload;
    procedure PrintIndented(AItems: array of TObject); overload;
    function  NextLineIf(AItem: TObject): boolean; overload;
    function  NextLineIf(AItems: array of TObject): boolean; overload;
  public
    property Settings: TFormatSettings read FSettings write FSettings;
  end;

  { ��������������� ��� ����� �������� �������������� ������ }
  TFormatterCmd = class
  public
    procedure PrintSelf(APrinter: TPrinter); virtual; abstract;
  end;

  { ��� ������� ��� ������ ����������� ������������ }
  TSpecialComment = class(TComment);

  { ��� ��������� � ������������� ������������� ���������� }
  TSyncNotification = procedure (AToken: TToken; ALine, ACol, ALen: integer) of object;

{ �������� �������� ��� ������ ���������������� ������ }
function CreateFormatterPrinter: TPrinter;

var
  { ��������� � ������������� ������������� ���������� }
  SyncNotification: TSyncNotification;

{ �������� ��������� � ������������� ������������� ���������� }
procedure SendSyncNotification(AToken: TToken; ALine, ACol, ALen: integer);

{ �����������, ������� ����� ��������� � TFormatterPrinter.PrintItems ��� �������������� }
function _NextLine: TObject;
function _Indent: TObject;
function _Undent: TObject;
function _IndentNextLine: TObject;
function _UndentNextLine: TObject;

implementation

uses SQLPlus, FormatterPrinter;

{ �������� �������� ��� ������ ���������������� ������ }
function CreateFormatterPrinter: TPrinter;
begin
  Result := TFormatterPrinter.Create;
end;

{ �������� ��������� � ������������� ������������� ���������� }
procedure SendSyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
begin
  if Assigned(SyncNotification) then
    SyncNotification(AToken, ALine, ACol, ALen);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//          ��������������� ��� ����� �������� �������������� ������          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type

  TNextLine = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TIndent = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TUndent = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TIndentNextLine = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

  TUndentNextLine = class(TFormatterCmd)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

function _NextLine: TObject;
begin
  Result := TNextLine.Create;
end;

function _Indent: TObject;
begin
  Result := TIndent.Create;
end;

function _Undent: TObject;
begin
  Result := TUndent.Create;
end;

function _IndentNextLine: TObject;
begin
  Result := TIndentNextLine.Create;
end;

function _UndentNextLine: TObject;
begin
  Result := TUndentNextLine.Create;
end;

procedure TNextLine.PrintSelf(APrinter: TPrinter);
begin
  APrinter.NextLine;
end;

procedure TIndent.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Indent;
end;

procedure TUndent.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Undent;
end;

procedure TIndentNextLine.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Indent;
  APrinter.NextLine;
end;

procedure TUndentNextLine.PrintSelf(APrinter: TPrinter);
begin
  APrinter.Undent;
  APrinter.NextLine;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//      ��������� ������ �� ������� � ������� �������� �������� ���������     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPrinter.PrintItems(AItems: array of TObject);
var i: integer;
begin
  for i := Low(AItems) to High(AItems) do PrintItem(AItems[i]);
end;

procedure TPrinter.PrintRulerItem(const ARuler: string; AItem: TObject);
begin
  Ruler(ARuler);
  PrintItem(AItem);
end;

procedure TPrinter.PrintRulerItems(const ARuler: string; AItems: array of TObject);
begin
  Ruler(ARuler);
  PrintItems(AItems);
end;

procedure TPrinter.PrintIndented(AItem: TObject);
begin
  if not Assigned(AItem) then exit;
  NextLine;
  Indent;
  PrintItem(AItem);
  Undent;
end;

procedure TPrinter.PrintIndented(AItems: array of TObject);
begin
  if not HasItems(AItems) then exit;
  NextLine;
  Indent;
  PrintItems(AItems);
  Undent;
end;

function TPrinter.NextLineIf(AItem: TObject): boolean;
begin
  Result := NextLineIf([AItem]);
end;

function TPrinter.NextLineIf(AItems: array of TObject): boolean;
begin
  Result := HasItems(AItems);
  if not Result then exit;
  NextLine;
  PrintItems(AItems);
end;

function TPrinter.HasItems(AItems: array of TObject): boolean;
var i: integer;
begin
  for i := Low(AItems) to High(AItems) do
    if Assigned(AItems[i]) and not (AItems[i] is TFormatterCmd) then exit(true);
  Result := false;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ��������� ��������������                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TFormatSettings.Default;
begin
  DeclarationSingleLineParamLimit := 1;
  NamedArgumentSingleLineParamLimit := 1;
  PositionalArgumentSingleLineParamLimit := 4;
  MatchParamLimit                 := 3;
  AlignVariables                  := true;
  AlignFields                     := true;
  AlignColumns                    := true;
  AlignExpressions                := false;
  AlignTableColumnComments        := true;
  AlignSpecialComments            := true;
  AlignSQLPLUS                    := true;
  ReplaceDefault                  := true;
  ReplaceAsIs                     := true;
  AddInAccessSpecificator         := true;
  PreferredExpressionLength       := 120;
end;

constructor TFormatSettings.ForTest;
begin
  DeclarationSingleLineParamLimit := 99;
  NamedArgumentSingleLineParamLimit := 99;
  PositionalArgumentSingleLineParamLimit := 999;
  PreferredExpressionLength       := 9999;
  MatchParamLimit                 := 99;
end;

end.

