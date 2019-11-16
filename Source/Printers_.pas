////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                       ����� ���������� � ������ �����                      //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Printers_;

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
  Classes, SysUtils, Math, Vcl.StdCtrls, Vcl.ComCtrls, System.Generics.Collections,
  Tokens, Windows;

type

  { ��������� ������ }
  TFormatSettings = class
  public
    DeclarationSingleLineParamLimit: integer;
    ArgumentSingleLineParamLimit: integer;
    MatchParamLimit: integer;
    AlignVariables: boolean;
    AlignFields: boolean;
    AlignExpressions: boolean;
    AlignTableColumnComments: boolean;
    AlignSpecialComments: boolean;
    ReplaceDefault: boolean;
    ReplaceAsIs: boolean;
    PreferredExpressionLength: integer;
  public
    constructor Default;
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
    procedure NextLine; virtual; abstract;
    procedure SupressNextLine(ASupress: boolean); virtual; abstract;
    procedure SupressSpaces(ASupress: boolean); virtual; abstract;
    procedure PrintSpecialComment(AValue: string); virtual; abstract;
    procedure Ruler(const ARuler: string; Enabled: boolean = true); virtual; abstract;
    function  MakeDraftPrinter: TPrinter; virtual; abstract;
    function  CurrentCol: integer; virtual; abstract;
    procedure ControlChanged; virtual; abstract;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); virtual; abstract;
    function  GetText: string; virtual; abstract;
  public
    procedure PrintItems(AItems: array of TObject);
    procedure PrintIndented(AItem: TObject); overload;
    procedure PrintIndented(AItems: array of TObject); overload;
    procedure NextLineIf(AItem: TObject); overload;
    procedure NextLineIf(AItems: array of TObject); overload;
  public
    property Settings: TFormatSettings read FSettings write FSettings;
  public
    class function CreateTokenizerPrinter(AListBox: TListBox): TPrinter;
    class function CreateSyntaxTreePrinter(ATreeView: TTreeView): TPrinter;
    class function CreateFormatterPrinter(AMemo: TMemo): TPrinter;
    class function CreateAlarmTokenPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
    class function CreateAlarmStatementPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
  end;

  { ��� ��������� � ������������� ������������� ���������� }
  TSyncNotification = procedure (AToken: TToken; ALine, ACol, ALen: integer) of object;

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

uses Statements, Attributes, SQLPlus;

type
  { ��� ������� ��� ������ ����������� ������������ }
  TSpecialComment = class(TComment);

{ �������� ��������� � ������������� ������������� ���������� }
procedure SendSyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
begin
  if Assigned(SyncNotification) then
    SyncNotification(AToken, ALine, ACol, ALen);
end;

type

  { ������� (������) ���������� �������� }
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
    procedure SupressNextLine(ASupress: boolean); override;
    procedure SupressSpaces(ASupress: boolean); override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure Ruler(const ARuler: string; Enabled: boolean = true); override;
    function  MakeDraftPrinter: TPrinter; override;
    function  CurrentCol: integer; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
    function  GetText: string; override;
  end;

  { ���������� � ������������� }
  TRulers = class
  private
    FMin, FMax: TDictionary<String, integer>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Ruler(const ARuler: string; ACol: integer);
    function Padding(const ARuler: string; ACol: integer): integer;
  end;

  { ����� �������� }
  TFormatterPrinterMode = (fpmNormal, fpmGetRulers, fpmSetRulers);

  { ������� ��� ������ ���������������� ������ }
  TFormatterPrinter = class(TBasePrinter)
  private
    Memo:    TMemo;
    Builder: TStringBuilder;
    Mode:    TFormatterPrinterMode;
    Shift:   integer;
    BOL:     boolean;
    BOT:     boolean;
    EmptyLine: boolean;
    Col:     integer;
    PrevCol: integer;
    Rulers:  TRulers;
    Padding: integer;
    PrevStatement: TStatement;
    PrevToken: TToken;
    IntoSync: boolean;
    TokenPos, TokenLen: TDictionary<TToken, integer>;
    SpecialComments: TObjectList<TToken>;
    SupSpace, SupNextLine: integer;
    IsDraft: boolean;
    Text:    string;
  protected
    function SpaceRequired(ALeft, ARight: TToken): boolean;
    function EmptyLineRequired(APrev, ANext: TStatement): boolean;
  public
    constructor Create(AMemo: TMemo);
    destructor Destroy; override;
    procedure BeginPrint; override;
    procedure EndPrint; override;
    procedure PrintItem(AItem: TObject); override;
    procedure PrintToken(AToken: TToken); override;
    procedure PrintStatement(AStatement: TStatement); override;
    procedure Indent; override;
    procedure Undent; override;
    procedure NextLine; override;
    procedure SupressNextLine(ASupress: boolean); override;
    procedure SupressSpaces(ASupress: boolean); override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure Ruler(const ARuler: string; Enabled: boolean = true); override;
    function MakeDraftPrinter: TPrinter; override;
    function CurrentCol: integer; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
    function  GetText: string; override;
  end;

  { ������� ��� ������ ������������������ ������ }
  TTokenizerPrinter = class(TBasePrinter)
  private
    ListBox: TListBox;
    Tokens: TDictionary<integer, TToken>;
    LineNumbers: TDictionary<TToken, integer>;
  public
    constructor Create(AListBox: TListBox);
    destructor Destroy; override;
    procedure BeginPrint; override;
    procedure PrintToken(AToken: TToken); override;
    procedure EndPrint; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
  end;

  { ������� ��� ���������� ��������������� ������ }
  TSyntaxTreePrinter = class(TBasePrinter)
  private
    TreeView: TTreeView;
    Parents: TStack<TTreeNode>;
    IntoSync: boolean;
    Tokens: TDictionary<TToken, TTreeNode>;
  public
    constructor Create(ATreeView: TTreeView);
    destructor Destroy; override;
    procedure BeginPrint; override;
    procedure PrintToken(AToken: TToken); override;
    procedure PrintStatement(AStatement: TStatement); override;
    procedure EndPrint; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
  end;

  { ���������� ������� ��� ������ ����������� ������ }
  TAlarmTokenPrinter = class(TTokenizerPrinter)
  private
    TabSheet: TTabSheet;
  protected
    procedure BeginPrint; override;
    procedure PrintToken(AToken: TToken); override;
    function CheckPrintToken(AToken: TToken): boolean; virtual;
  public
    constructor Create(AListBox: TListBox; ATabSheet: TTabSheet);
  end;

  { ���������� ������� ��� ������ ����������� ����������� }
  TAlarmStatementPrinter = class(TAlarmTokenPrinter)
  private
    Unexpected: boolean;
  protected
    procedure PrintStatement(AStatement: TStatement); override;
    function CheckPrintToken(AToken: TToken): boolean; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//          ��������������� ��� ����� �������� �������������� ������          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

type

  TFormatterCmd = class
  public
    procedure PrintSelf(APrinter: TPrinter); virtual; abstract;
  end;

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

class function TPrinter.CreateFormatterPrinter(AMemo: TMemo): TPrinter;
begin
  Result := TFormatterPrinter.Create(AMemo);
end;

class function TPrinter.CreateSyntaxTreePrinter(ATreeView: TTreeView): TPrinter;
begin
  Result := TSyntaxTreePrinter.Create(ATreeView);
end;

class function TPrinter.CreateTokenizerPrinter(AListBox: TListBox): TPrinter;
begin
  Result := TTokenizerPrinter.Create(AListBox);
end;

class function TPrinter.CreateAlarmTokenPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
begin
  Result := TAlarmTokenPrinter.Create(AListBox, ATabSheet);
end;

class function TPrinter.CreateAlarmStatementPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
begin
  Result := TAlarmStatementPrinter.Create(AListBox, ATabSheet);
end;

procedure TPrinter.PrintItems(AItems: array of TObject);
var i: integer;
begin
  for i := Low(AItems) to High(AItems) do PrintItem(AItems[i]);
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

procedure TPrinter.NextLineIf(AItem: TObject);
begin
  NextLineIf([AItem]);
end;

procedure TPrinter.NextLineIf(AItems: array of TObject);
begin
  if not HasItems(AItems) then exit;
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
//                   ���������� (������) ���������� ��������                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TBasePrinter.BeginPrint;
begin
  { ������ �� ������ }
end;

procedure TBasePrinter.EndPrint;
begin
  { ������ �� ������ }
end;

{ ��������� PrintItem �� PrintToken � PrintStatement }
procedure TBasePrinter.PrintItem(AItem: TObject);
begin
  if AItem is TToken then
    PrintToken(AItem as TToken)
  else if AItem is TStatement then
    PrintStatement(AItem as TStatement);
end;

procedure TBasePrinter.PrintToken(AToken: TToken);
begin
  { ������ �� ������ }
end;

procedure TBasePrinter.PrintSpecialComment(AValue: string);
begin
  { ������ �� ������ }
end;

procedure TBasePrinter.Ruler(const ARuler: string; Enabled: boolean = true);
begin
  { ������ �� ������ }
end;

function TBasePrinter.MakeDraftPrinter: TPrinter;
begin
  Result := TBasePrinter.Create;
end;

function TBasePrinter.CurrentCol: integer;
begin
  Result := -1;
end;

procedure TBasePrinter.ControlChanged;
begin
  { ������ �� ������ }
end;

procedure TBasePrinter.SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
begin
  { ������ �� ������ }
end;

procedure TBasePrinter.PrintStatement(AStatement: TStatement);
begin
  AStatement.PrintSelf(Self);
end;

procedure TBasePrinter.Indent;
begin
  { ������ �� ������ }
end;

procedure TBasePrinter.Undent;
begin
  { ������ �� ������ }
end;

procedure TBasePrinter.NextLine;
begin
  { ������ �� ������ }
end;

procedure TBasePrinter.SupressNextLine;
begin
  { ������ �� ������ }
end;

procedure TBasePrinter.SupressSpaces(ASupress: boolean);
begin
  { ������ �� ������ }
end;

function TBasePrinter.GetText: string;
begin
  Result := '';
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                  ������� ��� ������ ���������������� ������                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TFormatterPrinter.Create(AMemo: TMemo);
begin
  Memo     := AMemo;
  inherited Create;
  TokenPos := TDictionary<TToken, integer>.Create;
  TokenLen := TDictionary<TToken, integer>.Create;
  SpecialComments := TObjectList<TToken>.Create(true);
end;

destructor TFormatterPrinter.Destroy;
begin
  FreeAndNil(TokenPos);
  FreeAndNil(TokenLen);
  FreeAndNil(SpecialComments);
  inherited;
end;

{ ������������� ����� ������� ������ }
procedure TFormatterPrinter.BeginPrint;
begin
  inherited;
  Builder := TStringBuilder.Create;
  Shift   := 0;
  Col     := 1;
  BOL     := false;
  BOT     := true;
  Padding := 0;
  TokenPos.Clear;
  TokenLen.Clear;
  SpecialComments.Clear;
end;

{ ����� �������� ���������� }
procedure TFormatterPrinter.EndPrint;
begin
  if Assigned(Builder) then Text := Builder.ToString else Text := '';
  FreeAndNil(Builder);
  if Assigned(Memo) then Memo.Text := Text;
  PrevToken := nil;
  PrevStatement := nil;
  inherited;
end;

{ ������������� ����� ������ �� ������� �������������� �������� }
procedure TFormatterPrinter.PrintItem(AItem: TObject);
begin
  if AItem is TFormatterCmd then
    begin
      TFormatterCmd(AItem).PrintSelf(Self);
      FreeAndNil(AItem);
    end
  else
    inherited;
end;

{ ��������, ����� �� ������ ����� ����� ��������� }
function TFormatterPrinter.SpaceRequired(ALeft, ARight: TToken): boolean;
begin
  { ���� ������� ��������� - �� ������ �� }
  if SupSpace > 0 then exit(false);
  { ����� � ������� ��������� ������ �� ����� }
  if ARight.Value = ';' then exit(false);
  { ����� ��������� � ����� ������ �� ����� }
  if (ALeft.Value = '.') or (ARight.Value = '.') then exit(false);
  { ������� ��������� ������ �� ����� }
  if ARight.Value = ',' then exit(false);
  { � ����������� number(5,2) ������� ��������� � ����� ���� }
  if (ALeft.Value = ',') and TTerminal(ALeft).IntoNumber then exit(false);
  { ����������� ������ ��������� ������ � ��������������� � �������� ������ char, table, row }
  if (ARight.Value = '(') and (ALeft is TEpithet) and (SameText(ALeft.Value, 'char') or SameText(ALeft.Value, 'table') or SameText(ALeft.Value, 'row') or not TEpithet(ALeft).IsKeyword) then exit(false);
  { � ����������� ������ ��������� ������ �� }
  if ALeft.Value = '(' then exit(false);
  { ����������� ������ ��������� ������ �� ����� }
  if ARight.Value = ')' then exit(false);
  { �������� %type � �������� ��������� ������ �� ����� }
  if ARight.Value.StartsWith('%') then exit(false);
  { ������� �������� ��������� ����� � ���������� �� ���� }
  if (ALeft is TTerminal) and (TTerminal(ALeft).OpType = otUnary) then exit(false);
  { ���� ������� �� ���������, ������  ������ }
  Result := true;
end;

{ ��������, ����� �� ������ ������ ����� ����� ������������� }
function TFormatterPrinter.EmptyLineRequired(APrev, ANext: TStatement): boolean;
begin
  { ������� set, @ ����� ������ }
  if (APrev.ClassType = ANext.ClassType) and ((APrev.ClassType = TSet) or (APrev.ClassType = TAt)) then exit(false);
  { ������� ������ ������ ����� ����� ������������ �������� ������ }
  if not Assigned(ANext.Parent) then exit(true);
  { ������� �� ��������� }
  Result := false;
end;

{ ����� ��������� ������� � ������������ ���� ��������� �� �������������� }
procedure TFormatterPrinter.PrintToken(AToken: TToken);
var
  Value: string;
  AllowLF: boolean;
begin
  AllowLF := (SupNextLine = 0);
  { ���������� ������� ������ ������ }
  if EmptyLine and not BOT then
  begin
    if Assigned(Builder) and AllowLF then Builder.AppendLine;
    BOL := true;
    EmptyLine := false;
  end;
  { ���������� ������� �� ����� ������ }
  if BOL then
  begin
    if Assigned(Builder) and AllowLF and not BOT then Builder.AppendLine.Append(StringOfChar(' ', Shift));
    BOL := false;
    if AllowLF then
    begin
      Col := Shift + 1;
      PrevCol := 1;
      PrevToken := nil;
    end;
  end;
  { ���� �����, ������� ������ ����� ���������� � ����� ��������� }
  if Assigned(PrevToken) and Assigned(AToken) and SpaceRequired(PrevToken, AToken) then
  begin
    if Assigned(Builder) then Builder.Append(' ');
    Inc(Col);
  end;
  { ���� ������ ������������, ������� ��������������� ���������� �������� }
  if Padding > 0 then
  begin
    if Assigned(Builder) then Builder.Append(StringOfChar(' ', Padding));
    Inc(Col, Padding);
    Inc(PrevCol, Padding);
    Padding := 0;
  end;
  { �, �������, ���� ������ ������� - ���������� � }
  if Assigned(AToken) then
  begin
    Value := AToken.Value;
    { ���� ��������� ������ ������ �� �������� � ����� � ������ �������� }
    if (AToken is TEpithet) and TEpithet(AToken).IsKeyword and
       SameText(AToken.Value, 'default') and Settings.ReplaceDefault and AToken.CanReplace
      then Value := ':=';
    if (AToken is TEpithet) and TEpithet(AToken).IsKeyword and
       SameText(AToken.Value, 'as') and Settings.ReplaceAsIs and AToken.CanReplace
      then Value := 'is';
    if Attributes.HasAttribute(AToken.ClassType, LowerCaseAttribute)
      then Value := Value.ToLower;
    { � ������ �������� ������ - ���������� �������, ����� ������ ���� ����� ������� }
    if Assigned(Builder) then
    begin
      if not IsDraft and not (AToken is TSpecialComment) then
      begin
        TokenPos.Add(AToken, Builder.Length);
        TokenLen.Add(AToken, Value.Length);
      end;
      Builder.Append(Value);
      AToken.Printed := true;
    end;
    Inc(Col, Value.Length);
    PrevToken := AToken;
    BOT := false;
  end;
end;

{ ����� �������������� ����������� � ������������ ������������ }
procedure TFormatterPrinter.PrintStatement(AStatement: TStatement);

  var
    _Mode: TFormatterPrinterMode;
    _Shift, _Col: integer;
    _BOL, _EmptyLine: boolean;
    _Builder: TStringBuilder;
    _Rulers: TRulers;
    _PrevToken: TToken;
    _PrevStatement: TStatement;

  { ������ ��������� ��� ���������, ��������� � �������������� }
  procedure SimplePrintStatement(AStatement: TStatement);
  var _Shift: integer;
  begin
    EmptyLine := EmptyLine or (Assigned(PrevStatement) and Assigned(AStatement) and EmptyLineRequired(PrevStatement, AStatement));
    PrevStatement := AStatement;
    _Shift := Shift;
    inherited PrintStatement(AStatement);
    if Shift <> _Shift then
      raise Exception.CreateFmt('Invalid indents into %s - was %d but %d now', [AStatement.ClassName, _Shift, Shift]);
  end;

  { ���������� ������������ ��� �������� � ��� ����� �������� ������������ }
  procedure SaveCfg;
  begin
    _Shift   := Shift;
    _Col     := Col;
    _BOL     := BOL;
    _EmptyLine := EmptyLine;
    _Builder := Builder;
    _Rulers  := Rulers;
    _PrevToken := PrevToken;
    _PrevStatement := PrevStatement;
    _Mode    := Mode;
  end;

  { �������������� ��� ����� ������������, ������� ����� ��� ������ � �������������� }
  procedure RestoreCfgBeforePrint;
  begin
    Shift := _Shift;
    Col   := _Col;
    BOL   := _BOL;
    EmptyLine := _EmptyLine;
    Builder := _Builder;
    PrevToken := _PrevToken;
    PrevStatement := _PrevStatement;
  end;

  { ������������� �������������� ������������ ����� ������ � �������������� }
  procedure RestoreCfgAfterPrint;
  begin
    FreeAndNil(Rulers);
    Rulers := _Rulers;
    Mode := _Mode;
  end;

  { ������� ������ - ���� ���������� � ������������� }
  procedure PrintGetRulersMode;
  begin
    Builder := nil;
    Rulers  := TRulers.Create;
    Mode    := fpmGetRulers;
    PrevCol := Col;
    try
      SimplePrintStatement(AStatement);
    except
      { ��������� ������ � ����� ���������� ������ �� SetRulers, ����� ������ ���������� ����� �� ������� }
    end;
  end;

  { ������ � ������������ ������������ }
  procedure PrintSetRulersMode;
  begin
    Mode  := fpmSetRulers;
    PrevCol := Col;
    SimplePrintStatement(AStatement);
  end;

begin
  if AStatement.Aligned then
    begin
      SaveCfg;
      PrintGetRulersMode;
      RestoreCfgBeforePrint;
      PrintSetRulersMode;
      RestoreCfgAfterPrint;
    end
  else
    SimplePrintStatement(AStatement);
end;

{ ���������� ��������� }

procedure TFormatterPrinter.Indent;
begin
  Inc(Shift, 4);
end;

procedure TFormatterPrinter.Undent;
begin
  if Shift >= 4 then Dec(Shift, 4);
end;

{ ������� �� ��������� ������ }
procedure TFormatterPrinter.NextLine;
begin
  BOL := true;
end;

{ ��������� ������ ���������� ��������� ������ }
procedure TFormatterPrinter.SupressNextLine(ASupress: boolean);
begin
  PrintToken(nil);
  if ASupress
    then Inc(SupNextLine)
    else Dec(SupNextLine);
  PrintToken(nil);
end;

{ ��������� ������ ���������� �������� }
procedure TFormatterPrinter.SupressSpaces(ASupress: boolean);
begin
  if ASupress
    then Inc(SupSpace)
    else Dec(SupSpace);
end;

{ ����� �� ������� ������������ ����������� (�������������� � �������� ������)}
procedure TFormatterPrinter.PrintSpecialComment(AValue: string);

  procedure PrintSpecialToken(const Text: string);
  var T: TToken;
  begin
    T := TSpecialComment.Create(Text, -1, -1);
    SpecialComments.Add(T);
    PrintToken(T);
  end;

begin
  Self.Ruler('special-comment-start', Settings.AlignSpecialComments);
  PrintSpecialToken('/*/');
  PrintSpecialToken(AValue);
  Self.Ruler('special-comment-finish', Settings.AlignSpecialComments);
  PrintSpecialToken('/*/');
end;

{ ��������� "�������" ��� ������������ }
procedure TFormatterPrinter.Ruler(const ARuler: string; Enabled: boolean = true);
begin
  if not Enabled then exit;
  PrintToken(nil);
  case Mode of
    fpmGetRulers: Rulers.Ruler(ARuler, Col - PrevCol); { � ������ �������� �������� ������ ��������� }
    fpmSetRulers: Padding := Rulers.Padding(ARuler, Col - PrevCol); { � ������ �������� ���������� ����������� ���������� �������� }
  end;
  PrevCol := Col;
end;

{ �������� �������� ��� ������� ������ }
function TFormatterPrinter.MakeDraftPrinter: TPrinter;
begin
  Result := TFormatterPrinter.Create(nil);
  Result.Settings := Self.Settings;
  TFormatterPrinter(Result).IsDraft := true;
end;

{ ������� �������� ��������� ������� }
function TFormatterPrinter.CurrentCol: integer;
begin
  Result := Col;
end;

{ ������� �� �������� ������������ � ����������� � �������� Memo }
procedure TFormatterPrinter.ControlChanged;
var
  P: integer;
  T: TToken;
begin
  if IntoSync or not Assigned(Memo) then exit;
  try
    IntoSync := true;
    { ����� ������� ��� �������� � ����� ������ ���������� � ������������� }
    P := Memo.SelStart;
    for T in TokenPos.Keys do
      if (TokenPos[T] <= P) and (TokenPos[T] + TokenLen[T] >= P) then
        SendSyncNotification(T, T.Line, T.Col, T.Value.Length);
  finally
    IntoSync := false;
  end;
end;

{ ��������� ���������� � ������������� �� ������ ��������� ���������� }
procedure TFormatterPrinter.SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
var T: TToken;
begin
  if IntoSync or not Assigned(Memo) then exit;
  try
    IntoSync := true;
    { ���� ������� �� ������� - ����� ���������� �� ������� }
    if not Assigned(AToken) then
      for T in TokenPos.Keys do
        if (T.Line = ALine) and (T.Col <= ACol) and (T.Col + T.Value.Length > ACol) then
          AToken := T;
    { � ������� � � ������ }
    if not TokenPos.ContainsKey(AToken) then exit;
    Memo.SelStart := TokenPos[AToken];
    Memo.SelLength := TokenLen[AToken];
  finally
    IntoSync := false;
  end;
end;

function TFormatterPrinter.GetText: string;
begin
  if Assigned(Builder)
    then Result := Builder.ToString
    else Result := Text;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              ������� ��� ������ ������ ������������ �����������            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TTokenizerPrinter.Create(AListBox: TListBox);
begin
  Assert(AListBox <> nil);
  inherited Create;
  ListBox := AListBox;
  Tokens := TDictionary<integer, TToken>.Create;
  LineNumbers := TDictionary<TToken, integer>.Create;
end;

destructor TTokenizerPrinter.Destroy;
begin
  FreeAndNil(Tokens);
  FreeAndNil(LineNumbers);
  inherited;
end;

{ ������������� � ���������� � ������ }
procedure TTokenizerPrinter.BeginPrint;
begin
  inherited;
  ListBox.Items.BeginUpdate;
  ListBox.Items.Clear;
  Tokens.Clear;
  LineNumbers.Clear;
end;

{ ����� ��������� ������� }
procedure TTokenizerPrinter.PrintToken(AToken: TToken);
var
  Str: string;
  i: integer;
begin
  { ������� ������� � �������� }
  with AToken do
  begin
    Str := Format('%s "%s", ������ %d, ������� %d', [TokenType, Value, Line, Col]);
    if not Printed then Str := '>>>>> NOT PRINTED >>>>> ' + Str + ' <<<<< NOT PRINTED <<<<<';
  end;
  i := ListBox.Items.Add(Str);
  { �������� ���������� ������� }
  Tokens.Add(i, AToken);
  LineNumbers.Add(AToken, i);
end;

{ ���������� ������ }
procedure TTokenizerPrinter.EndPrint;
begin
  ListBox.Items.EndUpdate;
  inherited;
end;

{ ������� �� ��������� � ����������� ��������� }
procedure TTokenizerPrinter.ControlChanged;
begin
  SendSyncNotification(Tokens[ListBox.ItemIndex], 0, 0, 0);
end;

{ ������� �� ���������� �� ������ ��������� ���������� }
procedure TTokenizerPrinter.SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
var T: TToken;
begin
  { ���� �� ������� �������, ����� ���������� �� ������� � ������ }
  if not Assigned(AToken) then
    for T in LineNumbers.Keys do
      if (T.Line = ALine) and (T.Col <= ACol) and (T.Col + T.Value.Length > ACol) then
        AToken := T;
  { � ������������� �� �� }
  if Assigned(AToken) and LineNumbers.ContainsKey(AToken) then
    ListBox.ItemIndex := LineNumbers[AToken];
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                   ������� ��� ������ ��������������� ������                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TSyntaxTreePrinter.Create(ATreeView: TTreeView);
begin
  TreeView := ATreeView;
  inherited Create;
  Tokens := TDictionary<TToken, TTreeNode>.Create;
end;

destructor TSyntaxTreePrinter.Destroy;
begin
  FreeAndNil(Tokens);
  inherited;
end;

{ ������������� � ���������� � ������ }
procedure TSyntaxTreePrinter.BeginPrint;
begin
  inherited;
  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;
  Tokens.Clear;
  Parents := TStack<TTreeNode>.Create;
  Parents.Push(nil);
end;

{ ���������� ������ � ����� ���������� }
procedure TSyntaxTreePrinter.EndPrint;
begin
  FreeAndNil(Parents);
  TreeView.Items.EndUpdate;
  inherited;
end;

{ ������ ��������� - ������ ������ ������ TStatement ����� ������ � ����������� � ���� �������� }
procedure TSyntaxTreePrinter.PrintStatement(AStatement: TStatement);
begin
  Parents.Push(TreeView.Items.AddChild(Parents.Peek, '< ' + Trim(AStatement.Name) + ' >'));
  try
    inherited;
  finally
    Parents.Pop;
  end;
end;

{ ������ ������� - ������ ������ ������ TToken ������ ������ }
procedure TSyntaxTreePrinter.PrintToken(AToken: TToken);
begin
  Tokens.Add(AToken, TreeView.Items.AddChildObject(Parents.Peek, Format('%s [ %s ]', [AToken.TokenType, AToken.Value]), AToken));
end;


{ ������� �� �������� ������������ � ������ }
procedure TSyntaxTreePrinter.ControlChanged;
var Ptr: pointer;
begin
  if IntoSync then exit;
  try
    IntoSync := true;
    Ptr := TreeView.Selected.Data;
    if Assigned(Ptr) then SendSyncNotification(TToken(Ptr), 0, 0, 0);
  finally
    IntoSync := false;
  end;
end;

{ ��������� ���������� �� ������ ��������� ���������� }
procedure TSyntaxTreePrinter.SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
var T: TToken;
begin
  if IntoSync then exit;
  try
    IntoSync := true;
    { ����� ���������� ������� � ������������� �� ��������������� �� ���� ������ }
    for T in Tokens.Keys do
    begin
      if (T = AToken) or (T.Line = ALine) and (T.Col <= ACol) and (T.Col + T.Value.Length > ACol) then
      try
        TreeView.Items.BeginUpdate;
        TreeView.FullCollapse;
        TreeView.Selected := Tokens[T];
        exit;
      finally
        TreeView.Items.EndUpdate;
      end;
    end;
  finally
    IntoSync := false;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                �������� ��� ������ ���������� ��������������               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TAlarmTokenPrinter.Create(AListBox: TListBox; ATabSheet: TTabSheet);
begin
  TabSheet := ATabSheet;
  inherited Create(AListBox);
  TabSheet.TabVisible := false;
end;

procedure TAlarmTokenPrinter.BeginPrint;
begin
  TabSheet.TabVisible := false;
  inherited;
end;

procedure TAlarmTokenPrinter.PrintToken(AToken: TToken);
begin
  if CheckPrintToken(AToken) then
  begin
    inherited;
    TabSheet.TabVisible := true;
  end;
end;

function TAlarmTokenPrinter.CheckPrintToken(AToken: TToken): boolean;
begin
  Result := not AToken.Printed and not (AToken is TComment);
end;

procedure TAlarmStatementPrinter.PrintStatement(AStatement: TStatement);
begin
  Unexpected := (AStatement is TUnexpectedToken);
  inherited;
  Unexpected := false;
end;

function TAlarmStatementPrinter.CheckPrintToken(AToken: TToken): boolean;
begin
  Result := Unexpected;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                ���� � ���������� ���������� � �������������                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TRulers.Create;
begin
  FMin := TDictionary<String, integer>.Create;
  FMax := TDictionary<String, integer>.Create;
end;

destructor TRulers.Destroy;
begin
  FreeAndNil(FMin);
  FreeAndNil(FMax);
  inherited;
end;

procedure TRulers.Clear;
begin
  FMin.Clear;
  FMax.Clear;
end;

procedure TRulers.Ruler(const ARuler: string; ACol: integer);
begin
  if not FMin.ContainsKey(ARuler) then FMin.Add(ARuler, MaxInt);
  if not FMax.ContainsKey(ARuler) then FMax.Add(ARuler, -MaxInt);
  FMin[ARuler] := Math.Min(ACol, FMin[ARuler]);
  FMax[ARuler] := Math.Max(ACol, FMax[ARuler]);
end;

function TRulers.Padding(const ARuler: string; ACol: integer): integer;
begin
  Result := FMax[ARuler] - ACol;
end;

{ TFormatSettings }

constructor TFormatSettings.Default;
begin
  DeclarationSingleLineParamLimit := 1;
  ArgumentSingleLineParamLimit    := 3;
  MatchParamLimit                 := 3;
  AlignVariables                  := true;
  AlignFields                     := true;
  AlignExpressions                := true;
  AlignTableColumnComments        := true;
  AlignSpecialComments            := true;
  ReplaceDefault                  := true;
  ReplaceAsIs                     := true;
  PreferredExpressionLength       := 60;
end;

end.
