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
  Classes, SysUtils, Math, StdCtrls, ComCtrls, System.Generics.Collections,
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
    AlignSpecialComments: boolean;
    ReplaceDefault: boolean;
  end;

  { ��������� ������ �� ������� }
  TPrinter = class
  private
    FSettings: TFormatSettings;
  public
    procedure BeginPrint; virtual; abstract;
    procedure PrintItem(AItem: TObject); virtual; abstract;
    procedure EndPrint; virtual; abstract;
    procedure Indent; virtual; abstract;
    procedure Undent; virtual; abstract;
    procedure NextLine; virtual; abstract;
    procedure SupressNextLine; virtual; abstract;
    procedure PrintSpecialComment(AValue: string); virtual; abstract;
    procedure Ruler(const ARuler: string; Enabled: boolean = true); virtual; abstract;
    procedure ControlChanged; virtual; abstract;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); virtual; abstract;
  public
    procedure PrintItems(AItems: array of TObject);
    procedure PrintIndented(AItem: TObject); overload;
    procedure PrintIndented(AItems: array of TObject); overload;
    procedure SpaceOrNextLine(AMultiLine: boolean);
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

implementation

uses Statements, Attributes;

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
    procedure SupressNextLine; override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure Ruler(const ARuler: string; Enabled: boolean = true); override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
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
  protected
    function SpaceRequired(ALeft, ARight: TToken): boolean;
    function EmptyLineRequired(APrev, ANext: TStatement): boolean;
  public
    constructor Create(AMemo: TMemo);
    destructor Destroy; override;
    procedure BeginPrint; override;
    procedure EndPrint; override;
    procedure PrintToken(AToken: TToken); override;
    procedure PrintStatement(AStatement: TStatement); override;
    procedure Indent; override;
    procedure Undent; override;
    procedure NextLine; override;
    procedure SupressNextLine; override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure Ruler(const ARuler: string; Enabled: boolean = true); override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
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

procedure TPrinter.SpaceOrNextLine(AMultiLine: boolean);
begin
  if AMultiLine then NextLine;
end;

procedure TPrinter.PrintItems(AItems: array of TObject);
var i: integer;
begin
  for i := Low(AItems) to High(AItems) do PrintItem(AItems[i]);
end;

procedure TPrinter.PrintIndented(AItem: TObject);
begin
  NextLine;
  Indent;
  PrintItem(AItem);
  Undent;
end;

procedure TPrinter.PrintIndented(AItems: array of TObject);
begin
  NextLine;
  Indent;
  PrintItems(AItems);
  Undent;
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
  if not Assigned(AItem) then
    { ����������, ��� ��������� �������� ��� ���� if-�� � ������ ������ }
  else if AItem is TToken then
    PrintToken(AItem as TToken)
  else if AItem is TStatement then
    PrintStatement(AItem as TStatement)
  else
    raise Exception.CreateFmt('Cannot print item of class %s', [AItem.ClassName]);
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
  Padding := 0;
  TokenPos.Clear;
  TokenLen.Clear;
  SpecialComments.Clear;
end;

{ ����� �������� ���������� }
procedure TFormatterPrinter.EndPrint;
begin
  Memo.Lines.BeginUpdate;
  Memo.Text := Builder.ToString;
  Memo.Lines.EndUpdate;
  FreeAndNil(Builder);
  PrevToken := nil;
  PrevStatement := nil;
  inherited;
end;

{ ��������, ����� �� ������ ����� ����� ��������� }
function TFormatterPrinter.SpaceRequired(ALeft, ARight: TToken): boolean;
begin
  { ����� � ������� ��������� ������ �� ����� }
  if ARight.Value = ';' then exit(false);
  { ����� ��������� � ����� ������ �� ����� }
  if (ALeft.Value = '.') or (ARight.Value = '.') then exit(false);
  { ������� ��������� ������ �� ����� }
  if ARight.Value = ',' then exit(false);
  { � ����������� number(5,2) ������� ��������� � ����� ���� }
  if (ALeft.Value = ',') and TTerminal(ALeft).IntoNumber then exit(false);
  { ����������� ������ ��������� ������ � ��������������� }
  if (ARight.Value = '(') and (ALeft is TIdent) then exit(false);
  { ����������� ������ ��������� ������ � �������� ������ char, table }
  if (ARight.Value = '(') and (ALeft is TKeyword) and (SameText(ALeft.Value, 'char') or SameText(ALeft.Value, 'table')) then exit(false);
  { � ����������� ������ ��������� ������ �� }
  if ALeft.Value = '(' then exit(false);
  { ����������� ������ ��������� ������ �� ����� }
  if ARight.Value = ')' then exit(false);
  { �������� %type � �������� ��������� ������ �� ����� }
  if ARight.Value.StartsWith('%') then exit(false);
  { ������� �������� ��������� ����� � ���������� �� ���� }
  if (ALeft is TTerminal) and (TTerminal(ALeft).OpType = otUnary) then exit(false);
  { ���� ������� �� ���������, ����� ���������������� }
  Result := true;
end;

{ ��������, ����� �� ������ ������ ����� ����� ������������� }
function TFormatterPrinter.EmptyLineRequired(APrev, ANext: TStatement): boolean;
begin
  { ������� ������ ������ ����� ����� ������������ �������� ������ }
  if not Assigned(ANext.Parent) then exit(true);
  { ������� �� ��������� }
  Result := false;
end;

{ ����� ��������� ������� � ������������ ���� ��������� �� �������������� }
procedure TFormatterPrinter.PrintToken(AToken: TToken);
var
  Value: string;
begin
  { ���������� ������� ������ ������ }
  if EmptyLine then
  begin
    if Assigned(Builder) then Builder.AppendLine;
    BOL := true;
    EmptyLine := false;
  end;
  { ���������� ������� �� ����� ������ }
  if BOL then
  begin
    if Assigned(Builder) then Builder.AppendLine;
    if Assigned(Builder) then Builder.Append(StringOfChar(' ', Shift));
    BOL := false;
    Col := Shift + 1;
    PrevCol := 1;
    PrevToken := nil;
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
    Builder.Append(StringOfChar(' ', Padding));
    Inc(Col, Padding);
    Inc(PrevCol, Padding);
    Padding := 0;
  end;
  { �, �������, ���� ������ ������� - ���������� � }
  if Assigned(AToken) then
  begin
    Value := AToken.Value;
    { ���� ��������� ������ ������ �� �������� � ����� � ������ �������� }
    if (AToken is TKeyword) and SameText(AToken.Value, 'default') and Settings.ReplaceDefault
      then Value := ':=';
    if Attributes.HasAttribute(AToken.ClassType, LowerCaseAttribute)
      then Value := Value.ToLower;
    { � ������ �������� ������ - ���������� �������, ����� ������ ���� ����� ������� }
    if Assigned(Builder) then
    begin
      if not (AToken is TSpecialComment) then
      begin
        TokenPos.Add(AToken, Builder.Length);
        TokenLen.Add(AToken, Value.Length);
      end;
      Builder.Append(Value);
      AToken.Printed := true;
    end;
    Inc(Col, Value.Length);
    PrevToken := AToken;
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
begin
  { ���� ����� ����� ������ � �������������� }
  if AStatement.Aligned then
    begin
      { �������� ������������ }
      _Shift   := Shift;
      _Col     := Col;
      _BOL     := BOL;
      _EmptyLine := EmptyLine;
      _Builder := Builder;
      _Rulers  := Rulers;
      _PrevToken := PrevToken;
      _PrevStatement := PrevStatement;
      _Mode    := Mode;
      { ������ ���������� }
      Builder := nil;
      Rulers  := TRulers.Create;
      Mode    := fpmGetRulers;
      PrevCol := Col;
      try
        inherited;
      except
        { ��������� ������ � ����� ���������� ������ �� SetRulers, ����� ������ ���������� ����� �� ������� }
      end;
      { ����������� ������������ ��� ������ }
      Shift := _Shift;
      Col   := _Col;
      BOL   := _BOL;
      EmptyLine := _EmptyLine;
      Builder := _Builder;
      PrevToken := _PrevToken;
      PrevStatement := _PrevStatement;
      { ���������� "��-����������" (��������, � ������ ������������ fpmGetRulers)}
      Mode  := fpmSetRulers;
      PrevCol := Col;
      inherited;
      { � ������������ ����������� ������������ ��� �������� � ���������� }
      FreeAndNil(Rulers);
      Rulers := _Rulers;
      Mode := _Mode;
    end
  else
    begin
      { ��� ������������ - ������ ���������� � ��������, ��� �� ������ �������������� Indent/Undent-� }
      EmptyLine := Assigned(PrevStatement) and Assigned(AStatement) and EmptyLineRequired(PrevStatement, AStatement);
      PrevStatement := AStatement;
      _Shift := Shift;
      inherited;
      if Shift <> _Shift then
        raise Exception.CreateFmt('Invalid indents into %s - was %d but %d now', [AStatement.ClassName, _Shift, Shift]);
    end;
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

{ ������� �� ��������� ������ � ��� ���������� }

procedure TFormatterPrinter.NextLine;
begin
  BOL := true;
end;

procedure TFormatterPrinter.SupressNextLine;
begin
  BOL := false;
end;

{ ����� �� ������� ������������ ����������� (�������������� � �������� ������)}
procedure TFormatterPrinter.PrintSpecialComment(AValue: string);
var T: TToken;
begin
  T := TSpecialComment.Create('/*/ ' + AValue + ' /*/', -1, -1);
  PrintToken(T);
  SpecialComments.Add(T);
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

{ ������� �� �������� ������������ � ����������� � �������� Memo }
procedure TFormatterPrinter.ControlChanged;
var
  P: integer;
  T: TToken;
begin
  if IntoSync then exit;
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
  if IntoSync then exit;
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

{ TAlarmTokenPrinter }

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

{ TAlarmStatementPrinter }

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

{ TRulerInfo }

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

end.

