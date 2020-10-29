////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ������������ ����������                          //
//                                                                            //
//                ����� ���������������� ������ � GUI-���������               //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit GUIPrinters;

interface

uses
  SysUtils, StdCtrls, ExtCtrls, ComCtrls, System.Generics.Collections,
  Printer, Tokens;

{ ������� �������� ��������� ��������� ����� }
function CreateTokenizerPrinter(AListBox: TListBox): TPrinter;
function CreateSyntaxTreePrinter(ATreeView: TTreeView; AHideTransparentCheckBox: TCheckBox): TPrinter;
function CreateFormatterPrinter(ASettings: TFormatSettings; AMemo: TMemo): TPrinter;
function CreateAlarmTokenPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
function CreateAlarmStatementPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;

implementation

uses BasePrinter, FormatterPrinter, Statements;

type

  { ������� ��� ������ ���������������� ������ }
  TGUIFormatterPrinter = class(TFormatterPrinter)
  strict private
    Memo: TMemo;
    IntoSync: boolean;
  public
    constructor Create(ASettings: TFormatSettings; AMemo: TMemo);
    procedure EndPrint; override;
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
    procedure Clear; override;
    procedure PrintToken(AToken: TToken); override;
    procedure EndPrint; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
  end;

  { ������� ��� ���������� ��������������� ������ }
  TSyntaxTreePrinter = class(TBasePrinter)
  private
    TreeView: TTreeView;
    HideTransparentCheckBox: TCheckBox;
    Parents: TStack<TTreeNode>;
    IntoSync: boolean;
    Tokens: TDictionary<TToken, TTreeNode>;
  public
    constructor Create(ATreeView: TTreeView; AHideTransparentCheckBox: TCheckBox);
    destructor Destroy; override;
    procedure BeginPrint; override;
    procedure Clear; override;
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

function CreateFormatterPrinter(ASettings: TFormatSettings; AMemo: TMemo): TPrinter;
begin
  Result := TGUIFormatterPrinter.Create(ASettings, AMemo);
end;

function CreateSyntaxTreePrinter(ATreeView: TTreeView; AHideTransparentCheckBox: TCheckBox): TPrinter;
begin
  Result := TSyntaxTreePrinter.Create(ATreeView, AHideTransparentCheckBox);
end;

function CreateTokenizerPrinter(AListBox: TListBox): TPrinter;
begin
  Result := TTokenizerPrinter.Create(AListBox);
end;

function CreateAlarmTokenPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
begin
  Result := TAlarmTokenPrinter.Create(AListBox, ATabSheet);
end;

function CreateAlarmStatementPrinter(AListBox: TListBox; ATabSheet: TTabSheet): TPrinter;
begin
  Result := TAlarmStatementPrinter.Create(AListBox, ATabSheet);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//               ������� ��� ������ ���������������� ������ � GUI             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TGUIFormatterPrinter.Create(ASettings: TFormatSettings; AMemo: TMemo);
begin
  Assert(AMemo <> nil);
  inherited Create(ASettings, false, [], true);
  Memo := AMemo;
  Assert(Assigned(TokenPos) and Assigned(TokenLen));
end;

{ ��� ���������� ������ ������� �������������� ����� � Memo }
procedure TGUIFormatterPrinter.EndPrint;
begin
  inherited;
  Memo.Text := GetText;
end;

{ ��� ��������� ������������ � Memo ����� ������� ������� � ��������� � ���
  ������ �������� ���������� }
procedure TGUIFormatterPrinter.ControlChanged;
var
  P: integer;
  T: TToken;
begin
  if IntoSync then exit;
  try
    IntoSync := true;
    P := Memo.SelStart;
    for T in TokenPos.Keys do
      if (TokenPos[T] <= P) and (TokenPos[T] + TokenLen[T] >= P) then
        SendSyncNotification(T, T.Line, T.Col, Length(T.Value));
  finally
    IntoSync := false;
  end;
end;

{ ��� ���������� �� ������ ��������� ���������� ������� ��������� ������� }
procedure TGUIFormatterPrinter.SyncNotification(AToken: TToken; ALine, ACol, ALen: integer);
var T: TToken;
begin
  if IntoSync then exit;
  try
    IntoSync := true;
    { ���� ������� �� ������� - ����� ���������� �� ������� }
    if not Assigned(AToken) then
      for T in TokenPos.Keys do
        if (T.Line = ALine) and (T.Col <= ACol) and (T.Col + Length(T.Value) > ACol) then
          AToken := T;
    { � ������� � � ������ }
    if not TokenPos.ContainsKey(AToken) then exit;
    Memo.SelStart := TokenPos[AToken] + TokenLen[AToken];
    Memo.SelLength := -TokenLen[AToken]; { ��������� ��������� � �������� ������� ��� ������� ������ ��������� �������� � ������, � �� � ����� }
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

{ ���������� � ������ }
procedure TTokenizerPrinter.BeginPrint;
begin
  inherited;
  ListBox.Items.BeginUpdate;
end;

{ ������������� �������� }
procedure TTokenizerPrinter.Clear;
begin
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

constructor TSyntaxTreePrinter.Create(ATreeView: TTreeView; AHideTransparentCheckBox: TCheckBox);
begin
  TreeView := ATreeView;
  HideTransparentCheckBox := AHideTransparentCheckBox;
  inherited Create;
  Tokens := TDictionary<TToken, TTreeNode>.Create;
  Parents := TStack<TTreeNode>.Create;
end;

destructor TSyntaxTreePrinter.Destroy;
begin
  FreeAndNil(Tokens);
  FreeAndNil(Parents);
  inherited;
end;

{ ������������� � ���������� � ������ }
procedure TSyntaxTreePrinter.BeginPrint;
begin
  inherited;
  TreeView.Items.BeginUpdate;
end;

procedure TSyntaxTreePrinter.Clear;
begin
  TreeView.Items.Clear;
  Tokens.Clear;
  Parents.Clear;
  Parents.Push(nil);
end;

{ ���������� ������ � ����� ���������� }
procedure TSyntaxTreePrinter.EndPrint;
begin
  TreeView.Items.EndUpdate;
  inherited;
end;

{ ������ ��������� - ������ ������ ������ TStatement ����� ������ � ����������� � ���� �������� }
procedure TSyntaxTreePrinter.PrintStatement(AStatement: TStatement);
var Transparent: boolean;
begin
  Transparent := AStatement.Transparent and (not Assigned(HideTransparentCheckBox) or HideTransparentCheckBox.Checked);
  if not Transparent then Parents.Push(TreeView.Items.AddChild(Parents.Peek, '< ' + Trim(AStatement.Name) + ' >'));
  try
    inherited;
  finally
    if not Transparent then Parents.Pop;
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
  Result := not AToken.Printed;
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

end.

