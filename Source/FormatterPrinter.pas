unit FormatterPrinter;

interface

uses
  Classes, SysUtils, Math, System.Generics.Collections, Printers_, Tokens, Statements;

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
    procedure StartRuler(Enabled: boolean); override;
    procedure Ruler(const ARuler: string); override;
    function  MakeDraftPrinter: TPrinter; override;
    function  CurrentCol: integer; override;
    procedure ControlChanged; override;
    procedure SyncNotification(AToken: TToken; ALine, ACol, ALen: integer); override;
    function  GetText: string; override;
  end;

  { ���������� � ������������� }
  TRulers = class
  private
    Names: TStringList;
    MaxWidth: TDictionary<String, integer>;
    PrevLine, PrevCol: integer;
    DisablePadding: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure NewLine(ALine: integer);
    procedure Take(const ARuler: string; ALine, ACol: integer);
    function Fix(const ARuler: string): integer;
  end;

  { ����� �������� }
  TFormatterPrinterMode = (fpmNormal, fpmGetRulers, fpmSetRulers);

  { ������� ��� ������ ���������������� ������ }
  TFormatterPrinter = class(TBasePrinter)
  strict private
    Builder: TStringBuilder;
    Mode:    TFormatterPrinterMode;
    Shift:   integer;
    BOL:     boolean;
    EmptyLine: boolean;
    WasComment: boolean;
    WasBOL: boolean;
    Line:    integer;
    Col:     integer;
    Rulers:  TRulers;
    PaddingCol: integer;
    PrevStatement: TStatement;
    PrevToken: TToken;
    SpecialComments: TObjectList<TToken>;
    SupSpace, SupNextLine: integer;
    IsDraft: boolean;
    Text:    string;
    RulerEnabled: boolean;
  strict protected
    TokenPos, TokenLen: TDictionary<TToken, integer>;
    function SpaceRequired(ALeft, ARight: TToken): boolean;
    function EmptyLineRequired(APrev, ANext: TStatement): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginPrint; override;
    procedure EndPrint; override;
    procedure PrintItem(AItem: TObject); override;
    procedure PrintToken(AToken: TToken); override;
    procedure PrintStatement(AStatement: TStatement); override;
    procedure PrintRulerItem(const ARuler: string; AItem: TObject); override;
    procedure PrintRulerItems(const ARuler: string; AItems: array of TObject); override;
    procedure Indent; override;
    procedure Undent; override;
    procedure NextLine; override;
    procedure SupressNextLine(ASupress: boolean); override;
    procedure SupressSpaces(ASupress: boolean); override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure StartRuler(Enabled: boolean); override;
    procedure Ruler(const ARuler: string); override;
    function MakeDraftPrinter: TPrinter; override;
    function CurrentCol: integer; override;
    function  GetText: string; override;
  end;

implementation

uses Utils, Attributes, SQLPlus;

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

procedure TBasePrinter.StartRuler;
begin
  { ������ �� ������ }
end;

procedure TBasePrinter.Ruler(const ARuler: string);
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

constructor TFormatterPrinter.Create;
begin
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
  Line    := 1;
  Col     := 1;
  BOL     := false;
  PaddingCol := 0;
  TokenPos.Clear;
  TokenLen.Clear;
  SpecialComments.Clear;
end;

{ ����� �������� ���������� }
procedure TFormatterPrinter.EndPrint;
begin
  if Assigned(Builder) then Text := Builder.ToString else Text := '';
  FreeAndNil(Builder);
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
  Result := false;
  { ���� ������� ��������� - �� ������ �� }
  if SupSpace > 0 then exit;
  { ����� � ������� ��������� ������ �� ����� }
  if ARight.Value = ';' then exit;
  { ����� ��������� � ����� ������ �� ����� }
  if (ALeft.Value = '.') or (ARight.Value = '.') then exit;
  { ���������/��������� ��������� � ���������� �� ��� �������������� }
  if ((ALeft.Value = ':') or (ALeft.Value = '&')) and (ARight is TEpithet) then exit;
  { ������� ��������� ������ �� ����� }
  if ARight.Value = ',' then exit;
  { � ����������� number(5,2) ������� ��������� � ����� ���� }
  if (ALeft.Value = ',') and TTerminal(ALeft).IntoNumber then exit;
  { ����������� ������ ��������� ������ � ��������������� � �������� ������ char, table, row, lob, key, unique }
  if (ARight.Value = '(') and
     (ALeft is TEpithet) and
     (SameText(ALeft.Value, 'char') or
      SameText(ALeft.Value, 'table') or
      SameText(ALeft.Value, 'row') or
      SameText(ALeft.Value, 'lob') or
      SameText(ALeft.Value, 'key') or
      SameText(ALeft.Value, 'unique') or
      not TEpithet(ALeft).IsKeyword) then exit;
  { � ����������� ������ ��������� ������ �� }
  if ALeft.Value = '(' then exit;
  { �����������/����������� ������ ������ ��������� ���� � ����� }
  if ((ALeft.Value = '(') or (ALeft.Value = ')')) and ((ARight.Value = '(') or (ARight.Value = ')')) then exit;
  { ����������� ������ ��������� ������ �� ����� }
  if ARight.Value = ')' then exit;
  { �������� %type � �������� ��������� ������ �� ����� }
  if ARight.Value.StartsWith('%') then exit;
  { ������� �������� ��������� ����� � ���������� �� ���� }
  if (ALeft is TTerminal) and (TTerminal(ALeft).OpType = otUnary) then exit;
  { ���� ������� �� ���������, ������  ������ }
  Result := true;
end;

{ ��������, ����� �� ������ ������ ����� ����� ������������� }
function TFormatterPrinter.EmptyLineRequired(APrev, ANext: TStatement): boolean;
begin
  { ������� set, @, exec ����� ������ }
  if (APrev.ClassType = ANext.ClassType) and
     ((APrev.ClassType = TSet) or
      (APrev.ClassType = TAt) or
      (APrev.ClassType = TCall)) then exit(false);
  { ������� ������ ������ ����� ����� ������������ �������� ������ }
  if not Assigned(ANext.Parent) then exit(true);
  { ������� �� ��������� }
  Result := false;
end;

{ ����� ��������� ������� � ������������ ���� ��������� �� �������������� }
procedure TFormatterPrinter.PrintToken(AToken: TToken);
var
  Value: string;
  AllowLF, StartOfText, ForceNextLine: boolean;
  i: integer;
begin
  StartOfText := (Line = 1) and (Col = 1);
  AllowLF := (SupNextLine = 0) or WasComment;
  { ���������� ������� ������ ������ }
  if EmptyLine and not StartOfText then
  begin
    if Assigned(Builder) and AllowLF then
    begin
      Builder.AppendLine;
      Inc(Line);
    end;
    BOL := true;
    EmptyLine := false;
  end;
  { ���������� ������� �� ����� ������ }
  if BOL and WasBOL then
    BOL := false
  else if BOL then
  begin
    if Assigned(Builder) and AllowLF and not StartOfText then
    begin
      Builder.AppendLine;
      WasBOL := true;
    end;
    BOL := false;
    if AllowLF and not StartOfText then
    begin
      Inc(Line);
      Col := 1;
      PrevToken := nil;
    end;
  end;
  { ���� ������ ������������, ������� ��������������� ���������� �������� }
  if (PaddingCol > Col) and Assigned(AToken) then
  begin
    _Debug(' => col = %d, padding col = %d', [Col, PaddingCol]);
    if WasBOL then Dec(PaddingCol, Shift);
    if Assigned(Builder) then Builder.Append(StringOfChar(' ', PaddingCol - Col));
    Col := PaddingCol;
    PaddingCol := 0;
  end;
  { ���� �����, ������� ������ ����� ���������� � ����� ��������� }
  if Assigned(PrevToken) and Assigned(AToken) and SpaceRequired(PrevToken, AToken) then
  begin
    if Assigned(Builder) then Builder.Append(' ');
    Inc(Col);
  end;
  { ���� �� ������� ���� �����������, ���������� �� }
  if Assigned(AToken) and Assigned(AToken.CommentsAbove) and Assigned(Builder) and not IsDraft then
    for i := 0 to AToken.CommentsAbove.Count - 1 do
    begin
      if not WasBOL then NextLine;
      PrintToken(AToken.CommentsAbove[i]);
      NextLine;
      PrintToken(nil);
    end;
  { �, �������, ���� ������ ������� - ���������� � }
  if Assigned(AToken) then
  begin
    Value := AToken.Value;
    Value := StringReplace(Value, #13, #13#10, [rfReplaceAll]);
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
      if WasBOL then Builder.Append(StringOfChar(' ', Shift));
      if not IsDraft and not (AToken is TSpecialComment) then
      begin
        TokenPos.Add(AToken, Builder.Length);
        TokenLen.Add(AToken, Value.Length);
      end;
      Builder.Append(Value);
      AToken.Printed := true;
    end;
    if WasBOL then Inc(Col, Shift);
    Inc(Col, Value.Length);
    PrevToken := AToken;
    WasBOL := false;
  end;
  { ���� ��� ��� �����������, ������ ������ }
  if Assigned(AToken) then WasComment := AToken is TComment;
  { ���� ����� ������� ���� �����������, ���������� �� }
  if Assigned(AToken) then
  begin
    ForceNextLine := false;
    if Assigned(AToken.CommentsAfter) then
      for i := 0 to AToken.CommentsAfter.Count - 1 do
      begin
        WasComment := true;
        Ruler('right-comment');
        PrintToken(AToken.CommentsAfter[i]);
        ForceNextLine := ForceNextLine or AToken.CommentsAfter[i].Value.StartsWith('--');
      end;
    if ForceNextLine then NextLine;
    if Assigned(AToken.CommentsBelow) and Assigned(Builder) then
      for i := 0 to AToken.CommentsBelow.Count - 1 do
      begin
        WasComment := true;
        NextLine;
        PrintToken(AToken.CommentsBelow[i]);
        NextLine;
      end;
  end;
end;

{ ����� �������������� ����������� � ������������ ������������ }
procedure TFormatterPrinter.PrintStatement(AStatement: TStatement);

  var
    _Mode: TFormatterPrinterMode;
    _Shift, _Col, _Line, _PaddingCol: integer;
    _BOL, _EmptyLine, _WasComment, _WasBOL: boolean;
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
    _Line    := Line;
    _PaddingCol := PaddingCol;
    _BOL     := BOL;
    _EmptyLine := EmptyLine;
    _WasComment := WasComment;
    _WasBOL  := WasBOL;
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
    Line  := _Line;
    PaddingCol := _PaddingCol;
    BOL   := _BOL;
    EmptyLine := _EmptyLine;
    WasComment := _WasComment;
    WasBOL := _WasBOL;
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

procedure TFormatterPrinter.PrintRulerItem(const ARuler: string; AItem: TObject);
begin
  inherited;
  PaddingCol := 0;
end;

procedure TFormatterPrinter.PrintRulerItems(const ARuler: string; AItems: array of TObject);
begin
  inherited;
  PaddingCol := 0;
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

  function SpecialToken(const Text: string): TToken;
  begin
    Result := TSpecialComment.Create(Text, -1, -1);
    SpecialComments.Add(Result);
  end;

begin
  if not RulerEnabled then StartRuler(Settings.AlignSpecialComments);
  Self.PrintRulerItem('special-comment-start', SpecialToken('/*/'));
  PrintItem(SpecialToken(AValue));
  Self.PrintRulerItem('special-comment-finish', SpecialToken('/*/'));
end;

{ ������ ������������ � ��������� ������ }
procedure TFormatterPrinter.StartRuler(Enabled: boolean);
begin
  RulerEnabled := Enabled;
  if (Mode <> fpmGetRulers) or not Enabled then exit;
  PrintToken(nil);
  Rulers.NewLine(Line);
end;

{ ��������� "�������" ��� ������������ }
procedure TFormatterPrinter.Ruler(const ARuler: string);
begin
  if not RulerEnabled then exit;
  case Mode of
    fpmGetRulers: Rulers.Take(ARuler, Line, Col); { � ������ �������� �������� ������ ��������� }
    fpmSetRulers: PaddingCol := Rulers.Fix(ARuler) + Shift; { � ������ �������� ���������� ����������� ���������� �������� }
  end;
end;

{ �������� �������� ��� ������� ������ }
function TFormatterPrinter.MakeDraftPrinter: TPrinter;
begin
  Result := TFormatterPrinter.Create;
  Result.Settings := Self.Settings;
  TFormatterPrinter(Result).IsDraft := true;
end;

{ ������� �������� ��������� ������� }
function TFormatterPrinter.CurrentCol: integer;
begin
  Result := Col;
end;

function TFormatterPrinter.GetText: string;
begin
  if Assigned(Builder)
    then Result := Builder.ToString
    else Result := Text;
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                ���� � ���������� ���������� � �������������                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TRulers.Create;
begin
  Names := TStringList.Create;
  MaxWidth := TDictionary<String, integer>.Create;
end;

destructor TRulers.Destroy;
begin
  FreeAndNil(MaxWidth);
  FreeAndNil(Names);
  inherited;
end;

procedure TRulers.NewLine(ALine: integer);
begin
  if PrevLine < ALine
    then PrevLine := ALine
    else DisablePadding := true;
  PrevCol := 1;
end;

procedure TRulers.Take(const ARuler: string; ALine, ACol: integer);
var Width: integer;
begin
  _Debug('Rulers.Take ruler = "%s", line = %d, col = %d, prev col = %d', [ARuler, ALine, ACol, PrevCol]);
  if ALine <> PrevLine then DisablePadding := true;
  if DisablePadding then exit;
  if PrevLine <= 0 then raise Exception.Create('TRuler.NewLine required');
  if Names.IndexOf(ARuler) < 0 then Names.Add(ARuler);
  Width := ACol - PrevCol;
  if MaxWidth.ContainsKey(ARuler)
    then MaxWidth[ARuler] := Math.Max(MaxWidth[ARuler], Width)
    else MaxWidth.Add(ARuler, Width);
  _Debug(' => width = %d, max width = %d', [Width, MaxWidth[ARuler]]);
  PrevCol := ACol;
end;

function TRulers.Fix(const ARuler: string): integer;
var i: integer;
begin
  Result := 0;
  if DisablePadding then exit;
  for i := 0 to Names.IndexOf(ARuler) do
    Inc(Result, MaxWidth[Names[i]]);
  Inc(Result);
  _Debug('Rulers.Fix, ruler = "%s", fix = %d', [ARuler, Result]);
end;

end.

