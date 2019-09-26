////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                       Вывод информации в разных видах                      //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Printers_;

{ ----- Примечания -------------------------------------------------------------

  Для выдачи результатов разбора сформированные объекты печатают себя в
  предложенный им "принтер". Помимо основного принтера, выводящего
  отформатированный текст, приложение поддерживает дополнительные для
  выдачи отладочной информации - соль решения в том, что это оказывается
  доступным бесплатно, без нагрузки классов анализатора дополнительными
  методами.

  Класс TPrinter - это, по сути, интерфейс вывода на принтер. Пустая реализация
  спрятана в классе TBasePrinter; их не следует объединять, поскольку тогда
  получится циклическая ссылка между модулями Printers, Tokens и Statements.

------------------------------------------------------------------------------ }

interface

uses Classes, SysUtils, Math, ComCtrls, System.Generics.Collections;

type

  { Настройки вывода }
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

  { Интерфейс вывода на принтер }
  TPrinter = class
  private
    FSettings: TFormatSettings;
  public
    procedure BeginPrint; virtual; abstract;
    procedure PrintItem(AItem: TObject); virtual; abstract;
    procedure EndPrint; virtual; abstract;
    procedure Indent; virtual; abstract;
    procedure Undent; virtual; abstract;
    procedure Space; virtual; abstract;
    procedure SupressSpace; virtual; abstract;
    procedure NextLine; virtual; abstract;
    procedure SupressNextLine; virtual; abstract;
    procedure PrintSpecialComment(AValue: string); virtual; abstract;
    procedure Ruler(const ARuler: string; Enabled: boolean = true); virtual; abstract;
  public
    procedure SpaceOrNextLine(AMultiLine: boolean);
    procedure PrintIndented(AItem: TObject);
  public
    property Settings: TFormatSettings read FSettings write FSettings;
  public
    class function CreateTokenizerPrinter(AStrings: TStrings): TPrinter;
    class function CreateSyntaxTreePrinter(ATreeView: TTreeView): TPrinter;
    class function CreateFormatterPrinter(AStrings: TStrings): TPrinter;
  end;

implementation

uses Tokens, Statements, Attributes;

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
    procedure Space; override;
    procedure SupressSpace; override;
    procedure NextLine; override;
    procedure SupressNextLine; override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure Ruler(const ARuler: string; Enabled: boolean = true); override;
  end;

  { Информация о выравниваниях }
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

  { Режим принтера }
  TFormatterPrinterMode = (fpmNormal, fpmGetRulers, fpmSetRulers);

  { Принтер для вывода форматированного текста }
  TFormatterPrinter = class(TBasePrinter)
  private
    Strings: TStrings;
    Builder: TStringBuilder;
    Mode:    TFormatterPrinterMode;
    Shift:   integer;
    BOL:     boolean;
    SPC:     boolean;
    Col:     integer;
    PrevCol: integer;
    Rulers:  TRulers;
    Padding: integer;
    PrevToken: TToken;
  public
    constructor Create(AStrings: TStrings);
    procedure BeginPrint; override;
    procedure EndPrint; override;
    procedure PrintToken(AToken: TToken); override;
    procedure PrintStatement(AStatement: TStatement); override;
    procedure Indent; override;
    procedure Undent; override;
    procedure Space; override;
    procedure SupressSpace; override;
    procedure NextLine; override;
    procedure SupressNextLine; override;
    procedure PrintSpecialComment(AValue: string); override;
    procedure Ruler(const ARuler: string; Enabled: boolean = true); override;
  end;

  { Принтер для вывода последовательности лексем }
  TTokenizerPrinter = class(TBasePrinter)
  private
    Strings: TStrings;
  public
    constructor Create(AStrings: TStrings);
    procedure BeginPrint; override;
    procedure PrintToken(AToken: TToken); override;
    procedure EndPrint; override;
  end;

  { Принтер для построения синтаксического дерева }
  TSyntaxTreePrinter = class(TBasePrinter)
  private
    TreeView: TTreeView;
    Parents: TStack<TTreeNode>;
  public
    constructor Create(ATreeView: TTreeView);
    procedure BeginPrint; override;
    procedure PrintToken(AToken: TToken); override;
    procedure PrintStatement(AStatement: TStatement); override;
    procedure EndPrint; override;
  end;

{ TPrinter }

class function TPrinter.CreateFormatterPrinter(AStrings: TStrings): TPrinter;
begin
  Result := TFormatterPrinter.Create(AStrings);
end;

class function TPrinter.CreateSyntaxTreePrinter(ATreeView: TTreeView): TPrinter;
begin
  Result := TSyntaxTreePrinter.Create(ATreeView);
end;

class function TPrinter.CreateTokenizerPrinter(AStrings: TStrings): TPrinter;
begin
  Result := TTokenizerPrinter.Create(AStrings);
end;

procedure TPrinter.SpaceOrNextLine(AMultiLine: boolean);
begin
  if AMultiLine then NextLine else Space;
end;

procedure TPrinter.PrintIndented(AItem: TObject);
begin
  NextLine;
  Indent;
  PrintItem(AItem);
  Undent;
end;

{ TBasePrinter }

procedure TBasePrinter.BeginPrint;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.EndPrint;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.PrintItem(AItem: TObject);
begin
  if not Assigned(AItem) then
    { пропускаем, это позволяет обойтись без кучи if-ов в других местах }
  else if AItem is TToken then
    PrintToken(AItem as TToken)
  else if AItem is TStatement then
    PrintStatement(AItem as TStatement)
  else
    raise Exception.CreateFmt('Cannot print item of class %s', [AItem.ClassName]);
end;

procedure TBasePrinter.PrintToken(AToken: TToken);
begin
  { ничего не делаем }
end;

procedure TBasePrinter.PrintSpecialComment(AValue: string);
begin
  { ничего не делаем }
end;

procedure TBasePrinter.Ruler(const ARuler: string; Enabled: boolean = true);
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

procedure TBasePrinter.Space;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.SupressSpace;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.NextLine;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.SupressNextLine;
begin
  { ничего не делаем }
end;

{ TFormatterPrinter }

constructor TFormatterPrinter.Create(AStrings: TStrings);
begin
  Strings := AStrings;
  inherited Create;
end;

procedure TFormatterPrinter.BeginPrint;
begin
  inherited;
  Builder := TStringBuilder.Create;
  Shift   := 0;
  Col     := 1;
end;

procedure TFormatterPrinter.EndPrint;
begin
  Strings.BeginUpdate;
  Strings.Text := Builder.ToString;
  Strings.EndUpdate;
  FreeAndNil(Builder);
  inherited;
end;

procedure TFormatterPrinter.PrintToken(AToken: TToken);
var
  Value: string;
  Left: LeftSpaceAttribute;
  Right: RightSpaceAttribute;
  Space: integer;
begin
  if BOL then
  begin
    if Assigned(Builder) then Builder.AppendLine;
    if Assigned(Builder) then Builder.Append(StringOfChar(' ', Shift));
    BOL := false;
    SPC := false;
    Col := Shift + 1;
    PrevCol := 1;
    PrevToken := nil;
  end;
  if SPC and Assigned(AToken) then
  begin
    if Assigned(Builder) then Builder.Append(' ');
    SPC := false;
    Inc(Col);
  end;
  if Assigned(PrevToken) and Assigned(AToken) then
  begin
    Left := Attributes.GetAttribute(AToken, LeftSpaceAttribute) as LeftSpaceAttribute;
    Right := Attributes.GetAttribute(PrevToken, RightSpaceAttribute) as RightSpaceAttribute;
    Space := 0;
    if Assigned(Left) then Inc(Space, Left.Priority);
    if Assigned(Right) then Inc(Space, Right.Priority);
    if Space > 0 then
    begin
      Builder.Append(' ');
      Inc(Col);
    end;
  end;
  if Padding > 0 then
  begin
    Builder.Append(StringOfChar(' ', Padding));
    Inc(Col, Padding);
    Inc(PrevCol, Padding);
    Padding := 0;
  end;
  if Assigned(AToken) then
  begin
    Value := AToken.Value;
    if (AToken is TKeyword) and SameText(AToken.Value, 'default') and Settings.ReplaceDefault
      then Value := ':=';
    if Attributes.HasAttribute(AToken.ClassType, LowerCaseAttribute) then
      Value := Value.ToLower;
    if Assigned(Builder) then Builder.Append(Value);
    Inc(Col, Value.Length);
    PrevToken := AToken;
  end;
end;

procedure TFormatterPrinter.PrintStatement(AStatement: TStatement);
var
  _Mode: TFormatterPrinterMode;
  _Shift, _Col: integer;
  _BOL, _SPC: boolean;
  _Builder: TStringBuilder;
  _Rulers: TRulers;
  _PrevToken: TToken;
begin
  { Если задан режим печати с выравниваниями }
  if AStatement.Aligned then
    begin
      { Сохраним конфигурацию }
      _Shift   := Shift;
      _Col     := Col;
      _BOL     := BOL;
      _SPC     := SPC;
      _Builder := Builder;
      _Rulers  := Rulers;
      _PrevToken := PrevToken;
      _Mode    := Mode;
      { Соберём информацию }
      Builder := nil;
      Rulers  := TRulers.Create;
      Mode    := fpmGetRulers;
      PrevCol := Col;
      try
        inherited;
      except
        { проглотим ошибку и дадим упасть на SetRulers, чтобы выдать текст на принтер }
      end;
      { Восстановим конфигурацию для печати }
      Shift := _Shift;
      Col   := _Col;
      BOL   := _BOL;
      SPC   := _SPC;
      Builder := _Builder;
      PrevToken := _PrevToken;
      { Напечатаем "по-настоящему" (возможно, в рамках вышестоящего fpmGetRulers)}
      Mode  := fpmSetRulers;
      PrevCol := Col;
      inherited;
      { И окончательно восстановим конфигурацию для возврата к вызвавшему }
      FreeAndNil(Rulers);
      Rulers := _Rulers;
      Mode := _Mode;
    end
  else
    begin
      _Shift := Shift;
      inherited;
      if Shift <> _Shift then
        raise Exception.CreateFmt('Invalid indents into %s - was %d but %d now', [AStatement.ClassName, _Shift, Shift]);
    end;
end;

procedure TFormatterPrinter.Indent;
begin
  Inc(Shift, 4);
end;

procedure TFormatterPrinter.Undent;
begin
  if Shift >= 4 then Dec(Shift, 4);
end;

procedure TFormatterPrinter.NextLine;
begin
  BOL := true;
end;

procedure TFormatterPrinter.SupressNextLine;
begin
  BOL := false;
end;

procedure TFormatterPrinter.Space;
begin
  //SPC := true;
end;

procedure TFormatterPrinter.SupressSpace;
begin
  SPC := false;
end;

procedure TFormatterPrinter.PrintSpecialComment(AValue: string);
var T: TToken;
begin
  T := TToken.Create('/*/ ' + AValue + ' /*/', -1, -1);
  PrintToken(T);
  T.Free;
end;

procedure TFormatterPrinter.Ruler(const ARuler: string; Enabled: boolean = true);
begin
  if not Enabled then exit;
  PrintToken(nil);
  case Mode of
    fpmGetRulers: Rulers.Ruler(ARuler, Col - PrevCol);
    fpmSetRulers: Padding := Rulers.Padding(ARuler, Col - PrevCol);
  end;
  PrevCol := Col;
end;

{ TTokenizerPrinter }

constructor TTokenizerPrinter.Create(AStrings: TStrings);
begin
  Assert(AStrings <> nil);
  inherited Create;
  Strings := AStrings;
end;

procedure TTokenizerPrinter.BeginPrint;
begin
  inherited;
  Strings.BeginUpdate;
  Strings.Clear;
end;

procedure TTokenizerPrinter.PrintToken(AToken: TToken);
begin
  with AToken do
    Strings.Add(Format('%s "%s", строка %d, позиция %d', [TokenType, Value, Line, Col]));
end;

procedure TTokenizerPrinter.EndPrint;
begin
  Strings.EndUpdate;
  inherited;
end;

{ TSyntaxTreePrinter }

constructor TSyntaxTreePrinter.Create(ATreeView: TTreeView);
begin
  TreeView := ATreeView;
  inherited Create;
end;

procedure TSyntaxTreePrinter.BeginPrint;
begin
  inherited;
  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;
  Parents := TStack<TTreeNode>.Create;
  Parents.Push(nil);
end;

procedure TSyntaxTreePrinter.EndPrint;
begin
  FreeAndNil(Parents);
  TreeView.Items.EndUpdate;
  inherited;
end;

procedure TSyntaxTreePrinter.PrintStatement(AStatement: TStatement);
begin
  Parents.Push(TreeView.Items.AddChild(Parents.Peek, '< ' + Trim(AStatement.Name) + ' >'));
  try
    inherited;
  finally
    Parents.Pop;
  end;
end;

procedure TSyntaxTreePrinter.PrintToken(AToken: TToken);
begin
  with AToken do
    TreeView.Items.AddChild(Parents.Peek, Format('%s [ %s ]', [TokenType, Value]));
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

