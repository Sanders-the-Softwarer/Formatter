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

  { Интерфейс вывода на принтер }
  TPrinter = class
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
    procedure SpaceOrNextLine(AMultiLine: boolean);
  public
    class function CreateTokenizerPrinter(AStrings: TStrings): TPrinter;
    class function CreateSyntaxTreePrinter(ATreeView: TTreeView): TPrinter;
    class function CreateFormatterPrinter(AStrings: TStrings): TPrinter;
  end;

  TParserSettings = class
  public
    DeclarationSingleLineParamLimit: integer;
    ArgumentSingleLineParamLimit: integer;
    AlignSubroutineParams: boolean;
    AlignVariables: boolean;
    AlignCallArguments: boolean;
    AlignCommentInsert: boolean;
    CommentInsert: boolean;
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
    Rulers:  TRulers;
    Mode:    TFormatterPrinterMode;
    Shift:   integer;
    BOL:     boolean;
    SPC:     boolean;
    Col:     integer;
    PrevCol: integer;
    _Bldr:   TStringBuilder;
    _Shift:  integer;
    _BOL:    boolean;
    _SPC:    boolean;
    _Col:    integer;
    Padding: integer;
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
  Rulers  := TRulers.Create;
  Shift   := 0;
  Col     := 1;
end;

procedure TFormatterPrinter.EndPrint;
begin
  Strings.BeginUpdate;
  Strings.Text := Builder.ToString;
  Strings.EndUpdate;
  FreeAndNil(Builder);
  FreeAndNil(Rulers);
  inherited;
end;

procedure TFormatterPrinter.PrintToken(AToken: TToken);
begin
  if BOL then
  begin
    if Assigned(Builder) then Builder.AppendLine;
    if Assigned(Builder) then Builder.Append(StringOfChar(' ', Shift));
    BOL := false;
    SPC := false;
    Col := Shift + 1;
    PrevCol := 1;
  end;
  if SPC and Assigned(AToken) then
  begin
    if Assigned(Builder) then Builder.Append(' ');
    SPC := false;
    Inc(Col);
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
    if Assigned(Builder) then Builder.Append(AToken.Value);
    Inc(Col, AToken.Value.Length);
  end;
end;

procedure TFormatterPrinter.PrintStatement(AStatement: TStatement);
begin
  { Если задан режим печати с выравниваниями }
  if {Attributes.HasAttribute(AStatement.ClassType, AlignedAttribute)} AStatement.Aligned then
    begin
      if Mode <> fpmNormal then raise Exception.Create('Recursive ruler not supported');
      { Сохраним конфигурацию }
      _Shift := Shift;
      _Col   := Col;
      _BOL   := BOL;
      _SPC   := SPC;
      _Bldr  := Builder;
      Builder := nil;
      { Соберём информацию }
      Rulers.Clear;
      Mode := fpmGetRulers;
      PrevCol := Col;
      inherited;
      { Восстановим конфигурацию }
      Shift := _Shift;
      Col   := _Col;
      BOL   := _BOL;
      SPC   := _SPC;
      Builder := _Bldr;
      Mode  := fpmSetRulers;
      PrevCol := Col;
      { И напечатаем по-настоящему }
      inherited;
      Mode := fpmNormal;
      Rulers.Clear;
    end
  else
    inherited;
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
  SPC := true;
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
  Parents.Push(TreeView.Items.AddChild(Parents.Peek, AStatement.Name));
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

