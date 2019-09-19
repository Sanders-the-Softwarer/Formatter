////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           ‘орматизатор исходников                          //
//                                                                            //
//                       ¬ывод информации в разных видах                      //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Printers_;

{ ----- ѕримечани€ -------------------------------------------------------------

  ƒл€ выдачи результатов разбора сформированные объекты печатают себ€ в
  предложенный им "принтер". ѕомимо основного принтера, вывод€щего
  отформатированный текст, приложение поддерживает дополнительные дл€
  выдачи отладочной информации - соль решени€ в том, что это оказываетс€
  доступным бесплатно, без нагрузки классов анализатора дополнительными
  методами.

------------------------------------------------------------------------------ }

interface

uses Classes, System.SysUtils, Vcl.ComCtrls, System.Generics.Collections;

type

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
    procedure PaddingFrom; virtual; abstract;
    procedure PaddingTo(ALen: integer); virtual; abstract;
    procedure PrintSpecialComment(AValue: string); virtual; abstract;
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

uses Tokens, Statements;

type

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
    procedure PaddingFrom; override;
    procedure PaddingTo(ALen: integer); override;
    procedure PrintSpecialComment(AValue: string); override;
  end;

  TFormatterPrinter = class(TBasePrinter)
  private
    Strings: TStrings;
    Builder: TStringBuilder;
    Shift:   integer;
    BOL:     boolean;
    SPC:     boolean;
    PadPos:  integer;
    PAD:     boolean;
  public
    constructor Create(AStrings: TStrings);
    procedure BeginPrint; override;
    procedure EndPrint; override;
    procedure PrintItem(AItem: TObject); override;
    procedure PrintToken(AToken: TToken); override;
    procedure Indent; override;
    procedure Undent; override;
    procedure Space; override;
    procedure SupressSpace; override;
    procedure NextLine; override;
    procedure PaddingFrom; override;
    procedure PaddingTo(ALen: integer); override;
    procedure PrintSpecialComment(AValue: string); override;
  end;

  TTokenizerPrinter = class(TBasePrinter)
  private
    Strings: TStrings;
  public
    constructor Create(AStrings: TStrings);
    procedure BeginPrint; override;
    procedure PrintToken(AToken: TToken); override;
    procedure EndPrint; override;
  end;

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
    { пропускаем, это позвол€ет обойтись без кучи if-ов в других местах }
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

procedure TBasePrinter.PaddingFrom;
begin
  { ничего не делаем }
end;

procedure TBasePrinter.PaddingTo(ALen: integer);
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
  BOL     := true;
end;

procedure TFormatterPrinter.EndPrint;
begin
  Strings.BeginUpdate;
  Strings.Text := Builder.ToString;
  Strings.EndUpdate;
  FreeAndNil(Builder);
  inherited;
end;

procedure TFormatterPrinter.PrintItem(AItem: TObject);
begin
  if not Assigned(AItem) and PAD then
    PrintToken(nil)
  else
    inherited;
end;

procedure TFormatterPrinter.PrintToken(AToken: TToken);
begin
  if BOL then
  begin
    Builder.Append(StringOfChar(' ', Shift));
    BOL := false;
    SPC := false;
  end;
  if SPC and Assigned(AToken) then
  begin
    Builder.Append(' ');
    SPC := false;
  end;
  if PAD then
  begin
    PadPos := Builder.Length;
    PAD := false;
  end;
  if Assigned(AToken) then Builder.Append(AToken.Value);
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
  Builder.AppendLine;
  BOL := true;
end;

procedure TFormatterPrinter.Space;
begin
  SPC := true;
end;

procedure TFormatterPrinter.SupressSpace;
begin
  SPC := false;
end;

procedure TFormatterPrinter.PaddingFrom;
begin
  PAD := true;
end;

procedure TFormatterPrinter.PaddingTo(ALen: integer);
var i: integer;
begin
  for i := Builder.Length to PadPos + ALen - 1 do Builder.Append(' ');
  SPC := false;
end;

procedure TFormatterPrinter.PrintSpecialComment(AValue: string);
var T: TToken;
begin
  T := TToken.Create('/*/ ' + AValue + ' /*/', -1, -1);
  PrintToken(T);
  T.Free;
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
    Strings.Add(Format('%s "%s", строка %d, позици€ %d', [TokenType, Value, Line, Col]));
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
    TreeView.Items.AddChild(Parents.Peek, Format('%s [%s]', [TokenType, Value]));
end;

end.

