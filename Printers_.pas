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
  public
    class function CreateTokenizerPrinter(AStrings: TStrings): TPrinter;
    class function CreateSyntaxTreePrinter(ATreeView: TTreeView): TPrinter;
    class function CreateFormatterPrinter(AStrings: TStrings): TPrinter;
  end;

implementation

uses Tokens, Parser;

type

  TBasePrinter = class(TPrinter)
  public
    procedure BeginPrint; override;
    procedure PrintItem(AItem: TObject); override; final;
    procedure PrintToken(AToken: TToken); virtual;
    procedure PrintStatement(AStatement: TStatement); virtual;
    procedure EndPrint; override;
  end;

  TFormatterPrinter = class(TBasePrinter)
  private
    Strings: TStrings;
  public
    constructor Create(AStrings: TStrings);
    procedure BeginPrint; override;
    procedure EndPrint; override;
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

procedure TBasePrinter.PrintStatement(AStatement: TStatement);
begin
  AStatement.PrintSelf(Self);
end;

{ TFormatterPrinter }

procedure TFormatterPrinter.BeginPrint;
begin
  inherited;
  { ничего не делаем }
end;

constructor TFormatterPrinter.Create(AStrings: TStrings);
begin
  { ничего не делаем }
end;

procedure TFormatterPrinter.EndPrint;
begin
  inherited;
  { ничего не делаем }
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

