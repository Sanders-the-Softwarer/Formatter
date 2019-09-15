unit TreePrinter;

interface

uses System.SysUtils, Vcl.ComCtrls, System.Generics.Collections, Tokens;

type
  TTreePrinter = class
  private
    FTreeView: TTreeView;
    LastNode: TTreeNode;
    Parents: TStack<TTreeNode>;
  public
    property TreeView: TTreeView read FTreeView;
  public
    constructor Create(ATreeView: TTreeView);
    destructor Destroy; override;
    procedure PrintNode(Main, Additional: string); overload;
    procedure PrintNode(Main: string); overload;
    procedure PrintNode(Token: TToken); overload;
    procedure BeginChildren;
    procedure EndChildren;
  end;

implementation

{ TTreePrinter }

constructor TTreePrinter.Create(ATreeView: TTreeView);
begin
  FTreeView := ATreeView;
  TreeView.Items.Clear;
  Parents := TStack<TTreeNode>.Create;
  Parents.Push(nil);
end;

destructor TTreePrinter.Destroy;
begin
  FreeAndNil(Parents);
  inherited;
end;

procedure TTreePrinter.PrintNode(Main, Additional: string);
begin
  if Additional <> '' then Additional := ' [ ' + Additional + ' ]';
  PrintNode(Main + Additional);
end;

procedure TTreePrinter.PrintNode(Main: string);
begin
  LastNode := TreeView.Items.AddChild(Parents.Peek, Main);
end;

procedure TTreePrinter.PrintNode(Token: TToken);
begin
  if Assigned(Token) then PrintNode(Token.TokenType, Token.Value);
end;

procedure TTreePrinter.BeginChildren;
begin
  Parents.Push(LastNode);
end;

procedure TTreePrinter.EndChildren;
begin
  Parents.Pop;
end;

end.

