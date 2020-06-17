////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                           Команда SET  (SQL*Plus)                          //
//                                                                            //
//               Copyright(c) 2019-2020 by Sanders the Softwarer              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Set_SQLPlus;

interface

uses SysUtils, System.Generics.Collections, Tokens, Statements, Printer,
  Commons, SQLPlus;

type
  { Команда set }
  TSet = class(TSQLPlusStatement)
  strict private
    _Set, _Target: TEpithet;
    _Value: TTerminal;
  strict protected
    function InternalParse: boolean; override;
    procedure InternalPrintSelf(APrinter: TPrinter); override;
  public
    function Grouping: TStatementClass; override;
  end;

implementation

uses Parser;

type
  { Класс, поддерживающий список set-переменных }
  TSetVariables = class
  strict private
    Shorts, Longs: TDictionary<string, string>;
  strict protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Short, Rest: string);
    function Contains(const Str: string; out Short, Long: string): boolean;
  end;

var
  SetVariables: TSetVariables;

{ TSet }

function TSet.InternalParse: boolean;
var Short, Long: string;
begin
  Result := true;
  _Set := Keyword('set');
  if not Assigned(_Set) then exit(false);
  _Target := Epithet;
  if not Assigned(_Target) or not SetVariables.Contains(_Target.Value, Short, Long) then exit(false);
  _Value := SqlPlusString;
  inherited;
end;

procedure TSet.InternalPrintSelf(APrinter: TPrinter);
begin
  APrinter.StartRuler(Settings.AlignSQLPLUS);
  APrinter.PrintRulerItems('target', [_Set, _Target]);
  APrinter.PrintRulerItems('value', [_Value]);
  inherited;
end;

function TSet.Grouping: TStatementClass;
begin
  Result := TSet;
end;

{ TSetCommands }

constructor TSetVariables.Create;
begin
  Shorts := TDictionary<string, string>.Create;
  Longs  := TDictionary<string, string>.Create;
end;

destructor TSetVariables.Destroy;
begin
  FreeAndNil(Shorts);
  FreeAndNil(Longs);
  inherited;
end;

procedure TSetVariables.Add(Short, Rest: string);
var Long, Key: string;
begin
  Long := Short + Rest;
  Key := Short;
  repeat
    Shorts.Add(Key, Short);
    Longs.Add(Key, Long);
    if Rest = '' then exit;
    Key := Key + Rest[1];
    Rest := Rest.Substring(1);
  until false;
end;

function TSetVariables.Contains(const Str: string; out Short,
  Long: string): boolean;
begin
  Result := Shorts.ContainsKey(Str);
  if not Result then exit;
  Short := Shorts[Str];
  Long  := Longs[Str];
end;

initialization
  SetVariables := TSetVariables.Create;
  with SetVariables do
  begin
    Add('appi', 'nfo');
    Add('array', 'size');
    Add('auto', 'commit');
    Add('autop', 'rint');
    Add('autorecovery', '');
    Add('autot', 'race');
    Add('blo', 'ckterminator');
    Add('cmds', 'ep');
    Add('colinvi', 'sible');
    Add('col', ''); // мистическая команда, которой нет в документации, но которая, тем не менее, встречается в скриптах top
    Add('colsep', '');
    Add('con', 'cat');
    Add('copyc', 'ommit');
    Add('copytypecheck', '');
    Add('def', 'ine');
    Add('describe', '');
    Add('echo', '');
    Add('editf', 'ile');
    Add('emb', 'edded');
    Add('errorl', 'ogging');
    Add('esc', 'ape');
    Add('escchar', '');
    Add('exitc', 'ommit');
    Add('feed', 'back');
    Add('flagger', '');
    Add('flu', 'sh');
    Add('hea', 'ding');
    Add('heads', 'ep');
    Add('hist', 'ory');
    Add('instance', '');
    Add('lin', 'esize');
    Add('lobof', 'fset');
    Add('lobprefetch', '');
    Add('logsource', '');
    Add('long', '');
    Add('longc', 'hunksize');
    Add('mark', 'up');
    Add('newp', 'age');
    Add('', '');
    Add('null', '');
    Add('numf', 'ormat');
    Add('num', 'width');
    Add('pages', 'ize');
    Add('pau', 'se');
    Add('recsep', '');
    Add('recsepchar', '');
    Add('rowprefetch', '');
    Add('securedcol', '');
    Add('serverout', 'put');
    Add('shift', 'inout');
    Add('show', 'mode');
    Add('sqlbl', 'anklines');
    Add('sqlc', 'ase');
    Add('sqlco', 'ntinue');
    Add('sqln', 'umber');
    Add('sqlpluscompat', 'ibility');
    Add('sqlpre', 'fix');
    Add('sqlp', 'rompt');
    Add('sqlt', 'erminator');
    Add('statementc', 'ache');
    Add('suf', 'fix');
    Add('tab', '');
    Add('term', 'out');
    Add('ti', 'me');
    Add('timi', 'ng');
    Add('trim', 'out');
    Add('trims', 'pool');
    Add('und', 'erline');
    Add('ver', 'ify');
    Add('wra', 'p');
    Add('xmlopt', 'imizationcheck');
  end;

finalization
  FreeAndNil(SetVariables);

end.
