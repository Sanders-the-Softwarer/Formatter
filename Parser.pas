////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           Форматизатор исходников                          //
//                                                                            //
//                          Синтаксический анализатор                         //
//                                                                            //
//                  Copyright(c) 2019 by Sanders the Softwarer                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

unit Parser;

interface

uses Windows, System.SysUtils, Streams, Tokens, Statements, Printers_;

type
  { Синтаксический анализатор }
  TParser = class(TNextStream<TToken, TStatement>)
  strict private
    Settings: TParserSettings;
  strict protected
    function InternalNext: TStatement; override;
  public
    constructor Create(AStream: TBufferedStream<TToken>; ASettings: TParserSettings);
    class function ParseDML(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseCreation(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseOperator(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
    class function ParseAny(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
  end;

implementation

uses DDL, DML, PLSQL, Expressions;

type

  { "Пустое" выражение }

  TEOFStatement = class(TStatement)
  public
    procedure PrintSelf(APrinter: TPrinter); override;
  end;

{ TParser }

constructor TParser.Create(AStream: TBufferedStream<TToken>; ASettings: TParserSettings);
begin
  inherited Create(AStream);
  Settings := ASettings;
end;

class function TParser.ParseDML(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TSelect.Parse(AParent, ASource, AResult) or
            TInsert.Parse(AParent, ASource, AResult) or
            TUpdate.Parse(AParent, ASource, AResult) or
            TDelete.Parse(AParent, ASource, AResult) or
             TMerge.Parse(AParent, ASource, AResult);
end;

class function TParser.ParseCreation(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := TPackage.Parse(AParent, ASource, AResult) or
            TSubroutine.Parse(AParent, ASource, AResult);
end;

class function TParser.ParseOperator(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := ParseDML(AParent, ASource, AResult) or
            TAssignment.Parse(AParent, ASource, AResult) or
            TReturn.Parse(AParent, ASource, AResult) or
            TNull.Parse(AParent, ASource, AResult) or
            TRaise.Parse(AParent, ASource, AResult) or
            TIf.Parse(AParent, ASource, AResult) or
            TProcedureCall.Parse(AParent, ASource, AResult) or
            TAnonymousBlock.Parse(AParent, ASource, AResult);
end;


class function TParser.ParseAny(AParent: TStatement; ASource: TBufferedStream<TToken>; out AResult: TStatement): boolean;
begin
  Result := ParseOperator(AParent, ASource, AResult) or
            ParseCreation(AParent, ASource, AResult) or
            TCreateStatement.Parse(AParent, ASource, AResult) or
            TAnonymousBlock.Parse(AParent, ASource, AResult) or
            TExpression.Parse(AParent, ASource, AResult);
end;

function TParser.InternalNext: TStatement;
begin
  if ParseAny(nil, Source, Result) or
     TUnexpectedToken.Parse(nil, Source, Result)
    then Result.Settings := Self.Settings
    else Result := TEOFStatement.Create(nil, Source);
end;

{ TEOFStatement }

procedure TEOFStatement.PrintSelf(APrinter: TPrinter);
begin
  { ничего не делаем }
end;

end.
