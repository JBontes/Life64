{*******************************************************}
{                                                       }
{            RadStudio Debugger Visualizer Sample       }
{ Copyright(c) 2009-2018 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit SliceVisualizer;

interface

procedure Register;

implementation

uses
  Classes, Forms, SysUtils, ToolsAPI, StrUtils, System.Types;

resourcestring
  sSliceVisualizerName = 'Slice visualizer for Delphi';
  sSliceVisualizerDescription = 'Displays a slice in understandable format';

type
  TDebuggerSliceVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer,
    IOTADebuggerVisualizerValueReplacer, IOTAThreadNotifier, IOTAThreadNotifier160)
  private
    //FNotifierIndex: Integer;
    FCompleted: Boolean;
    FDeferredResult: string;
  public
    { IOTADebuggerVisualizer }
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string; var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    { IOTADebuggerVisualizerValueReplacer }
    function GetReplacementValue(const Expression, TypeName, EvalResult: string): string;
    { IOTAThreadNotifier }
    procedure EvaluteComplete(const ExprStr: string; const ResultStr: string;
      CanModify: Boolean; ResultAddress: Cardinal; ResultSize: Cardinal; ReturnCode: Integer);
    procedure ModifyComplete(const ExprStr: string; const ResultStr: string; ReturnCode: Integer);
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    { IOTAThreadNotifier160 }
    procedure EvaluateComplete(const ExprStr: string; const ResultStr: string;
      CanModify: Boolean; ResultAddress: TOTAAddress; ResultSize: LongWord; ReturnCode: Integer);
  end;

  TTypeLang = (tlDelphi);
  TSliceType = (dttSlice, dttPSlice);

  TSliceVisualizerType = record
    TypeName: string;
    TypeLang: TTypeLang;
    SliceType: TSliceType;
  end;

const
  SliceVisualizerTypes: array[0..1] of TSliceVisualizerType =
  (
    (TypeName: 'TSlice'; TypeLang: tlDelphi; SliceType: dttSlice;),
    (TypeName: 'function: TSlice'; TypeLang: tlDelphi; SliceType: dttSlice;)
  );

{ TDebuggerDateTimeVisualizer }

procedure TDebuggerSliceVisualizer.AfterSave;
begin
  // don't care about this notification
end;

procedure TDebuggerSliceVisualizer.BeforeSave;
begin
  // don't care about this notification
end;

procedure TDebuggerSliceVisualizer.Destroyed;
begin
  // don't care about this notification
end;

procedure TDebuggerSliceVisualizer.Modified;
begin
  // don't care about this notification
end;

procedure TDebuggerSliceVisualizer.ModifyComplete(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin
  // don't care about this notification
end;

procedure TDebuggerSliceVisualizer.EvaluteComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: Cardinal;
  ReturnCode: Integer);
begin
  EvaluateComplete(ExprStr, ResultStr, CanModify, TOTAAddress(ResultAddress),
    LongWord(ResultSize), ReturnCode);
end;

procedure TDebuggerSliceVisualizer.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress: TOTAAddress; ResultSize: LongWord;
  ReturnCode: Integer);
begin
  FCompleted := True;
  if ReturnCode = 0 then
    FDeferredResult := ResultStr;
end;

procedure TDebuggerSliceVisualizer.ThreadNotify(Reason: TOTANotifyReason);
begin
  // don't care about this notification
end;

function PopCount(i: integer): integer;
asm
  Popcnt eax,eax
end;

function TDebuggerSliceVisualizer.GetReplacementValue(const Expression, TypeName, EvalResult: string): string;
var
  Lang: TTypeLang;
  SliceType: TSliceType;
  I: Integer;

  //The string for a slice looks like this:
  //(((-1692815147999455361, (127, 151, 23, 129, 151, 233, 129, 232)), (8530097972531292567, (151, 233, 1, 96, 233, 254, 96, 118)), (8530097972531292567, (151, 233, 1, 96, 233, 254, 96, 118)), (1229614047230457441, (97, 118, 0, 16, 118, 119, 16, 17)), (-78532712897320553, (151, 233, 129, 232, 233, 254, 232, 254)), (8608349206138126057, (233, 254, 96, 118, 254, 255, 118, 119)), (8608349206138126057, (233, 254, 96, 118, 254, 255, 118, 119)), (1229895526519961462, (118, 119, 16, 17, 119, 119, 17, 17))), (16753928925710096255, 8530097972531292567, 8530097972531292567, 1229614047230457441, 18368211360812231063, 8608349206138126057, 8608349206138126057, 1229895526519961462), (2165806975, 3900828055, 1610738071, 1986068201, 1610738071, 1986068201, 268465761, 286291830, 3900828055, 4276682473, 1986068201, 2004287486, 1986068201, 2004287486, 286291830, 286357367), (38783, 33047, 59799, 59521, 59799, 24577, 65257, 30304, 59799, 24577, 65257, 30304, 30305, 4096, 30582, 4368, 59799, 59521, 65257, 65256, 65257, 30304, 65534, 30582, 65257, 30304, 65534, 30582, 30582, 4368, 30583, 4369), (127, 151, 23, 129, 151, 233, 129, 232, 151, 233, 1, 96, 233, 254, 96, 118, 151, 233, 1, 96, 233, 254, 96, 118, 97, 118, 0, 16, 118, 119, 16, 17, 151, 233, 129, 232, 233, 254, 232, 254, 233, 254, 96, 118, 254, 255, 118, 119, 233, 254, 96, 118, 254, 255, 118, 119, 118, 119, 16, 17, 119, 119, 17, 17))

  function FormatResult(const LEvalResult: string; DTType: TSliceType; out ResStr: string): Boolean;
  type
    TInt64 = record
      case boolean of
        true: (Data8: Uint64);
        false: (A: integer; B: integer);
    end;
  var
    Data: TInt64;
    SplitString: TStringDynArray;
    CleanStr: string;
    pcount: integer;
    i: integer;
  begin
    Result := True;
    try
      CleanStr:= LEvalResult;
      CleanStr:= StrUtils.ReplaceStr(CleanStr,'(','');
      CleanStr:= StrUtils.ReplaceStr(CleanStr,')','');
      SplitString:= StrUtils.SplitString(CleanStr,',');
      //Now pick element 0,8,16,...
      pcount:= 0;
      for i:= 0 to 7 do begin
        if not TryStrToInt64(SplitString[i*8], Int64(Data)) then exit(false);
        Inc(pCount, PopCount(Data.A));
        Inc(pCount, PopCount(Data.B));
      end;
      ResStr:= pCount.ToString;
    except
      Result := False;
    end;
  end;

begin
  Lang := TTypeLang(-1);
  SliceType := TSliceType(-1);
  for I := Low(SliceVisualizerTypes) to High(SliceVisualizerTypes) do begin
    if TypeName = SliceVisualizerTypes[I].TypeName then
    begin
      Lang := SliceVisualizerTypes[I].TypeLang;
      SliceType := SliceVisualizerTypes[I].SliceType;
      Break;
    end;
  end;

  if Lang = tlDelphi then begin
    if not FormatResult(EvalResult, SliceType, Result) then Result := EvalResult;
  end;
end;

function TDebuggerSliceVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := Length(SliceVisualizerTypes);
end;

procedure TDebuggerSliceVisualizer.GetSupportedType(Index: Integer; var TypeName: string;
  var AllDescendants: Boolean);
begin
  AllDescendants := False;
  TypeName := SliceVisualizerTypes[Index].TypeName;
end;

function TDebuggerSliceVisualizer.GetVisualizerDescription: string;
begin
  Result := sSliceVisualizerDescription;
end;

function TDebuggerSliceVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TDebuggerSliceVisualizer.GetVisualizerName: string;
begin
  Result := sSliceVisualizerName;
end;

var
  DateTimeVis: IOTADebuggerVisualizer;

procedure Register;
begin
  DateTimeVis := TDebuggerSliceVisualizer.Create;
  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(DateTimeVis);
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(DateTimeVis);
    DateTimeVis := nil;
  end;
end;

initialization
finalization
  RemoveVisualizer;
end.
