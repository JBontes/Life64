unit Unit2;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ComCtrls,
  JvComponentBase, JvAppStorage, JvAppRegistryStorage, System.Win.TaskbarCore,
  Vcl.Taskbar;

const
  cStanding = true;
  cFlat = false;

type
  PSlice = ^TSlice;

  TSliver = record
  public
    class operator BitwiseAnd(const a, b: TSliver): TSliver;
    class operator BitwiseOr(const a, b: TSliver): TSliver;
    class operator Equal(const a, b: TSliver): boolean;
    class operator NotEqual(const a, b: TSliver): boolean;
  private
    case integer of
      8: (Data8: int64);
      4: (Data4: array [0 .. 1] of uint32);
      2: (Data2: array [0 .. 3] of word);
      1: (bytes: array [0 .. 7] of byte);
  end;

  TSliverChanges = record
  private type
    TSliverState = (ssUnchanged, ssChanged, ssInvalid);
  public
    class operator explicit(a: TSliverChanges): boolean; // Changed/unchanged (note that here ssZero = ssChanged)
    class operator Implicit(a: boolean): TSliverChanges;
    class operator LogicalOr(const a: TSliverChanges; const b: boolean): TSliverChanges;
    class operator BitwiseAnd(const a: TSliverChanges; const b: TSliver): TSliverChanges;
    class operator Add(const a, b: TSliverChanges): TSliverChanges;
    function IsValid: boolean;
    function KeepGoing: boolean;
    function Invalid: boolean;
  private
    case integer of
      // 0: (AsState: TSliverState);
      1: (AsBoolean: boolean);
      2: (AsByte: byte);
  end;

  TMaskedBitsIndex = -3..3;

  TMaskedBits = record
  public
    FLength: integer;
    FData: array[0..24] of byte;
    class operator Add(const A,B: TMaskedBits): TMaskedBits;
    function GetItems(index: integer): integer;
    property Length: integer write FLength;
  public
    class operator Implicit(const A: array of byte): TMaskedBits;
    class operator Implicit(const A: TMaskedBits): TArray<byte>;
    class operator Equal(const a, b: TMaskedBits): boolean;
    function FilterKnownBits(KnownMask: integer): integer;
    constructor Create(EW, NS: TMaskedBitsIndex); overload;
    constructor Create(UnknownBits: integer); overload;
    function Count: integer;
    property Items[index: integer]: integer read GetItems; default;
  end;

  TSlice = record
  private
  public
    class operator GreaterThan(const a, b: TSlice): boolean;
    class operator LessThan(const a, b: TSlice): boolean;
    class operator Equal(const a, b: TSlice): boolean;
    class operator NotEqual(const a, b: TSlice): boolean;
    class operator BitwiseAnd(const a, b: TSlice): TSlice;
    class operator BitwiseXor(const a, b: TSlice): TSlice;
    class operator BitwiseOr(const a, b: TSlice): TSlice;
    class operator LogicalNot(const a: TSlice): TSlice;
    class function FullyUnknown: TSlice; static;
    function IsZero: boolean;
    function PopCount: integer;
    function CountDead(Pixel: integer): integer;
    function CountAlive(Pixel: integer): integer;
    function Print: string;
    function IsBitSet(Bit: integer): boolean;
    class function Print5x5(item: integer): string; static;
    class function GetReordering(Order: TArray<integer>; const Input: TArray<integer>): TArray<integer>; static;
    function GetBit(Index: integer): boolean;
    procedure SetBit(Index: integer; value: boolean = true);
    procedure ForceSingleBit(Index: integer);
    procedure ReorderSlice(const Reordering: TArray<integer>);
    function NextSetBit(previous: integer): integer;
    procedure Clear;
  private
    case integer of
      9: (Sliver: array [0 .. 7] of TSliver);
      8: (Data8: array [0 .. 7] of uint64);
      4: (Data4: array [0 .. (64 div 4) - 1] of uint32);
      2: (Data2: array [0 .. (64 div 2) - 1] of word);
      1: (bytes: array [0 .. 63] of byte);
  end;

  TSliverHelper = record helper for TSliver
    function East: TSlice;
    function West: TSlice;
    function North: TSlice;
    function South: TSlice;
    function NorthEast: TSlice;
    function NorthWest: TSlice;
    function SouthWest: TSlice;
    function SouthEast: TSlice;
    class function NS(const North, South: TSlice; var Changed: TSliverChanges): TSliver; static;
    class function EW(const East, West: TSlice; var Changed: TSliverChanges): TSliver; static;
    class function NWSE(const NorthWest, SouthEast: TSlice; var Changed: TSliverChanges): TSliver; static;
    class function NESW(const NorthEast, SouthWest: TSlice; var Changed: TSliverChanges): TSliver; static;
  end;

  TMegaSlice = record
  public
    class function NESW(const NorthEast, SouthWest: TSlice): TMegaSlice; static;
    class function NWSE(const NorthWest, SouthEast: TSlice): TMegaSlice; static;
  public
    case integer of
      64: (Slices: array [0 .. 32] of TSlice);
      8: (Data8: array [0 .. (2048 div 8) - 1] of uint64);
      4: (Ints: array [0 .. (2048 div 4) - 1] of uint32);
      2: (Words: array [0 .. (2048 div 2) - 1] of word);
      1: (bytes: array [0 .. 2047] of byte);
  end;

  TSuperSlice = record
  private
    class var Lookup0: array [byte] of word;
    class var Lookup2: array [byte] of word;
    class var Lookup012: array [byte] of uint64;
    class var LookupRemove0: array [word] of byte;
    class var LookupRemove2: array [word] of byte;
    class constructor init;
  public
    class operator BitwiseOr(const a, b: TSuperSlice): TSuperSlice;
    class operator BitwiseAnd(const a, b: TSuperSlice): TSuperSlice;
    class operator Equal(const a, b: TSuperSlice): boolean;
    class operator NotEqual(const a, b: TSuperSlice): boolean;
    class function NS(const North, South: TSlice): TSuperSlice; static;
    class function EW(const East, West: TSlice): TSuperSlice; static;
    function West: TSlice;
    function East: TSlice;
    function North: TSlice;
    function South: TSlice;
  public
    case integer of
      64: (Slices: array [0 .. 7] of TSlice);
      8: (Data8: array [0 .. (512 div 8) - 1] of uint64);
      4: (Ints: array [0 .. (512 div 4) - 1] of uint32);
      2: (Words: array [0 .. (512 div 2) - 1] of word);
      1: (bytes: array [0 .. 511] of byte);
  end;

  TOffset = type integer;
  TUnknownIndex = -3 .. 3;

const
  oCenter = 0;
  oNorth = 1;
  oEast = 2;
  oSouth = 3;
  oWest = 4;
  oNorthEast = 5;
  oSouthEast = 6;
  oSouthWest = 7;
  oNorthWest = 8;

type
  TOffsetHelper = record helper for TOffset
    function ToMaskedBits: TMaskedBits;
    class function Create(x,y: integer): TOffset; static;
    procedure XY(out E_offset, N_offset: integer);
  end;

type
  TLookupTable = record
  strict private
    procedure SetUnknownItems(E_offset, N_offset: TUnknownIndex; Index: uint32; const value: TSlice);
    function GetUnknownItems(E_offset, N_offset: TUnknownIndex; Index: uint32): TSlice;
    property UnknownItems[E_offset: TUnknownIndex; N_offset: TUnknownIndex; index: uint32]: TSlice read GetUnknownItems
      write SetUnknownItems;
  private
    class var IndexStride: integer;
    class var UnknownOffset: array [TUnknownIndex, TUnknownIndex] of integer;
    class var UnknownSize: uint64;
    class constructor Init;

  private
    FData: TArray<TSlice>;
    FIndex: TArray<integer>;
    FUnknownData: TArray<TSlice>;
    FCountData: TArray<uint64>;
    // FUnknownIndex: TArray<integer>;
    FCornerData: TArray<TSlice>;
    FCornerIndex: TArray<integer>;
    function GetItems(Offset: TOffset; Index: integer): TSlice;
    procedure SetItems(Offset: TOffset; Index: integer; const value: TSlice);
    procedure LoadSliceData(const Filename: string; var SliceData: TArray<TSlice>);
    procedure LoadIndex(const Filename: string; var IndexData: TArray<integer>);
    procedure LoadCountData(const Filename: string);
    procedure SaveSliceData(const Filename: string; const SliceData: TArray<TSlice>);
  public
    procedure LoadMainIndex(const Filename: string);
    procedure LoadMainData(const Filename: string);
    procedure LoadCornerIndex(const Filename: string);
    procedure LoadCornerData(const Filename: string);
    procedure LoadUnknownData(const Filename: string);
    function HasCornerData: boolean;
    function HasSliceData: boolean;
    function HasCountData: boolean;
    function HasUnknownData: boolean;
    property Items[Offset: TOffset; index: integer]: TSlice read GetItems write SetItems; default;
  end;

  //A cake is a 5x5 part of the future grid.
  //It is stored in two parts, 1 part with known pixels
  //and 1 part with unknown pixels.
  TCake = record
  private
    FKnownPart, FUnknownPart: integer;
  public
    constructor Create(Known, Unknown: integer);
    function IsKnown: boolean;
    function ToSlice: TSlice;
    class operator RightShift(const A: TCake; B: cardinal): TCake;
    class operator LeftShift(const A: TCake; B: cardinal): TCake;
    procedure SetKnown;
    procedure SetUnknown;
    function OddKnown: boolean;
    function OddUnknown: boolean;
    property Known: integer read FKnownPart;
    property Unknown: integer read FUnknownPart;
  end;

type
  TForm2 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    BtnProcess_7x7_CountLookup: TButton;
    StringGrid1: TStringGrid;
    BtnAppyLookupTable: TButton;
    StringGrid2: TStringGrid;
    BtnSolveRoundUsingChunks: TButton;
    BtnClearGrid: TButton;
    BtnTestLookup0_012: TButton;
    StringGrid3: TStringGrid;
    FileOpenDialog1: TFileOpenDialog;
    BtnMinimalSolve: TButton;
    TabSheet2: TTabSheet;
    SGSliceLayout: TStringGrid;
    Button8: TButton;
    BtnSolveCounter: TButton;
    BtnRotateCounter: TButton;
    SGDead: TStringGrid;
    SGAlive: TStringGrid;
    Label6: TLabel;
    Label7: TLabel;
    SGAliveDiff: TStringGrid;
    SGDeadDiff: TStringGrid;
    Label8: TLabel;
    Label9: TLabel;
    BtnSliverSolveRound: TButton;
    BtnInitWith_GoE: TButton;
    TabSheet3: TTabSheet;
    StringGrid4: TStringGrid;
    BtnLoadSmallLookups: TButton;
    BtnSolveWithChunkLookup: TButton;
    TabSheet4: TTabSheet;
    BtnValidateN1E1LookupTable: TButton;
    BtnValidateCountTable: TButton;
    SGMinOn: TStringGrid;
    SGMinOff: TStringGrid;
    BtnApplyNELookupTables: TButton;
    BtnApplyCornerLookupTables: TButton;
    StringGrid5: TStringGrid;
    BtnCreateUnknownLookupTable: TButton;
    FileSaveDialog1: TFileSaveDialog;
    BtnCreateLookupUsingSolver: TButton;
    Memo2: TMemo;
    BtnTest_TSliceNextSetBit: TButton;
    BtnTestCalcSouth: TButton;
    BtnTestDeleteBit: TButton;
    TabSheet5: TTabSheet;
    Memo3: TMemo;
    BtnStartUnitTests: TButton;
    BtnDoFailingTests: TButton;
    AppRegistry: TJvAppRegistryStorage;
    Taskbar1: TTaskbar;
    procedure BtnCreateLookupUsingSolverClick(Sender: TObject);
    procedure BtnRotateCounterClick(Sender: TObject);
    procedure BtnSliverSolveRoundClick(Sender: TObject);
    procedure BtnInitWith_GoEClick(Sender: TObject);
    procedure BtnLoadSmallLookupsClick(Sender: TObject);
    procedure BtnTest_TSliceNextSetBitClick(Sender: TObject);
    procedure BtnCreateUnknownLookupTableClick(Sender: TObject);
    procedure BtnDoFailingTestsClick(Sender: TObject);
    procedure BtnTestCalcSouthClick(Sender: TObject);
    procedure BtnTestDeleteBitClick(Sender: TObject);
    procedure BtnStartUnitTestsClick(Sender: TObject);
    procedure BtnSolveWithChunkLookupClick(Sender: TObject);
    procedure BtnValidateN1E1LookupTableClick(Sender: TObject);
    procedure BtnValidateCountTableClick(Sender: TObject);
    procedure BtnApplyNELookupTablesClick(Sender: TObject);
    procedure BtnApplyCornerLookupTablesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnProcess_7x7_CountLookupClick(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure BtnAppyLookupTableClick(Sender: TObject);
    procedure BtnSolveRoundUsingChunksClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnClearGridClick(Sender: TObject);
    procedure BtnTestLookup0_012Click(Sender: TObject);
    procedure BtnMinimalSolveClick(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure BtnSolveCounterClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure StringGrid2Click(Sender: TObject);
    procedure StringGrid2DrawCell(Sender: TObject; ACol, ARow: integer; Rect: TRect; State: TGridDrawState);
    procedure StringGrid3Click(Sender: TObject);
    procedure StringGrid4DrawCell(Sender: TObject; ACol, ARow: integer; Rect: TRect; State: TGridDrawState);
    procedure StringGrid5DblClick(Sender: TObject);
  private
    Buffer: TArray<TSlice>;
    ChunkLookup: array [boolean] of TArray<TSuperSlice>;
    NewLayout: TArray<integer>;
    procedure LoadLookupTable(var LookupTable: TArray<TSlice>);
    procedure InitNewLayout;
    procedure ShowNewLayout;
    function GetCounterLayout: TArray<integer>;
    procedure InitWithGoE;
    function GetStates(const Input: TSlice): TArray<integer>;
    procedure Display5x5(Cake: TCake; const SG: TStringGrid);
    function GetPixelChar(x, y: integer; SG: TStringGrid): Char;
    class procedure LeadingTrailingPopCount(UnknownMask: integer; out Leading,
      Trailing, Popcnt: integer); static;
    function UnknownSlice(Cake: TCake): TSlice;
    class function GetNextBitSet(previous, i: integer): integer; static;
  public
    LookupTable: TLookupTable;
    function FoldRemaining(Offset: TOffset; UnknownMask, KnownMask: integer; Filter: boolean = false): TSlice;
  end;

var
  Form2: TForm2;

const
  N1 = 1;
  N2 = 2;
  N3 = 3;
  S1 = -1;
  S2 = -2;
  S3 = -3;
  E1 = 1;
  E2 = 2;
  E3 = 3;
  W1 = -1;
  W2 = -2;
  W3 = -3;

function PopCount(Input: int64): integer;


implementation

uses
  Generics.Collections,
  Generics.Defaults,
  System.UITypes,
  System.Diagnostics,
  TestInsight.Client, TestInsight.DUnitX, UnitTests;

{$R *.dfm}
{$POINTERMATH on}

{ TODO -oOwner -cGeneral : Fix me }
const
  OffMask: array [0 .. 8] of TSlice = ((Data8: ($5555555555555555, $5555555555555555, $5555555555555555,
    $5555555555555555, $5555555555555555, $5555555555555555, $5555555555555555, $5555555555555555)),
    (Data8: ($3333333333333333, $3333333333333333, $3333333333333333, $3333333333333333, $3333333333333333,
    $3333333333333333, $3333333333333333, $3333333333333333)),
    (Data8: ($0F0F0F0F0F0F0F0F, $0F0F0F0F0F0F0F0F, $0F0F0F0F0F0F0F0F, $0F0F0F0F0F0F0F0F, $0F0F0F0F0F0F0F0F,
    $0F0F0F0F0F0F0F0F, $0F0F0F0F0F0F0F0F, $0F0F0F0F0F0F0F0F)),
    (Data8: ($00FF00FF00FF00FF, $00FF00FF00FF00FF, $00FF00FF00FF00FF, $00FF00FF00FF00FF, $00FF00FF00FF00FF,
    $00FF00FF00FF00FF, $00FF00FF00FF00FF, $00FF00FF00FF00FF)),
    (Data8: ($0000FFFF0000FFFF, $0000FFFF0000FFFF, $0000FFFF0000FFFF, $0000FFFF0000FFFF, $0000FFFF0000FFFF,
    $0000FFFF0000FFFF, $0000FFFF0000FFFF, $0000FFFF0000FFFF)),
    (Data8: ($00000000FFFFFFFF, $00000000FFFFFFFF, $00000000FFFFFFFF, $00000000FFFFFFFF, $00000000FFFFFFFF,
    $00000000FFFFFFFF, $00000000FFFFFFFF, $00000000FFFFFFFF)),
    (Data8: ($FFFFFFFFFFFFFFFF, $0000000000000000, $FFFFFFFFFFFFFFFF, $0000000000000000, $FFFFFFFFFFFFFFFF,
    $0000000000000000, $FFFFFFFFFFFFFFFF, $0000000000000000)),
    (Data8: ($FFFFFFFFFFFFFFFF, $FFFFFFFFFFFFFFFF, $0000000000000000, $0000000000000000, $FFFFFFFFFFFFFFFF,
    $FFFFFFFFFFFFFFFF, $0000000000000000, $0000000000000000)),
    (Data8: ($FFFFFFFFFFFFFFFF, $FFFFFFFFFFFFFFFF, $FFFFFFFFFFFFFFFF, $FFFFFFFFFFFFFFFF, $0000000000000000,
    $0000000000000000, $0000000000000000, $0000000000000000)));


const
   cUnknownSliceData = 'Filename_UnknownSlice';
   cMainSliceData = 'Filename_MainSlice';
   cCornerSliceData = 'Filename_CornerSlice';
   cAncestorCounts = 'Filename_AncestorCount';
   cMainIndex = 'Filename_MainIndex';
   cCornerIndex = 'Filename_CornerIndex';
   cCountData = 'Filename_CountData';

type
  TSlices11 = array [0 .. 15, 0 .. 15] of TSlice;

var
  MySlices: TSlices11;
  CloneSlices: TSlices11;
  OldSlices: TSlices11;

type
  TSliceComparer = class(TInterfacedObject, IComparer<TSlice>)
  class var
    FComparer: TSliceComparer;

    function Compare(const Left, Right: TSlice): integer;
  private
    class function Comparer: IComparer<TSlice>; static;
  end;

  TRotation = (rNone, rCounter, rClock, r180);

function DeleteBit(input, BitToDelete: integer): integer;
var
  before, after: integer;
begin
  before:= input and not(-1 shl BitToDelete);
  after:= (input shr 1) and (-1 shl BitToDelete);
  Result:= before or after;
end;


function DeleteBits(input: integer; BitsToDelete: TMaskedBits): integer;
var
  i: integer;
begin
  //Always remove higher bits first to make sure stuff does not move around on us.
  for i:= BitsToDelete.Count-1 downto 0 do begin
    input:= DeleteBit(input, BitsToDelete.FData[i]);
  end; {for i}
  Result:= input;
end;

procedure TOffsetHelper.XY(out E_offset, N_offset: integer);
var
  UnknownOffset: integer;
begin
  UnknownOffset:= ABS(Ord(Self));
  Dec(UnknownOffset);
  N_offset:= (UnknownOffset mod 7) - 3;
  E_offset:= (UnknownOffset div 7) - 3;
end;

function InsertZeroBit(value: integer; pos: integer): integer;
var
  x, y: integer;
begin
  // Split the input into two parts, one shifted and one not
  y:= value;
  x:= value shl 1;
  // mask out a single bit in the shifted part (insert a '0')
  x:= x and (not(1 shl pos));
  // keep the top bits of x
  x:= x and (-1 shl pos);
  // keep the bottom bits of y
  y:= y and not(-1 shl pos);
  // combine the two parts.
  Result:= x or y;
end;

function Remove012(const Data8: uint64): byte;
type
  TMyBytes = array [0 .. 7] of byte;
begin
  Result:= 0;
  if (TMyBytes(Data8)[0] <> 0) then Result:= 1;
  if (TMyBytes(Data8)[1] <> 0) then Inc(Result, 2);
  if (TMyBytes(Data8)[2] <> 0) then Inc(Result, 4);
  if (TMyBytes(Data8)[3] <> 0) then Inc(Result, 8);
  if (TMyBytes(Data8)[4] <> 0) then Inc(Result, 16);
  if (TMyBytes(Data8)[5] <> 0) then Inc(Result, 32);
  if (TMyBytes(Data8)[6] <> 0) then Inc(Result, 64);
  if (TMyBytes(Data8)[7] <> 0) then Inc(Result, 128);
end;

class function TSliceComparer.Comparer: IComparer<TSlice>;
begin
  if FComparer = nil then FComparer:= TSliceComparer.Create;
  Result:= IComparer<TSlice>(FComparer);
end;

type
  TFileStream = class(System.Classes.TFileStream)
    function Read64(Buffer: TBytes; Offset, Count: int64): int64;
  end;

function TFileStream.Read64(Buffer: TBytes; Offset, Count: int64): int64;
const
  BUCKETSIZE = $1000000; // 16M
var
  Step: integer;
begin
  Step:= Count div Bucketsize;
  Form2.TaskBar1.ProgressMaxValue:= Step;
  Form2.TaskBar1.ProgressValue:= 0;
  Form2.TaskBar1.ProgressState:= TTaskBarProgressState.Normal;
  Step:= 0;
  try
    Result:= 0;
    while Count >= BUCKETSIZE do begin
      Result:= Result + read(Buffer[Offset], BUCKETSIZE);
      Inc(Offset, BUCKETSIZE);
      Dec(Count, BUCKETSIZE);
      Inc(Step);
      Form2.TaskBar1.ProgressValue:= Step;
    end;
    if Count > 0 then Result:= Result + read(Buffer[Offset], Count);
    Form2.TaskBar1.ProgressValue:= Form2.TaskBar1.ProgressMaxValue;
  finally
    Form2.TaskBar1.ProgressState:= TTaskBarProgressState.None;
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  FS: TFileStream;
  i, j: integer;
  UniqueCount: integer;
  m, Min, Max, Onn, Off: integer;
  MinIndex, MaxIndex: integer;
  MinCount, MaxCount: integer;
  MinBuffer: TArray<integer>;
  Total0, Total1: int64;
  OnCount, OffCount: array [0 .. 8] of integer;
begin
  FillChar(OffCount, SizeOf(OffCount), 127);
  FillChar(OnCount, SizeOf(OnCount), 127);

  if FileOpenDialog1.Execute then begin
    FS:= TFileStream.Create(FileOpenDialog1.Filename, fmOpenRead);
    SetLength(Buffer, FS.Size div SizeOf(TSlice));
    FS.Read64(TBytes(Buffer), 0, FS.Size);
    FS.Free;
    Min:= Buffer[0].PopCount;
    Max:= Min;
    MinIndex:= 0;
    MaxIndex:= MinIndex;
    MinCount:= 0;
    MaxCount:= 0;
    for i:= 1 to high(Buffer) do begin
      m:= Buffer[i].PopCount;
      for j:= 0 to 8 do begin
        Off:= (Buffer[i] and OffMask[j]).PopCount;
        Onn:= m - Off;
        if (Off < OffCount[j]) then OffCount[j]:= Off;
        if (Onn < OnCount[j]) then OnCount[j]:= Onn;
      end;
      if m = 1 then begin
        Inc(MinCount);
        SetLength(MinBuffer, Length(MinBuffer) + 1);
        MinBuffer[high(MinBuffer)]:= i;
      end;
      if m < Min then begin
        Min:= m;
        MinIndex:= i;
      end;
      if m = Max then Inc(MaxCount);
      if m > Max then begin
        Max:= m;
        MaxIndex:= i;
      end;
      if Buffer[i] = Buffer[i + 1] then continue;
      Inc(UniqueCount);
    end;

    Total0:= 0;
    Total1:= 0;
    for i:= 0 to high(Buffer) do begin
      if (i and (1 shl 12)) = 0 then begin
        Inc(Total0, Buffer[i].PopCount)
      end else begin
        Inc(Total1, Buffer[i].PopCount);
      end;
    end;
    Label2.Caption:= 'Total0 = ' + Total0.ToString + ' Total1 = ' + Total1.ToString;
    Total0:= Total0 div (Length(Buffer) div 2);
    Total1:= Total1 div (Length(Buffer) div 2);
    Label1.Caption:= '0: average passes: ' + Total0.ToString + ' holes: ' + (512 - Total0).ToString +
      ' 1: average passes: ' + Total1.ToString + ' holes: ' + (512 - Total1).ToString;
    TArray.Sort<TSlice>(Buffer, TSliceComparer.Comparer);
    Button1.Caption:= 'Sort done';
    UniqueCount:= 1;
    for i:= 0 to high(Buffer) - 1 do begin
      if Buffer[i] = Buffer[i + 1] then continue;
      Inc(UniqueCount);
    end;
    FS:= TFileStream.Create(FileOpenDialog1.Filename, fmOpenRead);
    SetLength(Buffer, FS.Size div SizeOf(TSlice));
    FS.Read64(TBytes(Buffer), 0, FS.Size);
    FS.Free;
    Form2.Caption:= 'Count done: ' + UniqueCount.ToString + ' unique items,' + ' Min= ' + Min.ToString + ':' +
      MinIndex.ToString + ':' + MinCount.ToString + ' Max= ' + Max.ToString + ':' + MaxIndex.ToString + ':' +
      MaxCount.ToString;
    for i:= 0 to high(MinBuffer) do begin
      Form2.Memo1.Lines.Add(TSlice.Print5x5(MinBuffer[i]) + ' ' + Buffer[MinBuffer[i]].Print);
    end;
    for j:= 0 to 8 do begin
      Form2.Memo1.Lines.Add(format('Min off count %d = %d', [j, OffCount[j]]));
    end;
    Form2.Memo1.Lines.Add(' ');
    for j:= 0 to 8 do begin
      Form2.Memo1.Lines.Add(format('Min on count %d = %d', [j, OnCount[j]]));
    end;
  end;
end;

function TSlice.GetBit(Index: integer): boolean;
var
  Mask: uint64;
  element: integer;
begin
  Mask:= (1 shl (index and 63));
  element:= index div 64;
  Result:= (Data8[element] and Mask) <> 0;
end;

class function TSlice.GetReordering(Order: TArray<integer>; const Input: TArray<integer>): TArray<integer>;
var
  i, j, k, Mask: integer;
begin
  System.Assert(Length(Order) = 9);
  SetLength(Result, Length(Input));
  for i:= 1 to 510 do begin // The first and the last element never reorder in a transpose
    Result[i]:= 0;
    for j:= 0 to 8 do begin
      k:= 1 shl Order[j];
      Mask:= 1 shl j;
      if (Input[i] and Mask) <> 0 then begin
        Result[i]:= Result[i] + k;
      end;
    end; { for j }
  end; { for i }
end;

procedure TForm2.BtnProcess_7x7_CountLookupClick(Sender: TObject);
var
  FS: TFileStream;
  Counts: TArray<int64>;
  i: integer;
  Min, Max, Avg: int64;
  MinIndex, MaxIndex: integer;
begin
  if not FileOpenDialog1.Execute then Exit;
  FS:= TFileStream.Create(FileOpenDialog1.Filename, fmOpenRead);
  SetLength(Counts, FS.Size div SizeOf(int64));
  FS.Read64(TBytes(Counts), 0, FS.Size);
  FS.Free;
  Min:= Counts[0];
  Max:= Min;
  MinIndex:= 0;
  MaxIndex:= 0;
  Avg:= 0;
  for i:= 0 to high(Counts) do begin
    Inc(Avg, Counts[i]);
    if Counts[i] < Min then begin
      MinIndex:= i;
      Min:= Counts[i];
    end;
    if Counts[i] > Max then begin
      Max:= Counts[i];
      MaxIndex:= i;
    end;
  end;
  Avg:= Avg div Length(Counts);
  Label3.Caption:= 'Min= ' + Min.ToString + ' ' + 'index= ' + MinIndex.ToString + ' ' + 'Max= ' + Max.ToString + ' ' +
    'index= ' + MaxIndex.ToString + ' ' + 'Avg= ' + Avg.ToString;
end;

{ TODO -oJB -cRefactor : If we allow '?' then we can have more than one future cakes }
function GetFutureCake(const SG: TStringGrid; col, row: integer): TCake;
var
  x, y: integer;
begin
  Result:= TCake.Create(0,0);
  for y:= 0 to 4 do begin
    for x:= 4 downto 0 do begin
      Result:= Result shl 1;
      if ((col + x) < SG.ColCount) and ((row + y) < SG.RowCount) and ((col + x) >= 0) and ((row + y) >= 0) then begin
        if (SG.Cells[col + x, row + y] = 'X') then Result.SetKnown
        else if (SG.Cells[col + x, row + y] = '?') then Result.SetUnknown;
      end;
    end; {for x}
  end; {for y}
end;

function GetFutureStandingChunk(const SG: TStringGrid; col, row: integer): integer;
var
  x, y: integer;
begin
  Result:= 0;
  for y:= 0 to 3 do begin
    for x:= 2 downto 0 do begin
      Result:= Result shl 1;
      if (SG.Cells[col + x, row + y] = 'X') then begin
        Inc(Result);
      end;
    end; { for x }
  end; { for y }
end;

function GetFutureFlatChunk(const SG: TStringGrid; col, row: integer): integer;
var
  x, y: integer;
begin
  Result:= 0;
  for y:= 0 to 2 do begin
    for x:= 3 downto 0 do begin
      Result:= Result shl 1;
      if (SG.Cells[col + x, row + y] = 'X') then begin
        Inc(Result);
      end;
    end; { for x }
  end; { for y }
end;

function FutureGridToPastSlice(const SG: TStringGrid; col, row: integer; const LookupTable: TLookupTable; KnownOffset: TOffset): TSlice;
var
  Cake: TCake;
begin
  Cake:= GetFutureCake(SG, col, row);
  if (Cake.IsKnown) then begin
    Result:= LookupTable[KnownOffset, Cake.Known];
  end else begin
    Result:= Form2.UnknownSlice(Cake);
  end;
end;

function FutureGridToPastSliceSimple(const SG: TStringGrid; col, row: integer; const LookupTable: TLookupTable): TSlice;
var
  ZeroSlice, OneSlice: TSlice;
begin
  ZeroSlice:= LookupTable[oCenter, 0];
  OneSlice:= not(ZeroSlice);
  if (SG.Cells[col + 2, row + 2] = 'X') then Result:= OneSlice
  else Result:= ZeroSlice;
end;

procedure DisplaySlices(SG, SGDiff: TStringGrid; Slices: TSlices11; ForceRefresh: boolean = false);
var
  x, y: integer;
  Slice: TSlice;
  Old: integer;
begin
  for x:= 0 to SG.ColCount - 1 do begin
    for y:= 0 to SG.RowCount - 1 do begin
      Slice:= Slices[x, y];
      if (SG.Cells[x, y] <> '') then Old:= SG.Cells[x, y].ToInteger
      else begin
        case Form2.GetPixelChar(x + 2, y + 2, Form2.StringGrid1) of
          'X': Old:= 140;
          '?': Old:= 512;
        else Old:= 140;
        end;
      end;
      SG.Cells[x, y]:= Slice.PopCount.ToString;
      SGDiff.Cells[x, y]:= (Old - Slice.PopCount).ToString;
    end; { for y }
  end; { for x }
  if (ForceRefresh) then begin
    SG.Refresh;
    SGDiff.Refresh;
  end;
end;

function PopCount(Input: int64): integer;
asm
  popcnt rax,rcx
end;

procedure DoARun(Rotation: TRotation = rNone);
var
  SS: TSuperSlice;
  Slices: TSlices11;

  procedure Solve(x, y: integer);
  begin
    if x < (17 - 2) then begin
      SS:= TSuperSlice.EW(Slices[x + 1, y], Slices[x, y]);
      Slices[x, y]:= Slices[x, y] and SS.West;
      Slices[x + 1, y]:= Slices[x + 1, y] and SS.East;
    end; { handle EW }
    if y < (17 - 2) then begin
      SS:= TSuperSlice.NS(Slices[x, y], Slices[x, y + 1]);
      Slices[x, y]:= Slices[x, y] and SS.North;
      Slices[x, y + 1]:= Slices[x, y + 1] and SS.South;
    end; { handle NS }
  end;

  procedure SolveCounterClock(x, y: integer);
  // N -> W
  // S -> E
  // E -> N
  // W -> S
  begin
    // Every slice is rotated counter clockwise
    // Thus
    // N -> W
    // E -> N
    // W -> S
    // S -> E

    if x < (17 - 2) then begin
      // Let's first compare E-W, N was E, S was W.
      // SS:= TSuperSlice.WE({W}Slices[x, y], Slices[{E}x+1, y]);
      SS:= TSuperSlice.NS(Slices[ { E } x + 1, y], Slices[ { W } x, y]);
      // Slices[x, y]:= Slices[x, y] and SS.West;
      Slices[x, y]:= Slices[x, y] and SS.South;
      // Slices[x + 1, y]:= Slices[x + 1, y] and SS.East;
      Slices[x + 1, y]:= Slices[x + 1, y] and SS.North;
    end; { handle EW }
    if y < (17 - 2) then begin
      // Next up is NS: W was N, E was S
      // SS:= TSuperSlice.NS({N}Slices[x, y], {S}Slices[x, y + 1]);
      SS:= TSuperSlice.EW(Slices[x, y + 1], Slices[x, y]);
      Slices[x, y]:= Slices[x, y] and SS.West;
      Slices[x, y + 1]:= Slices[x, y + 1] and SS.East;
    end; { handle NS }
  end;

var
  x, y: integer;
begin
  OldSlices:= MySlices; // Save the previous state for comparison.
  Slices:= MySlices;
  for x:= 0 to 17 - 2 do begin
    for y:= 0 to 17 - 2 do begin
      case Rotation of
        rNone: Solve(x, y);
        rCounter: SolveCounterClock(x, y);
        rClock: System.Assert(false);
        r180: System.Assert(false);
      end;
    end; { for y }
  end; { for x }
  MySlices:= Slices;
end;

// Solve the given sliver with its neighbors to the East and South
// Returns true if there was a change.
function SliverSolve(Slices: PSlice; x, y: integer; MaxX, MaxY: integer): TSliverChanges;
var
  Sliver: TSliver;
  YSize: integer;
  IsChanged: TSliverChanges;
begin
  IsChanged:= false;
  YSize:= MaxY + 1;
  // EW
  // if x < (MaxX) then begin
  // Sliver:= TSliver.EW(Slices[x+1 + y*XSize],Slices[x+ y*XSize]);
  // Slices[x+ y*XSize]:= Slices[x+ y*XSize] and Sliver.West;
  // Slices[x + 1+ y*XSize]:= Slices[x + 1+ y*XSize] and Sliver.East;
  // end;
  // //NS
  // if y < (MaxY) then begin
  // Sliver:= TSliver.NS(Slices[x+ y*XSize], Slices[x+(y + 1)*XSize]);
  // Slices[x+ y*XSize]:= Slices[x+ y*XSize] and Sliver.North;
  // Slices[x+(y + 1)*XSize]:= Slices[x+ (y + 1)*XSize] and Sliver.South;
  // end; {handle NS}
  if x < (MaxX) then begin
    Sliver:= TSliver.EW(Slices[((x + 1) * YSize) + y], Slices[(x * YSize) + y], IsChanged);
    Slices[(x * YSize) + y]:= Slices[(x * YSize) + y] and Sliver.West;
    Slices[((x + 1) * YSize) + y]:= Slices[((x + 1) * YSize) + y] and Sliver.East;
  end;
  // NS
  if y < (MaxY) then begin
    Sliver:= TSliver.NS(Slices[(x * YSize) + y], Slices[(x * YSize) + (y + 1)], IsChanged);
    Slices[(x * YSize) + y]:= Slices[(x * YSize) + y] and Sliver.North;
    Slices[(x * YSize) + (y + 1)]:= Slices[(x * YSize) + (y + 1)] and Sliver.South;
  end; { handle NS }
  // //First do NESW
  // if (x < (MaxX)) and (y < (MaxY)) then begin
  // Sliver:= TSliver.NESW(Slices[x+1,y], Slices[x,y+1]);
  // Slices[x+1,y]:= Slices[x+1,y] and Sliver.NorthEast;
  // Slices[x,y+1]:= Slices[x,y+1] and Sliver.SouthWest;
  // end;
  // //Then NWSE
  // if (x < (MaxX)) and (y < (MaxY)) then begin
  // Sliver:= TSliver.NWSE(Slices[x,y], Slices[x+1,y+1]);
  // Slices[x,y]:= Slices[x,y] and Sliver.NorthWest;
  // Slices[x+1,y+1]:= Slices[x+1,y+1] and Sliver.SouthEast;
  // end;
  Result:= IsChanged;
end;

procedure DoASliverRun(Rotation: TRotation = rNone);
var
  Slices: TSlices11;
var
  x, y: integer;
begin
  OldSlices:= MySlices; // Save the previous state for comparison.
  Slices:= MySlices;
  for x:= 0 to 15 do begin
    for y:= 0 to 15 do begin
      case Rotation of
        rNone: SliverSolve(PSlice(@Slices), x, y, 15, 15);
        rCounter: System.Assert(false);
        rClock: System.Assert(false);
        r180: System.Assert(false);
      end;
    end; { for y }
  end; { for x }
  // for x:= 17-2 downto 0 do begin
  // for y:= 17-2 downto 0 do begin
  // case Rotation of
  // rNone: SliverSolve(x,y);
  // rCounter: System.Assert(false);
  // rClock: System.Assert(false);
  // r180: System.Assert(false);
  // end;
  // end; {for y}
  // end; {for x}
  MySlices:= Slices;
end;

procedure TForm2.BtnCreateLookupUsingSolverClick(Sender: TObject);
const
  MaxXY = 4;
  Middle = MaxXY div 2;
  ItemsToTestCount = 32 * 1024 * 1024;
type
  TSlices5 = array [0 .. MaxXY, 0 .. MaxXY] of TSlice;
var
  i: integer;
  Grid: TSlices5;
  OldGrid: TSlices5;
  Five: integer;
  x, y: integer;
  ZeroSlice: TSlice;
  OneSlice: TSlice;

  function SolveWithSliversDown(var Slices: TSlices5): TSliverChanges;
  var
    x, y: integer;
  begin
    Result:= false;
    for x:= 0 to MaxXY do
      for y:= 0 to MaxXY do begin
        Result:= Result + SliverSolve(PSlice(@Slices), x, y, MaxXY, MaxXY);
        if not(Result.IsValid) then Exit;
      end; { for xy }
  end;

  function SolveWithSliversUp(var Slices: TSlices5): TSliverChanges;
  var
    x, y: integer;
  begin
    Result:= false;
    for x:= MaxXY downto 0 do
      for y:= MaxXY downto 0 do begin
        Result:= Result + SliverSolve(PSlice(@Slices), x, y, MaxXY, MaxXY);
        if not(Result.IsValid) then Exit;
      end; { for xy }
  end;

var
  TotalDiff, TotalCount: uint64;
  Changed: boolean;
  a: integer;
  Status: TSliverChanges;
  ChangeCount: integer;
  Soll, Ist: TSlice;
  Oops: integer;
  StopWatch: TStopWatch;
  HoleCount: integer;
  NewLookup: TArray<TSlice>;
  FS: TFileStream;
begin
  if not(FileSaveDialog1.Execute) then Exit;
  SetLength(NewLookup, ItemsToTestCount);
  TotalDiff:= 0;
  TotalCount:= 0;
  StopWatch:= TStopWatch.StartNew;
  ZeroSlice:= LookupTable[oCenter, 0];
  OneSlice:= not(ZeroSlice);
  for i:= 0 to ItemsToTestCount - 1 do begin
    // Set up the slice
    // FillChar(Grid, SizeOf(Grid), $FF); //Borders are unknown
    for x:= 0 to MaxXY do
      for y:= 0 to MaxXY do begin
        Grid[x, y]:= ZeroSlice;
      end;
    Five:= i;
    for y:= 0 to 4 do begin
      for x:= 4 downto 0 do begin
        if Odd(Five) then Grid[x, y]:= OneSlice; // else Grid[x+1,y+1]:= ZeroSlice;
        Five:= Five shr 1;
      end; { for x }
    end; { for y }
    repeat // Keep pruning until there is nothing left to prune
      ChangeCount:= 0;
      repeat
        Changed:= boolean(SolveWithSliversDown(Grid));
        Inc(ChangeCount, integer(Changed));
        if (Changed) then Changed:= boolean(SolveWithSliversUp(Grid));
      until not(Changed);
      // Now solve for every allowed state in the center slice
      OldGrid:= Grid;
      for a:= 0 to 511 do begin
        if (Grid[Middle, Middle].IsBitSet(a)) then begin
          // Reset the grid back to the start
          Grid:= OldGrid;
          // Force the grid to the state.
          Grid[Middle, Middle].ForceSingleBit(a);
          repeat
            Status:= SolveWithSliversDown(Grid);
            if (Status.KeepGoing) then Status:= SolveWithSliversUp(Grid);
          until not(Status.KeepGoing);
          if (Status.Invalid) then OldGrid[Middle, Middle].SetBit(a, false);
        end else begin { skip disabled constellations } end;
      end; { for a }
      Grid:= OldGrid; // Update the grid with the changes.
    until ChangeCount = 0;
    // After this is done Grid == OldGrid, because it only exits if there are no changes.
    // Compare the result against the lookup table
    Soll:= LookupTable[oCenter, i];
    Ist:= Grid[Middle, Middle];
    if (Ist.PopCount <> 372) then begin
      // ShowMessage('Ist = '+Ist.PopCount.ToString);
    end;
    if (Soll and not(ZeroSlice)).PopCount = 0 then HoleCount:= 372 - Soll.PopCount
    else HoleCount:= 140 - Soll.PopCount;
    Inc(TotalDiff, (Soll xor Ist).PopCount);
    Inc(TotalCount, HoleCount);

    Oops:= (Soll and not(Ist)).PopCount;

    // if (Ist <> Soll) then begin
    if (Oops <> 0) then begin
      Memo2.Lines.Add(i.ToString + ' does not match lookup, soll = ' + Soll.PopCount.ToString + ' ist = ' +
        Ist.PopCount.ToString + ' Diff = ' + (Soll xor Ist).PopCount.ToString + ' Oops = ' + (Soll and not(Ist))
        .PopCount.ToString);
      // exit;
    end;
    NewLookup[i]:= Ist;
    if ((i + 1) mod (1024 * 1024)) = 0 then Memo2.Lines.Add((i div (1024 * 1024)).ToString + 'of 32');
  end; { for i }
  StopWatch.Stop;
  Memo2.Lines.Add(StopWatch.Elapsed.ToString + ' HiRes = ' + StopWatch.IsHighResolution.ToString);
  Memo2.Lines.Add('Soll = ' + TotalCount.ToString + ' Diff = ' + TotalDiff.ToString + ' Avg = ' +
    (TotalDiff / ItemsToTestCount).ToString + ' Err = ' + (((TotalDiff * 10000) div TotalCount) / 100).ToString + '%');

  FS:= TFileStream.Create(FileSaveDialog1.Filename, fmCreate);
  FS.Write(TBytes(NewLookup), Length(NewLookup) * SizeOf(TSlice));
  FS.Free;
end;

procedure TForm2.BtnRotateCounterClick(Sender: TObject);
var
  Slice: TSlice;
begin
  GetCounterLayout;
  for Slice in MySlices do begin
    Slice.ReorderSlice(NewLayout);
  end;
end;

procedure TForm2.BtnSliverSolveRoundClick(Sender: TObject);
const
  ForceRefresh = true;
begin
  DoASliverRun;
  DisplaySlices(StringGrid2, StringGrid3, MySlices, ForceRefresh);
end;

procedure TForm2.BtnInitWith_GoEClick(Sender: TObject);
begin
  InitWithGoE;
end;

procedure TForm2.BtnLoadSmallLookupsClick(Sender: TObject);
var
  FS: TFileStream;
  OldTitle: string;
begin
  OldTitle:= FileOpenDialog1.Title;
  FileOpenDialog1.Title:= 'Standing lookup';
  if not(FileOpenDialog1.Execute) then Exit;
  FS:= TFileStream.Create(FileOpenDialog1.Filename, fmOpenRead);
  SetLength(ChunkLookup[cStanding], FS.Size div SizeOf(TSuperSlice));
  FS.Read64(TBytes(ChunkLookup[cStanding]), 0, FS.Size);
  FS.Free;

  FileOpenDialog1.Title:= 'Flat lookup';
  if not(FileOpenDialog1.Execute) then Exit;
  FS:= TFileStream.Create(FileOpenDialog1.Filename, fmOpenRead);
  SetLength(ChunkLookup[cFlat], FS.Size div SizeOf(TSuperSlice));
  FS.Read64(TBytes(ChunkLookup[cFlat]), 0, FS.Size);
  FS.Free;

  FileOpenDialog1.Title:= OldTitle;
end;

procedure TForm2.BtnTest_TSliceNextSetBitClick(Sender: TObject);
var
  S: TSlice;
  i: integer;
begin
  for i:= 1 to 511 do begin
    S:= TSlice.FullyUnknown;
    // S.NextSetBit(8);
    // S.SetBit(i);
    if (S.NextSetBit(i) <> (i + 1)) then begin
      System.Assert(S.NextSetBit(i) = (i + 1));
    end;
  end;
end;

procedure TForm2.BtnTestCalcSouthClick(Sender: TObject);
var
  Index: integer;
  Ax, Bx: TArray<integer>;

  procedure FoldUnknownBits(const UnknownBits: array of integer; Count: integer; Start: integer;
    var Result: TArray<integer>);
  var
    BitToRemove: integer;
    StartA, StartB: integer;
  begin
    // Recursively fold two slices together, thus adding an unknown bit.
    if (Count = 0) then begin
      // Result:= LookupTable[Offset, Start];
      Result[index]:= Start;
      Inc(index);
    end else begin
      BitToRemove:= UnknownBits[Count - 1];
      StartA:= Start;
      StartB:= Start xor (1 shl BitToRemove);
      FoldUnknownBits(UnknownBits, Count - 1, StartA, Result);
      FoldUnknownBits(UnknownBits, Count - 1, StartB, Result);
      // Result:= A or B;
    end;
  end;

  function CalcEast1(i: integer): TArray<integer>;
  var
    a, b, j: integer;
  begin
    SetLength(Result, 32);
    a:= (((i shr 0) and $F) shl 1) xor (((i shr 4) and $F) shl 6) xor (((i shr 8) and $F) shl 11)
      xor (((i shr 12) and $F) shl 16) xor (((i shr 16) and $F) shl 21);
    Result[0]:= a;
    for j:= 31 downto 1 do begin
      b:= (((j shr 0) and 1) shl 0) xor (((j shr 1) and 1) shl 5) xor (((j shr 2) and 1) shl 10)
        xor (((j shr 3) and 1) shl 15) xor (((j shr 4) and 1) shl 20);
      Result[j]:= a xor b;
    end; { for j }
    TArray.Sort<integer>(Result);
  end;

  function CalcEast1Alternative(i: integer): TArray<integer>;
  var
    BitsToRemove: array [0 .. 4] of integer;
    j: integer;
  begin
    SetLength(Result, 32);
    for j:= 0 to 4 do begin
      BitsToRemove[j]:= (j * 5);
      i:= InsertZeroBit(i, BitsToRemove[j]);
    end;
    FoldUnknownBits(BitsToRemove, 5, i, Result);
    TArray.Sort<integer>(Result);
  end;

var
  i, k: integer;
begin
  for i:= (1 shl 20) - 1 downto 0 do begin
    Ax:= CalcEast1(i);
    index:= 0;
    Bx:= CalcEast1Alternative(i);
    for k:= 0 to 31 do begin
      if (Ax[k] <> Bx[k]) then begin
        Memo2.Lines.Add('i=' + i.ToString + ', mismatch');
        Exit;
      end;
    end; { for k }
  end; { for i }
end;

procedure TForm2.BtnCreateUnknownLookupTableClick(Sender: TObject);
var
  i: integer;
  // LookupUnknown: array[TDirection, TDistance] of TArray<TSlice>;

  function FoldUnknownBits(Offset: TOffset; const UnknownBits: array of integer; Count: integer; Start: integer): TSlice;
  var
    BitToRemove: integer;
    StartA, StartB: integer;
    a, b: TSlice;
  begin
    // Recursively fold two slices together, thus adding an unknown bit.
    if (Count = 0) then Result:= LookupTable[Offset, Start]
    else begin
      BitToRemove:= UnknownBits[Count - 1];
      StartA:= Start;
      StartB:= Start xor (1 shl BitToRemove);
      a:= FoldUnknownBits(Offset, UnknownBits, Count - 1, StartA);
      b:= FoldUnknownBits(Offset, UnknownBits, Count - 1, StartB);
      Result:= a or b;
    end;
  end;

  function CalcAlternative(i: integer; Offset: TOffset; plus: integer = 0; multi: integer = 1;
    iterations: integer = 5): TSlice;
  var
    BitsToRemove: array [0 .. 4] of integer;
    j: integer;
  begin
    for j:= 0 to iterations - 1 do begin
      BitsToRemove[j]:= (j * multi) + plus;
      i:= InsertZeroBit(i, BitsToRemove[j]);
    end;
    Result:= FoldUnknownBits(Offset, BitsToRemove, iterations, i);
  end;

  function Calc(i: integer; Offset: TOffset; const BitsToRemove: array of integer): TSlice;
  var
    j: integer;
  begin
    for j:= 0 to High(BitsToRemove) do begin
      i:= InsertZeroBit(i, BitsToremove[j]);
    end;
    Result:= FoldUnknownBits(Offset, BitsToremove, Length(BitsToRemove), i);
  end;

// NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

  function CalcNorth1Alternative(i: integer; Offset: TOffset; iterations: integer = 5): TSlice;
  begin
    Result:= CalcAlternative(i, Offset, 0, 1, iterations);
  end;

  function CalcNorth2Alternative(i: integer; Offset: TOffset): TSlice;
  begin
    Result:= CalcAlternative(i, Offset);
  end;

  function CalcNorth3Alternative(i: integer; Offset: TOffset): TSlice;
  begin
    Result:= CalcAlternative(i, Offset);
  end;

// SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS

  function CalcSouth1Alternative(i: integer; Offset: TOffset; iterations: integer = 5): TSlice;
  begin
    Result:= CalcAlternative(i, Offset, 20, 1, iterations);
  end;

  function CalcSouth2Alternative(i: integer; Offset: TOffset): TSlice;
  begin
    Result:= CalcAlternative(i, Offset, 15);
  end;

  function CalcSouth3Alternative(i: integer; Offset: TOffset): TSlice;
  begin
    Result:= CalcAlternative(i, Offset, 10);
  end;

// WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW

  function CalcWest1Alternative(i: integer; Offset: TOffset; iterations: integer = 5): TSlice;
  begin
    Result:= CalcAlternative(i, Offset, 4, 5, iterations);
  end;

  function CalcWest2Alternative(i: integer; Offset: TOffset): TSlice;
  begin
    Result:= CalcAlternative(i, Offset, 3, 4);
  end;

  function CalcWest3Alternative(i: integer; Offset: TOffset): TSlice;
  begin
    Result:= CalcAlternative(i, Offset, 2, 3);
  end;

// EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE

  function CalcEast1Alternative(i: integer; Offset: TOffset; iterations: integer = 5): TSlice;
  begin
    Result:= CalcAlternative(i, Offset, 0, 5, iterations);
  end;

  function CalcEast2Alternative(i: integer; Offset: TOffset): TSlice;
  begin
    Result:= CalcAlternative(i, Offset, 0, 4);
  end;

  function CalcEast3Alternative(i: integer; Offset: TOffset): TSlice;
  begin
    Result:= CalcAlternative(i, Offset, 0, 3);
  end;

var
  Slice: TSlice;
  DoubleCheck: TSlice;
  Pop: uint64;
  Filename: string;

begin
  Filename:= AppRegistry.ReadString(cUnknownSliceData);
  if (Filename = '') then begin
    if not(FileSaveDialog1.Execute) then Exit;
    Filename:= FileSaveDialog1.Filename;
  end;
  {$REGION 'compute unknown slices'}
  SetLength(LookupTable.FUnknownData, TLookupTable.UnknownSize);
  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
  // Top row unknown
  // SetLength(LookupUnknown[_North, 1], 1 shl 20);
  for i:= (1 shl 20) - 1 downto 0 do begin
    Slice:= CalcNorth1Alternative(i, oCenter);
    LookupTable[TOffset.Create(0, N1), i]:= Slice;
  end; { for i }

  for i:= (1 shl 15) - 1 downto 0 do begin
    Slice:= CalcNorth2Alternative(i, TOffset.Create(0, N1));
    LookupTable[TOffset.Create(0, N2), i]:= Slice;
  end; { for i }

  for i:= (1 shl 10) - 1 downto 0 do begin
    Slice:= CalcNorth2Alternative(i, TOffset.Create(0, N2));
    LookupTable[TOffset.Create(0, N3), i]:= Slice;
  end; { for i }

  // Bottom row unknown
  // SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
  // SetLength(LookupUnknown[_South,1], 1 shl 20);
  for i:= (1 shl 20) - 1 downto 0 do begin
    Slice:= CalcSouth1Alternative(i, oCenter);
    LookupTable[TOffset.Create(0, S1), i]:= Slice;
  end; { for i }

  for i:= (1 shl 15) - 1 downto 0 do begin
    Slice:= CalcSouth2Alternative(i, TOffset.Create(0, S1));
    LookupTable[TOffset.Create(0, S2), i]:= Slice;
  end; { for i }

  for i:= (1 shl 10) - 1 downto 0 do begin
    Slice:= CalcSouth3Alternative(i, TOffset.Create(0, S2));
    LookupTable[TOffset.Create(0, S3), i]:= Slice;
  end; { for i }

  // West row unknown
  // WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
  // SetLength(LookupUnknown[_West,1], 1 shl 20);
  for i:= (1 shl 20) - 1 downto 0 do begin
    Slice:= CalcWest1Alternative(i, oCenter);
    LookupTable[TOffset.Create(W1, 0), i]:= Slice;
  end; { for i }

  // SW1----------------------------------SW1
  for i:= (1 shl 16) - 1 downto 0 do begin
    Slice:= CalcWest1Alternative(i, TOffset.Create(0, S1), 4);
    //DoubleCheck:= CalcAlternative(i, TOffset.Create(W1, 0), 16, 1, 4);
    //if (Slice <> DoubleCheck) then begin
    //  System.Assert(Slice = DoubleCheck);
    //end;
    LookupTable[TOffset.Create(W1, S1), i]:= Slice;
  end; { for i }

  // NW1----------------------------------NW1
  for i:= (1 shl 16) - 1 downto 0 do begin
    Slice:= CalcWest1Alternative(i, TOffset.Create(0, N1), 4);
    //DoubleCheck:= CalcNorth1Alternative(i, TOffset.Create(W1, 0), 4);
    //if (Slice <> DoubleCheck) then begin
    //  System.Assert(Slice = DoubleCheck);
    //end;
    LookupTable[TOffset.Create(W1, N1), i]:= Slice;
  end; { for i }

  for i:= (1 shl 15) - 1 downto 0 do begin
    Slice:= CalcWest2Alternative(i, TOffset.Create(W1, 0));
    LookupTable[TOffset.Create(W2, 0), i]:= Slice;
  end; { for i }

  for i:= (1 shl 10) - 1 downto 0 do begin
    Slice:= CalcWest3Alternative(i, TOffset.Create(W2, 0));
    LookupTable[TOffset.Create(W3, 0), i]:= Slice;
  end; { for i }

  // East row unknown
  // EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
  // SetLength(LookupUnknown[_East,1], 1 shl 20);
  for i:= (1 shl 20) - 1 downto 0 do begin
    Slice:= CalcEast1Alternative(i, oCenter);
    LookupTable[TOffset.Create(E1, 0), i]:= Slice;
  end; { for i }

  // NE1----------------------------------NE1
  for i:= (1 shl 16) - 1 downto 0 do begin
    Slice:= CalcEast1Alternative(i, TOffset.Create(0, N1), 4);
    //DoubleCheck:= CalcNorth1Alternative(i, TOffset.Create(E1, 0), 4);
    //System.Assert(Slice = DoubleCheck);
    LookupTable[TOffset.Create(E1, N1), i]:= Slice;
  end; { for i }

  // SE1----------------------------------SE1
  for i:= (1 shl 16) - 1 downto 0 do begin
    Slice:= CalcEast1Alternative(i, TOffset.Create(0, S1), 4);
    //DoubleCheck:= CalcAlternative(i, TOffset.Create(E1, 0), 16, 1, 4);
    //if (Slice <> DoubleCheck) then begin
    //  System.Assert(Slice = DoubleCheck);
    //end;
    LookupTable[TOffset.Create(E1, S1), i]:= Slice;
  end; { for i }

  //E2--------------------------------------E2
  for i:= (1 shl 15) - 1 downto 0 do begin
    Slice:= CalcEast2Alternative(i, TOffset.Create(E1, 0));
    LookupTable[TOffset.Create(E2, 0), i]:= Slice;
  end; { for i }

  //E2S1---------------------------------E2S1
  for i:= (1 shl 12)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(E2,0), [12,13,14]);
    //DoubleCheck:= Calc(i, TOffset.Create(E1,S1), [0,4,8,12]);
    //if (Slice <> DoubleCheck) then begin
    //  System.Assert(Slice = DoubleCheck);
    //end;
    LookupTable[TOffset.Create(E2,S1),i]:= Slice;
  end; {for i}

  //E2N1-----------------------------------E2N1
  for i:= (1 shl 12)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(E2,0), [0,1,2]);
    //DoubleCheck:= Calc(i, TOffset.Create(E1,N1), [0,4,8,12]);
    //if (Slice <> DoubleCheck) then begin
    //  System.Assert(Slice = DoubleCheck);
    //end;
    LookupTable[TOffset.Create(E2,N1),i]:= Slice;
  end; {for i}

  //E1N2---------------------------------E1N2
  for i:= (1 shl 12)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(0,N2), [0,5,10]);
    //DoubleCheck:= Calc(i, TOffset.Create(E1,N1),[0,1,2,3]);
    //if (Slice <> DoubleCheck) then begin
    //  System.Assert(Slice = DoubleCheck);
    //end;
    LookupTable[TOffset.Create(E1,N2),i]:= Slice;
  end; {for i}

  //W1N2---------------------------------W1N2
  for i:= (1 shl 12)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(0,N2), [4,9,14]);
    DoubleCheck:= Calc(i, TOffset.Create(W1,N1),[0,1,2,3]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W1,N2),i]:= Slice;
  end; {for i}

  //W2N1---------------------------------W2N1
  for i:= (1 shl 12)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(W2,0), [0,1,2]);
    DoubleCheck:= Calc(i, TOffset.Create(W1,N1),[3,7,11,15]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W2,N1),i]:= Slice;
  end; {for i}

  //W1S2---------------------------------W1S2
  for i:= (1 shl 12)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(0,S2), [4,9,14]);
    DoubleCheck:= Calc(i, TOffset.Create(W1,S1),[12,13,14,15]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W1,S2),i]:= Slice;
  end; {for i}

  //W2S1---------------------------------W2S1
  for i:= (1 shl 12)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(W2,0), [12,13,14]);
    DoubleCheck:= Calc(i, TOffset.Create(W1,S1),[3,7,11,15]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W2,S1),i]:= Slice;
  end; {for i}

  //E1S2---------------------------------E1S2
  for i:= (1 shl 12)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(0,S2), [0,5,10]);
    DoubleCheck:= Calc(i, TOffset.Create(E1,S1),[12,13,14,15]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(E1,S2),i]:= Slice;
  end; {for i}

  for i:= (1 shl 10) - 1 downto 0 do begin
    Slice:= CalcEast3Alternative(i, TOffset.Create(E2, 0));
    LookupTable[TOffset.Create(E3, 0), i]:= Slice;
  end; { for i }

  //E1N3-----------------------------------E1N3
  for i:= (1 shl 8)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(0,N3), [0,5]);
    DoubleCheck:= Calc(i, TOffset.Create(E1,N2),[0,1,2,3]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(E1,N3),i]:= Slice;
  end; {for i}

  //E1S3-----------------------------------E1S3
  for i:= (1 shl 8)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(0,S3), [0,5]);
    DoubleCheck:= Calc(i, TOffset.Create(E1,S2),[8,9,10,11]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(E1,S3),i]:= Slice;
  end; {for i}

  //W1N3-----------------------------------W1N3
  for i:= (1 shl 8)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(0,N3), [4,9]);
    DoubleCheck:= Calc(i, TOffset.Create(W1,N2),[0,1,2,3]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W1,N3),i]:= Slice;
  end; {for i}

  //W3N1-----------------------------------W3N1
  for i:= (1 shl 8)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(W3,0), [0,1]);
    DoubleCheck:= Calc(i, TOffset.Create(W2,N1),[2,5,8,11]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W3,N1),i]:= Slice;
  end; {for i}

  //W3S1-----------------------------------W3S1
  for i:= (1 shl 8)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(W3,0), [8,9]);
    DoubleCheck:= Calc(i, TOffset.Create(W2,S1),[2,5,8,11]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W3,S1),i]:= Slice;
  end; {for i}

  //W1S3-----------------------------------W1S3
  for i:= (1 shl 8)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(0,S3), [4,9]);
    DoubleCheck:= Calc(i, TOffset.Create(W1,S2),[8,9,10,11]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W1,S3),i]:= Slice;
  end; {for i}

  //E3N1-----------------------------------E3N1
  Pop:= 0;
  for i:= (1 shl 8)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(E3,0), [0,1]);
    DoubleCheck:= Calc(i, TOffset.Create(E2,N1),[0,3,6,9]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(E3,N1),i]:= Slice;
    Inc(Pop, Slice.PopCount);
  end; {for i}
  if Pop = ((1 shl 8) * 512) then Memo2.Lines.Add('E3N1 is overloaded')
  else Memo2.Lines.Add('E3N1 load = '+(Pop div (1 shl 8)).ToString);

  //E3S1-----------------------------------E3S1
  Pop:= 0;
  for i:= (1 shl 8)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(E3,0), [8,9]);
    DoubleCheck:= Calc(i, TOffset.Create(E2,S1),[0,3,6,9]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(E3,S1),i]:= Slice;
    Inc(Pop, Slice.PopCount);
  end; {for i}
  if Pop = ((1 shl 8) * 512) then Memo2.Lines.Add('E3S1 is overloaded')
  else Memo2.Lines.Add('E3S1 load = '+(Pop div (1 shl 8)).ToString);

  //E2N2-----------------------------------E2N2
  Pop:= 0;
  for i:= (1 shl 9)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(E2,N1), [0,1,2]);
    DoubleCheck:= Calc(i, TOffset.Create(E1,N2),[0,4,8]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(E2,N2),i]:= Slice;
    Inc(Pop, Slice.PopCount);
  end; {for i}
  if Pop = ((1 shl 9) * 512) then Memo2.Lines.Add('E2N2 is overloaded')
  else Memo2.Lines.Add('E2N2 load = '+(Pop div (1 shl 9)).ToString);

  //W2N2-----------------------------------W2N2
  for i:= (1 shl 9)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(W2,N1), [0,1,2]);
    DoubleCheck:= Calc(i, TOffset.Create(W1,N2),[3,7,11]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W2,N2),i]:= Slice;
  end; {for i}

  //E2S2-----------------------------------E2S2
  for i:= (1 shl 9)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(E2,S1), [9,10,11]);
    DoubleCheck:= Calc(i, TOffset.Create(E1,S2),[0,4,8]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(E2,S2),i]:= Slice;
  end; {for i}

  //W2S2-----------------------------------W2S2
  for i:= (1 shl 9)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(W2,S1), [9,10,11]);
    DoubleCheck:= Calc(i, TOffset.Create(W1,S2),[3,7,11]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W2,S2),i]:= Slice;
  end; {for i}

  //E3N2-----------------------------------E3N2
  Pop:= 0;
  for i:= (1 shl 6)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(E3,N1), [0,1]);
    DoubleCheck:= Calc(i, TOffset.Create(E2,N2),[0,3,6]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(E3,N2),i]:= Slice;
    Inc(Pop, Slice.PopCount);
  end; {for i}
  if Pop = ((1 shl 6) * 512) then Memo2.Lines.Add('E3N2 is overloaded')
  else Memo2.Lines.Add('E3N2 load = '+(Pop div (1 shl 6)).ToString);

  //W2N3-----------------------------------W2N3
  for i:= (1 shl 6)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(W2,N2), [0,1,2]);
    DoubleCheck:= Calc(i, TOffset.Create(W1,N3),[3,7]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W2,N3),i]:= Slice;
  end; {for i}

  //W3N2-----------------------------------W3N2
  for i:= (1 shl 6)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(W2,N2), [2,5,8]);
    DoubleCheck:= Calc(i, TOffset.Create(W3,N1),[0,1]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W3,N2),i]:= Slice;
  end; {for i}

  //E3S2-----------------------------------E3S2
  for i:= (1 shl 6)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(E2,S2), [0,3,6]);
    DoubleCheck:= Calc(i, TOffset.Create(E3,S1),[6,7]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(E3,S2),i]:= Slice;
  end; {for i}

  //E2S3-----------------------------------E2S3
  for i:= (1 shl 6)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(E2,S2), [6,7,8]);
    DoubleCheck:= Calc(i, TOffset.Create(E1,S3),[0,4]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(E2,S3),i]:= Slice;
  end; {for i}

  //W2S3-----------------------------------W2S3
  for i:= (1 shl 6)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(W2,S2), [6,7,8]);
    DoubleCheck:= Calc(i, TOffset.Create(W1,S3),[3,7]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W2,S3),i]:= Slice;
  end; {for i}

  //W3S2-----------------------------------W3S2
  for i:= (1 shl 6)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(W2,S2), [2,5,8]);
    DoubleCheck:= Calc(i, TOffset.Create(W3,S1),[6,7]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(W3,S2),i]:= Slice;
  end; {for i}

  //E2N3-----------------------------------E2N3
  Pop:= 0;
  for i:= (1 shl 6)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(E1,N3), [0,4]);
    DoubleCheck:= Calc(i, TOffset.Create(E2,N2),[0,1,2]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(E2,N3),i]:= Slice;
    Inc(Pop, Slice.PopCount);
  end; {for i}
  if Pop = ((1 shl 6) * 512) then Memo2.Lines.Add('E2N3 is overloaded')
  else Memo2.Lines.Add('E2N3 load = '+(Pop div (1 shl 6)).ToString);

  //E3N3-----------------------------------E3N3
  Pop:= 0;
  for i:= (1 shl 4)-1 downto 0 do begin
    Slice:= Calc(i, TOffset.Create(E2,N3), [0,3]);
    DoubleCheck:= Calc(i, TOffset.Create(E3,N2),[0,1]);
    if (Slice <> DoubleCheck) then begin
      System.Assert(Slice = DoubleCheck);
    end;
    LookupTable[TOffset.Create(E3,N3),i]:= Slice;
    LookupTable[TOffset.Create(W3,N3),i]:= Slice;
    LookupTable[TOffset.Create(E3,S3),i]:= Slice;
    LookupTable[TOffset.Create(W3,S3),i]:= Slice;
    Inc(Pop, Slice.PopCount);
  end; {for i}
  {$ENDREGION}
  if Pop = ((1 shl 4) * 512) then Memo2.Lines.Add('E3N3 is overloaded')
  else Memo2.Lines.Add('E3N3 load = '+(Pop div (1 shl 4)).ToString);
  LookupTable.SaveSliceData(Filename, LookupTable.FUnknownData);
  AppRegistry.WriteString(cUnknownSliceData,Filename);
end;

function DeleteBitAlternative(input, BitTodelete: integer): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 31 downto 0 do begin
    if (i = BitToDelete) then Continue;
    Result:= result shl 1;
    if ((input and (1 shl i)) <> 0) then Inc(Result);
  end;
end;

procedure TForm2.BtnTestDeleteBitClick(Sender: TObject);
var
  i,j: integer;
begin
  for i:= 0 to 256*256*256-1 do begin
    for j:= 0 to 31 do begin
      if (DeleteBit(i,j) <> DeleteBitAlternative(i,j)) then begin
        System.Assert(DeleteBit(i,j) = DeleteBitAlternative(i,j));
      end;
    end;
  end;
end;

function IsTestInsightRunning: Boolean;
var
  client: ITestInsightClient;
begin
  client := TTestInsightRestClient.Create;
  client.StartedTesting(0);
  Result := not client.HasError;
end;

procedure TForm2.BtnDoFailingTestsClick(Sender: TObject);
begin
  UnknownSlice(TCake.Create(524560,32742631));
end;

procedure TForm2.BtnStartUnitTestsClick(Sender: TObject);
begin
  if IsTestInsightRunning then TestInsight.DUnitX.RunRegisteredTests
  else ShowMessage('Run the tests inside the IDE using TestInsight');
end;

function TForm2.FoldRemaining(Offset: TOffset; UnknownMask, KnownMask: integer; Filter: boolean = false): TSlice;

  function FoldUnknownBits(Offset: TOffset; const UnknownBits: TMaskedBits; Count: integer; Start: integer): TSlice;
  var
    BitToRemove: integer;
    StartA, StartB: integer;
    a, b: TSlice;
  begin
    // Recursively fold two slices together, thus adding an unknown bit.
    if (Count = 0) then begin
      Result:= LookupTable[Offset, Start]
    end else begin
      BitToRemove:= UnknownBits[Count - 1];
      StartA:= Start;
      StartB:= Start xor (1 shl BitToRemove);
      a:= FoldUnknownBits(Offset, UnknownBits, Count - 1, StartA);
      b:= FoldUnknownBits(Offset, UnknownBits, Count - 1, StartB);
      Result:= a or b;
    end;
  end;

var
  UnknownBits: TMaskedBits;
  OffsetBits: TMaskedBits;
begin
  UnknownBits:= TMaskedBits.Create(UnknownMask);
  if Filter then begin
    OffsetBits:= Offset.ToMaskedBits;
    KnownMask:= OffsetBits.FilterKnownBits(KnownMask);
  end;
  Result:= FoldUnknownBits(Offset, UnknownBits, UnknownBits.Count, KnownMask);
end;


function TForm2.UnknownSlice(Cake: TCake): TSlice;
const
  MaskEW: array[W3..E3] of integer = ($1CE739C, $18C6318, $1084210, $FFFFFFF, $108421, $318C63, $739CE7);
  MaskNS: array[S3..N3] of integer = ($1FFFC00, $1FF8000, $1F00000, $FFFFFFF, $00001F, $0003FF, $007FFF);
type
  TI = TMaskedBitsIndex;
var
  Leading, Trailing, Popcnt: integer;
  BitsToDelete: TMaskedBits;
  RectResult: TPair<TI, TI>;
  Offset: TOffset;
  MaskNSEW: integer;
  UnknownMask, KnownMask: integer;
begin
  UnknownMask:= Cake.Unknown; KnownMask:= Cake.Known;
  TForm2.LeadingTrailingPopCount(UnknownMask, Leading, Trailing, PopCnt);
  Result.Clear;
  RectResult:= TPair<TI,TI>.Create(0,0); //Assume failure
  case PopCnt of
    5: begin  //N1, E1, W1, S1
      if (UnknownMask and MaskNS[N1]) = MaskNS[N1] then RectResult:= TPair<TI,TI>.Create(0,N1)
      else if (UnknownMask and MaskNS[S1]) = MaskNS[S1] then RectResult:= TPair<TI,TI>.Create(0,S1)
      else if (UnknownMask and MaskEW[E1]) = MaskEW[E1] then RectResult:= TPair<TI,TI>.Create(E1,0)
      else if (UnknownMask and MaskEW[W1]) = MaskEW[W1] then RectResult:= TPair<TI,TI>.Create(W1,0);
    end; {5:}
    10: begin //N2, E2, W2, S2
      if (UnknownMask and MaskNS[N2]) = MaskNS[N2] then RectResult:= TPair<TI,TI>.Create(0,N2)
      else if (UnknownMask and MaskNS[S2]) = MaskNS[S2] then RectResult:= TPair<TI,TI>.Create(0,S2)
      else if (UnknownMask and MaskEW[E2]) = MaskEW[E2] then RectResult:= TPair<TI,TI>.Create(E2,0)
      else if (UnknownMask and MaskEW[W2]) = MaskEW[W2] then RectResult:= TPair<TI,TI>.Create(W2,0);
    end; {10:}
    15: begin //N3, E3, W3, S3
      if (UnknownMask and MaskNS[N3]) = MaskNS[N3] then RectResult:= TPair<TI,TI>.Create(0,N3)
      else if (UnknownMask and MaskNS[S3]) = MaskNS[S3] then RectResult:= TPair<TI,TI>.Create(0,S3)
      else if (UnknownMask and MaskEW[E3]) = MaskEW[E3] then RectResult:= TPair<TI,TI>.Create(E3,0)
      else if (UnknownMask and MaskEW[W3]) = MaskEW[W3] then RectResult:= TPair<TI,TI>.Create(W3,0);
    end; {15:}
    9: begin  //NW1, SE1, NE1, SW1
      if (UnknownMask and (MaskNS[N1] or MaskEW[W1])) = (MaskNS[N1] or MaskEW[W1]) then RectResult:= TPair<TI,TI>.Create(W1,N1)
      else if (UnknownMask and (MaskNS[S1] or MaskEW[W1])) = (MaskNS[S1] or MaskEW[W1]) then RectResult:= TPair<TI,TI>.Create(W1,S1)
      else if (UnknownMask and (MaskNS[S1] or MaskEW[E1])) = (MaskNS[S1] or MaskEW[E1]) then RectResult:= TPair<TI,TI>.Create(E1,S1)
      else if (UnknownMask and (MaskNS[N1] or MaskEW[E1])) = (MaskNS[N1] or MaskEW[E1]) then RectResult:= TPair<TI,TI>.Create(E1,N1)
    end; {9:}
    16: begin  //NW2, SE2, NE2, SW2
      if (UnknownMask and (MaskNS[N2] or MaskEW[W2])) = (MaskNS[N2] or MaskEW[W2]) then RectResult:= TPair<TI,TI>.Create(W2,N2)
      else if (UnknownMask and (MaskNS[S2] or MaskEW[W2])) = (MaskNS[S2] or MaskEW[W2]) then RectResult:= TPair<TI,TI>.Create(W2,S2)
      else if (UnknownMask and (MaskNS[S2] or MaskEW[E2])) = (MaskNS[S2] or MaskEW[E2]) then RectResult:= TPair<TI,TI>.Create(E2,S2)
      else if (UnknownMask and (MaskNS[N2] or MaskEW[E2])) = (MaskNS[N2] or MaskEW[E2]) then RectResult:= TPair<TI,TI>.Create(E2,N2);
    end; {16:}
    21: begin  //NW3, SE3, NE3, SW3
      //There are only 4 known pixels. This means the result will always be fully unknown.
      //No need for further processing.
      RectResult:= TPair<TI,TI>.Create(W3,N3); //NW3 is the same as all the others, i.e. fully unknown.
    end; {21:}
    13: begin //N1E2, N2E1, N1W2, N2W1, S1E2, S2E1, S1W2, S2W1,
      if      (UnknownMask and (MaskNS[N1] or MaskEW[E2])) = (MaskNS[N1] or MaskEW[E2]) then RectResult:= TPair<TI,TI>.Create(E2,N1)
      else if (UnknownMask and (MaskNS[S1] or MaskEW[E2])) = (MaskNS[S1] or MaskEW[E2]) then RectResult:= TPair<TI,TI>.Create(E2,S1)
      else if (UnknownMask and (MaskNS[N2] or MaskEW[E1])) = (MaskNS[N2] or MaskEW[E1]) then RectResult:= TPair<TI,TI>.Create(E1,N2)
      else if (UnknownMask and (MaskNS[S2] or MaskEW[E1])) = (MaskNS[S2] or MaskEW[E1]) then RectResult:= TPair<TI,TI>.Create(E1,S2)
      else if (UnknownMask and (MaskNS[N1] or MaskEW[W2])) = (MaskNS[N1] or MaskEW[W2]) then RectResult:= TPair<TI,TI>.Create(W2,N1)
      else if (UnknownMask and (MaskNS[S1] or MaskEW[W2])) = (MaskNS[S1] or MaskEW[W2]) then RectResult:= TPair<TI,TI>.Create(W2,S1)
      else if (UnknownMask and (MaskNS[N2] or MaskEW[W1])) = (MaskNS[N2] or MaskEW[W1]) then RectResult:= TPair<TI,TI>.Create(W1,N2)
      else if (UnknownMask and (MaskNS[S2] or MaskEW[W1])) = (MaskNS[S2] or MaskEW[W1]) then RectResult:= TPair<TI,TI>.Create(W1,S2);
    end; {13:}
    17: begin //N1E3, N3E1, N1W3, N3W1, S1E3, S3E1, S1W3, S3W1,
      if      (UnknownMask and (MaskNS[N1] or MaskEW[E3])) = (MaskNS[N1] or MaskEW[E3]) then RectResult:= TPair<TI,TI>.Create(E3,N1)
      else if (UnknownMask and (MaskNS[S1] or MaskEW[E3])) = (MaskNS[S1] or MaskEW[E3]) then RectResult:= TPair<TI,TI>.Create(E3,S1)
      else if (UnknownMask and (MaskNS[N3] or MaskEW[E1])) = (MaskNS[N3] or MaskEW[E1]) then RectResult:= TPair<TI,TI>.Create(E1,N3)
      else if (UnknownMask and (MaskNS[S3] or MaskEW[E1])) = (MaskNS[S3] or MaskEW[E1]) then RectResult:= TPair<TI,TI>.Create(E1,S3)
      else if (UnknownMask and (MaskNS[N1] or MaskEW[W3])) = (MaskNS[N1] or MaskEW[W3]) then RectResult:= TPair<TI,TI>.Create(W3,N1)
      else if (UnknownMask and (MaskNS[S1] or MaskEW[W3])) = (MaskNS[S1] or MaskEW[W3]) then RectResult:= TPair<TI,TI>.Create(W3,S1)
      else if (UnknownMask and (MaskNS[N3] or MaskEW[W1])) = (MaskNS[N3] or MaskEW[W1]) then RectResult:= TPair<TI,TI>.Create(W1,N3)
      else if (UnknownMask and (MaskNS[S3] or MaskEW[W1])) = (MaskNS[S3] or MaskEW[W1]) then RectResult:= TPair<TI,TI>.Create(W1,S3);
    end; {17:}
    19: begin //N2E3, N3E2, N2W3, N3W2, S2E3, S3E2, S2W3, S3W2,
      if      (UnknownMask and (MaskNS[N2] or MaskEW[E3])) = (MaskNS[N2] or MaskEW[E3]) then RectResult:= TPair<TI,TI>.Create(E3,N2)
      else if (UnknownMask and (MaskNS[S2] or MaskEW[E3])) = (MaskNS[S2] or MaskEW[E3]) then RectResult:= TPair<TI,TI>.Create(E3,S2)
      else if (UnknownMask and (MaskNS[N3] or MaskEW[E2])) = (MaskNS[N3] or MaskEW[E2]) then RectResult:= TPair<TI,TI>.Create(E2,N3)
      else if (UnknownMask and (MaskNS[S3] or MaskEW[E2])) = (MaskNS[S3] or MaskEW[E2]) then RectResult:= TPair<TI,TI>.Create(E2,S3)
      else if (UnknownMask and (MaskNS[N2] or MaskEW[W3])) = (MaskNS[N2] or MaskEW[W3]) then RectResult:= TPair<TI,TI>.Create(W3,N2)
      else if (UnknownMask and (MaskNS[S2] or MaskEW[W3])) = (MaskNS[S2] or MaskEW[W3]) then RectResult:= TPair<TI,TI>.Create(W3,S2)
      else if (UnknownMask and (MaskNS[N3] or MaskEW[W2])) = (MaskNS[N3] or MaskEW[W2]) then RectResult:= TPair<TI,TI>.Create(W2,N3)
      else if (UnknownMask and (MaskNS[S3] or MaskEW[W2])) = (MaskNS[S3] or MaskEW[W2]) then RectResult:= TPair<TI,TI>.Create(W2,S3);
    end; {19:}
  end; {case}
  if (RectResult.Key  <> 0) or (RectResult.Value <> 0) then begin
  //Did we get a match?
    BitsToDelete:= TMaskedBits.Create(RectResult.Key, RectResult.Value);
    KnownMask:= DeleteBits(KnownMask, BitsToDelete);
    Result:= LookupTable[TOffset.Create(RectResult.Key, RectResult.Value),KnownMask];
    exit;
  end else begin
  //If not then we have an irregular result. We'll need to do some digging.
    //Let's see if we have a continous block of unknown cells at the borders.
    if (Popcnt - Popcount(UnknownMask and not MaskNS[N1])) = 5 then begin
      //Unknown pixels are north facing
      if (Popcnt - Popcount(UnknownMask and not MaskNS[N3])) = 15 then begin
        //We have a variant of N3.
        //Lets fold the remaining slices
        RectResult:= TPair<TI,TI>.Create(0,N3)
      end else if (Popcnt - Popcount(UnknownMask and not MaskNS[N2])) = 10 then begin
        //We have a variant of N2
        RectResult:= TPair<TI,TI>.Create(0,N2);
      end else begin
        //We have a variant of N1
        RectResult:= TPair<TI,TI>.Create(0,N1);
      end;
    end {explore N} else if (Popcnt - Popcount(UnknownMask and not MaskNS[S1])) = 5 then begin
      //Unknown pixels are south facing
      if (Popcnt - Popcount(UnknownMask and not MaskNS[S3])) = 15 then begin
        //We have a variant of S3.
        //Lets fold the remaining slices
        RectResult:= TPair<TI,TI>.Create(0,S3);
      end else if (Popcnt - Popcount(UnknownMask and not MaskNS[S2])) = 10 then begin
        //We have a variant of S2
        RectResult:= TPair<TI,TI>.Create(0,S2);
      end else begin
        //We have a variant of S1
        RectResult:= TPair<TI,TI>.Create(0,S1);
      end;
    end {explore S} else if (Popcnt - Popcount(UnknownMask and not MaskEW[W1])) = 5 then begin
      //Unknown pixels are west facing
      if (Popcnt - Popcount(UnknownMask and not MaskEW[W3])) = 15 then begin
        //We have a variant of W3.
        //Lets fold the remaining slices
        RectResult:= TPair<TI,TI>.Create(W3,0);
      end else if (Popcnt - Popcount(UnknownMask and not MaskEW[W2])) = 10 then begin
        //We have a variant of W2
        RectResult:= TPair<TI,TI>.Create(W2,0);
      end else begin
        //We have a variant of W1
        RectResult:= TPair<TI,TI>.Create(W1,0);
      end;
    end {explore W} else if (Popcnt - Popcount(UnknownMask and not MaskEW[E1])) = 5 then begin
      //Unknown pixels are east facing
      if (Popcnt - Popcount(UnknownMask and not MaskEW[E3])) = 15 then begin
        //We have a variant of E3.
        //Lets fold the remaining slices
        RectResult:= TPair<TI,TI>.Create(E3,0);
      end else if (Popcnt - Popcount(UnknownMask and not MaskEW[E2])) = 10 then begin
        //We have a variant of E2
        RectResult:= TPair<TI,TI>.Create(E2,0);
      end else begin
        //We have a variant of E1
        RectResult:= TPair<TI,TI>.Create(E1,0);
      end;
    end {explore E} else begin
      //Unknown pixels are all over the place
      RectResult:= TPair<TI,TI>.Create(0,0);
    end;
    //Now do the irregular folding
    BitsToDelete:= TMaskedBits.Create(RectResult.Key, RectResult.Value);
    KnownMask:= DeleteBits(KnownMask, BitsToDelete);
    Offset:= TOffset.Create(RectResult.Key, RectResult.Value);
    MaskNSEW:= MaskEW[RectResult.Key] and MaskNS[RectResult.Value];
    if (RectResult.Key <> 0) or (RectResult.Value <> 0) then begin
      UnknownMask:= UnknownMask and not(MaskNSEW);
      //Now shrink the UnknownMask using the BitsToDelete
      UnknownMask:= DeleteBits(UnknownMask, BitsToDelete);
    end;
    Result:= FoldRemaining(Offset, UnknownMask, KnownMask);
  end;
end;

class procedure TForm2.LeadingTrailingPopCount(UnknownMask: integer; out Leading, Trailing, Popcnt: integer);
asm
  //ecx = UnknownMask
  //[rdx] = Leading
  //[r8] = Trailing
  //[r9] = popcount
  rep bsr eax,ecx//lzcnt [rdx],rcx
  mov [rdx],eax
  rep bsf eax,ecx//tzcnt [r8],rcx
  mov [r8],eax
  popcnt eax,ecx
  mov [r9],eax
end;

class function TForm2.GetNextBitSet(previous, i: integer): integer;
asm
  //ecx = previous
  //edx = i
  inc ecx          //make sure we get the NEXT bit.
  shr edx,cl      //remove the previous bits
  rep bsf eax,edx //tzcnt eax,edx //Count the number of LSB that are zero
  add eax,ecx     //add the previous count back in.
end;

{$Region 'old comments'}
  // //Now do the NESW2 parts
  // //These are based on the NESW1 parts
  // //Top row unknown
  // //SetLength(LookupUnknown[_North, 2], 1 shl 15);
  //
  //
  // for i:= (1 shl 15)-1 downto 0 do begin
  // a:= i * 32;
  // //Slice:= LookupUnknown[_North, 1][a];
  // Slice:= LookupTable[TOffset.Create(0,1),a];
  // for j:= 31 downto 1 do begin
  // Slice:= Slice or LookupTable[TOffset.Create(0,1),a+j];
  // end; {for j}
  // //LookupUnknown[_North,2][i]:= Slice;
  // //LookupTable.UnknownData[2,0,i]:= Slice;
  // LookupTable[TOffset.Create(0,2),i]:= Slice;
  // end; {for i}
  //
  // //Bottom row unknown
  // //SetLength(LookupUnknown[_South,2], 1 shl 15);
  // for i:= (1 shl 15)-1 downto 0 do begin
  // a:= i;
  // Slice:= LookupUnknown[_South, 1][a];
  // for j:= 31 downto 1 do begin
  // b:= j shl 15;
  // Slice:= Slice or LookupUnknown[_South, 1][a xor b];
  // end; {for j}
  // //LookupUnknown[_South,2][i]:= Slice;
  // LookupTable.UnknownData[-2,0,i]:= Slice;
  // end; {for i}
  //
  // //West row unknown
  // //SetLength(LookupUnknown[_West,2], 1 shl 15);
  // for i:= (1 shl 15)-1 downto 0 do begin
  // a:= (((i shr 0) and $7) shl 0) xor
  // (((i shr 3) and $7) shl 4) xor
  // (((i shr 6) and $7) shl 8) xor
  // (((i shr 9) and $7) shl 12) xor
  // (((i shr 12) and $7) shl 16);
  // Slice:= LookupUnknown[_West,1][a];
  // for j:= 31 downto 1 do begin
  // b:= (((j shr 0) and 1) shl 3) xor
  // (((j shr 1) and 1) shl 7) xor
  // (((j shr 2) and 1) shl 11) xor
  // (((j shr 3) and 1) shl 15) xor
  // (((j shr 4) and 1) shl 19);
  // Slice:= Slice or LookupUnknown[_West,1][a xor b];
  // end; {for j}
  // //LookupUnknown[_West,2][i]:= Slice;
  // LookupTable.UnknownData[0,-2,i]:= Slice;
  // end; {for i}
  //
  // //East row unknown
  // //SetLength(LookupUnknown[_East,2], 1 shl 15);
  // for i:= (1 shl 15)-1 downto 0 do begin
  // a:= (((i shr 0) and $7) shl 1) xor
  // (((i shr 3) and $7) shl 5) xor
  // (((i shr 6) and $7) shl 9) xor
  // (((i shr 9) and $7) shl 13) xor
  // (((i shr 12) and $7) shl 17);
  // Slice:= LookupUnknown[_East,1][a];
  // for j:= 31 downto 1 do begin
  // b:= (((j shr 0) and 1) shl 0) xor
  // (((j shr 1) and 1) shl 4) xor
  // (((j shr 2) and 1) shl 8) xor
  // (((j shr 3) and 1) shl 12) xor
  // (((j shr 4) and 1) shl 16);
  // Slice:= Slice or LookupUnknown[_East,1][a xor b];
  // end; {for j}
  // //LookupUnknown[_East,2][i]:= Slice;
  // LookupTable.UnknownData[0,2,i]:= Slice;
  // end; {for i}
  //
  // //Now do the NESW3 parts
  // //These are based on the NESW1 parts
  // //Top row unknown
  // //SetLength(LookupUnknown[_North, 3], 1 shl 10);
  // for i:= (1 shl 10)-1 downto 0 do begin
  // a:= i * 32;
  // Slice:= LookupUnknown[_North, 2][a];
  // for j:= 31 downto 1 do begin
  // Slice:= Slice or LookupUnknown[_North, 2][a+j];
  // end; {for j}
  // //LookupUnknown[_North,3][i]:= Slice;
  // LookupTable.UnknownData[3,0,i]:= Slice;
  // end; {for i}
  //
  // //Bottom row unknown
  // //SetLength(LookupUnknown[_South,3], 1 shl 10);
  // for i:= (1 shl 10)-1 downto 0 do begin
  // a:= i;
  // Slice:= LookupUnknown[_South, 2][a];
  // for j:= 31 downto 1 do begin
  // b:= j shl 10;
  // Slice:= Slice or LookupUnknown[_South, 2][a xor b];
  // end; {for j}
  // //LookupUnknown[_South,3][i]:= Slice;
  // LookupTable.UnknownData[-3,0,i]:= Slice;
  // end; {for i}
  //
  // //West row unknown
  // //SetLength(LookupUnknown[_West,3], 1 shl 10);
  // for i:= (1 shl 10)-1 downto 0 do begin
  // a:= (((i shr 0) and $3) shl 0) xor
  // (((i shr 2) and $3) shl 3) xor
  // (((i shr 4) and $3) shl 6) xor
  // (((i shr 6) and $3) shl 9) xor
  // (((i shr 8) and $3) shl 12);
  // Slice:= LookupUnknown[_West,2][a];
  // for j:= 31 downto 1 do begin
  // b:= (((j shr 0) and 1) shl 2) xor
  // (((j shr 1) and 1) shl 5) xor
  // (((j shr 2) and 1) shl 8) xor
  // (((j shr 3) and 1) shl 11) xor
  // (((j shr 4) and 1) shl 14);
  // Slice:= Slice or LookupUnknown[_West,2][a xor b];
  // end; {for j}
  // //LookupUnknown[_West,3][i]:= Slice;
  // LookupTable.UnknownData[0,-3,i]:= Slice;
  // end; {for i}
  //
  // //East row unknown  EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
  // //SetLength(LookupUnknown[_East,3], 1 shl 10);
  // for i:= (1 shl 10)-1 downto 0 do begin
  // a:= (((i shr 0) and $3) shl 1) xor
  // (((i shr 2) and $3) shl 4) xor
  // (((i shr 4) and $3) shl 7) xor
  // (((i shr 6) and $3) shl 10) xor
  // (((i shr 8) and $3) shl 13);
  // Slice:= LookupUnknown[_East,2][a];
  // for j:= 31 downto 1 do begin
  // b:= (((j shr 0) and 1) shl 0) xor
  // (((j shr 1) and 1) shl 3) xor
  // (((j shr 2) and 1) shl 6) xor
  // (((j shr 3) and 1) shl 9) xor
  // (((j shr 4) and 1) shl 12);
  // Slice:= Slice or LookupUnknown[_East,2][a xor b];
  // end; {for j}
  // //LookupUnknown[_East,3][i]:= Slice;
  // LookupTable.UnknownData[0,3,i]:= Slice;
  // end; {for i}
  //
  // //Write the data to disk
  // //Save the tables to disk:
  // if not(FileSaveDialog1.Execute) then exit;
  // FS:= TFileStream.Create(FileSaveDialog1.Filename, fmCreate);
  // for i:= 1 to 4 do begin
  // for d:= _North to _West do begin
  // FS.Write(TBytes(LookupUnknown[d,i]), Length(LookupUnknown[d,i]) * SizeOf(TSlice));
  // end;
  // end;
  // FS.Free;
  // {TODO -oJB -cAdd stuff : How to add the lookupUnknown for the corners (NE, SW etc)}
  // {TODO -oJB -cSave : Save the lookup unknown data}

{$EndRegion}

procedure TForm2.BtnSolveWithChunkLookupClick(Sender: TObject);
var
  x, y: integer;
  Slices: TSlices11;
  Chunk: TSuperSlice;
begin
  // Load the lookup table entries
  if Length(ChunkLookup[cFlat]) = 0 then BtnLoadSmallLookups.Click;
  FillChar(Slices, SizeOf(Slices), $FF);
  for x:= 0 to 17 - 2 do begin
    for y:= 0 to 17 - 2 do begin
      Chunk:= ChunkLookup[cStanding, GetFutureStandingChunk(StringGrid1, x + 1, y + 1)];
      Slices[x, y]:= Slices[x, y] and Chunk.South;
      if (y < (17 - 2)) then Slices[x, y + 1]:= Slices[x, y + 1] and Chunk.North;

      Chunk:= ChunkLookup[cFlat, GetFutureFlatChunk(StringGrid1, x + 1, y + 1)];
      Slices[x, y]:= Slices[x, y] and Chunk.East;
      if (x < (17 - 2)) then Slices[x + 1, y]:= Slices[x + 1, y] and Chunk.West;
    end;
  end;
  DisplaySlices(StringGrid2, StringGrid3, Slices);
  MySlices:= Slices;
end;

procedure TForm2.BtnValidateN1E1LookupTableClick(Sender: TObject);

  function RotateLeft90(Input: integer): integer;
  const
    Translate: array [0 .. 24] of byte = (4, 9, 14, 19, 24, 3, 8, 13, 18, 24, 2, 7, 12, 17, 22, 1, 6, 11, 16, 21, 0, 5,
      10, 15, 20);
  var
    BitsIn, BitsOut: array [0 .. 24] of byte;
    i: integer;
  begin
    FillChar(BitsIn, SizeOf(BitsIn), 0);
    for i:= 0 to 24 do begin
      if Odd(Input) then BitsIn[i]:= 1;
      Input:= Input shr 1;
    end;
    for i:= 0 to 24 do begin
      BitsOut[i]:= BitsIn[Translate[i]];
    end;
    Result:= 0;
    for i:= 0 to 24 do begin
      if (BitsOut[i] = 1) then Result:= Result or (1 shl i);
    end;
  end;

var
  i: integer;
  a, b: TSlice;
  LookupN1, LookupE1: TArray<TSlice>;
begin
  LoadLookupTable(LookupN1);
  LoadLookupTable(LookupE1);

  for i:= 0 to (1 shl 25) - 1 do begin
    a:= LookupN1[RotateLeft90(i)];
    b:= LookupE1[i];
    if (a <> b) then ShowMessage('Oops');
  end;

end;

procedure TForm2.Display5x5(Cake: TCake; const SG: TStringGrid);
var
  x, y: integer;
  Known, Unknown: integer;
begin
  Known:= Cake.Known;
  Unknown:=Cake.Unknown;
  for y:= 0 to 4 do begin
    for x:= 4 downto 0 do begin
      if Odd(Known) then SG.Cells[x, y]:= 'X'
      else if (Odd(Unknown)) then SG.Cells[x, y]:= '?'
      else SG.Cells[x, y]:= '';
      Known:= Known shr 1;
      Unknown:= Unknown shr 1;
    end;
  end;
end;

procedure TForm2.BtnValidateCountTableClick(Sender: TObject);
var
  Soll, Ist: uint64;
  MinOn, MinOff: uint64;
  MinOnIndex, MinOffIndex: integer;
  Counts: TArray<uint64>;
  item: uint64;
  FS: TFileStream;
  i: integer;
begin
  if not(FileOpenDialog1.Execute) then Exit;
  FS:= TFileStream.Create(FileOpenDialog1.Filename, fmOpenRead);
  SetLength(Counts, FS.Size div SizeOf(uint64));
  FS.Read64(TBytes(Counts), 0, FS.Size);
  FS.Free;
  Ist:= 0;
  for item in Counts do Inc(Ist, item);
  Soll:= 1;
  Soll:= Soll shl 49;
  System.Assert(Ist = Soll);
  MinOn:= Ist;
  MinOff:= Ist;
  // Find the minimum on and off slices
  MinOnIndex:= 0;
  MinOffIndex:= 0;
  for i:= 0 to high(Counts) do begin
    case ((i and (1 shl 12)) <> 0) of
      true: if (Counts[i] < MinOn) then begin
        MinOn:= Counts[i];
        MinOnIndex:= i;
      end;
      false: if (Counts[i] < MinOff) then begin
        MinOff:= Counts[i];
        MinOffIndex:= i;
      end;
    end;
  end;
  Display5x5(TCake.Create(MinOnIndex,0), SGMinOn);
  Display5x5(TCake.Create(MinOffIndex,0), SGMinOff);
  BtnValidateCountTable.Caption:= 'MinOn=' + MinOn.ToString + ' MinOff=' + MinOff.ToString;
end;

procedure TForm2.BtnApplyNELookupTablesClick(Sender: TObject);
var
  x, y: integer;
  Slices: TSlices11;
begin
  // Read the lookup tables
  Slices:= MySlices;
  // Get the slice data from the grid, by looking it up in the lookup table
  // Apply the North lookup table
  for x:= 0 to 15 do begin
    for y:= -1 to 15 - 1 do begin
      Slices[x, y + 1]:= Slices[x, y + 1] and FutureGridToPastSlice(StringGrid1, x, y, LookupTable, oNorth);
      StringGrid3.Cells[x, y + 1]:= '';
    end;
  end;
  // Apply the East lookup table
  for x:= 1 to 15 + 1 do begin
    for y:= 0 to 15 do begin
      Slices[x - 1, y]:= Slices[x - 1, y] and FutureGridToPastSlice(StringGrid1, x, y, LookupTable, oEast);
      StringGrid3.Cells[x - 1, y]:= '';
    end;
  end;
  // Apply the South lookup table
  for x:= 0 to 15 do begin
    for y:= 1 to 15 + 1 do begin
      Slices[x, y - 1]:= Slices[x, y - 1] and FutureGridToPastSlice(StringGrid1, x, y, LookupTable, oSouth);
      StringGrid3.Cells[x, y - 1]:= '';
    end;
  end;
  // Apply the West lookup table
  for x:= -1 to 15 - 1 do begin
    for y:= 0 to 15 do begin
      Slices[x + 1, y]:= Slices[x + 1, y] and FutureGridToPastSlice(StringGrid1, x, y, LookupTable, oWest);
      StringGrid3.Cells[x + 1, y]:= '';
    end;
  end;
  DisplaySlices(StringGrid2, StringGrid3, Slices);
  // Take every slice and confront it with its neighbors to the west, east, north and south.
  MySlices:= Slices;
end;

procedure TForm2.BtnApplyCornerLookupTablesClick(Sender: TObject);
var
  x, y: integer;
  Slices: TSlices11;
  Filename: string;
begin
  // Read the lookup tables
  if not(LookupTable.HasCornerData) then begin
    Filename:= AppRegistry.ReadString(cCornerIndex);
    if (Filename = '') then begin
      FileOpenDialog1.Title:= 'Read Corner index';
      if not(FileOpenDialog1.Execute) then Exit;
      Filename:= FileOpenDialog1.Filename;
      AppRegistry.WriteString(cCornerIndex, Filename);
    end;
    LookupTable.LoadCornerIndex(Filename);

    Filename:= AppRegistry.ReadString(cCornerSliceData);
    if (Filename = '') then begin
      FileOpenDialog1.Title:= 'Read Corner Data';
      if not(FileOpenDialog1.Execute) then Exit;
      Filename:= FileOpenDialog1.Filename;
      AppRegistry.WriteString(cCornerSliceData, Filename);
    end;
    LookupTable.LoadCornerData(Filename);
  end;

  Slices:= MySlices;
  // Get the slice data from the grid, by looking it up in the lookup table
  // Apply the NorthEast lookup table
  for x:= 1 to 15 + 1 do begin
    for y:= -1 to 15 - 1 do begin
      Slices[x - 1, y + 1]:= Slices[x - 1, y + 1] and FutureGridToPastSlice(StringGrid1, x, y, LookupTable, oNorthEast);
      StringGrid3.Cells[x - 1, y + 1]:= '';
    end;
  end;
  // Apply the SouthEast lookup table
  for x:= 1 to 15 + 1 do begin
    for y:= 1 to 15 + 1 do begin
      Slices[x - 1, y - 1]:= Slices[x - 1, y - 1] and FutureGridToPastSlice(StringGrid1, x, y, LookupTable, oSouthEast);
      StringGrid3.Cells[x - 1, y - 1]:= '';
    end;
  end;
  // Apply the SouthWest lookup table
  for x:= -1 to 15 - 1 do begin
    for y:= 1 to 15 + 1 do begin
      Slices[x + 1, y - 1]:= Slices[x + 1, y - 1] and FutureGridToPastSlice(StringGrid1, x, y, LookupTable, oSouthWest);
      StringGrid3.Cells[x + 1, y - 1]:= '';
    end;
  end;
  // Apply the NorthWest lookup table
  for x:= -1 to 15 - 1 do begin
    for y:= -1 to 15 - 1 do begin
      Slices[x + 1, y + 1]:= Slices[x + 1, y + 1] and FutureGridToPastSlice(StringGrid1, x, y, LookupTable, oNorthWest);
      StringGrid3.Cells[x + 1, y + 1]:= '';
    end;
  end;
  DisplaySlices(StringGrid2, StringGrid3, Slices);
  // Take every slice and confront it with its neighbors to the west, east, north and south.
  MySlices:= Slices;
end;

procedure TForm2.LoadLookupTable(var LookupTable: TArray<TSlice>);
var
  FS: TFileStream;
begin
  // Read the lookup table
  if not FileOpenDialog1.Execute then Exit;
  FS:= TFileStream.Create(FileOpenDialog1.Filename, fmOpenRead);
  SetLength(LookupTable, FS.Size div SizeOf(TSlice));
  FS.Read64(TBytes(LookupTable), 0, FS.Size);
  FS.Free;
end;

procedure TForm2.BtnAppyLookupTableClick(Sender: TObject);
var
  x, y: integer;
  Slices: TSlices11; // array[0..10,0..10] of TSlice;
  // LookupTable: TArray<TSlice>;
  // FS: TFileStream;
begin
  // Read the lookup table
  // if not FileOpenDialog1.Execute then Exit;
  // FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
  // SetLength(LookupTable, FS.Size div SizeOf(TSlice));
  // FS.Read64(TBytes(LookupTable), 0, FS.Size);
  // FS.Free;
  FillChar(Slices, SizeOf(Slices), #0);
  // Get the slice data from the grid, by looking it up in the lookup table
  for x:= 0 to 17 - 2 do begin
    for y:= 0 to 17 - 2 do begin
      Slices[x, y]:= FutureGridToPastSlice(StringGrid1, x, y, LookupTable, oCenter);
      StringGrid3.Cells[x, y]:= '';
    end;
  end;
  DisplaySlices(StringGrid2, StringGrid3, Slices);
  // Take every slice and confront it with its neighbors to the west, east, north and south.
  MySlices:= Slices;
end;

procedure TForm2.BtnSolveRoundUsingChunksClick(Sender: TObject);
begin
  DoARun;
  DisplaySlices(StringGrid2, StringGrid3, MySlices);
end;

procedure TForm2.BtnClearGridClick(Sender: TObject);
var
  x, y: integer;
begin
  for y:= 0 to StringGrid1.RowCount - 1 do begin
    for x:= 0 to StringGrid1.ColCount - 1 do begin
      StringGrid1.Cells[x, y]:= '';
    end; { for x }
  end; { for y }
end;

procedure TForm2.BtnTestLookup0_012Click(Sender: TObject);
type
  TW3 = array [0 .. 3] of word;
  TW2 = array [0 .. 1] of word;
var
  w: word;
  b: integer;
  i: integer;
  i64: uint64;
  // w3: array[0..3] of word absolute i64;
  b2: array [0 .. 3] of byte;
  // w2: array[0..1] of word absolute b2;
begin
  // Test every possible lookup0 and its reverse.
  for i:= 0 to 255 do begin
    w:= TSuperSlice.Lookup0[i];
    b:= TSuperSlice.LookupRemove0[w];
    if (b <> i) then begin
      ShowMessage('Oops');
    end;
  end;
  for i:= 0 to 255 do begin
    i64:= TSuperSlice.Lookup012[i];
    b2[0]:= TSuperSlice.LookupRemove0[TW3(i64)[0]];
    b2[1]:= TSuperSlice.LookupRemove0[TW3(i64)[1]];
    b2[2]:= TSuperSlice.LookupRemove0[TW3(i64)[2]];
    b2[3]:= TSuperSlice.LookupRemove0[TW3(i64)[3]];
    b2[0]:= TSuperSlice.LookupRemove0[TW2(b2)[0]];
    b2[1]:= TSuperSlice.LookupRemove0[TW2(b2)[1]];
    b:= TSuperSlice.LookupRemove0[TW2(b2)[0]];
    if (b <> i) then begin
      ShowMessage('Oops');
    end;
  end;
end;

procedure TForm2.BtnMinimalSolveClick(Sender: TObject);
var
  x, y: integer;
  Slices: TSlices11; // array[0..10,0..10] of TSlice;
begin
  FillChar(Slices, SizeOf(Slices), #0);
  for x:= 0 to 17 - 2 do begin
    for y:= 0 to 17 - 2 do begin
      Slices[x, y]:= FutureGridToPastSliceSimple(StringGrid1, x, y, LookupTable);
      StringGrid3.Cells[x, y]:= '';
    end;
  end;
  DisplaySlices(StringGrid2, StringGrid3, Slices);
  // Take every slice and confront it with its neighbors to the west, east, north and south.
  MySlices:= Slices;
end;

// orig    counter   clock    diagonal other diag
// 012     258       630      036      852
// 345     147       741      147      741
// 678     036       852      258      630

// orig    counter
// 210     036
// 543     147
// 876     258

procedure TForm2.Button8Click(Sender: TObject);
var
  Ordering: TArray<integer>;
begin
  SetLength(Ordering, 9);
  // Ordering[0]:= 0;
  // Ordering[1]:= 1;
  // Ordering[2]:= 2;
  // Ordering[3]:= 3;
  // Ordering[4]:= 4;
  // Ordering[5]:= 5;
  // Ordering[6]:= 6;
  // Ordering[7]:= 7;
  // Ordering[8]:= 8;
  Ordering[0]:= 0;
  Ordering[1]:= 3;
  Ordering[2]:= 6;
  Ordering[3]:= 1;
  Ordering[4]:= 4;
  Ordering[5]:= 7;
  Ordering[6]:= 2;
  Ordering[7]:= 5;
  Ordering[8]:= 8;
  NewLayout:= TSlice.GetReordering(Ordering, NewLayout);
  ShowNewLayout;
end;

function TForm2.GetCounterLayout: TArray<integer>;
begin
  Button8Click(Form2);
  Result:= NewLayout;
end;

procedure TForm2.BtnSolveCounterClick(Sender: TObject);
begin
  DoARun(rCounter);
  DisplaySlices(StringGrid2, StringGrid3, MySlices);
end;



procedure TForm2.ShowNewLayout;
var
  row, col: integer;
  S: string;
begin
  for col:= 0 to 31 do begin
    for row:= 0 to 15 do begin
      if (NewLayout[col * 16 + row] = col * 16 + row) then S:= '+'
      else S:= '';
      S:= S + NewLayout[col * 16 + row].ToString;
      SGSliceLayout.Cells[col, row]:= S;
    end;
  end;
end;

procedure TForm2.InitWithGoE;
var
  SG: TStringGrid;
  x, y: integer;
  i: integer;
const
  Pattern: string = '-X-XXX-X--,' + '--X-X-X--X,' + 'X-XXX--XX-,' + '-X-XXXXX-X,' + 'X--X--XXXX,' + 'XXXX--X--X,' +
    'X-XXXXX-X-,' + '-XX--XXX-X,' + 'X--X-X-X--,' + '--X-XXX-X-.';
begin
  SG:= StringGrid1;
  x:= 5;
  y:= 5;
  i:= 1;
  while Pattern[i] <> '.' do begin
    if Pattern[i] = 'X' then SG.Cells[x, y]:= 'X';
    if Pattern[i] = ',' then begin
      x:= 5;
      Inc(y);
    end
    else Inc(x);
    Inc(i);
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  InitWithGoE;
  InitNewLayout;
end;

procedure TForm2.FormActivate(Sender: TObject);
var
  Filename: string;
begin
  //Load main lookup table + index
  if not(LookupTable.HasSliceData) then begin
    Filename:= AppRegistry.ReadString(cMainIndex);
    if (Filename = '') then begin
      FileOpenDialog1.Title:= 'open index';
      if not (FileOpenDialog1.Execute) then exit;
      Filename:= FileOpenDialog1.Filename;
      AppRegistry.WriteString(cMainIndex, Filename);
    end;
    LookupTable.LoadMainIndex(Filename);

    Filename:= AppRegistry.ReadString(cMainSliceData);
    if (Filename = '') then begin
      FileOpenDialog1.Title:= 'Open data file';
      if not(FileOpenDialog1.Execute) then exit;
      Filename:= FileOpenDialog1.Filename;
      AppRegistry.WriteString(cMainSliceData, Filename);
    end;
    LookupTable.LoadMainData(Filename);
  end;
  //Load count data
  if not(LookupTable.HasCountData) then begin
    Filename:= AppRegistry.ReadString(cCountData);
    if (Filename = '') then begin
      FileOpenDialog1.Title:= 'Open counts file';
      if not(FileOpenDialog1.Execute) then exit;
      Filename:= FileOpenDialog1.Filename;
      AppRegistry.WriteString(cCountData, Filename);
    end;
    LookupTable.LoadCountData(Filename);
  end;
  //Load unknown data
  if not(LookupTable.HasUnknownData) then begin
    Filename:= AppRegistry.ReadString(cUnknownSliceData);
    if (Filename = '') then begin
      FileOpenDialog1.Title:= 'Open unknown data file';
      if not(FileOpenDialog1.Execute) then exit;
      Filename:= FileOpenDialog1.Filename;
      AppRegistry.WriteString(cUnknownSliceData, Filename);
    end;
    LookupTable.LoadUnknownData(Filename);
  end;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  ShowNewLayout;
end;

procedure TForm2.InitNewLayout;
var
  i: integer;
begin
  SetLength(NewLayout, 512);
  for i:= 0 to 511 do NewLayout[i]:= i;
end;

function TForm2.GetStates(const Input: TSlice): TArray<integer>;
var
  i, a: integer;
  alive, dead: TSlice;
begin
  SetLength(Result, Input.PopCount);
  dead:= Input and LookupTable[oCenter, 0];
  alive:= Input and (not(LookupTable[oCenter, 0]));
  System.Assert((alive or dead) = Input);
  a:= 0;
  for i:= 0 to 511 do begin
    if (alive.IsBitSet(i)) then begin
      Result[a]:= -i;
      Inc(a);
    end;
  end;
  for i:= 0 to 511 do begin
    if (dead.IsBitSet(i)) then begin
      Result[a]:= i;
      Inc(a);
    end;
  end;
end;

procedure TForm2.StringGrid5DblClick(Sender: TObject);
var
  ForceState: integer;
  SG: TStringGrid;
  SourceSG: TStringGrid;
  ForceSlice: TSlice;
begin
  MySlices:= CloneSlices;
  // Get the index of the clicked item
  SG:= StringGrid5;
  SourceSG:= StringGrid2;
  try
    ForceState:= SG.Cells[SG.col, SG.row].ToInteger;
    ForceSlice:= MySlices[SourceSG.col, SourceSG.row];
    ForceSlice.Clear;
    ForceSlice.SetBit(ForceState);
    MySlices[SourceSG.col, SourceSG.row]:= ForceSlice;
    // DisplaySlices(StringGrid2, StringGrid3, MySlices);
    BtnSliverSolveRoundClick(Self);
  except
    { ignore }
  end;
end;

function TForm2.GetPixelChar(x, y: integer; SG: TStringGrid): Char;
begin
  Result:= (SG.Cells[SG.col, SG.row] + ' ')[1];
end;

procedure TForm2.StringGrid1DblClick(Sender: TObject);
var
  a: Char;
  SG: TStringGrid;
  R,C: integer;
begin
  SG:= StringGrid1;
  for R := SG.Selection.Top to SG.Selection.Bottom do begin
    for C:= SG.Selection.Left to SG.Selection.Right do begin
      a:= GetPixelChar(C, R, SG);
      case a of
        'X': SG.Cells[C, R]:= '?';
        '?': SG.Cells[C, R]:= ' ';
        ' ': SG.Cells[C, R]:= 'X';
      end;
    end; {for C}
  end; {for R}
end;

procedure TForm2.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  StringGrid1DblClick(Sender);
end;

procedure TForm2.StringGrid2Click(Sender: TObject);
var
  x, y: integer;
  CurrentSlice: TSlice;
  i: integer;
  States: TArray<integer>;
  SG: TStringGrid;

  procedure DisplayStates(OutSG: TStringGrid);
  var
    x, y, a: integer;
  begin
    if (OutSG.RowCount = 1) then begin
      OutSG.ColCount:= Length(States);
    end;
    a:= 0;
    for y:= 0 to OutSG.RowCount - 1 do begin
      for x:= 0 to OutSG.ColCount - 1 do begin
        OutSG.Cells[x, y]:= '';
        if (a >= Length(States)) then continue;
        OutSG.Cells[x, y]:= States[a].ToString;
        Inc(a);
      end; { for y }
    end; { for x }
  end;

begin
  SG:= StringGrid2;

  x:= SG.col;
  y:= SG.row;
  CurrentSlice:= MySlices[x, y];
  for i:= 0 to 8 do begin
    SGDead.Cells[i mod 3, i div 3]:= CurrentSlice.CountDead(i).ToString;
    SGAlive.Cells[i mod 3, i div 3]:= CurrentSlice.CountAlive(i).ToString;
  end;
  States:= GetStates(CurrentSlice);
  DisplayStates(StringGrid4);
  DisplayStates(StringGrid5);
  CloneSlices:= MySlices;
end;

procedure TForm2.StringGrid2DrawCell(Sender: TObject; ACol, ARow: integer; Rect: TRect; State: TGridDrawState);
var
  C: TCanvas;
  CellText: string;
  OldAlignment: word;
  Shift: integer;
begin
  if (Sender <> StringGrid1) then Shift:= 2 else Shift:= 0;
  Rect.Offset(-2, 0);
  Rect.Inflate(2, 0);
  C:= TStringGrid(Sender).Canvas;
  if (StringGrid1.Cells[ACol + Shift, ARow + Shift] = '?') then begin
    C.Brush.Color:= RGB(255, 255, 160);
  end else if (StringGrid1.Cells[ACol + Shift, ARow + Shift] = 'X') then begin
    C.Brush.Color:= RGB(255, 200, 200);
  end else begin
    C.Brush.Color:= clWhite;
  end;
  C.FillRect(Rect);
  CellText:= TStringGrid(Sender).Cells[ACol, ARow];
  if (CellText.Contains('-')) then C.Font.Color:= clRed
  else C.Font.Color:= clBlack;
  if (CellText = '0') then C.Font.Color:= clGreen;
  OldAlignment:= SetTextAlign(C.Handle, TA_CENTER);
  // C.TextOut( Rect.Left+2, Rect.Top+2, CellText );
  C.TextRect(Rect, Rect.Left + (Rect.Width div 2) - 1, Rect.Top + 4, CellText);
  SetTextAlign(C.Handle, OldAlignment);
end;

procedure TForm2.StringGrid3Click(Sender: TObject);
var
  x, y: integer;
  CurrentSlice: TSlice;
  i: integer;
begin
  x:= StringGrid2.col;
  y:= StringGrid2.row;
  CurrentSlice:= (MySlices[x, y] xor OldSlices[x, y]);
  for i:= 0 to 8 do begin
    SGDeadDiff.Cells[i mod 3, i div 3]:= CurrentSlice.CountDead(i).ToString;
    SGAliveDiff.Cells[i mod 3, i div 3]:= CurrentSlice.CountAlive(i).ToString;
  end;
end;

procedure TForm2.StringGrid4DrawCell(Sender: TObject; ACol, ARow: integer; Rect: TRect; State: TGridDrawState);
var
  SG: TStringGrid;
  value: integer;
  S: string;
  x, y: integer;
  C: TCanvas;
  PixelRect: TRect;
  OldColor: TColor;
  Colors: array [boolean, 0 .. 2] of TColor;
  Selected: boolean;
  MyLabel: string;
begin
  // Darker colors for selection
  Colors[false, 1]:= clNavy;
  Colors[true, 1]:= RGB(0, 0, 85);
  Colors[false, 0]:= clOlive;
  Colors[true, 0]:= RGB(85, 85, 0);
  Colors[false, 2]:= clAqua;
  Colors[true, 2]:= RGB(0, 213, 213);

  Selected:= gdSelected in State;
  Rect.Offset(-2, 0);
  Rect.Inflate(0, -2);
  SG:= TStringGrid(Sender);
  C:= SG.Canvas;
  S:= SG.Cells[ACol, ARow];
  // C.Pen.Style:= psDot;
  // C.Pen.Color:= clLtGray;
  // for x:= 1 to 2 do for y:= 1 to 2 do begin
  // C.MoveTo(Rect.Left+x*12, Rect.Top);
  // C.LineTo(Rect.Left+x*12, Rect.Top+36);
  // C.MoveTo(Rect.Left, Rect.Top+y*12);
  // C.LineTo(Rect.Left+36, Rect.Top+y*12);
  // end;
  if (S <> '') then begin
    OldColor:= C.Brush.Color;
    value:= ABS(S.ToInteger);
    MyLabel:= Char(Ord('A')+(value mod 26)) + Char(Ord('A')+(value div 26));
    C.Brush.Color:= Colors[Selected, 2];
    C.FillRect(Rect);
    if (value and 16) <> 0 then C.Brush.Color:= Colors[Selected, 1]
    else C.Brush.Color:= Colors[Selected, 0];
    if (value <> -1) then begin
      for y:= 0 to 2 do begin
        for x:= 2 downto 0 do begin
          if (Odd(value)) then begin
            PixelRect:= System.Classes.Rect(Rect.Left, Rect.Top, Rect.Left + 12, Rect.Top + 12);
            PixelRect.Offset(x * 12, y * 12);
            C.FillRect(PixelRect);
          end;
          value:= value shr 1;
        end; { for x }
      end; { for y }
    end;
    C.Brush.Color:= OldColor;
    SetBkMode(C.handle, TRANSPARENT);
    OldColor:= C.Font.Color;
    C.Font.Color:= clBlack;
    Rect.Offset(-1,-1);
    DrawTextEx(C.handle,PWideChar(MyLabel),-1,Rect,DT_CENTER or DT_VCENTER or DT_SINGLELINE,nil);
    Rect.Offset(2,2);
    DrawTextEx(C.handle,PWideChar(MyLabel),-1,Rect,DT_CENTER or DT_VCENTER or DT_SINGLELINE,nil);
    Rect.Offset(-2,0);
    DrawTextEx(C.handle,PWideChar(MyLabel),-1,Rect,DT_CENTER or DT_VCENTER or DT_SINGLELINE,nil);
    Rect.Offset(2,-2);
    DrawTextEx(C.handle,PWideChar(MyLabel),-1,Rect,DT_CENTER or DT_VCENTER or DT_SINGLELINE,nil);
    C.Font.Color:= OldColor;
    Rect.Offset(-1,1);
    DrawTextEx(C.handle,PWideChar(MyLabel),-1,Rect,DT_CENTER or DT_VCENTER or DT_SINGLELINE,nil);
  end; { if PaintNeeded }
end;

{ TSlice }

class operator TSlice.GreaterThan(const a, b: TSlice): boolean;
var
  i: integer;
begin
  for i:= 0 to high(a.Data8) do begin
    if a.Data8[i] > b.Data8[i] then Exit(true);
    if a.Data8[i] < b.Data8[i] then Exit(false);
  end;
  Result:= false;
end;

function TSlice.IsBitSet(Bit: integer): boolean;
begin
  System.Assert((Bit >= 0) and (Bit <= 511));
  Result:= (Self.Data4[Bit shr 5] and (1 shl (31 and Bit)) <> 0);
end;

function TSlice.IsZero: boolean;
begin
  Result:= ((Data8[0] or Data8[1] or Data8[2] or Data8[3] or Data8[4] or Data8[5] or Data8[6] or Data8[7]) = 0);
end;

class operator TSlice.LessThan(const a, b: TSlice): boolean;
var
  i: integer;
begin
  for i:= 0 to high(a.Data8) do begin
    if a.Data8[i] > b.Data8[i] then Exit(false);
    if a.Data8[i] < b.Data8[i] then Exit(true);
  end;
  Result:= false;
end;

function TSlice.NextSetBit(previous: integer): integer;
asm
  // EDX: previous
  // xor r9,r9
  mov eax,512           // assume failure
  cmp edx,511           // are we all done?
  jae @done             // yes, done
  mov r10,rcx           // We need cl for shifting
  inc edx               // we're looking for the next bit
  mov ecx,edx           // shift out the bits already processed
  and ecx,63            // only take 0..63 into account so we don't upset the offset calc
  // Get the relevant int64
  shr edx,6             // Get the revelant 64 bit section
  lea r8,[rdx*8]        // but note that that 64 bits = 8 bytes (rounded down).
@repeat:
  mov rax,[r10+r8]      // get the next section to investigate
  shr rax,cl            // shift out the bits we've already looked at
  rep bsf rax,rax       // get the next set bit, CF=0 if we found it
  lea eax,[eax+ecx]     // add the bitcount shifted out back in
  // if eax=64 then all bits set in our section are clear.
  // If r8d = 7 then we are done, if not we need to look further.
  lea rax,[rax+r8*8]    // Add the section offset back in.
  jnc @done             // We found a bit
  // Oops, CF=0, meaning the section is empty, investigate the next section.
  xor ecx,ecx           // reset at the start of the next session
  cmp r8d,64-8          // Did we investigate all sections? ZF=1 if true
  lea r8,[r8+8]         // Let's prepare for a new round
  jne @repeat
@done:

end;

class operator TSlice.NotEqual(const a, b: TSlice): boolean;
begin
  Result:= not(a = b);
end;

function TSlice.PopCount: integer;
asm
  // self = rcx
  // output = eax
  popcnt rdx,[rcx]
  popcnt rax,[rcx+8]
  popcnt r8,[rcx+16]
  popcnt r9,[rcx+24]
  add edx,eax
  add r8,r9
  add edx,r8d
  mov r10,rdx
  popcnt rdx,[rcx+32]
  popcnt rax,[rcx+40]
  popcnt r8,[rcx+48]
  popcnt r9,[rcx+56]
  add edx,eax
  add r8,r9
  add edx,r8d
  lea eax,[edx+r10d]
end;

function TSlice.Print: string;
var
  i, j, a: integer;
begin
  for i:= 0 to high(Data4) do begin
    if Data4[i] <> 0 then begin
      a:= Data4[i];
      for j:= 0 to 31 do begin
        a:= a shr 1;
        if a = 0 then Exit((i * 32 + j).ToString);
      end; { for j }
    end;
  end; { for i }
end;

class function TSlice.Print5x5(item: integer): string;
var
  i, Index: integer;
begin
  Result:= '00000-00000-00000-00000-00000';
  index:= 1;
  for i:= 1 to 25 do begin
    if Odd(item) then Result[index]:= '1';
    Inc(index);
    item:= item shr 1;
    if i mod 5 = 0 then Inc(index);
  end;
end;

procedure TSlice.ReorderSlice(const Reordering: TArray<integer>);
var
  i: integer;
  Original: TSlice;
begin
  Original:= Self;
  FillChar(Self, SizeOf(Self), #0);
  for i:= 0 to 511 do begin
    if Original.GetBit(i) then Self.SetBit(Reordering[i]);
  end;
end;

procedure TSlice.SetBit(Index: integer; value: boolean = true);
var
  One: uint64;
  Mask: uint64;
  element: integer;
begin
  One:= 1;
  index:= ABS(index);
  Mask:= (One shl (index and 63));
  element:= index div 64;
  if (value = false) then Data8[element]:= Data8[element] and not Mask
  else Data8[element]:= Data8[element] or Mask;
end;

procedure TSlice.ForceSingleBit(Index: integer);
begin
  FillChar(Self, SizeOf(TSlice), 0);
  Self.SetBit(index);
end;

class operator TSlice.BitwiseAnd(const a, b: TSlice): TSlice;
var
  i: integer;
begin
  for i:= 0 to 7 do begin
    Result.Data8[i]:= a.Data8[i] and b.Data8[i];
  end;
end;

class operator TSlice.LogicalNot(const a: TSlice): TSlice;
var
  i: integer;
begin
  for i:= 0 to high(a.Data8) do begin
    Result.Data8[i]:= not(a.Data8[i]);
  end;
end;

class operator TSlice.BitwiseOr(const a, b: TSlice): TSlice;
var
  i: integer;
begin
  for i:= 0 to 7 do begin
    Result.Data8[i]:= a.Data8[i] or b.Data8[i];
  end;
end;

class operator TSlice.BitwiseXor(const a, b: TSlice): TSlice;
var
  i: integer;
begin
  for i:= 0 to 7 do begin
    Result.Data8[i]:= a.Data8[i] xor b.Data8[i];
  end;
end;

procedure TSlice.Clear;
begin
  FillChar(Self, SizeOf(TSlice), #0);
end;

function TSlice.CountAlive(Pixel: integer): integer;
begin
  System.Assert((Pixel >= 0) and (Pixel <= 8));
  Result:= (Self and not(OffMask[Pixel])).PopCount;
end;

function TSlice.CountDead(Pixel: integer): integer;
begin
  System.Assert((Pixel >= 0) and (Pixel <= 8));
  Result:= (Self and (OffMask[Pixel])).PopCount;
end;

class function TSlice.FullyUnknown: TSlice;
begin
  FillChar(Result, SizeOf(Result), $FF);
end;

class operator TSlice.Equal(const a, b: TSlice): boolean;
var
  i: integer;
begin
  for i:= 0 to high(a.Data8) do begin
    if a.Data8[i] <> b.Data8[i] then Exit(false);
  end;
  Result:= true;
end;

{ TSliceComparer<TSlice> }

function TSliceComparer.Compare(const Left, Right: TSlice): integer;
begin
  // Result is less than zero (<0)
  // Left is less than Right.
  //
  // Result is equal to zero (=0)
  // Left is equal to Right.
  //
  // Result is greater than zero (>0)
  // Left is greater than Right.
  if Left > Right then Exit(-1);
  if Left < Right then Exit(1)
  else Result:= (0);
end;

{ TSuperSlice }

class operator TSuperSlice.BitwiseAnd(const a, b: TSuperSlice): TSuperSlice;
var
  i: integer;
begin
  for i:= 0 to high(a.Data8) do begin
    Result.Data8[i]:= a.Data8[i] and b.Data8[i];
  end;
end;

class operator TSuperSlice.BitwiseOr(const a, b: TSuperSlice): TSuperSlice;
var
  i: integer;
begin
  for i:= 0 to high(a.Data8) do begin
    Result.Data8[i]:= a.Data8[i] or b.Data8[i];
  end;
end;

class constructor TSuperSlice.init;
type
  TB = uint64;
var
  i, j: integer;
  a: array [0 .. 7] of byte;
  // b: uint64 absolute a;
begin
  for i:= 0 to 255 do begin
    // every bit gets broadcasted 8 times.
    for j:= 0 to 7 do begin
      if Odd(i shr j) then a[j]:= $FF
      else a[j]:= 0;
    end;
    Lookup012[i]:= TB(a);
    Lookup0[i]:= ((i and 001) shl 0) or ((i and 001) shl 1) xor ((i and 002) shl 1) or ((i and 002) shl 2)
      xor ((i and 004) shl 2) or ((i and 004) shl 3) xor ((i and 008) shl 3) or ((i and 008) shl 4)
      xor ((i and 016) shl 4) or ((i and 016) shl 5) xor ((i and 032) shl 5) or ((i and 032) shl 6)
      xor ((i and 064) shl 6) or ((i and 064) shl 7) xor ((i and 128) shl 7) or ((i and 128) shl 8);

    // Double every nibble into a byte
    Lookup2[i]:= ((i and $0F) shl 0) or ((i and $0F) shl 4) xor ((i and $F0) shl 4) or ((i and $F0) shl 8);

  end; { for i }
  for i:= 0 to 256 * 256 - 1 do begin
    LookupRemove0[i]:= ((i and 001) shr 0) or ((i and 002) shr 1) xor ((i and 004) shr 1) or ((i and 008) shr 2)
      xor ((i and 016) shr 2) or ((i and 032) shr 3) xor ((i and 064) shr 3) or ((i and 128) shr 4)
      xor ((i and 256) shr 4) or ((i and 512) shr 5) xor ((i and 1024) shr 5) or ((i and 2048) shr 6)
      xor ((i and 4096) shr 6) or ((i and 8192) shr 7) xor ((i and 16384) shr 7) or ((i and 32768) shr 8);
  end;
  for i:= 0 to 256 * 256 - 1 do begin
    LookupRemove2[i]:= ((i shr 0) and $0F) or ((i shr 4) and $0F) or ((i shr 4) and $F0) or ((i shr 8) and $F0);
  end;
end;

class function TSuperSlice.NS(const North, South: TSlice): TSuperSlice;
var
  i: integer;
  Temp: TSuperSlice;
begin
  // Add pixels 012 to the South Slice.
  for i:= 0 to 63 do begin
    Temp.Data8[i]:= Lookup012[South.bytes[i]];
  end;
  // Write out the North Slice 8 times
  for i:= 0 to 7 do begin
    Move(North, Result.Slices[i], SizeOf(TSlice));
  end;
  Result:= Result and Temp;
end;

class function TSuperSlice.EW(const East, West: TSlice): TSuperSlice;
var
  i: integer;
  W1, W2: TSuperSlice;
  E1, E2: TSuperSlice;
begin
  // Add pixels 036 to the east slice
  // Add pixel 6 by doubling every int64
  // We start with 64 bytes = 8 int64's
  for i:= 0 to 7 do begin
    W1.Data8[i * 2]:= West.Data8[i];
    W1.Data8[i * 2 + 1]:= West.Data8[i];
  end;
  // add pixel 3 by doubling every byte;
  // Start with 128 bytes
  for i:= 0 to 127 do begin
    W2.Words[i]:= W1.bytes[i] or (W1.bytes[i] shl 8);
  end;
  // Finally add pixel 0
  // Start with 256 bytes
  for i:= 0 to 255 do begin
    W1.Words[i]:= Lookup0[W2.bytes[i]];
  end;

  // Now add pixel 3,7,11 to the west slice
  // Add pixel 6 (later to be 7 after adding in 3) by doubling every int64
  // Start with 64 bytes = 8 int64's
  for i:= 0 to 7 do begin // Expand from 64 bytes to 128 bytes
    E1.Data8[i * 2]:= East.Data8[i];
    E1.Data8[i * 2 + 1]:= East.Data8[i];
  end;
  // add pixel 3 by doubling every byte;
  // Start with 128 bytes
  for i:= 0 to 127 do begin
    E2.Words[i]:= E1.bytes[i] or (E1.bytes[i] shl 8);
  end;
  // Finally add pixel 11 by doubling the slice
  // Start with 256 bytes
  Move(E2, Result.Data8[0], 256);
  Move(E2, Result.Data8[32], 256);
  Result:= Result and W1;
end;

{ TSuperSliceHelper }

function TSuperSlice.West: TSlice;
var
  i: integer;
  Temp1, Temp2: TSuperSlice;
begin
  // Remove pixel 048
  // Next remove pixel0 by oring every bit with its neighbor
  // Start with 512 bytes
  for i:= 0 to 255 do begin
    Temp1.bytes[i]:= LookupRemove0[Self.Words[i]];
  end;
  // Now pixels 48 renumber to 37
  // Next remove pixel 3 by oring every byte with its neighbor
  // Start with 256 bytes
  for i:= 0 to 127 do begin
    Temp2.bytes[i]:= Temp1.bytes[i * 2] or Temp1.bytes[i * 2 + 1];
  end;
  // Pixel 7 renumbers again to 6.
  // first remove pixel 6 by oring every int64 with its neighbor
  // Start with 128 bytes = 16 int64's
  for i:= 0 to 7 do begin
    Result.Data8[i]:= Temp2.Data8[i * 2] or Temp2.Data8[i * 2 + 1];
  end;
end;

class operator TSuperSlice.Equal(const a, b: TSuperSlice): boolean;
var
  i: integer;
begin
  for i:= 0 to high(a.Data8) do begin
    if (a.Data8[i] <> b.Data8[i]) then Exit(false);
  end;
  Result:= true;
end;

function TSuperSlice.North: TSlice;
var
  i: integer;
begin
  // Remove pixel 9-10-11 by oring the 8 parts of a slice on top of each other.
  Result:= Self.Slices[0];
  for i:= 1 to 7 do begin
    Result:= Result or Self.Slices[i];
  end;
end;

class operator TSuperSlice.NotEqual(const a, b: TSuperSlice): boolean;
begin
  Result:= not(a = b);
end;

function TSuperSlice.South: TSlice;
var
  i: integer;
  b: byte;
begin
  FillChar(Result, SizeOf(Result), #0);
  // Remove pixel 012 by collapsing every byte into a bit.
  for i:= 0 to 63 do begin
    b:= 0;
    if (Self.bytes[i * 8 + 0] <> 0) then b:= 1;
    if (Self.bytes[i * 8 + 1] <> 0) then Inc(b, 2);
    if (Self.bytes[i * 8 + 2] <> 0) then Inc(b, 4);
    if (Self.bytes[i * 8 + 3] <> 0) then Inc(b, 8);
    if (Self.bytes[i * 8 + 4] <> 0) then Inc(b, 16);
    if (Self.bytes[i * 8 + 5] <> 0) then Inc(b, 32);
    if (Self.bytes[i * 8 + 6] <> 0) then Inc(b, 64);
    if (Self.bytes[i * 8 + 7] <> 0) then Inc(b, 128);
    Result.bytes[i]:= b;
    //
    //
    // Result.bytes[i]:= 1 and (byte(Self.Bytes[i*8+0] <> 0) shl 0) or
    // 2 and (byte(Self.Bytes[i*8+1] <> 0) shl 1) or
    // 4 and (byte(Self.Bytes[i*8+2] <> 0) shl 2) or
    // 8 and (byte(Self.Bytes[i*8+3] <> 0) shl 3) or
    // 16 and (byte(Self.Bytes[i*8+4] <> 0) shl 4) or
    // 32 and (byte(Self.Bytes[i*8+5] <> 0) shl 5) or
    // 64 and (byte(Self.Bytes[i*8+6] <> 0) shl 6) or
    // 128 and (byte(Self.Bytes[i*8+7] <> 0) shl 7);
  end;
end;

function TSuperSlice.East: TSlice;
var
  i: integer;
  Temp1, Temp2: TSuperSlice;
begin
  // Remove bytes 3, 7 and 11
  // First remove 3, (renumbering 7-11 to 6-10) by oring every byte with its neighbor
  // Start with 512 bytes
  for i:= 0 to 255 do begin
    Temp1.bytes[i]:= Self.bytes[i * 2] or Self.bytes[i * 2 + 1];
  end;
  // Now remove 6 (was 7) by oring every int64 with its neighbor
  // Start with 256 bytes = 32 int64's
  for i:= 0 to 15 do begin
    Temp2.Data8[i]:= Temp1.Data8[i * 2] or Temp1.Data8[i * 2 + 1];
  end;
  // Start with 128 bytes = 2 slices
  // Now remove 9 (was 11, then 10) by folding the remaining two slices together.
  Result:= Temp2.Slices[0] or Temp2.Slices[1];
end;

{ TSliver }

class operator TSliver.BitwiseAnd(const a, b: TSliver): TSliver;
begin
  Result.Data8:= a.Data8 and b.Data8;
end;

class operator TSliver.BitwiseOr(const a, b: TSliver): TSliver;
begin
  Result.Data8:= a.Data8 or b.Data8;
end;

class operator TSliver.Equal(const a, b: TSliver): boolean;
begin
  Result:= a.Data8 = b.Data8;
end;

class operator TSliver.NotEqual(const a, b: TSliver): boolean;
begin
  Result:= a.Data8 <> b.Data8;
end;

class function TSliverHelper.NS(const North, South: TSlice; var Changed: TSliverChanges): TSliver;
var
  N, S: TSliver;
  i: integer;
begin
  // First take the north slice and remove pixels 0,1,2
  for i:= 0 to 7 do begin
    // Remove pixel 012 by collapsing every byte into a bit.
    N.bytes[i]:= Remove012(North.Data8[i]);
  end;
  // Next remove pixels 678 from South
  // Do this by folding 8 slivers
  S:= South.Sliver[0];
  for i:= 1 to 7 do begin
    S:= S or South.Sliver[i];
  end;
  // Conjunct the two slivers
  Result:= N and S;
  Changed:= Changed or (N <> S); // or 1 if changes
  Changed:= Changed and Result; // or 2 if invalid
end;

class function TSliverHelper.NWSE(const NorthWest, SouthEast: TSlice; var Changed: TSliverChanges): TSliver;
var
  NW, SE: TSliver;
  i: integer;
begin
  // First take the NW slice and remove pixel 0,1,2
  for i:= 0 to 7 do begin
    NW.bytes[i]:= Remove012(NorthWest.Data8[i]);
  end;
  // Now we are left with 6 pixels in the following layout
  // 210
  // 543
  // The west pixels need to be removed
  // First remove pixel 5, this means folding 2 dword
  // This reduces the sliver to 4 bytes
  NW.Data4[0]:= NW.Data4[0] or NW.Data4[1];
  // Finally remove pixel 2
  // This reduces the sliver to a 16 bits.
  NW.bytes[0]:= TSuperSlice.LookupRemove2[NW.Data2[0]];
  NW.bytes[1]:= TSuperSlice.LookupRemove2[NW.Data2[1]];
  // Next we process the SE slice
  // First remove 678
  SE.Data8:= SouthEast.Data8[0];
  for i:= 1 to 7 do begin
    SE.Data8:= SE.Data8 or SouthEast.Data8[i];
  end;
  // Now we are left with 6 pixels in the following layout
  // 210
  // 543
  // We need to remove the eastern most pixels
  // First remove pixel 3 by folding every byte with its neighbor
  // This reduces the sliver from 8 to 4 bytes
  for i:= 0 to 3 do begin
    SE.bytes[i]:= SE.bytes[i * 2] or SE.bytes[i * 2 + 1];
  end;
  // The last step is to remove bit 0.
  // This reduces the sliver to 2 bytes.
  SE.bytes[0]:= TSuperSlice.LookupRemove0[SE.Data2[0]];
  SE.bytes[1]:= TSuperSlice.LookupRemove0[SE.Data2[1]];
  // Return the combined minislivers
  Result:= SE and NW;
  Changed:= Changed or (SE <> NW); // or 1 if changed
  Changed:= Changed and Result; // or 2 if invalid
end;

class function TSliverHelper.EW(const East, West: TSlice; var Changed: TSliverChanges): TSliver;
var
  E, w: TSliver;
  Temp1, Temp2: TSlice;
  i: integer;
begin
  // West: drop pixels 2,5,8
  // first eliminate pixel 5
  // This reduces the size from 64 bytes = 16ints to 32 bytes = 8 ints
  for i:= 0 to 7 do begin
    Temp1.Data4[i]:= West.Data4[i * 2] or West.Data4[i * 2 + 1];
  end;
  // Next eliminate pixel 2.
  // This means folding every nibble with its neighbor
  // this reduces the size from 32 bytes into 16 bytes
  for i:= 0 to 15 do begin
    Temp2.bytes[i]:= TSuperSlice.LookupRemove2[Temp1.Data2[i]];
  end;
  // Eliminate pixel 8
  // Finally fold the two remaining slivers into one.
  // This reduces the size from 16 bytes to 8 bytes
  w:= Temp2.Sliver[0] or Temp2.Sliver[1];

  // Next create E by dropping pixels 0,3,6
  // First remove pixel6
  // This or's every int64 with its neighbor
  // reducing 64 (8 int64's) to 32 (4 int64's) bytes.
  for i:= 0 to 3 do begin
    Temp1.Data8[i]:= East.Data8[i * 2] or East.Data8[i * 2 + 1];
  end;
  // first remove pixel3
  // This or's every byte with its neighbor
  // We start with 32 bytes and reduce it to 16.
  for i:= 0 to 15 do begin
    Temp2.bytes[i]:= Temp1.bytes[i * 2] or Temp1.bytes[i * 2 + 1]
  end;
  // Finally remove pixel0.
  // This reduces from 16 bytes to 8
  for i:= 0 to 7 do begin
    E.bytes[i]:= TSuperSlice.LookupRemove0[Temp2.Data2[i]];
  end;
  Result:= w and E;
  Changed:= Changed or (w <> E); // or 1 if changed
  Changed:= Changed and Result; // or 2 if invalid
end;

function TSliverHelper.West: TSlice;
var
  Temp1, Temp2: TSlice;
  i: integer;
begin
  // Add pixels 2,5,8
  // First add pixel 2.
  // Expanding 8 into 16 bytes
  for i:= 0 to 7 do begin
    Temp1.Data2[i]:= TSuperSlice.Lookup2[Self.bytes[i]];
  end;
  // Now add pixel 5
  // Double every uint32
  // Expanding 16 bytes (=4 int32) into 32 bytes
  for i:= 0 to 3 do begin
    Temp2.Data4[i * 2]:= Temp1.Data4[i];
    Temp2.Data4[i * 2 + 1]:= Temp1.Data4[i];
  end;
  // Finally add pixel 8
  // Just copy the half-slice twice.
  // Expanding 32 bytes into 64.
  Move(Temp2, Result, 32);
  Move(Temp2, (@Result.bytes[32])^, 32);
end;

class function TSliverHelper.NESW(const NorthEast, SouthWest: TSlice; var Changed: TSliverChanges): TSliver;
var
  NE, SW: TSliver;
  i: integer;
begin
  // First take the NE slice and remove pixel 0,1,2
  for i:= 0 to 7 do begin
    NE.bytes[i]:= Remove012(NorthEast.Data8[i]);
  end;
  // Now we are left with 6 pixels in the following layout
  // 210
  // 543
  // The east pixels need to be removed
  // First remove pixel 3 by folding every byte with its neighbor
  // This reduces the sliver from 8 to 4 bytes
  for i:= 0 to 3 do begin
    NE.bytes[i]:= NE.bytes[i * 2] or NE.bytes[i * 2 + 1];
  end;
  // The last step is to remove bit 0.
  // This reduces the sliver to 2 bytes.
  NE.bytes[0]:= TSuperSlice.LookupRemove0[NE.Data2[0]];
  NE.bytes[1]:= TSuperSlice.LookupRemove0[NE.Data2[1]];
  // Next we process the SW slice
  // First remove 678
  SW.Data8:= SouthWest.Data8[0];
  for i:= 1 to 7 do begin
    SW.Data8:= SW.Data8 or SouthWest.Data8[i];
  end;
  // Now we are left with 6 pixels in the following layout
  // 210
  // 543
  // First remove pixel 5, this means folding every dword
  // This reduces the sliver to 4 bytes
  SW.Data4[0]:= SW.Data4[0] or SW.Data4[1];
  // Finally remove pixel 2
  // This reduces the sliver to a 16 bits.
  SW.bytes[0]:= TSuperSlice.LookupRemove2[SW.Data2[0]];
  SW.bytes[1]:= TSuperSlice.LookupRemove2[SW.Data2[1]];
  // Combine the two minislivers
  Result:= SW and NE;
  Changed:= Changed or (SW <> NE);
  Changed:= Changed and Result;
end;

function TSliverHelper.North: TSlice;
var
  i: integer;
begin
  // Add pixels 0,1,2
  // This means expanding every bit into a byte
  // Or rather every byte into an int64;
  for i:= 0 to 7 do begin
    Result.Data8[i]:= TSuperSlice.Lookup012[Self.bytes[i]];
  end;
end;

function TSliverHelper.East: TSlice;
var
  i: integer;
  Temp1, Temp2: TSlice;
begin
  // Add pixel 0, 3, 6
  // First add pixel 0.
  // This expands the sliver from 8 to 16 bytes
  for i:= 0 to 7 do begin
    Temp1.Data2[i]:= TSuperSlice.Lookup0[Self.bytes[i]];
  end;
  // Next add pixel 3, double every byte
  // this expands the sliver from 16 to 32 bytes
  for i:= 0 to 15 do begin
    Temp2.bytes[i * 2]:= Temp1.bytes[i];
    Temp2.bytes[i * 2 + 1]:= Temp1.bytes[i];
  end;
  // Finally add pixel 6, his doubles every int64.
  // Expanding the slice from 32 (= 4 int64) into 64 bytes (=8int64)
  for i:= 0 to 3 do begin
    Result.Data8[i * 2]:= Temp2.Data8[i];
    Result.Data8[i * 2 + 1]:= Temp2.Data8[i];
  end;
end;

function TSliverHelper.NorthEast: TSlice;
var
  Temp1, Temp2: TSlice;
  i: integer;
begin
  // We start with
  // ---   ---
  // 10-   BA-
  // 32-   DC-
  // Add pixels 0,1,2
  // This means expanding every bit into a byte
  // Or rather every byte into an int64;
  // This expands 2 bytes into 16 bytes
  for i:= 0 to 1 do begin
    Temp1.Data8[i]:= TSuperSlice.Lookup012[Self.bytes[i]];
  end;
  // Now we have
  // 210   210
  // BA-   43-
  // DC-   65-
  // We already have pixel 0, so we only need to add pixel 3 and 6.
  // Next add pixel 3, double every byte
  // this expands the sliver from 16 to 32 bytes
  for i:= 0 to 15 do begin
    Temp2.bytes[i * 2]:= Temp1.bytes[i];
    Temp2.bytes[i * 2 + 1]:= Temp1.bytes[i];
  end;
  // Finally add pixel 6, his doubles every int64.
  // Expanding the slice from 32 (= 4 int64) into 64 bytes (=8int64)
  for i:= 0 to 3 do begin
    Result.Data8[i * 2]:= Temp2.Data8[i];
    Result.Data8[i * 2 + 1]:= Temp2.Data8[i];
  end;
end;

function TSliverHelper.NorthWest: TSlice;
var
  Temp1, Temp2: TSlice;
  i: integer;
begin
  // Add pixels 0,1,2   {the north part}
  // This means expanding every bit into a byte
  // Or rather every byte into an int64;
  for i:= 0 to 7 do begin
    Temp1.Data8[i]:= TSuperSlice.Lookup012[Self.bytes[i]];
  end;
  // We already have pixel 2, so just add pixel 5 and 8
  // Now add pixel 5
  // Double every uint32
  // Expanding 16 bytes (=4 int32) into 32 bytes
  for i:= 0 to 3 do begin
    Temp2.Data4[i * 2]:= Temp1.Data4[i];
    Temp2.Data4[i * 2 + 1]:= Temp1.Data4[i];
  end;
  // Finally add pixel 8
  // Just copy the half-slice twice.
  // Expanding 32 bytes into 64.
  Move(Temp2, Result, 32);
  Move(Temp2, (@Result.bytes[32])^, 32);
end;

function TSliverHelper.South: TSlice;
var
  i: integer;
begin
  // Add pixels 678
  // This means copying the pattern 8 times
  for i:= 0 to 7 do begin
    Result.Data8[i]:= Self.Data8;
  end;
end;

function TSliverHelper.SouthEast: TSlice;
var
  i: integer;
  Temp1, Temp2: TSlice;
begin
  // Add pixels 0, 3, 6, 7, 8
  // First add pixel 0.
  // This expands the sliver from 2 to 4 bytes
  for i:= 0 to 1 do begin
    Temp1.Data2[i]:= TSuperSlice.Lookup0[Self.bytes[i]];
  end;
  // Next add pixel 3, double every byte
  // this expands the sliver from 4 to 8 bytes
  for i:= 0 to 3 do begin
    Temp2.bytes[i * 2]:= Temp1.bytes[i];
    Temp2.bytes[i * 2 + 1]:= Temp1.bytes[i];
  end;
  // Add pixels 678
  // This means copying the pattern 8 times
  for i:= 0 to 7 do begin
    Result.Data8[i]:= Temp2.Data8[0];
  end;
end;

function TSliverHelper.SouthWest: TSlice;
var
  Temp1: TSlice;
  i: integer;
begin
  // Add pixels 2,5,6,7,8
  // First add pixel 2.
  // Expanding 2 into 4 bytes
  for i:= 0 to 1 do begin
    Temp1.Data2[i]:= TSuperSlice.Lookup2[Self.bytes[i]];
  end;
  // Now add pixel 5
  // Double every uint32
  // Expanding 4 bytes (=1 int32) into 8 bytes
  Temp1.Data4[1]:= Temp1.Data4[0];
  // Add pixels 678
  // This means copying the pattern 8 times
  for i:= 0 to 7 do begin
    Result.Data8[i]:= Temp1.Data8[0];
  end;
end;

{ TMegaSlice }

class function TMegaSlice.NESW(const NorthEast, SouthWest: TSlice): TMegaSlice;
begin

end;

class function TMegaSlice.NWSE(const NorthWest, SouthEast: TSlice): TMegaSlice;
begin

end;

{ TCake }



{ TLookupTable }

function TLookupTable.GetItems(Offset: TOffset; Index: integer): TSlice;
var
  IndexOffset: integer;
  // UnknownOffset: integer;
  N_Part, E_Part: integer;
begin
  if (Offset in [oNorthEast .. oNorthWest]) then begin
    IndexOffset:= (Ord(Offset) - Ord(oNorthEast)) * TLookupTable.IndexStride;
    Result:= FCornerData[FCornerIndex[index + IndexOffset]];
  end else if (Offset in [oCenter .. oWest]) then begin
    IndexOffset:= Ord(Offset) * TLookupTable.IndexStride;
    Result:= FData[FIndex[index + IndexOffset]];
  end else begin
    // UnknownTable
    Offset.XY(E_Part, N_Part);
    Result:= UnknownItems[E_Part, N_Part, index];
  end;
end;

procedure TLookupTable.SaveSliceData(const Filename: string; const SliceData: TArray<TSlice>);
var
  FS: TFileStream;
begin
  FS:= TFileStream.Create(Filename, fmCreate);
  try
    FS.Write(TBytes(SliceData), Length(SliceData) * SizeOf(TSlice));
  finally
    FS.Free;
  end;
end;

procedure TLookupTable.SetItems(Offset: TOffset; Index: integer; const value: TSlice);
var
  N_Part, E_Part: integer;
begin
  if (Offset in [oNorth .. oNorthWest]) then begin
    System.Assert(false);
  end else begin
    // Unknown table
    Offset.XY(E_Part, N_Part);
    UnknownItems[E_Part, N_Part, index]:= value;
  end;
end;

procedure TLookupTable.LoadMainData(const Filename: string);
begin
  LoadSliceData(Filename, FData);
  if not(Form2.AppRegistry.ValueStored(cMainSliceData)) then Form2.AppRegistry.WriteString(cMainSlicedata, Filename);
end;

function TLookupTable.GetUnknownItems(E_offset, N_offset: TUnknownIndex; Index: uint32): TSlice;

// -3
// -2
// -1
// -3 -2 -1  0  1  2  3
// 1
// 2
// 3

// The counts in 1 shl x terms for this matrix are
//  4   6   8  10   8   6   4
//  6   9  12  15  12   9   6
//  8  12  16  20  16  12   8
// 10  15  20      20  15  10
//  8  12  16  20  16  12   8
//  6   9  12  15  12   9   6
//  4   6   8  10   8   6   4
var
  Offset: uint32;
begin
  // Note that N_index=0, E_index=0 is stored in the OCenter table, because that combination does
  // not have any unknown pixels.
  // Count:= 1 shl ( (5-ABS(N_offset)) * (5-ABS(S_index)) );
  Offset:= TLookupTable.UnknownOffset[E_offset, N_offset];
  if (Offset + Index) >= Length(FUnknownData) then begin
    Assert(false, 'index out of range');
  end;
  Result:= FUnknownData[Offset + index];
end;

procedure TLookupTable.SetUnknownItems(E_offset, N_offset: TUnknownIndex; Index: uint32; const value: TSlice);
var
  Offset: uint32;
begin
  Offset:= TLookupTable.UnknownOffset[E_offset, N_offset];
  FUnknownData[Offset + index]:= value;
end;

function TLookupTable.HasCornerData: boolean;
begin
  Result:= (Length(FCornerData) > 0) and (Length(FCornerIndex) > 0);
end;

function TLookupTable.HasCountData: boolean;
begin
  Result:= (Length(FCountData) > 0);
end;

function TLookupTable.HasSliceData: boolean;
begin
  Result:= (Length(FData) > 0) and (Length(FIndex) > 0)
end;

function TLookupTable.HasUnknownData: boolean;
begin
  Result:= (Length(FUnknownData) > 0);
end;

class constructor TLookupTable.Init;
var
  Offset: integer;
  px, py, p: integer;
  x, y: integer;
begin
  Offset:= 0;
  for x:= -3 to 3 do begin
    for y:= -3 to 3 do begin
      TLookupTable.UnknownOffset[x, y]:= Offset;
      px:= 5 - ABS(x);
      py:= 5 - ABS(y);
      p:= 1 shl (px * py);
      if (x = 0) and (y = 0) then p:= 0;
      Inc(Offset, p);
    end;
  end;
  TLookupTable.UnknownSize:= Offset;
end;

procedure TLookupTable.LoadCornerData(const Filename: string);
begin
  LoadSliceData(Filename, FCornerdata);
end;

procedure TLookupTable.LoadMainIndex(const Filename: string);
//var
//  FS: TFileStream;
begin
//  FS:= TFileStream.Create(Filename, fmOpenRead);
//  SetLength(FIndex, FS.Size div SizeOf(integer));
//  TLookupTable.IndexStride:= FS.Size div (SizeOf(integer) * 5); // we have 5 lookup tables stored
//  try
//    FS.Read64(TBytes(FIndex), 0, FS.Size);
//  finally
//    FS.Free;
//  end;
  LoadIndex(Filename, FIndex);
  TLookupTable.IndexStride:=  (Length(FIndex) div 5); // we have 5 lookup tables stored
end;

procedure TLookupTable.LoadSliceData(const Filename: string; var SliceData: TArray<TSlice>);
var
  FS: TFileStream;
begin
  FS:= TFileStream.Create(Filename, fmOpenRead);
  Assert((FS.Size mod SizeOf(TSlice)) = 0, 'File does not contain slice data because filesize does not make sense');
  SetLength(SliceData, FS.Size div SizeOf(TSlice));
  try
    FS.Read64(TBytes(SliceData), 0, FS.Size);
  finally
    FS.Free;
  end;
end;

procedure TLookupTable.LoadUnknownData(const Filename: string);
begin
  LoadSliceData(Filename, FUnknownData);
end;

procedure TLookupTable.LoadCornerIndex(const Filename: string);
begin
  LoadIndex(Filename, FCornerIndex);
end;

procedure TLookupTable.LoadCountData(const Filename: string);
var
  FS: TFileStream;
begin
  FS:= TFileStream.Create(Filename, fmOpenRead);
  Assert((FS.Size mod SizeOf(uint64)) = 0, 'Incorrect filesize for the count data');
  SetLength(FCountData, FS.Size div SizeOf(uint64));
  try
    FS.Read64(TBytes(FCountData), 0, FS.Size);
  finally
    FS.Free;
  end;
end;

procedure TLookupTable.LoadIndex(const Filename: string; var IndexData: TArray<integer>);
var
  FS: TFileStream;
begin
  FS:= TFileStream.Create(Filename, fmOpenRead);
  SetLength(Indexdata, FS.Size div SizeOf(integer));
  try
    FS.Read64(TBytes(IndexData), 0, FS.Size);
  finally
    FS.Free;
  end;
end;

{ TSliverMood }

class operator TSliverChanges.Add(const a, b: TSliverChanges): TSliverChanges;
begin
  Result.AsByte:= a.AsByte or b.AsByte;
end;

class operator TSliverChanges.BitwiseAnd(const a: TSliverChanges; const b: TSliver): TSliverChanges;
begin
  Result:= a;
  if (b.Data8 = 0) then Result.AsByte:= Result.AsByte or byte(Ord(ssInvalid));
end;

class operator TSliverChanges.Implicit(a: boolean): TSliverChanges;
begin
  Result.AsBoolean:= (a <> false); // make sure malformed booleans still transform correctly
end;

function TSliverChanges.IsValid: boolean;
begin
  Result:= Self.AsByte in [0, 1];
end;

function TSliverChanges.KeepGoing: boolean;
begin
  Result:= (Self.AsByte = byte(Ord(ssChanged)));
end;

function TSliverChanges.Invalid: boolean;
begin
  Result:= not(Self.AsByte in [0, 1]);
end;

class operator TSliverChanges.explicit(a: TSliverChanges): boolean;
begin
  Result:= a.AsBoolean;
end;

class operator TSliverChanges.LogicalOr(const a: TSliverChanges; const b: boolean): TSliverChanges;
begin
  Result.AsByte:= a.AsByte or byte(b <> false);
end;

{ TDeleteBits }


class operator TMaskedBits.Add(const A, B: TMaskedBits): TMaskedBits;
var
  Count: integer;
  i,j: integer;
  Temp: TArray<byte>;
begin
  if (A.Count = 0) then Result:= B
  else if (B.Count = 0) then Result:= A
  else begin
    Count:= A.Count + B.Count;
    SetLength(Temp, Count);
    Move(A.FData[0],Temp[0],A.Count);
    Move(B.FData[0],Temp[A.Count], B.Count);
    TArray.Sort<byte>(Temp);
    Result.Length:= Count;
    i:= 1;
    j:= 0;
    Result.FData[0]:= Temp[0];
    while i < count do begin
      if (Result[j] < Temp[i]) then Inc(j);
      Result.FData[j]:= Temp[i];
      inc(i);
    end; {while}
    Result.Length:= j+1;
  end;
end;

function TMaskedBits.Count: integer;
begin
  Result:= FLength;
end;

constructor TMaskedBits.Create(UnknownBits: integer);
var
  previous: integer;
  i: integer;
  count: integer;
begin
  Count:= PopCount(UnknownBits);
  FLength:= Count;
  if (Count = 0) then exit;
  previous:= -1;
  i:= 0;
  while i < Count do begin
    Previous:= TForm2.GetNextBitSet(previous,UnknownBits);
    FData[i]:= Previous;
    Inc(i);
  end;
end;

class operator TMaskedBits.Equal(const a, b: TMaskedBits): boolean;
var
  i: integer;
begin
  if (a.Count <> b.Count) then exit(false);
  for i:= 0 to a.Count -1 do begin
    if (a[i] <> b[i]) then exit(false);
  end;
  Result:= true;
end;

function TMaskedBits.FilterKnownBits(KnownMask: integer): integer;
begin
  Result:= DeleteBits(KnownMask, Self);
end;

constructor TMaskedBits.Create(EW, NS: TMaskedBitsIndex);
var
  A,B: TMaskedBits;
begin
  case NS of
    N3: A:= [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14];
    N2: A:= [0,1,2,3,4,5,6,7,8,9];
    N1: A:= [0,1,2,3,4];
     0: A.Length:= 0;
    S3: A:= [10,11,12,13,14,15,16,17,18,19,20,21,22,23,24];
    S2: A:= [               15,16,17,18,19,20,21,22,23,24];
    S1: A:= [                              20,21,22,23,24];
  end;
  case EW of
   E3: B:= [0,1,2,5,6,7,10,11,12,15,16,17,20,21,22];
   E2: B:= [0,1,  5,6,  10,11,   15,16,   20,21];
   E1: B:= [0,    5,    10,      15,      20];
   0: B.Length:= 0;
   W3: B:= [2,3,4,7,8,9,12,13,14,17,18,19,22,23,24];
   W2: B:= [  3,4,  8,9,   13,14,   18,19,   23,24];
   W1: B:= [    4,    9,      14,      19,      24];
  end;
  Self:= A + B;
end;

function TMaskedBits.GetItems(index: integer): integer;
begin
  Result:= FData[index];
end;

class operator TMaskedBits.Implicit(const A: array of byte): TMaskedBits;
begin
  move(A[0], Result.FData[0], System.Length(a) * SizeOf(byte));
  Result.FLength:= System.Length(a);
  //Result.FData:= A;
end;

class operator TMaskedBits.Implicit(const A: TMaskedBits): TArray<byte>;
begin
  SetLength(Result, A.Count);
  move(A.FData[0], Result[0], A.Count * SizeOf(byte));
end;

{ TOffsetHelper }

class function TOffsetHelper.Create(x, y: integer): TOffset;
var
  Index: integer;
begin
  index:= -(((x + 3) * 7) + (y + 3));
  Dec(index); // Make sure it is always < 0
  Result:= TOffset(index);
  //The center 0,0 uses the plain (known) table as a starting point instead.
  if ((x or y) = 0) then Result:= TOffset(0);
end;

function TOffsetHelper.ToMaskedBits: TMaskedBits;
var
  EW, NS: integer;
begin
  Self.XY(EW, NS);
  Result:= TMaskedBits.Create(EW, NS);
end;

{ TCake }

constructor TCake.Create(Known, Unknown: integer);
begin
  Self.FUnknownPart:= (Unknown and $1FFFFFF);
  Self.FKnownPart:= (Known and $1FFFFFF);
end;

function TCake.IsKnown: boolean;
begin
  Result:= (FUnknownPart = 0);
end;

class operator TCake.LeftShift(const A: TCake; B: cardinal): TCake;
begin
  Result.FUnknownPart:= A.FUnknownPart shl B;
  Result.FknownPart:= A.FKnownPart shl B;
end;

function TCake.OddKnown: boolean;
begin
  Result:= Odd(FKnownPart);
end;

function TCake.OddUnknown: boolean;
begin
  Result:= Odd(FUnknownPart);
end;

class operator TCake.RightShift(const A: TCake; B: cardinal): TCake;
begin
  Result.FUnknownPart:= A.FUnknownPart shr B;
  Result.FKnownPart:=   A.FKnownPart shr B;
end;

procedure TCake.SetKnown;
begin
  Inc(FKnownPart);
end;

procedure TCake.SetUnknown;
begin
  Inc(FUnknownPart);
end;

function TCake.ToSlice: TSlice;
begin

end;

end.
