unit Unit2;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ComCtrls;

const
  cStanding = true;
  cFlat = false;

type
  //PSlice = ^TSlice;

  TCake = record
  private
    Data: integer;
  public
    class operator Implicit(a: TCake): integer; overload;
    class operator Implicit(a: integer): TCake; overload;
    function North: integer;
    function South: integer;
    function East: integer;
    function West: integer;
  end;


  TSliver = record
  public
    class operator BitwiseAnd(const a,b: TSliver): TSliver;
    class operator BitwiseOr(const a,b: TSliver): TSliver;
    class operator Equal(const a, b: TSliver): boolean;
    class operator NotEqual(const a,b: TSliver): boolean;
  private
    case integer of
      8: (Data8: int64);
      4: (Data4: array[0..1] of uint32);
      2: (Data2: array[0..3] of word);
      1: (bytes: array[0..7] of byte);
  end;

  TSlice = record
  private
    class var
  public
    class operator GreaterThan(const a, b: TSlice): boolean;
    class operator LessThan(const a, b: TSlice): boolean;
    class operator Equal(const a, b: TSlice): boolean;
    class operator NotEqual(const a, b: TSlice): boolean;
    class operator BitwiseAnd(const a, b: TSlice): TSlice;
    class operator BitwiseXor(const a, b: TSlice): TSlice;
    class operator BitwiseOr(const a, b: TSlice): TSlice;
    class operator LogicalNot(const a: TSlice): TSlice;
    class function Empty: TSlice; static;
    function PopCount: integer;
    function CountDead(Pixel: integer): integer;
    function CountAlive(Pixel: integer): integer;
    function Print: string;
    function IsBitSet(Bit: integer): boolean;
    class function Print5x5(item: integer): string; static;
    class function GetReordering(Order: TArray<integer>; const Input: TArray<integer>): TArray<integer>; static;
    function GetBit(Index: integer): boolean;
    procedure SetBit(Index: integer; value: boolean = true);
    procedure ReorderSlice(const Reordering: TArray<integer>);
  private
    case integer of
      9: (Sliver: array[0 .. 7] of TSliver);
      8: (Data8: array [0 .. 7] of uint64);
      4: (Data4: array [0 .. (64 div 4) - 1] of uint32);
      2: (Data2: array [0 .. (64 div 2) -1] of word);
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
    class function NS(const North, South: TSlice): TSliver; static;
    class function EW(const East, West: TSlice): TSliver; static;
    class function NWSE(const NorthWest, SouthEast: TSlice): TSliver; static;
    class function NESW(const NorthEast, SouthWest: TSlice): TSliver; static;
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
    class var Lookup0: array[byte] of word;
    class var Lookup2: array[byte] of word;
    class var Lookup012: array[byte] of uint64;
    class var LookupRemove0: array[word] of byte;
    class var LookupRemove2: array[word] of byte;
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

  TOffset = (oCenter, oNorth, oEast, oSouth, oWest);

  TLookupTable = class
  private
    class var IndexStride: integer;
  private
    FData: TArray<TSlice>;
    FIndex: TArray<integer>;
    function GetItems(Offset: TOffset; Index: integer): TSlice;
  public
    procedure LoadIndex(const Filename: string);
    procedure LoadData(const Filename: string);
    property Items[Offset: TOffset; Index: integer]: TSlice read GetItems; default;
  end;

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
    Button2: TButton;
    StringGrid1: TStringGrid;
    Button3: TButton;
    StringGrid2: TStringGrid;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    StringGrid3: TStringGrid;
    FileOpenDialog1: TFileOpenDialog;
    Button7: TButton;
    TabSheet2: TTabSheet;
    SGSliceLayout: TStringGrid;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    SGDead: TStringGrid;
    SGAlive: TStringGrid;
    Label6: TLabel;
    Label7: TLabel;
    SGAliveDiff: TStringGrid;
    SGDeadDiff: TStringGrid;
    Label8: TLabel;
    Label9: TLabel;
    Button11: TButton;
    Button12: TButton;
    TabSheet3: TTabSheet;
    StringGrid4: TStringGrid;
    BtnLoadSmallLookups: TButton;
    Button13: TButton;
    TabSheet4: TTabSheet;
    Button14: TButton;
    Button15: TButton;
    SGMinOn: TStringGrid;
    SGMinOff: TStringGrid;
    Button16: TButton;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure BtnLoadSmallLookupsClick(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SGDeadDblClick(Sender: TObject);
    procedure StringGrid2Click(Sender: TObject);
    procedure StringGrid2DrawCell(Sender: TObject; ACol, ARow: Integer; Rect:
        TRect; State: TGridDrawState);
    procedure StringGrid3Click(Sender: TObject);
    procedure StringGrid4DrawCell(Sender: TObject; ACol, ARow: Integer; Rect:
        TRect; State: TGridDrawState);
  private
    Buffer: TArray<TSlice>;
    ChunkLookup: array[boolean] of TArray<TSuperSlice>;
    LookupTable: TLookupTable;
    LookupN1, LookupE1: TArray<TSlice>;
    NewLayout: TArray<integer>;
    procedure LoadLookupTable(var LookupTable: TArray<TSlice>);
    procedure InitNewLayout;
    procedure ShowNewLayout;
    function GetCounterLayout: TArray<integer>;
    procedure InitWithGoE;
    function GetStates(const input: TSlice): TArray<integer>;
    procedure Display5x5(Cake: integer; const SG: TStringGrid);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  Generics.Collections,
  Generics.Defaults;

{$R *.dfm}


{TODO -oOwner -cGeneral : Fix me}
const
  OffMask: array[0..8] of TSlice = (
  (data8: ($5555555555555555,$5555555555555555,$5555555555555555,$5555555555555555,$5555555555555555,$5555555555555555,$5555555555555555,$5555555555555555)),
  (data8: ($3333333333333333,$3333333333333333,$3333333333333333,$3333333333333333,$3333333333333333,$3333333333333333,$3333333333333333,$3333333333333333)),
  (data8: ($0F0F0F0F0F0F0F0F,$0F0F0F0F0F0F0F0F,$0F0F0F0F0F0F0F0F,$0F0F0F0F0F0F0F0F,$0F0F0F0F0F0F0F0F,$0F0F0F0F0F0F0F0F,$0F0F0F0F0F0F0F0F,$0F0F0F0F0F0F0F0F)),
  (data8: ($00FF00FF00FF00FF,$00FF00FF00FF00FF,$00FF00FF00FF00FF,$00FF00FF00FF00FF,$00FF00FF00FF00FF,$00FF00FF00FF00FF,$00FF00FF00FF00FF,$00FF00FF00FF00FF)),
  (data8: ($0000FFFF0000FFFF,$0000FFFF0000FFFF,$0000FFFF0000FFFF,$0000FFFF0000FFFF,$0000FFFF0000FFFF,$0000FFFF0000FFFF,$0000FFFF0000FFFF,$0000FFFF0000FFFF)),
  (data8: ($00000000FFFFFFFF,$00000000FFFFFFFF,$00000000FFFFFFFF,$00000000FFFFFFFF,$00000000FFFFFFFF,$00000000FFFFFFFF,$00000000FFFFFFFF,$00000000FFFFFFFF)),
  (data8: ($FFFFFFFFFFFFFFFF,$0000000000000000,$FFFFFFFFFFFFFFFF,$0000000000000000,$FFFFFFFFFFFFFFFF,$0000000000000000,$FFFFFFFFFFFFFFFF,$0000000000000000)),
  (data8: ($FFFFFFFFFFFFFFFF,$FFFFFFFFFFFFFFFF,$0000000000000000,$0000000000000000,$FFFFFFFFFFFFFFFF,$FFFFFFFFFFFFFFFF,$0000000000000000,$0000000000000000)),
  (data8: ($FFFFFFFFFFFFFFFF,$FFFFFFFFFFFFFFFF,$FFFFFFFFFFFFFFFF,$FFFFFFFFFFFFFFFF,$0000000000000000,$0000000000000000,$0000000000000000,$0000000000000000)));

type
  TSlices11 = array [0 .. 15, 0 .. 15] of TSlice;

var
  MySlices: TSlices11;
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

function Remove012(const data8: uint64): byte;
var
  Bytes: array[0..7] of byte absolute data8;
begin
  Result:= 0;
  if (bytes[0] <> 0) then Result:= 1;
  if (bytes[1] <> 0) then Inc(Result, 2);
  if (bytes[2] <> 0) then Inc(Result, 4);
  if (bytes[3] <> 0) then Inc(Result, 8);
  if (bytes[4] <> 0) then Inc(Result, 16);
  if (bytes[5] <> 0) then Inc(Result, 32);
  if (bytes[6] <> 0) then Inc(Result, 64);
  if (bytes[7] <> 0) then Inc(Result, 128);
end;

class function TSliceComparer.Comparer: IComparer<TSlice>;
begin
  if FComparer = nil then FComparer:= TSliceComparer.Create;
  Result:= IComparer<TSlice>(FComparer);
end;

type
  TFileStream = class(System.Classes.TFileStream)
    function Read64(Buffer: TBytes; Offset, Count: Int64): Int64;
  end;

function TFileStream.Read64(Buffer: TBytes; Offset, Count: Int64): Int64;
const
  BUKETSIZE = $20000000; // 512M
begin
  Result:= 0;
  while Count >= BUKETSIZE do begin
    Result:= Result + read(Buffer[Offset], BUKETSIZE);
    Inc(Offset, BUKETSIZE);
    Dec(Count, BUKETSIZE);
  end;
  if Count > 0 then Result:= Result + read(Buffer[Offset], Count);
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  FS: TFileStream;
  i,j: integer;
  UniqueCount: integer;
  m, Min, Max, Onn, Off: integer;
  MinIndex, MaxIndex: integer;
  MinCount, MaxCount: integer;
  MinBuffer: TArray<integer>;
  Total0, Total1: Int64;
  OnCount, OffCount: array[0..8] of integer;
begin
  FillChar(OffCount,SizeOf(OffCount),127);
  FillChar(OnCount,SizeOf(OnCount),127);

  if FileOpenDialog1.Execute then begin
    FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
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
        Off:= (Buffer[i] and OffMask[j]).Popcount;
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
    FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
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
      Form2.Memo1.Lines.Add(format('Min off count %d = %d',[j,Offcount[j]]));
    end;
    Form2.Memo1.Lines.Add(' ');
    for j:= 0 to 8 do begin
      Form2.Memo1.Lines.Add(format('Min on count %d = %d',[j,Oncount[j]]));
    end;
  end;
end;

function TSlice.GetBit(Index: integer): boolean;
var
  Mask: uint64;
  element: integer;
begin
  Mask:= (1 shl (Index and 63));
  element:= Index div 64;
  Result:= (Data8[Element] and Mask) <> 0;
end;

class function TSlice.GetReordering(Order: TArray<integer>; const Input: TArray<integer>): TArray<integer>;
var
  i,j,k,mask: integer;
begin
  Assert(Length(Order) = 9);
  SetLength(Result, Length(Input));
  for i:= 1 to 510 do begin  //The first and the last element never reorder in a transpose
    Result[i]:= 0;
    for j:= 0 to 8 do begin
      k:= 1 shl Order[j];
      mask:= 1 shl j;
      if (Input[i] and mask) <> 0 then begin
        Result[i]:= Result[i] + k;
      end;
    end; {for j}
  end; {for i}
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  FS: TFileStream;
  Counts: TArray<Int64>;
  i: integer;
  Min, Max, Avg: Int64;
  MinIndex, MaxIndex: integer;
begin
  if not FileOpenDialog1.Execute then Exit;
  FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
  SetLength(Counts, FS.Size div SizeOf(Int64));
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

function GetFutureCake(const SG: TStringGrid; col, row: integer): integer;
var
  Cake: integer;
  x, y: integer;
begin
  Cake:= 0;
  for y:= 0 to 4 do begin
    for x:= 4 downto 0 do begin
      Cake:= Cake shl 1;
      if ((col+x) < SG.ColCount) and ((row+y) < SG.RowCount)
         and ((col+x) >= 0) and ((row+y) >= 0)
         and (SG.Cells[col + x, row + y] = 'X') then begin
        Inc(Cake);
      end;
    end;
  end;
  Result:= Cake;
end;

function GetFutureStandingChunk(const SG: TStringGrid; col, row: integer): integer;
var
  x,y: integer;
begin
  Result:= 0;
  for y:= 0 to 3 do begin
    for x:= 2 downto 0 do begin
      Result:= Result shl 1;
      if (SG.Cells[col+x, row+y]= 'X') then begin
        Inc(Result);
      end;
    end; {for x}
  end; {for y}
end;

function GetFutureFlatChunk(const SG: TStringGrid; col, row: integer): integer;
var
  x,y: integer;
begin
  Result:= 0;
  for y:= 0 to 2 do begin
    for x:= 3 downto 0 do begin
      Result:= Result shl 1;
      if (SG.Cells[col+x, row+y]= 'X') then begin
        Inc(Result);
      end;
    end; {for x}
  end; {for y}
end;

function FutureGridToPastSlice(const SG: TStringGrid; col, row: integer; const LookupTable: TArray<TSlice>): TSlice;
var
  Cake: integer;
begin
  Cake:= GetFutureCake(SG, col, row);
  if Cake <> 0 then begin
    Result:= LookupTable[Cake];
  end
  else Result:= LookupTable[Cake];
end;

function FutureGridToPastSliceSimple(const SG: TStringGrid; col, row: integer; const LookupTable: TArray<TSlice>): TSlice;
var
  Cake: integer;
  ZeroSlice, OneSlice: TSlice;
begin
  ZeroSlice:= LookupTable[0];
  OneSlice:= not(ZeroSlice);
  Cake:= 0;
  if (SG.Cells[col+2, row+2] = 'X') then Result:= OneSlice
  else Result:= ZeroSlice;
end;


procedure DisplaySlices(SG, SGDiff: TStringGrid; Slices: TSlices11);
var
  x, y: integer;
  Slice: TSlice;
  Old: integer;
begin
  for x:= 0 to SG.ColCount-1 do begin
    for y:= 0 to SG.RowCount-1 do begin
      Slice:= Slices[x, y];
      if (SG.Cells[x,y] <> '') then Old:= SG.Cells[x, y].ToInteger
      else begin
        if (Form2.StringGrid1.Cells[x+2, y+2] = 'X') then Old:= 140 else Old:= 372;
      end;
      SG.Cells[x, y]:= Slice.PopCount.ToString;
      SGDiff.Cells[x, y]:= (Old - Slice.PopCount).ToString;
    end; { for y }
  end; { for x }
end;



procedure DoARun(Rotation: TRotation = rNone);
var
  SS: TSuperSlice;
  Slices: TSlices11;

procedure Solve(x,y: integer);
begin
  if x < (17 - 2) then begin
    SS:= TSuperSlice.EW(Slices[x+1, y],Slices[x, y]);
    Slices[x, y]:= Slices[x, y] and SS.West;
    Slices[x + 1, y]:= Slices[x + 1, y] and SS.East;
  end;  {handle EW}
  if y < (17 - 2) then begin
    SS:= TSuperSlice.NS(Slices[x, y], Slices[x, y + 1]);
    Slices[x, y]:= Slices[x, y] and SS.North;
    Slices[x, y + 1]:= Slices[x, y + 1] and SS.South;
  end; {handle NS}
end;



procedure SolveCounterClock(x,y: integer);
//N -> W
//S -> E
//E -> N
//W -> S
begin
  //Every slice is rotated counter clockwise
  //Thus
  //N -> W
  //E -> N
  //W -> S
  //S -> E

  if x < (17 - 2) then begin
    //Let's first compare E-W, N was E, S was W.
    //SS:= TSuperSlice.WE({W}Slices[x, y], Slices[{E}x+1, y]);
    SS:= TSuperSlice.NS(Slices[{E}x+1, y], Slices[{W}x, y]);
    //Slices[x, y]:= Slices[x, y] and SS.West;
    Slices[x, y]:= Slices[x, y] and SS.South;
    //Slices[x + 1, y]:= Slices[x + 1, y] and SS.East;
    Slices[x + 1, y]:= Slices[x + 1, y] and SS.North;
  end;  {handle EW}
  if y < (17 - 2) then begin
    //Next up is NS: W was N, E was S
    //SS:= TSuperSlice.NS({N}Slices[x, y], {S}Slices[x, y + 1]);
    SS:= TSuperSlice.EW(Slices[x, y+1],Slices[x, y]);
    Slices[x, y]:= Slices[x, y] and SS.West;
    Slices[x, y + 1]:= Slices[x, y + 1] and SS.East;
  end; {handle NS}
end;

var
  x,y: integer;
begin
  OldSlices:= MySlices; //Save the previous state for comparison.
  Slices:= MySlices;
  for x:= 0 to 17 - 2 do begin
    for y:= 0 to 17 - 2 do begin
      case Rotation of
        rNone: Solve(x,y);
        rCounter: SolveCounterClock(x,y);
        rClock: Assert(false);
        r180: Assert(false);
      end;
    end; {for y}
  end; {for x}
  MySlices:= Slices;
end;

procedure DoASliverRun(Rotation: TRotation = rNone);
var
  Slices: TSlices11;

  procedure SliverSolve(x,y: integer);
  var
    Sliver: TSliver;
  begin
    //EW
    if x < (17 - 2) then begin
      Sliver:= TSliver.EW(Slices[x+1, y],Slices[x, y]);
      Slices[x, y]:= Slices[x, y] and Sliver.West;
      Slices[x + 1, y]:= Slices[x + 1, y] and Sliver.East;
    end;
    //NS
    if y < (17 - 2) then begin
      Sliver:= TSliver.NS(Slices[x, y], Slices[x, y + 1]);
      Slices[x, y]:= Slices[x, y] and Sliver.North;
      Slices[x, y + 1]:= Slices[x, y + 1] and Sliver.South;
    end; {handle NS}
    //First do NESW
    if (x < (17-2)) and (y < (17-2)) then begin
      Sliver:= TSliver.NESW(Slices[x+1,y], Slices[x,y+1]);
      Slices[x+1,y]:= Slices[x+1,y] and Sliver.NorthEast;
      Slices[x,y+1]:= Slices[x,y+1] and Sliver.SouthWest;
    end;
    //Then NWSE
    if (x < (17-2)) and (y < (17-2)) then begin
      Sliver:= TSliver.NWSE(Slices[x,y], Slices[x+1,y+1]);
      Slices[x,y]:= Slices[x,y] and Sliver.NorthWest;
      Slices[x+1,y+1]:= Slices[x+1,y+1] and Sliver.SouthEast;
    end;
  end;

var
  x,y: integer;
begin
  OldSlices:= MySlices; //Save the previous state for comparison.
  Slices:= MySlices;
  for x:= 0 to 17 - 2 do begin
    for y:= 0 to 17 - 2 do begin
      case Rotation of
        rNone: SliverSolve(x,y);
        rCounter: Assert(false);
        rClock: Assert(false);
        r180: Assert(false);
      end;
    end; {for y}
  end; {for x}
//  for x:= 17-2 downto 0 do begin
//    for y:= 17-2 downto 0 do begin
//      case Rotation of
//        rNone: SliverSolve(x,y);
//        rCounter: Assert(false);
//        rClock: Assert(false);
//        r180: Assert(false);
//      end;
//    end; {for y}
//  end; {for x}
  MySlices:= Slices;
end;

procedure TForm2.Button10Click(Sender: TObject);
var
  Slice: TSlice;
begin
  GetCounterLayout;
  for Slice in MySlices do begin
    Slice.ReorderSlice(NewLayout);
  end;
end;

procedure TForm2.Button11Click(Sender: TObject);
begin
  DoASliverRun;
  DisplaySlices(StringGrid2, StringGrid3, MySlices);
end;

procedure TForm2.Button12Click(Sender: TObject);
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

procedure TForm2.Button13Click(Sender: TObject);
var
  Entry: TCake;
  x,y: integer;
  North, South, East, West: TSlice;
  Slices: TSlices11;
  Chunk: TSuperSlice;
begin
  //Load the lookup table entries
  if Length(ChunkLookup[cFlat]) = 0 then BtnLoadSmallLookups.Click;
  FillChar(Slices, SizeOf(Slices), $FF);
  for x:= 0 to 17-2 do begin
    for y:= 0 to 17-2 do begin
      Chunk:= ChunkLookup[cStanding, GetFutureStandingChunk(StringGrid1, x+1,y+1)];
      Slices[x,y]:= Slices[x,y] and Chunk.South;
      if (y < (17-2)) then Slices[x,y+1]:= Slices[x,y+1] and Chunk.North;

      Chunk:= ChunkLookup[cFlat, GetFutureFlatChunk(Stringgrid1,x+1,y+1)];
      Slices[x,y]:= Slices[x,y] and Chunk.East;
      if (x < (17-2)) then Slices[x+1,y]:= Slices[x+1,y] and Chunk.West;
    end;
  end;
  DisplaySlices(StringGrid2, StringGrid3, Slices);
  MySlices:= Slices;
end;

procedure TForm2.Button14Click(Sender: TObject);

  function RotateLeft90(input: integer): integer;
  const
    Translate: array[0..24] of byte = (4,9,14,19,24,3,8,13,18,24,2,7,12,17,22,1,6,11,16,21,0,5,10,15,20);
  var
    BitsIn, BitsOut: array[0..24] of byte;
    i: integer;
  begin
    FillChar(BitsIn, SizeOf(BitsIn), 0);
    for i:= 0 to 24 do begin
      if Odd(input) then BitsIn[i]:= 1;
      input:= input shr 1;
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
  A,B: TSlice;
  LookupN1, LookupE1: TArray<TSlice>;
begin
  LoadLookupTable(LookupN1);
  LoadLookupTable(LookupE1);

  for i:= 0 to (1 shl 25)-1 do begin
    A:= LookupN1[RotateLeft90(i)];
    B:= LookupE1[i];
    if (A <> B) then ShowMessage('Oops');
  end;

end;

procedure TForm2.Display5x5(Cake: integer; const SG: TStringGrid);
var
  x,y: integer;
begin
  for y:= 0 to 4 do begin
    for x:= 4 downto 0 do begin
      if Odd(Cake) then SG.Cells[x, y]:= 'X'
      else SG.Cells[x,y]:= '';
      Cake:= Cake shr 1;
    end;
  end;
end;

procedure TForm2.Button15Click(Sender: TObject);
var
  Soll, Ist: UInt64;
  MinOn, MinOff: UInt64;
  MinOnIndex, MinOffIndex: integer;
  Counts: TArray<UInt64>;
  Item: UInt64;
  FS: TFileStream;
  i: integer;
begin
  if not(FileOpenDialog1.Execute) then exit;
  FS:= TFileStream.Create(FileOpenDialog1.Filename, fmOpenRead);
  SetLength(Counts, FS.Size div SizeOf(UInt64));
  FS.Read64(TBytes(Counts), 0, FS.Size);
  FS.Free;
  Ist:= 0;
  for Item in Counts do Inc(Ist, Item);
  Soll:= 1;
  Soll:= Soll shl 49;
  Assert(Ist = Soll);
  MinOn:= Ist;
  MinOff:= Ist;
  //Find the minimum on and off slices
  MinOnIndex:= 0;
  MinOffIndex:= 0;
  for i:= 0 to High(Counts) do begin
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
  Display5x5(MinOnIndex, SGMinOn);
  Display5x5(MinOffIndex, SGMinOff);
  Button15.Caption:= 'MinOn='+MinOn.ToString+' MinOff='+MinOff.ToString;
end;

procedure TForm2.Button16Click(Sender: TObject);
var
  x,y: integer;
  FS: TFileStream;
  OldTitle: string;
  Slices: TSlices11;
begin
  //Read the lookup tables
  OldTitle:= FileOpendialog1.Title;
  FileOpenDialog1.Title:= 'Load North Lookup';
  if not FileOpenDialog1.Execute then Exit;
  FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
  SetLength(LookupN1, FS.Size div SizeOf(TSlice));
  FS.Read64(TBytes(LookupN1), 0, FS.Size);
  FS.Free;
  FileOpenDialog1.Title:= 'Load East Lookup';
  if not FileOpenDialog1.Execute then Exit;
  FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
  SetLength(LookupE1, FS.Size div SizeOf(TSlice));
  FS.Read64(TBytes(LookupE1), 0, FS.Size);
  FS.Free;
  FileOpenDialog1.Title:= OldTitle;
  Slices:= MySlices;
  // Get the slice data from the grid, by looking it up in the lookup table
  //Apply the North lookup table
  for x:= 0 to 17 - 2 do begin
    for y:= -1 to 17 - 3 do begin
      Slices[x, y+1]:= Slices[x,y+1] and FutureGridToPastSlice(StringGrid1, x, y, LookupN1);
      Stringgrid3.Cells[x,y+1]:= '';
    end;
  end;
  //Apply the East lookup table
  for x:= 1 to 17 - 1 do begin
    for y:= 0 to 17 - 2 do begin
      Slices[x-1, y]:= Slices[x-1,y] and FutureGridToPastSlice(StringGrid1, x, y, LookupE1);
      Stringgrid3.Cells[x-1,y]:= '';
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
  FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
  SetLength(LookupTable, FS.Size div SizeOf(TSlice));
  FS.Read64(TBytes(LookupTable), 0, FS.Size);
  FS.Free;
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  x, y: integer;
  Slices: TSlices11; // array[0..10,0..10] of TSlice;
  //LookupTable: TArray<TSlice>;
  //FS: TFileStream;
begin
  // Read the lookup table
//  if not FileOpenDialog1.Execute then Exit;
//  FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
//  SetLength(LookupTable, FS.Size div SizeOf(TSlice));
//  FS.Read64(TBytes(LookupTable), 0, FS.Size);
//  FS.Free;
  FillChar(Slices, SizeOf(Slices), #0);
  // Get the slice data from the grid, by looking it up in the lookup table
  for x:= 0 to 17 - 2 do begin
    for y:= 0 to 17 - 2 do begin
      Slices[x, y]:= FutureGridToPastSlice(StringGrid1, x, y, LookupTable);
      Stringgrid3.Cells[x,y]:= '';
    end;
  end;
  DisplaySlices(StringGrid2, StringGrid3, Slices);
  // Take every slice and confront it with its neighbors to the west, east, north and south.
  MySlices:= Slices;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  DoARun;
  DisplaySlices(StringGrid2, StringGrid3, MySlices);
end;

procedure TForm2.Button5Click(Sender: TObject);
var
  x, y: integer;
begin
  for y:= 0 to StringGrid1.RowCount - 1 do begin
    for x:= 0 to StringGrid1.ColCount - 1 do begin
      StringGrid1.Cells[x, y]:= '';
    end; { for x }
  end; { for y }
end;

procedure TForm2.Button6Click(Sender: TObject);
var
  w: word;
  b: integer;
  i: integer;
  i64: uint64;
  w3: array[0..3] of word absolute i64;
  b2: array[0..3] of byte;
  w2: array[0..1] of word absolute b2;
begin
 //Test every possible lookup0 and its reverse.
   for i:= 0 to 255 do begin
     w:= TSuperSlice.Lookup0[i];
     b:= TSuperSlice.LookupRemove0[w];
     if (b <> i) then begin
       ShowMessage('Oops');
     end;
   end;
   for i:= 0 to 255 do begin
     i64:= TSuperSlice.Lookup012[i];
     b2[0]:= TSuperSlice.LookupRemove0[w3[0]];
     b2[1]:= TSuperSlice.LookupRemove0[w3[1]];
     b2[2]:= TSuperSlice.LookupRemove0[w3[2]];
     b2[3]:= TSuperSlice.LookupRemove0[w3[3]];
     b2[0]:= TSuperSlice.LookupRemove0[w2[0]];
     b2[1]:= TSuperSlice.LookupRemove0[w2[1]];
     b:= TSuperSlice.LookupRemove0[w2[0]];
     if (b <> i) then begin
       ShowMessage('Oops');
     end;
   end;
end;

procedure TForm2.Button7Click(Sender: TObject);
var
  x, y: integer;
  Slices: TSlices11; // array[0..10,0..10] of TSlice;
begin
  FillChar(Slices, SizeOf(Slices), #0);
  for x:= 0 to 17 - 2 do begin
    for y:= 0 to 17 - 2 do begin
      Slices[x, y]:= FutureGridToPastSliceSimple(StringGrid1, x, y, LookupTable);
      Stringgrid3.Cells[x,y]:= '';
    end;
  end;
  DisplaySlices(StringGrid2, StringGrid3, Slices);
  // Take every slice and confront it with its neighbors to the west, east, north and south.
  MySlices:= Slices;
end;

//orig    counter   clock    diagonal other diag
//012     258       630      036      852
//345     147       741      147      741
//678     036       852      258      630

//orig    counter
//210     036
//543     147
//876     258


procedure TForm2.Button8Click(Sender: TObject);
var
  Ordering: TArray<integer>;
begin
  SetLength(Ordering, 9);
//  Ordering[0]:= 0;
//  Ordering[1]:= 1;
//  Ordering[2]:= 2;
//  Ordering[3]:= 3;
//  Ordering[4]:= 4;
//  Ordering[5]:= 5;
//  Ordering[6]:= 6;
//  Ordering[7]:= 7;
//  Ordering[8]:= 8;
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

procedure TForm2.Button9Click(Sender: TObject);
begin
  DoARun(rCounter);
  DisplaySlices(StringGrid2, StringGrid3, MySlices);
end;

procedure TForm2.ShowNewLayout;
var
  row,col: integer;
  s: string;
begin
  for col:= 0 to 31 do begin
    for row:= 0 to 15 do begin
      if (NewLayout[col*16+row] = col*16+row) then s:= '+' else s:= '';
      s:= s + NewLayout[col*16+row].ToString;
      SGSliceLayout.Cells[Col, Row]:= s;
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
  LoadLookupTable(LookupTable);
  InitNewLayout;
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

function TForm2.GetStates(const input: TSlice): TArray<integer>;
var
  i,a: integer;
  alive, dead: TSlice;
begin
  SetLength(Result, input.PopCount);
  Dead:= input and LookupTable[0];
  Alive:= input and (Not(LookupTable[0]));
  Assert((Alive or Dead) = input);
  a:= 0;
  for i:= 0 to 511 do begin
    if (Alive.IsBitSet(i)) then begin
      Result[a]:= -i;
      Inc(a);
    end;
  end;
  for i:= 0 to 511 do begin
    if (Dead.IsBitSet(i)) then begin
      Result[a]:= i;
      inc(a);
    end;
  end;
end;

procedure TForm2.SGDeadDblClick(Sender: TObject);
var
  States: TArray<integer>;
  SG: TStringGrid;
  a: integer;
  x,y: integer;
begin
  SG:= StringGrid2;
  States:= GetStates(MySlices[SG.Col, SG.Row]);
  a:= 0;
  for y:= 0 to StringGrid4.RowCount-1 do begin
    for x:= 0 to StringGrid4.ColCount-1 do begin
      StringGrid4.Cells[x,y]:= '';
      if (a >= Length(States)) then Continue;
      StringGrid4.Cells[x,y]:= States[a].ToString;
      Inc(a);
    end; {for y}
  end; {for x}
end;

procedure TForm2.StringGrid1DblClick(Sender: TObject);
begin
  if (StringGrid1.Cells[StringGrid1.col, StringGrid1.row] = 'X') then begin
    StringGrid1.Cells[StringGrid1.col, StringGrid1.row]:= '';
  end else begin
    StringGrid1.Cells[StringGrid1.col, StringGrid1.row]:= 'X';
  end;
end;

procedure TForm2.StringGrid2Click(Sender: TObject);
var
  x,y: integer;
  CurrentSlice: TSlice;
  i: integer;
begin
  x:= StringGrid2.Col;
  y:= StringGrid2.Row;
  CurrentSlice:= MySlices[x,y];
  for i:= 0 to 8 do begin
    SGDead.Cells[i mod 3, i div 3]:= CurrentSlice.CountDead(i).ToString;
    SGAlive.Cells[i mod 3, i div 3]:= CurrentSlice.CountAlive(i).ToString;
  end;
end;

procedure TForm2.StringGrid2DrawCell(Sender: TObject; ACol, ARow: Integer;
    Rect: TRect; State: TGridDrawState);
var
  C: TCanvas;
  CellText: string;
begin
  Rect.Offset(-2,0);
  Rect.Inflate(2,0);
  C:= TStringGrid(Sender).Canvas;
  if (StringGrid1.Cells[ACol+2, ARow+2] <> '') then begin
    C.Brush.Color:= RGB(255,200,200);
  end else begin
    C.Brush.Color:= clWhite;
  end;
  C.FillRect(Rect);
  CellText:= TStringGrid(Sender).Cells[ACol, ARow];
  if (CellText.Contains('-')) then C.Font.Color:= clRed
  else C.Font.Color:= clBlack;
  if (CellText = '0') then C.Font.Color:= clGreen;
  C.TextOut( Rect.Left+2, Rect.Top+2, CellText );
end;

procedure TForm2.StringGrid3Click(Sender: TObject);
var
  x,y: integer;
  CurrentSlice: TSlice;
  i: integer;
begin
  x:= StringGrid2.Col;
  y:= StringGrid2.Row;
  CurrentSlice:= (MySlices[x,y] xor OldSlices[x,y]);
  for i:= 0 to 8 do begin
    SGDeadDiff.Cells[i mod 3, i div 3]:= CurrentSlice.CountDead(i).ToString;
    SGAliveDiff.Cells[i mod 3, i div 3]:= CurrentSlice.CountAlive(i).ToString;
  end;
end;

procedure TForm2.StringGrid4DrawCell(Sender: TObject; ACol, ARow: Integer;
    Rect: TRect; State: TGridDrawState);
var
  SG: TStringGrid;
  Value: integer;
  S: string;
  x,y: integer;
  C: TCanvas;
  PixelRect: TRect;
  OldColor: TColor;
begin
  Rect.Offset(-2,0);
  Rect.Inflate(0,-4);
  SG:= TStringGrid(Sender);
  C:= SG.Canvas;
  S:= SG.Cells[ACol, ARow];
//  C.Pen.Style:= psDot;
//  C.Pen.Color:= clLtGray;
//  for x:= 1 to 2 do for y:= 1 to 2 do begin
//    C.MoveTo(Rect.Left+x*12, Rect.Top);
//    C.LineTo(Rect.Left+x*12, Rect.Top+36);
//    C.MoveTo(Rect.Left, Rect.Top+y*12);
//    C.LineTo(Rect.Left+36, Rect.Top+y*12);
//  end;
  if (S <> '') then begin
    OldColor:= C.Brush.Color;
    Value:= S.ToInteger;
    if (Value < 0) then begin
       C.Brush.Color:= clYellow;
       Value:= -Value;
    end else C.Brush.Color:= clAqua;
    C.FillRect(Rect);
    if (Value and 16) <> 0 then C.Brush.Color:= clNavy
    else C.Brush.Color:= clOlive;
    if (Value <> -1) then begin
      for y:= 0 to 2 do begin
        for x:= 2 downto 0 do begin
          if (Odd(Value)) then begin
            PixelRect:= System.Classes.Rect(Rect.Left, Rect.Top, Rect.Left+12, Rect.Top + 12);
            PixelRect.Offset(x*12, y*12);
            C.FillRect(PixelRect);
          end;
          Value:= Value shr 1;
        end; {for x}
      end; {for y}
    end;
    C.Brush.Color:= OldColor;
  end; {if PaintNeeded}
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
  Assert((Bit >= 0) and (Bit <= 511));
  Result:= (Self.Data4[Bit shr 5] and (1 shl (31 and Bit)) <> 0);
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
  i, index: integer;
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
  Mask: uint64;
  element: integer;
begin
  Mask:= (1 shl (Index and 63));
  element:= Index div 64;
  if (Value = false) then Data8[element]:= Data8[element] and not mask
  else Data8[element]:= Data8[element] or mask;
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
  for i:= 0 to High(a.Data8) do begin
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

function TSlice.CountAlive(Pixel: integer): integer;
begin
  Assert((Pixel >= 0) and (Pixel <= 8));
  Result:= (Self and not(OffMask[Pixel])).PopCount;
end;

function TSlice.CountDead(Pixel: integer): integer;
begin
  Assert((Pixel >= 0) and (Pixel <= 8));
  Result:= (Self and (OffMask[Pixel])).PopCount;
end;

class function TSlice.Empty: TSlice;
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
var
  i, j: integer;
  a: array [0 .. 7] of byte;
  b: uint64 absolute a;
begin
  for i:= 0 to 255 do begin
    // every bit gets broadcasted 8 times.
    for j:= 0 to 7 do begin
      if Odd(i shr j) then a[j]:= $FF
      else a[j]:= 0;
    end;
    Lookup012[i]:= b;
    Lookup0[i]:= ((i and 001) shl 0) or ((i and 001) shl 1) xor ((i and 002) shl 1) or ((i and 002) shl 2)
      xor ((i and 004) shl 2) or ((i and 004) shl 3) xor ((i and 008) shl 3) or ((i and 008) shl 4)
      xor ((i and 016) shl 4) or ((i and 016) shl 5) xor ((i and 032) shl 5) or ((i and 032) shl 6)
      xor ((i and 064) shl 6) or ((i and 064) shl 7) xor ((i and 128) shl 7) or ((i and 128) shl 8);

    //Double every nibble into a byte
    Lookup2[i]:= ((i and $0F) shl 0) or ((i and $0F) shl 4)
             xor ((i and $F0) shl 4) or ((i and $F0) shl 8);

  end; { for i }
  for i:= 0 to 256 * 256 - 1 do begin
    LookupRemove0[i]:= ((i and 001) shr 0) or ((i and 002) shr 1)
                   xor ((i and 004) shr 1) or ((i and 008) shr 2)
                   xor ((i and 016) shr 2) or ((i and 032) shr 3)
                   xor ((i and 064) shr 3) or ((i and 128) shr 4)
                   xor ((i and 256) shr 4) or ((i and 512) shr 5)
                   xor ((i and 1024) shr 5) or ((i and 2048) shr 6)
                   xor ((i and 4096) shr 6) or ((i and 8192) shr 7)
                   xor ((i and 16384) shr 7) or ((i and 32768) shr 8);
  end;
  for i:= 0 to 256*256-1 do begin
    LookupRemove2[i]:= ((i shr 0) and $0F) or ((i shr 4) and $0F)
                    or ((i shr 4) and $F0) or ((i shr 8) and $F0);
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
  //Start with 128 bytes
  for i:= 0 to 127 do begin
    E2.Words[i]:= E1.bytes[i] or (E1.bytes[i] shl 8);
  end;
  // Finally add pixel 11 by doubling the slice
  //Start with 256 bytes
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
  //Start with 512 bytes
  for i:= 0 to 255 do begin
    Temp1.bytes[i]:= LookupRemove0[Self.Words[i]];
  end;
  // Now pixels 48 renumber to 37
  // Next remove pixel 3 by oring every byte with its neighbor
  //Start with 256 bytes
  for i:= 0 to 127 do begin
    Temp2.bytes[i]:= Temp1.bytes[i * 2] or Temp1.bytes[i * 2 + 1];
  end;
  // Pixel 7 renumbers again to 6.
  // first remove pixel 6 by oring every int64 with its neighbor
 //Start with 128 bytes = 16 int64's
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
  //Start with 512 bytes
  for i:= 0 to 255 do begin
    Temp1.bytes[i]:= Self.bytes[i * 2] or Self.bytes[i * 2 + 1];
  end;
  // Now remove 6 (was 7) by oring every int64 with its neighbor
  //Start with 256 bytes = 32 int64's
  for i:= 0 to 15 do begin
    Temp2.Data8[i]:= Temp1.Data8[i * 2] or Temp1.Data8[i * 2 + 1];
  end;
  //Start with 128 bytes = 2 slices
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

class function TSliverHelper.NS(const North, South: TSlice): TSliver;
var
  N,S: TSliver;
  i: integer;
begin
  //First take the north slice and remove pixels 0,1,2
  for i:= 0 to 7 do begin
  // Remove pixel 012 by collapsing every byte into a bit.
    N.bytes[i]:= Remove012(North.Data8[i]);
  end;
  //Next remove pixels 678 from South
  //Do this by folding 8 slivers
  S:= South.Sliver[0];
  for i:= 1 to 7 do begin
    S:= S or South.Sliver[i];
  end;
  //Conjunct the two slivers
  Result:= N and S;
end;

class function TSliverHelper.NWSE(const NorthWest, SouthEast: TSlice): TSliver;
var
  NW, SE: TSliver;
  i: integer;
begin
  //First take the NW slice and remove pixel 0,1,2
  for i:= 0 to 7 do begin
    NW.bytes[i]:= Remove012(NorthWest.Data8[i]);
  end;
  //Now we are left with 6 pixels in the following layout
  //210
  //543
  //The west pixels need to be removed
  //First remove pixel 5, this means folding 2 dword
  //This reduces the sliver to 4 bytes
  NW.data4[0]:= NW.data4[0] or NW.data4[1];
  //Finally remove pixel 2
  //This reduces the sliver to a 16 bits.
  NW.bytes[0]:= TSuperSlice.LookupRemove2[NW.data2[0]];
  NW.bytes[1]:= TSuperSlice.LookupRemove2[NW.data2[1]];
  //Next we process the SE slice
  //First remove 678
  SE.Data8:= SouthEast.Data8[0];
  for i:= 1 to 7 do begin
    SE.Data8:= SE.Data8 or SouthEast.Data8[i];
  end;
  //Now we are left with 6 pixels in the following layout
  //210
  //543
  //We need to remove the eastern most pixels
  //First remove pixel 3 by folding every byte with its neighbor
  //This reduces the sliver from 8 to 4 bytes
  for i:= 0 to 3 do begin
    SE.bytes[i]:= SE.bytes[i*2] or SE.bytes[i*2+1];
  end;
  //The last step is to remove bit 0.
  //This reduces the sliver to 2 bytes.
  SE.bytes[0]:= TSuperSlice.LookupRemove0[SE.Data2[0]];
  SE.bytes[1]:= TSuperSlice.LookupRemove0[SE.Data2[1]];
  //Return the combined minislivers
  Result:= SE and NW;
end;

class function TSliverHelper.EW(const East, West: TSlice): TSliver;
var
  E,W: TSliver;
  Temp1, Temp2: TSlice;
  i: integer;
begin
  //West: drop pixels 2,5,8
  //first eliminate pixel 5
  //This reduces the size from 64 bytes = 16ints to 32 bytes = 8 ints
  for i:= 0 to 7 do begin
    Temp1.Data4[i]:= West.Data4[i*2] or West.Data4[i*2+1];
  end;
  //Next eliminate pixel 2.
  //This means folding every nibble with its neighbor
  //this reduces the size from 32 bytes into 16 bytes
  for i:= 0 to 15 do begin
    Temp2.bytes[i]:= TSuperSlice.LookupRemove2[Temp1.Data2[i]];
  end;
  //Eliminate pixel 8
  //Finally fold the two remaining slivers into one.
  //This reduces the size from 16 bytes to 8 bytes
  W:= Temp2.Sliver[0] or Temp2.Sliver[1];

  //Next create E by dropping pixels 0,3,6
  //First remove pixel6
  //This or's every int64 with its neighbor
  //reducing 64 (8 int64's) to 32 (4 int64's) bytes.
  for i:= 0 to 3 do begin
    Temp1.Data8[i]:= East.Data8[i*2] or East.Data8[i*2+1];
  end;
  //first remove pixel3
  //This or's every byte with its neighbor
  //We start with 32 bytes and reduce it to 16.
  for i:= 0 to 15 do begin
    Temp2.bytes[i]:= Temp1.bytes[i*2] or Temp1.bytes[i*2+1]
  end;
  //Finally remove pixel0.
  //This reduces from 16 bytes to 8
  for i:= 0 to 7 do begin
    E.bytes[i]:= TSuperSlice.LookupRemove0[Temp2.Data2[i]];
  end;
  Result:= W and E;
end;

function TSliverHelper.West: TSlice;
var
  Temp1, Temp2: TSlice;
  i: integer;
begin
  //Add pixels 2,5,8
  //First add pixel 2.
  //Expanding 8 into 16 bytes
  for i:= 0 to 7 do begin
    Temp1.Data2[i]:= TSuperSlice.Lookup2[Self.Bytes[i]];
  end;
  //Now add pixel 5
  //Double every uint32
  //Expanding 16 bytes (=4 int32) into 32 bytes
  for i:= 0 to 3 do begin
    Temp2.Data4[i*2]:= Temp1.Data4[i];
    Temp2.Data4[i*2+1]:= Temp1.Data4[i];
  end;
  //Finally add pixel 8
  //Just copy the half-slice twice.
  //Expanding 32 bytes into 64.
  Move(Temp2, Result, 32);
  Move(Temp2, (@Result.bytes[32])^, 32);
end;

class function TSliverHelper.NESW(const NorthEast, SouthWest: TSlice): TSliver;
var
  NE, SW: TSliver;
  i: integer;
begin
  //First take the NE slice and remove pixel 0,1,2
  for i:= 0 to 7 do begin
    NE.bytes[i]:= Remove012(NorthEast.Data8[i]);
  end;
  //Now we are left with 6 pixels in the following layout
  //210
  //543
  //The east pixels need to be removed
  //First remove pixel 3 by folding every byte with its neighbor
  //This reduces the sliver from 8 to 4 bytes
  for i:= 0 to 3 do begin
    NE.bytes[i]:= NE.bytes[i*2] or NE.bytes[i*2+1];
  end;
  //The last step is to remove bit 0.
  //This reduces the sliver to 2 bytes.
  NE.bytes[0]:= TSuperSlice.LookupRemove0[NE.Data2[0]];
  NE.bytes[1]:= TSuperSlice.LookupRemove0[NE.Data2[1]];
  //Next we process the SW slice
  //First remove 678
  SW.Data8:= SouthWest.Data8[0];
  for i:= 1 to 7 do begin
    SW.Data8:= SW.Data8 or SouthWest.Data8[i];
  end;
  //Now we are left with 6 pixels in the following layout
  //210
  //543
  //First remove pixel 5, this means folding every dword
  //This reduces the sliver to 4 bytes
  SW.data4[0]:= SW.data4[0] or SW.data4[1];
  //Finally remove pixel 2
  //This reduces the sliver to a 16 bits.
  SW.bytes[0]:= TSuperSlice.LookupRemove2[SW.data2[0]];
  SW.bytes[1]:= TSuperSlice.LookupRemove2[SW.data2[1]];
  //Combine the two minislivers
  Result:= SW and NE;
end;




function TSliverHelper.North: TSlice;
var
  i: integer;
begin
  //Add pixels 0,1,2
  //This means expanding every bit into a byte
  //Or rather every byte into an int64;
  for i:= 0 to 7 do begin
    Result.Data8[i]:= TSuperSlice.Lookup012[Self.bytes[i]];
  end;
end;

function TSliverHelper.East: TSlice;
var
  i: integer;
  Temp1, Temp2: TSlice;
begin
  //Add pixel 0, 3, 6
  //First add pixel 0.
  //This expands the sliver from 8 to 16 bytes
  for i:= 0 to 7 do begin
    Temp1.Data2[i]:= TSuperSlice.Lookup0[Self.bytes[i]];
  end;
  //Next add pixel 3, double every byte
  //this expands the sliver from 16 to 32 bytes
  for i:= 0 to 15 do begin
    Temp2.bytes[i*2]:= Temp1.bytes[i];
    Temp2.bytes[i*2+1]:= Temp1.bytes[i];
  end;
  //Finally add pixel 6, his doubles every int64.
  //Expanding the slice from 32 (= 4 int64) into 64 bytes (=8int64)
  for i:= 0 to 3 do begin
    Result.Data8[i*2]:= Temp2.Data8[i];
    Result.Data8[i*2+1]:= Temp2.Data8[i];
  end;
end;


function TSliverHelper.NorthEast: TSlice;
var
  Temp1, Temp2: TSlice;
  i: integer;
begin
  //We start with
  //---   ---
  //10-   BA-
  //32-   DC-
  //Add pixels 0,1,2
  //This means expanding every bit into a byte
  //Or rather every byte into an int64;
  //This expands 2 bytes into 16 bytes
  for i:= 0 to 1 do begin
    Temp1.Data8[i]:= TSuperSlice.Lookup012[Self.bytes[i]];
  end;
  //Now we have
  //210   210
  //BA-   43-
  //DC-   65-
  //We already have pixel 0, so we only need to add pixel 3 and 6.
  //Next add pixel 3, double every byte
  //this expands the sliver from 16 to 32 bytes
  for i:= 0 to 15 do begin
    Temp2.bytes[i*2]:= Temp1.bytes[i];
    Temp2.bytes[i*2+1]:= Temp1.bytes[i];
  end;
  //Finally add pixel 6, his doubles every int64.
  //Expanding the slice from 32 (= 4 int64) into 64 bytes (=8int64)
  for i:= 0 to 3 do begin
    Result.Data8[i*2]:= Temp2.Data8[i];
    Result.Data8[i*2+1]:= Temp2.Data8[i];
  end;
end;

function TSliverHelper.NorthWest: TSlice;
var
  Temp1, Temp2: TSlice;
  i: integer;
begin
  //Add pixels 0,1,2   {the north part}
  //This means expanding every bit into a byte
  //Or rather every byte into an int64;
  for i:= 0 to 7 do begin
    Temp1.Data8[i]:= TSuperSlice.Lookup012[Self.bytes[i]];
  end;
  //We already have pixel 2, so just add pixel 5 and 8
  //Now add pixel 5
  //Double every uint32
  //Expanding 16 bytes (=4 int32) into 32 bytes
  for i:= 0 to 3 do begin
    Temp2.Data4[i*2]:= Temp1.Data4[i];
    Temp2.Data4[i*2+1]:= Temp1.Data4[i];
  end;
  //Finally add pixel 8
  //Just copy the half-slice twice.
  //Expanding 32 bytes into 64.
  Move(Temp2, Result, 32);
  Move(Temp2, (@Result.bytes[32])^, 32);
end;

function TSliverHelper.South: TSlice;
var
  i: integer;
begin
  //Add pixels 678
  //This means copying the pattern 8 times
  for i:= 0 to 7 do begin
    Result.Data8[i]:= Self.Data8;
  end;
end;

function TSliverHelper.SouthEast: TSlice;
var
  i: integer;
  Temp1, Temp2: TSlice;
begin
  //Add pixels 0, 3, 6, 7, 8
  //First add pixel 0.
  //This expands the sliver from 2 to 4 bytes
  for i:= 0 to 1 do begin
    Temp1.Data2[i]:= TSuperSlice.Lookup0[Self.bytes[i]];
  end;
  //Next add pixel 3, double every byte
  //this expands the sliver from 4 to 8 bytes
  for i:= 0 to 3 do begin
    Temp2.bytes[i*2]:= Temp1.bytes[i];
    Temp2.bytes[i*2+1]:= Temp1.bytes[i];
  end;
  //Add pixels 678
  //This means copying the pattern 8 times
  for i:= 0 to 7 do begin
    Result.Data8[i]:= Temp2.Data8[0];
  end;
end;

function TSliverHelper.SouthWest: TSlice;
var
  Temp1: TSlice;
  i: integer;
begin
  //Add pixels 2,5,6,7,8
  //First add pixel 2.
  //Expanding 2 into 4 bytes
  for i:= 0 to 1 do begin
    Temp1.Data2[i]:= TSuperSlice.Lookup2[Self.Bytes[i]];
  end;
  //Now add pixel 5
  //Double every uint32
  //Expanding 4 bytes (=1 int32) into 8 bytes
  Temp1.Data4[1]:= Temp1.Data4[0];
  //Add pixels 678
  //This means copying the pattern 8 times
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

class operator TCake.Implicit(a: TCake): integer;
begin
  Result:= a.Data;
end;

class operator TCake.Implicit(a: integer): TCake;
begin
  Result.Data:= a;
end;

function TCake.East: integer;
begin
  //Remove bits 01234,9,14,19, 20..24
  Result:= (((Data shr  5) and $0F) shl 0) xor //Isolate 5678
           (((Data shr 10) and $0F) shl 4) xor //10 11 12 13
           (((Data shr 15) and $0F) shl 8);    //15 16 17 18
end;

function TCake.North: integer;
begin
  Result:= (((Data shr  1) and $7) shl 0) xor
           (((Data shr  6) and $7) shl 3) xor
           (((Data shr 11) and $7) shl 6) xor
           (((Data shr 16) and $7) shl 9);
end;

function TCake.South: integer;
begin
  Result:= (((Data shr  6) and 7) shl 0) xor
           (((Data shr 11) and 7) shl 3) xor
           (((Data shr 16) and 7) shl 6) xor
           (((Data shr 21) and 7) shl 9);
end;

function TCake.West: integer;
begin
  Result:= (((Data shr  6) and $0F) shl 0) xor
           (((Data shr 11) and $0F) shl 4) xor
           (((Data shr 16) and $0F) shl 8);
end;

{ TLookupTable }


function TLookupTable.GetItems(Offset: TOffset; Index: integer): TSlice;
var
  IndexOffset: integer;
begin
  IndexOffset:= Ord(Offset) * TLookupTable.IndexStride;
  Result:= FData[FIndex[Index+IndexOffset]];
end;

procedure TLookupTable.LoadData(const Filename: string);
var
  FS: TFileStream;
begin
  FS:= TFileStream.Create(Filename, fmOpenRead);
  SetLength(FData, FS.Size div SizeOf(TSlice));
  try
    FS.Read64(TBytes(FData), 0, FS.Size);
  finally
    FS.Free;
  end;
end;

procedure TLookupTable.LoadIndex(const Filename: string);
var
  FS: TFileStream;
begin
  FS:= TFileStream.Create(Filename, fmOpenRead);
  SetLength(FIndex, FS.Size div SizeOf(integer));
  TlookupTable.IndexStride:= FS.Size div (SizeOf(integer) * 5); //we have 5 lookup tables stored
  try
    FS.Read64(TBytes(FIndex), 0, FS.Size);
  finally
    FS.Free;
  end;
end;

end.
