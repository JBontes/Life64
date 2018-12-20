unit Universe;

interface

uses
  System.Classes, System.Types, WinApi.Windows;

type

  { TDiBits }
  /// <summary>
  ///  Encapsulation of a monochrome bitmap
  ///  for displaying Life patterns on the screen
  /// </summary>
  TDiBits = record
  public
    function Stride: integer; inline;
    class function Create: TDIBits; static;
  public
    bmpinfo: PBitmapInfo;
    data: PByte;
    width, height: cardinal;
  end;

  /// <summary>
  ///  One of the six directions supported by stagger stepping
  /// </summary>
  TDirection = (dN,dW,dNW,dS,dE,dSE);

  { TUnit }

const
  BytesPerUnit = 16;  //Units are made up of XMM words.
  MaxBytesPerUnit = BytesPerUnit -1;
  North = -8;
  South = -North;
  West = -1;
  East = -West;
  NorthWest = -9;
  SouthEast = -NorthWest;
  UnitsPerBlock = 4 {x} * 8 {y};   //32 units per block. or 64x64 pixels.

type
  /// <summary>
  ///  Used by a Node of Leaf to track the activity at its borders.
  ///  Because of stagger stepping we only need to track N,W,NW activity in the P stage
  ///  and S,E,SE activity in the Q stage.
  ///  This maps to the Horz, Vert and Corner.
  /// </summary>
  TBorderMap = record
  private
    FHorz, FVert, FCorn: integer;
  public
    procedure Activate(index, offset: integer);
    procedure ClearAll;
    procedure ActivateAll;
    property Horizontal: integer read FHorz;
    property Vertical: integer read FVert;
    property Corner: integer read FCorn;
  end;

  /// <summary>
  ///  A list of 64 items to process in a TNode.
  /// </summary>
  TDoMap2 = record
  private
    a: Int64;
  public
    function Next(previous: integer): integer;
    procedure Clear;
    procedure ActivateAll;
    procedure Activate(index: integer); overload;
    function Activate(index, Offset: integer): boolean; overload;
    procedure Invert(index: integer);
    procedure SubtractAdd(var Subtract, Add: TDoMap2);
    function IsActive(Direction: TDirection): boolean;
    function IsActiveP2(Direction: TDirection): boolean;
  end;

  /// <summary>
  ///  A list of 32 items to process in a TCellBlock
  /// </summary>
  TDoMap = record
  {strict} private
    a: integer;
    {TODO -oJB -cTDoMap : Check the masks}
  public
    function Next(previous: integer): integer;
    function TestNext(previous: integer): integer;
    procedure ActivateAll;
    procedure Clear;
    procedure ActivateMultiple(Mask: integer);
    procedure Activate(index: integer); overload;
    function Activate(index, Offset: integer): boolean; overload;
    procedure Deactivate(index: integer);
    procedure Invert(index: integer);
    procedure SubtractAdd(var Subtract, Add: TDoMap);
    function IsActive(direction: TDirection): boolean;
    function IsActiveP2(direction: TDirection): boolean;
  end;

  PUnit = ^TUnit;
  /// <summary>
  ///  A Unit is a 16 x 8 pixel subunit of the universe
  ///  TUnit is always contained within a TCellBlock
  ///  It matches a xmm word inside the CPU
  /// </summary>
  TUnit = record
    procedure Display(const BitmapStart: pointer; const LineOffset: integer);
    procedure SetPixel(x,y: integer; value: boolean = true);
    function GetPixel(x, y: integer): boolean;
    case integer of
      1: (b: array[0..MaxBytesPerUnit] of byte);
      2: (w: array[0..(BytesPerUnit div 2)-1] of word);
      3: (i: array[0..(BytesPerUnit div 4)-1] of integer);
      4: (q: array[0..(BytesPerUnit div 8)-1] of int64);
  end;



/// Blocks are organized like so:
/// TUnit is the lowest level, rows of 16 bits are stored like so:
/// 0123456789ABCDEF     row  0  LSByte    LSB = Left, MSB = Right
/// 0123456789ABCDEF     row  1
/// 0123456789ABCDEF     row  2
/// 0123456789ABCDEF     row  3
/// 0123456789ABCDEF     row  4
/// 0123456789ABCDEF     row  5
/// 0123456789ABCDEF     row  6
/// 0123456789ABCDEF     row  7  MSByte
/// Thus 16 bytes are stored in a TUnit.
/// A TUnit fits in a XMM register and is always processed as a whole.
///
/// 16 * 8  = 128 Units make up a block like so:
/// 00 01 02 03 04 05 06 07     row 0
/// 08 09 0A 0B 0C 0D 0E 0F     row 1
/// 10 11 12 13 14 15 16 17     row 2
/// 18 19 1A 1B 1C 1D 1E 1F     row 3
/// 20 21 22 23 24 25 26 27     row 4
/// 28 29 2A 2B 2C 2D 2E 2F     row 5
/// 30 31 32 33 34 35 36 37     row 6
/// 38 39 3A 3B 3C 3D 3E 3F     row 7
/// 40 41 42 43 44 45 46 47     row 8
/// 48 49 4A 4B 4C 4D 4E 4F     row 9
/// 50 51 52 53 54 55 56 57     row A
/// 58 59 5A 5B 5C 5D 5E 5F     row B
/// 60 61 62 63 64 65 66 67     row C
/// 68 69 6A 6B 6C 6D 6E 6F     row D
/// 70 71 72 73 74 75 76 77     row E
/// 78 79 7A 7B 7C 7D 7E 7F     row F
/// Every unit is a 16W x 8H bitmap


type
  //bit    7----6----5----4----3----2----1----0
  //      P2+  P2+  P2+ alive P2+ alive  P1+ alive
  //      corn all   EW   EW   NS  NS   all  all

  //         b0,       b1,        b2,       .....
  TStatus = (AllAlive, AllActive, NS_Alive, NS_P2Plus, EW_Alive, EW_P2Plus, AllP2Plus, Corner_P2Plus);
  TGenerateStatus = set of TStatus;

  /// <summary>
  ///  An empty record used to overlap structures
  /// </summary>
  TPlaceHolder = record
  end;

  /// <summary>
  ///  The data (P or Q) of a TCellBlock
  /// </summary>
  TUnitStorage = array[0..UnitsPerBlock-1] of TUnit;
  TCellBlock = class;
  TNode = class;


  ///  The universe is stored as a 64-ary tree
  ///  The leafs hold the data in TCellBlocks (see below)
  ///  TCellBlocks hold 64x64 pixels of data
  ///  The nodes  hold 64 leafs in a 8x8 grid.
  TAbstractNode = class
  private
    class var Generation: int64;
  private
    FLevel: integer;
    ///  Every node stores the real x/y coordinate of its 0,0 point here.
    ///  A Cellblock occupies 64x64 pixels and thus takes the lower 6 bits from the
    ///  x and y coordinates to get the pixel.
    ///  A node first shifts x and y down by 3+another 3 * level.
    ///  Then we take newx/newy and 7 to get the local x/y.

    FX,FY: integer;
    FIndex: integer; //duplicate with x,y to prevent constant recalculations
    FParent: TNode;
    function GetNodeIndex(x, y: integer): integer;
    function GetRect: TRect;
  protected
    function GetLifeCellBlock(x,y: integer; ForceCreation: boolean): TCellBlock; virtual; abstract;
    function GetSubNode(index: integer): TAbstractNode; virtual;
    function GetIsLeaf: boolean; inline;
    function GoNorth(index: integer): TAbstractNode;
    function GoWest(index: integer): TAbstractNode;
    function GoNorthWest(index: integer): TAbstractNode;
    function GoSouth(index: integer): TAbstractNode;
    function GoEast(index: integer): TAbstractNode;
    function GoSouthEast(index: integer): TAbstractNode;
  public
    function GeneratePtoQ: TGenerateStatus; virtual; abstract;
    function GenerateQtoP: TGenerateStatus; virtual; abstract;
    procedure Display(const DrawSurface: TDIBits; const DisplayRect: TRect); virtual; abstract;
    procedure SetPixel(x,y: integer; state: boolean = true); virtual; abstract;
    function GetPixel(x,y: integer): boolean; virtual; abstract;
  public
    property Level: integer read FLevel;
    property IsLeaf: boolean read GetIsLeaf;
  end;


  /// <summary>
  ///  TCellBlock is a 64 x 64 pixel bitmap inside the universe.
  ///  It consists of 4 Units in the x-dimension
  ///  and 8 Units in the y dimension
  /// </summary>
  TCellBlock = class(TAbstractNode)
  private
    class var FEmpty: TUnit;
    class var qstate: boolean;
    class constructor Init;
  protected
    function GetLifeCellBlock(x,y: integer; ForceCreation: boolean): TCellBlock; override;
  public
    constructor Create(X,Y: integer; Parent: TNode); overload;
    procedure CoordinateNodeToBitmap(var x,y: integer; DisplayQ: boolean); inline;
    function GeneratePtoQ: TGenerateStatus; override;
    function GenerateQtoP: TGenerateStatus; override;
    /// <summary>
    ///  Draws the units of the cellblock on a monochrome bitmap.
    /// </summary>
    /// <param name="DrawSurface">
    ///  The bitmap to draw on
    /// </param>
    /// <param name="DrawRect">
    ///  The bounding rect of the bitmap. In pixel coordinates.
    ///  A bitmap always starts on a multiple of 16x16 pixels and the width/height of a bitmap
    ///  is always a multiple of 16 pixels.
    /// </param>
    procedure Display(const DrawSurface: TDIBits; const DrawRect: TRect); override;
    procedure SetPixel(x,y: integer; state: boolean = true); override;
    function GetPixel(x,y: integer): boolean; override;
    /// <summary>
    ///  Set the inner 4x4 Block.
    ///  The input is a word where the least significant nibble is the top row
    /// </summary>
    procedure SetMiniCore(const Block: integer);
    procedure SetCore(const Block: Int64);
    /// <summary>
    ///  Calculate 16 4x4 blocks as fast as possible
    ///  Here we only care about the 16 inner 2x2 blocks that get generated as a result.
    ///  The proc requires that p[0] and p[4] have been prefilled with the data to be calculated
    ///  prior. The input is an array of int64 that are pasted into the data to be calculated.
    ///  Returns 16 blocks of nibbles holding the 16 inner 2x2 blocks for each 4x4 block inputted.
    /// </summary>
    function CalculatePtoQ_16_4x4_Blocks(const input: TArray<int64>): Int64;
    function Get4x4(AUnit: pointer; FirstByte: integer): integer;
    function Get2x2(AUnit: pointer; FirstByte: integer): integer;
  protected
    function CellCount: integer;
    function GetSubNode(index: integer): TAbstractNode; override;
    //procedure Difference(Generation: integer; var Diff: TUnitStorage);
    //function DifferenceCount(Generation: integer): integer;
  public
    //4 units wide by 8 units high
    p: array[0..UnitsPerBlock-1] of TUnit;
    q: array[0..UnitsPerBlock-1] of TUnit;   //must be continuous, p must come before q
    AddSubtract: TPlaceHolder;
    Subtractions: TDoMap;                    //Subtractions are all the units that should die
    Additions: TDoMap;                       //Additions are all the units that should come alive
    Border: TBorderMap;
    ToDoList: TDoMap;                        //All the blocks that are to be processed.
    //First we do the subtractions and then we do the additions, to make sure that in case of conflict the
    //additions win out.
  end;

  /// <summary>
  ///  TNode is a node in the 64-ary tree.
  ///  It can hold either TCellBlocks in its SubNodes, in which case it is a Leaf node
  ///  or more TNodes in its SubNodes.
  /// </summary>
  TNode = class(TAbstractNode)
  public
    function GeneratePtoQ: TGenerateStatus; override;
    function GenerateQtoP: TGenerateStatus; override;
    procedure DisplayQ(const DrawSurface: TDIBits);
    procedure DisplayP(const DrawSurface: TDIBits);
    constructor Create(X,Y: integer; Parent: TNode);
    procedure SetPixel(x,y: integer; state: boolean = true); override;
    function GetPixel(x,y: integer): boolean; override;
  private
    ToDoList: TDoMap2;
    AddSubtract: TPlaceHolder;
    Subtractions: TDoMap2;
    Additions: TDoMap2;
    Border: TDoMap2;
    FSubNodes: array[0..63] of TAbstractNode;    //can be either PCellBlock or TNode.
    function GetIsRoot: boolean; inline;
    function GetSubNode(x, y: integer): TAbstractNode; reintroduce; overload; inline;
    procedure SetSubNode(x,y: integer; const Value: TAbstractNode);
    function NewSubNode(x, y: integer): TAbstractNode;
  protected
    function GetLifeCellBlock(x,y: integer; ForceCreation: boolean): TCellBlock; override;
  public
    procedure Display(const DrawSurface: TDIBits; const DisplayRect: TRect); override;
    property SubNode[x,y: integer]: TAbstractNode read GetSubNode write SetSubNode;
    property IsRoot: boolean read GetIsRoot;
    property Pixel[x,y: integer]: boolean read GetPixel write SetPixel;
  public
    FStaticCount: int64;
    FOscillatingCount: int64;
    FChoaticCount: int64;
    FDyingcount: int64;
  end;


   {$L 'C:\Users\Johan\Documents\Embarcadero\Studio\Projects\Life64\Lazarus\lib\x86_64-win64\AVXGenerate.o'}

function GenerateQtoP_AVX_32(main, N,W,NW: pointer): byte;
  external name 'AVXGENERATE_$$_GENERATEQTOP_AVX_32$POINTER$POINTER$POINTER$POINTER$$BYTE';
function GeneratePtoQ_AVX_32(main, S,E,SE: pointer): byte;
  external name 'AVXGENERATE_$$_GENERATEPTOQ_AVX_32$POINTER$POINTER$POINTER$POINTER$$BYTE';
procedure ReverseBitsInAllBytes(ReverseMe: pointer);
  external name 'AVXGENERATE_$$_REVERSEBITSINALLBYTES$POINTER';

implementation

uses
  Math;

{ inline methods }

function TAbstractNode.GetIsLeaf: boolean;
begin
  Result:= FLevel = 0;
end;

function TAbstractNode.goNorth(index: integer): TAbstractNode;
begin
  if index in [0..7] then begin
    Result:= FParent.GoNorth(index).GetSubNode(index + North + 64);
  end else Result:= FParent.GetSubNode(index + North);
end;

function TAbstractNode.goWest(index: integer): TAbstractNode;
begin
  if (index and 7) = 0 then begin
    Result:= FParent.GoWest(index).GetSubNode(index + West + 8)
  end else Result:= FParent.GetSubNode(index + West);
end;


function TAbstractNode.GetNodeIndex(x, y: integer): integer;
var
  Shift: byte;
begin
  if (Level = 0) then begin
    Result:= ((x and 63) shr 4) + (y and $38);
  end else begin
    ///  Every node stores the real x/y coordinate of its 0,0 point here.
    ///  A Cellblock occupies 64x64 pixels and thus takes the lower 6 bits from the
    ///  x and y coordinates to get the pixel.
    ///  A node first shifts x and y down by 3+another 3 * level.
    ///  Then we take newx/newy and 7 to get the local x/y.
    Shift:= 3 + (3 * Level);
    Result:= ((x shr Shift) and 7) + ((y shr Shift) and 7) * 8;
  end;
end;

function TAbstractNode.GetRect: TRect;
begin
  var Width:= 1 shl (6+(3 * (FLevel + 1)));
  Result:= Rect(FX, FY, FX+Width, FY+Width);
end;

function TAbstractNode.GetSubNode(index: integer): TAbstractNode;
begin
  Result:= Self;
end;

function TAbstractNode.goNorthWest(index: integer): TAbstractNode;
begin
  if index = 0 then Result:= FParent.GoNorthWest(index).GetSubNode(63)
  else if (index in [1..7]) then Result:= FParent.GoNorth(index).GetSubNode(index + NorthWest + 64)
  else if ((index and 7)=0) then Result:= FParent.GoWest(index).GetSubNode(index + NorthWest + 8)
  else Result:= FParent.GetSubNode(index + NorthWest);
end;

function TAbstractNode.goSouth(index: integer): TAbstractNode;
begin
  if index in [56..63] then begin
    Result:= FParent.GoSouth(index).GetSubNode(index + South - 64);
  end else Result:= FParent.GetSubNode(index + South);
end;

function TAbstractNode.goEast(index: integer): TAbstractNode;
begin
  if (index and 7) = 7 then begin
    Result:= FParent.GoEast(index).GetSubNode(index + East - 8)
  end else Result:= FParent.GetSubNode(index + East)
end;

function TAbstractNode.goSouthEast(index: integer): TAbstractNode;
begin
  if index = 63 then Result:= FParent.GoSouthEast(index).GetSubNode(0)
  else if (index in [56..63]) then Result:= FParent.GoSouth(index).GetSubNode(index + SouthEast - 64)
  else if ((index and 7)=7) then Result:= FParent.GoEast(index).GetSubNode(index + SouthEast - 8)
  else Result:= FParent.GetSubNode(index + SouthEast);
end;


{ TNode }

function TNode.GetSubNode(x,y: integer): TAbstractNode;
begin
  Result:= FSubNodes[GetNodeIndex(x,y)];
end;

constructor TNode.Create(X, Y: integer; Parent: TNode);
begin
  inherited Create;
  FParent:= Parent;
  if (Assigned(Parent)) then begin
    FLevel:= Parent.Level - 1;
    FIndex:= GetNodeIndex(FX,FY);
    {TODO -oJB -cTNode.Create : Query the neighbor nodes to see if they have activity}
  end;
  FX:= X and not(((FLevel+1) * 8)-1);
  FY:= Y and not(((FLevel+1) * 8)-1);
end;

procedure TNode.Display(const DrawSurface: TDIBits; const DisplayRect: TRect);

  //Let's not call a WinAPI function just to overlap two rects.
  function IntersectRect(const A,B: TRect): TRect;
  begin
    Result:= TRect.Empty;
    var Left:= Max(A.Left, B.Left);
    var Right:= Min(A.Right, B.Right);
    var Top:= Max(A.Top, B.Top);
    var Bottom:= Min(A.Bottom, B.Bottom);
    if (Right >= Left) and (Bottom >= Top) then Result:= Rect(Left, Top, Right, Bottom);
  end;

begin
  var Rect:= IntersectRect(DisplayRect, Self.GetRect);
  if Rect.IsEmpty then exit
  else for var i:= 0 to 63 do begin
    FSubNodes[i].Display(DrawSurface, DisplayRect);
  end; {for i}
end;

procedure TNode.DisplayP(const DrawSurface: TDIBits);
begin

end;

procedure TNode.DisplayQ(const DrawSurface: TDIBits);
begin

end;

function TNode.GeneratePtoQ: TGenerateStatus;
var
  i: integer;
  P: TAbstractNode;
  status: TGenerateStatus;
begin
  FillChar(AddSubtract, SizeOf(TDoMap2)*2, 0);
  i:= ToDoList.Next(-1);
  while (i < 64) do begin
    P:= FSubNodes[i];
    status:= P.GeneratePtoQ;
    //All of the cell.
    //if (AllActive in status) then begin
    if ([AllP2Plus, NS_P2Plus, EW_P2Plus] * status <> []) then begin
      Additions.Activate(i);
      if (NS_P2Plus in status) then begin
        Additions.Activate(i,North);
        Border.Activate(i, North);
      end;
      if (EW_P2Plus in status) then begin
        Additions.Activate(i,West);
        Border.Activate(i,West);
        if (Corner_P2Plus in status) then begin
          Additions.Activate(i,NorthWest);
          Border.Activate(i,NorthWest);
        end;
      end;
    end else Subtractions.Activate(i);
    i:= ToDoList.Next(i);
  end; {while/for}
  ToDoList.SubtractAdd(Subtractions, Additions);
end;

function TNode.GenerateQtoP: TGenerateStatus;
var
  i: integer;
  Q: TAbstractNode;
  status: TGenerateStatus;
begin
  FillChar(AddSubtract, SizeOf(TDoMap2)*2, 0);
  //for i:= 127 downto 0 do begin
  i:= ToDoList.Next(-1);
  while (i < 64) do begin
    Q:= FSubNodes[i];
    status:= Q.GenerateQtoP;
    //All of the cell.
    //if (AllActive in status) then begin
    if ([AllP2Plus, NS_P2Plus, EW_P2Plus] * status <> []) then begin
      Additions.Activate(i);
      if (NS_P2Plus in status) then Additions.Activate(i,South);
      if (EW_P2Plus in status) then begin
        Additions.Activate(i,East);
        if (Corner_P2Plus in status) then Additions.Activate(i,SouthEast);
      end;
    end else Subtractions.Activate(i);
    i:= ToDoList.Next(i);
  end; {while/for}
  ToDoList.SubtractAdd(Subtractions, Additions);
end;

function TNode.GetIsRoot: boolean;
begin
  Result:= FParent = nil;
end;

function TNode.GetLifeCellBlock(x, y: integer; ForceCreation: boolean): TCellBlock;
begin
  var Subnode:= GetSubNode(x,y);
  if ForceCreation and (SubNode = nil) then begin
    SubNode:= NewSubNode(x,y);
  end;
  Result:= SubNode.GetLifeCellBlock(x,y,ForceCreation);
end;

function TNode.GetPixel(x, y: integer): boolean;
begin
  Result:= FSubNodes[GetNodeIndex(x,y)].GetPixel(x,y);
end;


function TNode.NewSubNode(x,y: integer): TAbstractNode;
begin
  if (Level = 0) then Result:= TCellBlock.Create(x,y,self)
  else Result:= TNode.Create(x,y,Self);
  SubNode[x,y]:= Result;
end;

procedure TNode.SetPixel(x, y: integer; state: boolean);
begin
  FSubnodes[GetNodeIndex(x,y)].SetPixel(x,y,state);
end;

procedure TNode.SetSubNode(x, y: integer; const Value: TAbstractNode);
begin
  FSubNodes[GetNodeIndex(x,y)]:= Value;
end;

{ TAbstractNode }

function TCellBlock.CalculatePtoQ_16_4x4_Blocks(const input: TArray<int64>): Int64;
  procedure PasteData;
  var
    l: integer;
    i,j: integer;
    output: int64;
  begin
    l:= Length(input)-1;
    if (l=-1) then exit;
    for i:= 0 to 3 do begin
      output:= 0;
      for j:= 0 to 3 do begin
        output:= output or (input[l] shl j);
        dec(l);
        if (l < 0) then break;
      end;
      p[(i and 2) * 2].q[i and 1]:= output;
      if (l < 0) then exit;
    end;
  end;

  function Extract16Cores: Int64;
  asm
    //The input was in P, the output will be in Q.
    //Q is located one pixel SE from P, so the first output nibble will be in the
    //LSB corner of Q[0].
    mov r10d,2                    //repeat 2 times
  @loop:
    mov eax,[rcx+TCellBlock.q]    //Get the first 4 rows of Q[0].
    mov edx,eax
    and eax,$33330000             //mask off the part we are interested in, 4 cores
    and edx,$00003333             //Get the low bits in edx
    shr eax,16-2                  //Line up the row2 with row 1
    or eax,edx                    //We have the first 4 cores in eax (16 bytes)
    mov edx,[rcx+TCellBlock.q+8]  //Get the next 4 rows of Q[0]
    mov r8,rdx                    //
    and edx,$00003333             //upper row in edx
    and r8d,$33330000             //lower row in r8d
    shl edx,16-2                  //line up the upper and lower rows, remember we need
                                  //to still shift up by 2 later
    or edx,r8d                    //combine the two lines into 4 cores: 4567
    lea eax,[eax+edx*4]           //combine cores 0123 with cores 4567 (incl the delayed shift by 2)

    sub r10d,1                    //
    cmovnz r9,rax                 //save part1 for later
    lea rcx,[rcx+(4*type TUnit)]  //repeat this for Q[4]
    jnz @loop
    shl rax,32                    //return cores 89ABCDEF in the high part
    or rax,r9                     //combine with cores 01234567 to retun all 16 cores
  end;

begin
  PasteData;
  GeneratePtoQ_AVX_32(@p[0], @p[4],@TCellBlock.FEmpty,@TCellBlock.FEmpty);
  GeneratePtoQ_AVX_32(@p[4], @TCellBlock.FEmpty,@TCellBlock.FEmpty,@TCellBlock.FEmpty);
  Result:= Extract16Cores;
end;


function TCellBlock.CellCount: integer;
asm
  //rcx = self
  //rdx = generation
  and edx,1
  shl edx,11
  lea rcx,[rcx+rdx+2048]
  mov rdx,-2048
  xor eax,eax
  @loop:
    popcnt r8,qword ptr [rcx+rdx]
    add rax,r8
    add rdx,8
  jnz @loop
end;

procedure TCellBlock.CoordinateNodeToBitmap(var x, y: integer; DisplayQ: boolean);
begin
  x:= x * 128;
  y:= y * 128;
end;


constructor TCellBlock.Create(X, Y: integer; Parent: TNode);
begin
  FX:= X;
  FY:= Y;
  FIndex:= GetNodeIndex(x,y);
  FParent:= Parent;
  //Assert(Assigned(Parent), 'A Cellblock cannot have a nil parent');
end;


function Max(A,B: integer): integer;
asm
  //ecx=a
  //edx=b
  cmp   A, B
  mov   eax, B
  cmovnl eax, A
end;

function Min(A,B: integer): integer;
asm
  //ecx=a
  //edx=b
  cmp   A, B
  mov   eax, B
  cmovl eax, A
end;

function IntersectRect(const RectA,RectB: TRect): boolean;
begin
  Result:= (RectA.Left < RectB.Right) and (RectA.Right > RectB.Left) and
           (RectA.Top < RectB.Bottom) and (RectA.Bottom > RectB.Top);
end;

procedure TCellBlock.Display(const DrawSurface: TDiBits; const DrawRect: TRect);
var
  stride: integer;
  MinX, MaxX, MinY, MaxY: integer;
  OffsetX, OffsetY, Offset: integer;
  x,y,i: integer;
begin
  X:= Self.FX;
  Y:= Self.FY;
  if (IntersectRect(DrawRect, Rect(X*64, Y*64, (X+63)*64, (Y+63)*64))) then begin
    Stride:= DrawRect.width div 8; //64 pixels per CellBlock, 8 pixels per byte, thus 8 bytes per CellBlock
    // The TCellBlock is 64X by 64Y pixels. 4 units of 16 px horz and 8 units of 8 px vertically
    MinX:= Max((FX * 64) - DrawRect.Left, 0) div 16;
    MaxX:= Min((FX * 64) + 63 - DrawRect.Right, 63) div 16;
    MinY:= Max((FY * 64) - DrawRect.Top, 0) div 8;
    MaxY:= Min((FY * 64) + 63 - DrawRect.Bottom, 63) div 8;
    OffsetX:= (FX * 64) - DrawRect.Left div 8;  //Offset in bytes
    OffsetY:= (FY * 64) - DrawRect.Top * Stride;
    for x:= MinX to MaxX do for y:= MinY to MaxY do begin
      i:= x+y*4;
      Offset:= OffsetX + (x*16) + OffsetY + (y * Stride);
      if (Self.qstate) then Self.q[i].Display(pointer(NativeInt(DrawSurface.data) + Offset), Stride)
      else Self.p[i].Display(pointer(NativeInt(DrawSurface.data) + Offset), Stride);
    end;
  end;
end;

function TCellBlock.GeneratePtoQ: TGenerateStatus;
var
  i: integer;
  N,W,NW: PUnit;
  bN, bW, bNW: TAbstractNode;
  NIndex, WIndex, NWIndex: integer;
  status: TGenerateStatus;
begin
  bN:= FParent.GoNorth(FIndex);
  bW:= FParent.GoWest(FIndex);
  bNW:= FParent.GoNorthWest(FIndex);
  FillChar(AddSubtract, SizeOf(TDoMap)*2, 0);
  i:= ToDoList.Next(-1);
  while (i < 32) do begin
    NIndex:= i - 4;
    WIndex:= i - 1;
    NWIndex:= i - 5;
    N:= @p[NIndex];
    W:= @p[WIndex];
    NW:= @p[NWIndex];
    if i < 4 then begin
      N:= @TCellBlock(bN).p[Nindex + 32];
      NW:= @TCellBlock(bNW).p[NWIndex + 32];
    end;
    if not(boolean(i and 3)) then begin
      W:= @TCellBlock(bW).p[WIndex + 4];
      NW:= @TCellBlock(bNW).p[NWIndex + 4];
    end;
    status:= TGenerateStatus(GeneratePtoQ_AVX_32(@p[i],N,W,NW));
    //All of the cell.
    //if (AllActive in status) then begin
    if ([AllP2Plus, NS_P2Plus, EW_P2Plus] * status <> []) then begin
      Additions.Activate(i);
      if (NS_P2Plus in status) then begin
        Additions.Activate(i,North);
        Border.Activate(i, North);
      end;
      if (EW_P2Plus in status) then begin
        Additions.Activate(i,West);
        Border.Activate(i,West);
        if (Corner_P2Plus in status) then begin
          Additions.Activate(i,NorthWest);
          Border.Activate(i,NorthWest);
        end;
      end;
    end else Subtractions.Activate(i);
    i:= ToDoList.Next(i);
  end; {while/for}
  ToDoList.SubtractAdd(Subtractions, Additions);
end;

function TCellBlock.GenerateQtoP: TGenerateStatus;
var
  i: integer;
  S,E,SE: PUnit;
  Sindex, EIndex, SEIndex: integer;
  bS, bE, bSE: TAbstractNode;
  status: TGenerateStatus;
begin
  bS:= FParent.GoSouth(FIndex);
  bE:= FParent.GoEast(FIndex);
  bSE:= FParent.GoSouthEast(FIndex);
  FillChar(AddSubtract, SizeOf(TDoMap)*2, 0);
  //for i:= 127 downto 0 do begin
  i:= ToDoList.Next(-1);
  while (i < 32) do begin
    SIndex:= i + 4;
    EIndex:= i + 1;
    SEIndex:= i + 5;
    S:= @q[SIndex];
    E:= @q[EIndex];
    SE:= @q[SEIndex];
    if i >= (32-4) then begin
      S:= @TCellBlock(bS).q[Sindex - 32];
      SE:= @TCellBlock(bSE).q[SEIndex - 32];
    end;
    if (i and 3) = 3 then begin
      E:= @TCellBlock(bE).q[EIndex - 4];
      SE:= @TCellBlock(bSE).q[SEIndex - 4];
    end;
    status:= TGenerateStatus(GenerateQtoP_AVX_32(@q[i],S,E,SE));
    //All of the cell.
    //if (AllActive in status) then begin
    if ([AllP2Plus, NS_P2Plus, EW_P2Plus] * status <> []) then begin
      Additions.Activate(i);
      if (NS_P2Plus in status) then Additions.Activate(i,South);
      if (EW_P2Plus in status) then begin
        Additions.Activate(i,East);
        if (Corner_P2Plus in status) then Additions.Activate(i,SouthEast);
      end;
    end else Subtractions.Activate(i);
    i:= ToDoList.Next(i);
  end; {while/for}
  ToDoList.SubtractAdd(Subtractions, Additions);
end;

function TCellBlock.GetLifeCellBlock(x, y: integer; ForceCreation: boolean): TCellBlock;
begin
  Result:= self;
end;

function TCellBlock.GetPixel(x, y: integer): boolean;
var
  Offset: integer;
begin
  Offset:= SizeOf(p) * Integer(qstate);
  Result:= p[GetNodeIndex(x,y)+Offset].GetPixel(x,y);
end;

function TCellBlock.GetSubNode(index: integer): TAbstractNode;
begin
  Result:= self;
end;

class constructor TCellBlock.Init;
begin
  FillChar(FEmpty, SizeOf(FEmpty), #0);
  QState:= false; //start with pstate.
end;

const Shuffle8bytesToRight: array[0..15] of byte = (00,$FF,01,$FF,02,$FF,03,$FF,04,$FF,05,$FF,06,$FF,07,$FF);

procedure TCellBlock.SetCore(const Block: Int64);
asm
  //A Cellblock is 8 units wide and 16 units high, making up 128 pixels
  //A unit is 16 pixels wide and 8 units high
  //We fill an offcenter block like so:
  //8 bits
  //8 bits
  //....
  //8 bits
  //for this we take Block 60 and fill byte 1,3,5,7,... with the input value
  //registers:
  //rcx = self
  //rdx = Block
  movdqu xmm13,[Shuffle8bytesToRight+rip]
  mov r8,rdx       //get data
  mov eax,$3F00    //mask for 6 pixels
  and r8d,$3F      //get byte 1
  lea r9,[rdx*4]   //r9 = next 6 pixels
  and r9,rax       //mask off 6 pixels
  or r8,r9         //combine row 1 and 2
  shl eax,8        //next byte
  shl rdx,2+2      //move next 6 pixels in line with mask
  mov r9,rdx
  and r9,rax       //get row 3
  or r8,r9         //add to result
  shl eax,8        //next byte
  lea r9,[rdx*4]   //r9 = next 6 pixels
  and r9,rax       //mask row 4
  or r8,r9         //add row 4 to result
  shl rdx,4        //realign source with mask
  shl rax,8        //move mask to next byte
  mov r9,rdx       //r9=source 5
  and r9,rax       //mask row 5
  or r8,r9         //add row 5 to result
  lea r9,[rdx*4]   //r9=row 4
  shl rax,8        //mask the next byte
  and r9,rax       //get the last row
  or r8,r9         //add the row to the result
  shl r8,8         //put the 6 rows in the middle vertically
  movq xmm12,r8
  pshufb xmm12,xmm13
  movdqu [rcx+TCellBlock.p+16*17],xmm12
  mov eax,$00004000
  mov [rcx+TCellBlock.ToDoList],eax
end;

procedure TCellBlock.SetMiniCore(const Block: integer);
asm
  //A Cellblock is 8 units wide and 16 units high, making up 128 pixels
  //A unit is 16 pixels wide and 8 units high
  //We fill an offcenter block like so:
  //8 bits
  //8 bits
  //....
  //8 bits
  //for this we take Block 60 and fill byte 1,3,5,7,... with the input value
  //registers:
  //rcx = self
  //rdx = Block
  mov r8,rdx        //copy of the data
  mov r9,rdx
  mov eax,edx       //mask for the second
  and r8d,$F        //mask the first row
  and eax,$F0       //get row 2
  and r9d,$F00      //get row 3
  and edx,$F000     //get row 4
  shl eax,16-4       //row 2 in byte 3
  shl r9,32-8       //put in byte 3+2=5
  shl rdx,12+24     //put in byte 4+3=7
  or r8,rax         //combine row 1+2
  or r9,rdx         //combine row 3+4
  or r8,r9          //combine row 12 + 34
  //shl r8,24         //Put 4 rows in the middle vertically
  movq xmm12,r8
  pslldq xmm12,3     //put 4 rows in the middle vertically
  //no need for pshufb, we've already done that
  movdqu [rcx+TCellBlock.p+16*17],xmm12
  mov eax,$00020000 //process block 17
  mov [rcx+TCellBlock.ToDoList],eax
end;

function TCellBlock.Get4x4(AUnit: pointer; FirstByte: integer): integer;
asm
  ///  registers
  ///  RCX = self
  ///  rdx = pointer to unit
  ///  r8 = offset to first byte
  ///  The data is in bytes 0,2,4,6
  mov rax,$3C003C003C003C //Mask off the relevant bytes
  mov rdx,[rdx+r8]        //Get 4 rows in bytes 0,2,4,6
  and rdx,rax             //rax = relevant bits
  xor eax,eax             //clear result.
  shr rdx,2               //mov row 0 in place
  mov al,dl               //dl = row 0.
  //and al,$0f
  shr rdx,12              //put row 1 in place
  //and dl,$f0
  or al,dl                //combine row 0+1
  shr rdx,12              //put row 2 in place
  mov ah,dh               //combine row 0+1+2
  //and ah,$0f
  shr rdx,12              //put row 3 in place
  //and dh,$f0
  or ah,dh                //combine row 0+1+2+3
end;

function TCellBlock.Get2x2(AUnit: pointer; FirstByte: integer): integer;
asm
  mov rcx,$C000C00           //Mask off the relevant bytes
  and rcx,[rdx+r8]           //Get 2 rows in bytes 2,4
  xor eax,eax
  shr rcx,10               //put row 1 in place
  mov al,cl
  shr ecx,16-2               //put row 2 in place
  or eax,ecx                 //combine row 1 and 2
end;

procedure TCellBlock.SetPixel(x, y: integer; state: boolean);
var
  ux,uy: integer;
begin
  if (Self.qstate) then begin
    Dec(x); Dec(y);
  end;
  ux:= x and 15;
  uy:= y and 7;
  x:= x shr 4;     //div 16
  y:= y shr 3;     //div 8
  if (qstate) then begin
    q[x+y*8].SetPixel(ux,uy, state);
  end else begin
    p[x+y*8].SetPixel(ux,uy,state);
  end;
end;

{ TBorderMap }

procedure TBorderMap.Activate(index, offset: integer);
begin
  //The offset is either positive (when south facing) or negative (when north facing).
  //The offset is never zero.
  case offset of
    North: if index in [0..7] then FHorz:= FHorz or (1 shl index);
    West: if ((index and 7) = 0) then FVert:= FVert or (1 shl (index shr 3));
    South: if index in [120..127] then FHorz:= FHorz or (1 shl (index - 120));
    East: if ((index and 7) = 7) then FVert:= FVert or (1 shl ((index -7) shr 3));
    NorthWest: if index = 0 then FCorn:= 1
      else if (index in [1..7]) then FHorz:= FHorz or (1 shl (index -1))
      else if ((index and 7) = 0) then FVert:= FVert or (1 shl ((index shr 3)-1));
    SouthEast: if index = 127 then FCorn:= 1
      else if (index in [120..126]) then FHorz:= FHorz or (1 shl (index - 119))
      else if ((index and 7) = 7) then FVert:= FVert or (1 shl ((index shr 3)+1));
    else Assert(false, 'TBorderMap.Activate: Offset out of range');
  end;
end;

procedure TBorderMap.ActivateAll;
begin
  FHorz:= -1;
  FVert:= -1;
  FCorn:= -1;
end;

procedure TBorderMap.ClearAll;
begin
  FHorz:= 0;
  FVert:= 0;
  FCorn:= 0;
end;

{ TDoMap2 }

procedure TDoMap2.Activate(index: integer);
asm
  bts [rcx],rdx
end;

function TDoMap2.IsActive(direction: TDirection): boolean;
begin
  case Direction of
    dN: Result:= (a and $000000000000000F) <> 0;
    dW: Result:= (a and $1111111111111111) <> 0;
    dNW:Result:= (a and $0000000000000001) <> 0;
    dS: Result:= (a and $F000000000000000) <> 0;
    dE: Result:= (a and $8888888888888888) <> 0;
    dSE: Result:=(a and $8000000000000000) <> 0;
    else Result:= true;
  end;
end;

function TDoMap2.IsActiveP2(direction: TDirection): boolean;
begin
  case Direction of
    dN: Result:= (a and $00000000000000FF) <> 0;
    dW: Result:= (a and $3333333333333333) <> 0;
    dNW:Result:= (a and $0000000000000033) <> 0;
    dS: Result:= (a and $FF00000000000000) <> 0;
    dE: Result:= (a and $CCCCCCCCCCCCCCCC) <> 0;
    dSE: Result:=(a and $CC00000000000000) <> 0;
    else Result:= true;
  end;
end;

function TDoMap2.Activate(index, Offset: integer): boolean;
asm
//register usage:
//RCX : self
//EDX: index
//r8d: offset
  test r8d,r8d          //test if r8 is positive or negative
  lea r10d,[edx+r8d]    //store the desired result if <0 or >127 then problem.
  mov eax,0             //assume failure
  jns @positive
@negative:
  and edx,7             //if zero then potential problem
  shr r8d,1             //is offset odd?
  sbb edx,-1            //if ZF=1 then we have a problem
  bt r10d,31            //if CF=1 then we have a problem, is r10d negative?
  jbe @done
  bts [rcx],r10
  inc eax               //signal success
  ret
@positive:
  or edx,not(7)         //-1 if eax has 7 at the end.
  shr r8d,1             //is offset odd?
  adc edx,0             //ZF=1 if odd offset and index has a 7
  bt r10d,6             //CF=1 if overflow, is r10d > 64?
  jbe @done
  bts [rcx],r10
  inc eax               //signal success
@done:
  rep ret
end;

procedure TDoMap2.ActivateAll;
begin
  a:= -1;
end;

procedure TDoMap2.Clear;
begin
  a:= 0;
end;

procedure TDoMap2.Invert(index: integer);
asm
  btc [rcx],rdx
end;

function TDoMap2.Next(previous: integer): integer;
asm
  //register usage
  //RCX: self
  //RDX: previous
  mov rax,[rcx]
  lea ecx,[edx+1]
  shr rax,cl
  rep bsf rax,rax
  add eax,ecx
end;

procedure TDoMap2.SubtractAdd(var Subtract, Add: TDoMap2);
///  rcx = self
///  rdx = @Subtract
///  r8 =  @Add
asm
  mov r10,[rdx]     //get subtract
  mov rax,[rcx]     //get self
  xor r11,r11       //r11 = 0
  //not r10           //not subtract
  //and rax,r10       //self:= self and not subtract
  //andn rax, r10, rax      //andn is in BMI1, we already rely on TZCNT and AVX, so that's fine.
  db $c4, $e2, $a8, $f2, $c0//andn rax, r10, rax
  or rax,[r8]       //self:= self or add
  mov [rdx],r11     //clear subtract
  mov [r8],r11      //clear add
  mov [rcx],rax     //save self
end;

{ TDiBits }

class function TDiBits.Create: TDIBits;
var
  ScanLineWidth: integer;
  color: PRGBQuad;
begin
  with Result do begin
    ScanlineWidth:= 128 div 8;
    if (ScanlineWidth mod 4) <> 0 then Inc(ScanlineWidth, 4 - ScanlineWidth mod 4);
    GetMem(bmpinfo, SizeOf(TBitmapInfo) + SizeOf(TRGBQUAD));
    GetMem(data, ScanlineWidth * 128);
    color:= @bmpinfo^.bmiColors[0];
    color^.rgbRed:= 255;
    color^.rgbBlue:= 255;
    color^.rgbGreen:= 255;
    color^.rgbReserved:= 0;
    Inc(color);
    color^.rgbRed:= 0;
    color^.rgbBlue:= 0;
    color^.rgbGreen:= 0;
    color^.rgbReserved:= 0;

    with bmpinfo.bmiHeader do begin
      biSize:= SizeOf(bmpinfo.bmiHeader);
      biWidth:= 128;
      biHeight:= -128;
      biPlanes:= 1;
      biBitCount:= 1;
      biCompression:= BI_RGB;
      biSizeImage:= 0;
      biXPelsPerMeter:= 0;
      biYPelsPerMeter:= 0;
      biClrUsed:= 0;
      biClrImportant:= 0;
    end;
  end;
end;

function TDiBits.Stride: integer;
begin
  Result:= ((width or 7)+1 div 8);
end;

{ TDoMap }

procedure TDoMap.Activate(index: integer);
asm
  test index,index
  js @Done
  bts [rcx],rdx
  @Done:
end;

function TDoMap.Activate(index, Offset: integer): boolean;
asm
//register usage:
//RCX : self
//EDX: index
//r8d: offset
  test r8d,r8d          //test if offset is positive or negative
  lea r10d,[edx+r8d]    //store the desired result if <0 or >31 then problem.
  mov eax,0             //assume failure
  jns @positive
//Now we test if the bit to activate is out of bounds.
@negative:
  //we have 4 columns
  and edx,3             //if zero then we are at the left edge
  //if column # = 0 and offset = negative and offset = odd, then we are moving to
  //the left, but we are already at the left edge.
  shr r8d,1             //CF=1 = do we want to move left?
  //edx=0 if left edge, 0-(1-CF) = 0 if we are out of bounds
  sbb edx,-1            //if ZF=1 then we have a problem
  //if index+offset is negative we are also out of bounds
  bt r10d,31            //if CF=1 then we have a problem, is r10d negative?
  jbe @done
  bts [rcx],r10         //not out of bounds, make the change
  inc eax               //signal success
  ret
@positive:
  //if edx=4 then we are at the right edge
  or edx,not(3)         //-1 if eax has 7 at the end.
  //4 or not 3 =-1
  shr r8d,1             //is offset odd?
  //-1+0+CF = 0 if at the right edge and we are trying to move further right
  adc edx,0             //ZF=1 if odd offset and index has a 7
  //if index+offset >= 32 then we are also out of bounds.
  bt r10d,5             //CF=1 if overflow, is r10d > 32?
  jbe @done
  bts [rcx],r10         //not out of bounds, make the change
  inc eax               //signal success
@done:
  rep ret
end;

procedure TDoMap.ActivateAll;
asm
  mov dword ptr [rcx],-1
end;

procedure TDoMap.ActivateMultiple(Mask: integer);
begin
  Self.a:= Mask;
end;

procedure TDoMap.Clear;
asm
  mov dword ptr [rcx],0
end;

procedure TDoMap.Deactivate(index: integer);
asm
  btr [rcx],edx
end;

procedure TDoMap.Invert(index: integer);
asm
  btc [rcx],edx
end;

function TDoMap.IsActive(direction: TDirection): boolean;
begin
  case Direction of
    dN: Result:= (a and $0000000F) <> 0;
    dW: Result:= (a and $11111111) <> 0;
    dNW:Result:= (a and $00000001) <> 0;
    dS: Result:= (a and $F0000000) <> 0;
    dE: Result:= (a and $88888888) <> 0;
    dSE: Result:=(a and $80000000) <> 0;
    else Result:= true;
  end;
end;

function TDoMap.IsActiveP2(direction: TDirection): boolean;
begin
  case Direction of
    dN: Result:= (a and $000000FF) <> 0;
    dW: Result:= (a and $33333333) <> 0;
    dNW:Result:= (a and $00000033) <> 0;
    dS: Result:= (a and $FF000000) <> 0;
    dE: Result:= (a and $CCCCCCCC) <> 0;
    dSE: Result:=(a and $CC000000) <> 0;
    else Result:= true;
  end;
end;

function TDoMap.Next(previous: integer): integer;
asm
  //register usage
  //RCX: self
  //EDX: previous
  mov eax,[rcx]
  lea ecx,[edx+1]
  shr eax,cl
  rep bsf eax,eax
  add eax,ecx
end;



procedure TDoMap.SubtractAdd(var Subtract, Add: TDoMap);
///  register usage:
///  rcx: self
///  rdx: subtract
///  r8:  add
asm
  mov r10d,[rdx]     //get subtract
  mov eax,[rcx]     //get self
  xor r11,r11       //r11 = 0
  //not r10           //not subtract
  //and rax,r10       //self:= self and not subtract
  //andn rax, r10, rax      //andn is in BMI1, we already rely on TZCNT and AVX, so that's fine.
  db $c4, $e2, $a8, $f2, $c0//andn rax, r10, rax
  or eax,[r8]       //self:= self or add
  mov [rdx],r11d     //clear subtract
  mov [r8],r11d      //clear add
  mov [rcx],eax     //save self
end;


function TDoMap.TestNext(previous: integer): integer;
asm
  //register use:
  //rcx : self
  //rdx : previous
  mov eax,edx
@loop:
  inc eax
  cmp eax,UnitsPerBlock
  jae @done
  bt [rcx],eax
  jnc @loop
@done:
end;

{ TUnit }



procedure TUnit.Display(const BitmapStart: pointer; const LineOffset: integer);
asm
  ///  rcx = self
  ///  rdx = bitmapstart
  ///  r8  = LineOffset
  //mov r10,[rcx+8]
  //mov rcx,[rcx]
  //push rdx
  call ReverseBitsInAllBytes    //returns reversed bits in xmm0.
  movq rax,xmm0                 //Process the first 4 words
  //vpsrldq xmm0,xmm0,8           //shift the high qword into the low qword.
  db $c5,$f9,$73,$d8,$08         //vpsrldq xmm0,xmm0,8
  //pop rdx
  mov [rdx],ax
  shr rax,16
  mov [rdx+r8],ax
  shr rax,16
  lea rdx,[rdx+r8*2]
  mov [rdx],ax
  shr eax,16
  mov [rdx+r8],ax
  lea rdx,[rdx+r8*2]
  movq rax,xmm0
  mov [rdx],ax
  shr rax,16
  mov [rdx+r8],ax
  shr rax,16
  lea rdx,[rdx+r8*2]
  mov [rdx],ax
  shr eax,16
  mov [rdx+r8],ax
end;

procedure TUnit.SetPixel(x, y: integer; value: boolean);
var
  Mask: word;
begin
  Mask:= 1 shl x;
  if value then w[y]:= w[y] or Mask
  else w[y]:= w[y] and not mask;
end;

function TUnit.GetPixel(x,y: integer): boolean;
var
  Mask: word;
begin
  Mask:= 1 shl x;
  Result:= w[y] and Mask <> 0;
end;

end.
