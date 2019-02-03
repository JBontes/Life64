unit Unit2;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ComCtrls,
  JvComponentBase, JvAppStorage, JvAppRegistryStorage, System.Win.TaskbarCore,
  Vcl.Taskbar, System.Actions, Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnMan, Universe, OtlCommon, OtlParallel, OtlEventMonitor,
  System.Generics.Collections, System.Generics.Defaults;

const
  cStanding = true; //deprecated;
  cFlat = false; //deprecated;

type
  PSlice = ^TSlice;

  /// <summary>
  ///  A sliver is the 2x3 overlap between two 3x3 slices
  ///  This is a partial record, the rest of the methods
  ///  are listed in TSliverHelper
  /// </summary>
  TSliver = record
  public
    class operator BitwiseAnd(const a, b: TSliver): TSliver;
    class operator BitwiseOr(const a, b: TSliver): TSliver;
    class operator Equal(const a, b: TSliver): boolean;
    class operator NotEqual(const a, b: TSliver): boolean;
    class operator Implicit(const a: Uint64): TSliver;
    function IsValid: boolean;
    function PopCount: integer;
  private
    case integer of
      8: (Data8: int64);
      1: (bytes: array [0 .. 7] of byte);
  end;

  /// <summary>
  ///  When we create a sliver, it is because we want to reduce
  ///  the number of allowed states.
  ///  A sliver is created from two slices and is then used to transfer any changes
  ///  picked up in it back to those slices.
  ///
  ///  At this point we might as well track if there are any changes
  ///  and if we have stumbled on a invalid slice (zero allowed states).
  ///  rather than performing a seperate test.
  ///
  ///  As a cool side effect we can easily see which of the two slices will change
  /// </summary>
  TSliverChanges = record
  strict private type
    TSliverState = (scNEChanged=0, scSWChanged=1, scInvalid=2, scSkipped=3);
    TSliverStates = set of TSliverState;
  public
    /// <summary>
    ///  Return TSliverChanges.Unchanged if false, TSliverChanges.Changed if true.
    /// </summary>
    //class operator Implicit(a: boolean): TSliverChanges;
    /// <summary>
    ///  a=Unchanged or true => a:= Changed
    ///  a=Unchanged or false => a:= Unchanged
    ///  a=Changed or whatever => a:= changed
    ///  a=invalid or whatever => a:= Invalid
    /// </summary>
    //class operator LogicalOr(const a: TSliverChanges; const b: boolean): TSliverChanges;
    /// <summary>
    ///  If b is invalid then add invalid to the result.
    ///  otherwise return a
    /// </summary>
    class operator BitwiseAnd(const a: TSliverChanges; const b: TSliver): TSliverChanges;
    /// <summary>
    ///  Result:= a or b (I did not use OR, because to avoid triggering an implicit
    ///  conversion of a TSliverChanges into a boolean)
    /// </summary>
    class operator BitwiseOr(const a, b: TSliverChanges): TSliverChanges;
    /// <summary>
    ///  Used to check if we should stop overlapping slices.
    /// </summary>
    class operator Add(a: integer; const b: TSliverChanges): integer; inline;
    class operator Equal(a,b: TSliverChanges): boolean; inline;
    class operator NotEqual(a,b: TSliverChanges): boolean; inline;
    /// <summary>
    ///  Return a new TSliverChanges containing an Invalid state.
    /// </summary>
    class function Invalid: TSliverChanges; static; inline;
    /// <summary>
    ///  Return a new TSliverChanges containing an Unchanged state
    /// </summary>
    class function UnChanged: TSliverChanges; static; inline;
    /// <summary>
    ///  Return a new TSliverChanges indicating that processing was skipped.
    /// </summary>
    class function Skipped: TSliverChanges; static; inline;
    /// <summary>
    ///  Return a new TSliverChanges containing a changed state.
    /// </summary>
    class function Changed: TSliverChanges; static; inline;

    class function Changes(const NE, SW, Sliver: TSliver): TSliverChanges; static;

    function IsValid: boolean; inline;
    /// <summary>
    ///  Result:= (Self = scChanged)
    /// </summary>
    function KeepGoing: boolean; inline;
    /// <summary>
    ///  Result:= (Self = scInvalid)
    /// </summary>
    function IsInvalid: boolean; inline;
    /// <summary>
    ///  Result:= (Self = scChanged) or (Self = scInvalid)
    /// </summary>
    function IsChanged: boolean; inline;
    /// <summary>
    ///  Result:= Self = scSkipped;
    /// </summary>
    function IsSkipped: boolean; inline;
    /// <summary>
    ///  Result:= (Self = scUnChanged)
    /// </summary>
    function IsUnchanged: boolean; inline;
    function NorthChanged: boolean; inline;
    function SouthChanged: boolean; inline;
    function EastChanged: boolean; inline;
    function WestChanged: boolean; inline;
  private
    case integer of
      1: (Data: TSliverStates);
      2: (Raw: byte); //0 = unchanged, 1=NE, 2=SW, 3=All, >=4=Invalid
  end;

  /// <summary>
  ///  In a 5x5 future bitmap we can have unknown pixels.
  ///  We track overlaps between two 5x5 rects, one known and one unknown.
  ///  The unknown bitmap is at the bottom and the known bitmap at the top.
  ///  The overlap were both rects fully overlap is 0,0.
  ///  This means the bitmap is fully known (the top, known, bitmap wins).
  ///  From 0,0 we offset to the north and south and east and west.
  /// </summary>
  TMaskedBitsIndex = -3..3;

  /// <summary>
  ///  In a 5x5 bitmap we can have a maximum of 25 masked pixels.
  ///  (tracked in FData).
  ///  Masked pixels can be unknown, but we reuse it for other purposes as well.
  ///  FLength = number of masked pixels
  ///  FData = indexes of masked pixels (0 = topright, 24 = bottomleft)
  /// </summary>
  TMaskedBits = record
  public
    FLength: integer;
    FData: array[0..24] of byte;
    /// <summary>
    ///  Return the union of A and B.
    ///  Make sure the result is sorted
    /// </summary>
    class operator Add(const A,B: TMaskedBits): TMaskedBits;
    function GetItems(index: integer): integer;
    property Length: integer write FLength;
  public
    class operator Implicit(const A: array of byte): TMaskedBits;
    class operator Implicit(const A: TMaskedBits): TArray<byte>;
    class operator Equal(const a, b: TMaskedBits): boolean;
    /// <summary>
    ///  Take a KnownMask (a set of bits) and in that KnownMask
    ///  zero out the bits listed in FData, return that Mask.
    /// </summary>
    function FilterKnownBits(KnownMask: integer): integer;
    /// <summary>
    ///  EW and NS is the offset as described in TMaskedBitsIndex.
    ///  use this to build a list of all pixels that are unknown (or
    ///  rather all pixels where the two rects do not overlap)
    /// </summary>
    constructor Create(EW, NS: TMaskedBitsIndex); overload;
    /// <summary>
    ///  UnknownBits is a bitset where every set bit denotes a pixel
    ///  where two 5x5 rects do not overlap.
    ///  the LSB = 0 = topright and the MSB = 24 = bottomleft.
    /// </summary>
    constructor Create(UnknownBits: integer); overload;
    property Count: integer read FLength;
    property Items[index: integer]: integer read GetItems; default;
  end;

  /// <summary>
  ///  A slice is a truthtable or a set of bits containing all possible immediate pasts
  ///  for a single future cell.
  ///  A 3x3 grid. A 3x3 grid can have 2^(3x3) = 512 possible constellations.
  ///  Constellation 0 = all dead cells.
  ///  Constellation 511 = all live cells.
  ///  The main metric for a slice is its population count.
  ///  If popcount = 1, then there is only a single allowed constellation and we
  ///  will have found a specific past for our given future.
  ///  If popcount = 0, then that future is not allowed and we have a contradiction
  ///  meaning we either have UNSAT or must backtrack.
  ///  If popcount > 1, then we have multiple possible pasts.
  ///  The future for a given slice can be determined by comparing it against the
  ///  norm slice for all 140 constellations that generate a future ON cell,
  ///  or the norm slice for all 372 constellations that generate a future OFF cell.
  ///  If popcount (self AND norm140) = 0 then the future = ON
  ///  If popcount (self AND norm372) = 0 then the future = OFF
  ///  else the future is unknown.
  /// </summary>
  TSlice = record
  private
    class var RandomSeed: uint64;
    class constructor InitRandomSeed;
  public
    /// <summary>
    ///  Used for sorting, the actual order is immaterial
    /// </summary>
    class operator GreaterThan(const a, b: TSlice): boolean;
    /// <summary>
    ///  Used for sorting, the actual order is immaterial
    /// </summary>
    class operator LessThan(const a, b: TSlice): boolean;
    class operator Equal(const a, b: TSlice): boolean;
    class operator NotEqual(const a, b: TSlice): boolean;
    class operator BitwiseAnd(const a, b: TSlice): TSlice;
    class operator BitwiseXor(const a, b: TSlice): TSlice;
    class operator BitwiseOr(const a, b: TSlice): TSlice;
    class operator LogicalNot(const a: TSlice): TSlice;
    /// <summary>
    ///  Returns a new slice with all 1's (all constellations are allowed)
    /// </summary>
    class function FullyUnknown: TSlice; static;
    /// <summary>
    ///  Returns a new slice with all 0's (no constellations are allowed, a contradicton)
    /// </summary>
    class function FullyEmpty: TSlice; static;
    /// <summary>
    ///  Returns a random slice for unit testing
    /// </summary>
    class function Random: TSlice; static;
    /// <summary>
    ///  True if popcount(self) = 0.
    /// </summary>
    function IsZero: boolean;
    /// <summary>
    ///  Count the number of allowed constellations
    /// </summary>
    function PopCount: integer;
    /// <summary>
    ///  Used for visualisation purposes
    /// </summary>
    function CountDead(Pixel: integer): integer;
    /// <summary>
    ///  Used for visualization purposes
    /// </summary>
    function CountAlive(Pixel: integer): integer;
    /// <summary>
    ///  Used for visualization purposes
    /// </summary>
    function Print: string;
    /// <summary>
    ///  Return the allowed states as a listing of constellation indexes.
    ///  Used to visualize constellations
    /// </summary>
    function GetStates: TArray<integer>;
    /// <summary>
    ///   Debugging
    /// </summary>
    class function Print5x5(item: integer): string; static;
    /// <summary>
    ///  Transpose a slice using the input array to transpose the bits
    /// </summary>
    class function GetReordering(const Order: TArray<integer>; const Input: TArray<integer>): TArray<integer>; static;
    /// <summary>
    ///  Is bit Index set?
    /// </summary>
    function GetBit(Index: integer): boolean;
    /// <summary>
    ///  Set or a Reset bit x
    /// </summary>
    procedure SetBit(Index: integer; value: boolean = true);
    /// <summary>
    ///  Empty the slice (reset all bits) and set only a single bit.
    ///  This is mainly used in the speculative exploration part
    /// </summary>
    procedure ForceSingleBit(Index: integer);
    /// <summary>
    ///  Do a matrix transpose on a slice
    ///  This is not really used, because we don't need it.
    ///  This is an expensive operation!
    /// </summary>
    procedure ReorderSlice(const Reordering: TArray<integer>);
    /// <summary>
    ///  The bits in a slice are numbered from 0 to 511.
    ///  Given a starting bit (can be -1, if we want to start from the beginning)
    ///  return the index of the next set bit.
    ///  This can be used to traverse through the enabled bits in a slice fast.
    /// </summary>
    /// <returns>
    ///  The index of the next set bit if found, or > 511 if not found.
    /// </returns>
    function NextSetBit(previous: integer): integer;
    /// <summary>
    ///  Finds the next set bit (using NextSetBit internally) and returns a bitmap
    ///  representation of that constellation.
    ///  This looks like
    ///  MSB          LSB
    ///  -------------210  (low 16 bits)
    ///  -------------543
    ///  -------------876  (high 16 bits)
    ///  ----------------  (top 16 bits are 0).
    ///  This can be shifted around as needed, depending on the location of the slice.
    ///  previous is updated with the found position, so that this function can be called
    ///  repeatedly to generate a list of bitmaps.
    /// </summary>
    function GetBitmap(out KnownBitmap, UnknownBitmap: UInt64): TSliverChanges;
    /// <summary>
    ///  Fill the slice with all zeros
    /// </summary>
    procedure Clear;
    /// <summary>
    ///  Reverse the left-right order of a slice
    /// </summary>
    function ReverseLeftRight: TSlice;


  {$IFDEF TESTINSIGHT} public {$ELSE}
  private {$ENDIF}
    case integer of
      9: (Sliver: array [0 .. 7] of TSliver); //used in some speed-tricks (a sliver is really an int64)
      8: (Data8: array [0 .. 7] of uint64);
      4: (Data4: array [0 .. (64 div 4) - 1] of uint32);
      2: (Data2: array [0 .. (64 div 2) - 1] of word);
      1: (bytes: array [0 .. 63] of byte);
  end;

  TSliceHelper = record helper for TSlice
  private
    /// <summary>
    ///  A slice holds 9 pixels, this is the mask for every single pixel
    ///  in a slice.
    ///  Pixel 0 (topright) = 010101010101....
    ///  Pixel 1 (topmiddle) = 001100110011....
    ///  Pixel 2 (topleft) = 0000111100001111....
    ///  etc.
    /// </summary>
    class var BitMask: array[0..8] of TSlice;
    class constructor Init;
  public
  end;

  /// <summary>
  ///  Part2 of the Sliver.
  ///  All these routines transform slice into a sliver or visa versa.
  ///  We can transform sliver into a slice by assuming the added pixels are all allowed states (ORing)
  ///  We transform a slice into a sliver (ORing)
  ///  Both transformations tend in INCREASE the number of allowed states.
  ///  States are DECREASED by ANDing two slivers.
  ///  The Slow routines are the SOLL's for unit testing
  ///  The other routines are optimized ones for the Gridwalker algorithm.
  /// </summary>
  TSliverHelper = record helper for TSliver
    /// <summary>
    ///  Add the east pixels 0, 3, 6
    /// </summary>
    function SlowEast: TSlice;
    /// <summary>
    ///  Add the east pixels 0, 3, 6
    /// </summary>
    function East: TSlice;
    /// <summary>
    ///  Add the west pixels 2, 5, 8
    /// </summary>
    function SlowWest: TSlice;
    /// <summary>
    ///  Add the west pixels 2, 5, 8
    /// </summary>
    function West: TSlice;
    /// <summary>
    ///  Add the north pixels 0, 1, 2
    /// </summary>
    function SlowNorth: TSlice;
    /// <summary>
    ///  Add the north pixels 0, 1, 2
    /// </summary>
    function North: TSlice;
    /// <summary>
    ///  Add the south pixels 6, 7, 8
    /// </summary>
    function SlowSouth: TSlice;
    /// <summary>
    ///  Add the south pixels 6, 7, 8
    /// </summary>
    function South: TSlice;
    /// <summary>
    ///  Take two neighboring slices N and S.
    ///  Transform N -> SliverS, transform S -> SliverN.
    ///  Now SliverS and SliverN fully overlap, AND them and return the result.
    ///  Changed:= scUnchanged if SliverN = SliverS = Result, :=scChanged otherwise
    ///  Changed = scInvalid if result = 0.
    /// </summary>
    class function NSSlow(const North, South: TSlice; out Changed: TSliverChanges): TSliver; static;
    /// <summary>
    ///  Take two neighboring slices N and S.
    ///  Transform N -> SliverS, transform S -> SliverN.
    ///  Now SliverS and SliverN fully overlap, AND them and return the result.
    ///  Changed:= scUnchanged if SliverN = SliverS = Result, :=scChanged otherwise
    ///  Changed = scInvalid if result = 0.
    /// </summary>
    class function NS(const North, South: TSlice; out Changed: TSliverChanges): TSliver; static;
    /// <summary>
    ///  Take two neighboring slices E and W.
    ///  Transform E -> SliverW, transform W -> SliverE.
    ///  Now SliverW and SliverE fully overlap, AND them and return the result.
    ///  Changed:= scUnchanged if SliverE = SliverW = Result, :=scChanged otherwise
    ///  Changed = scInvalid if result = 0.
    /// </summary>
    class function EWSlow(const East, West: TSlice; out Changed: TSliverChanges): TSliver; static;
    /// <summary>
    ///  Take two neighboring slices E and W.
    ///  Transform E -> SliverW, transform W -> SliverE.
    ///  Now SliverW and SliverE fully overlap, AND them and return the result.
    ///  Changed:= scUnchanged if SliverE = SliverW = Result, :=scChanged otherwise
    ///  Changed = scInvalid if result = 0.
    /// </summary>
    class function EW(const East, West: TSlice; out Changed: TSliverChanges): TSliver; static;
  private
  end;

  TSliceResult = record
    SW: integer;
    NE: integer;
    Changes: TSliverChanges;
    constructor Create(NE, SW: integer; Change: TSliverChanges);
  end;


  IntPair = TPair<integer, integer>;
  /// <summary>
  ///  Look up the confrontation between either two NS slices or two EW slices.
  /// </summary>
  TSliceDictionary = TDictionary<IntPair, TSliceResult>;

  /// <summary>
  ///  Deprecated, a superslice is the union between to slices, a 4x3 area.
  ///  However, unexpectatly taking the intersection (a 2x3 slice) performs
  ///  exactly (as in identical) the same as taking the union (a 4x3).
  ///  the latter is much more expensive, so we drop this.
  ///  Only the lookUp functions are still used, I should probably refactor this
  /// </summary>
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

  /// <summary>
  ///  A offset is an alias for the lookuptable to choose from when selecting a
  ///  table to lookup future 5x5 bitmaps to current slices AND we have unknown
  ///  pixels.
  ///  It contains 2 dimensions, a x-offset (-3..3) and an y-offset(-3..3).
  ///  0,0 gets you the normal 2GB lookuptable with no unknown pixels
  ///  I should make this a record, so that it cannot be aliased by an integer.
  /// </summary>
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
  /// <summary>
  ///  class helper for the index into the unknown lookup tables
  /// </summary>
  TOffsetHelper = record helper for TOffset
    /// <summary>
    ///  transform the lookup table index into a list of unknown pixels
    /// </summary>
    function ToMaskedBits: TMaskedBits;
    /// <summary>
    ///  Get the correct lookup table, given an x and y offset
    /// </summary>
    class function Create(x,y: integer): TOffset; static;
    /// <summary>
    ///  Extract the x and y offset.
    ///  Note that positive offsets are to the East and North and negative offsets to the West and South
    /// </summary>
    procedure XY(out E_offset, N_offset: integer);
  end;

type
  PLookupTable = ^TLookupTable;
  /// <summary>
  ///  A -compressed- lookup table
  ///  The known lookup table has 2^25 entries, generating 1 slice for every future 5x5 bitmap.
  ///  There are 48 unknown lookup tables. Totaling 270MB in size.
  ///  The unknown lookup tables are smaller, because they have fewer entries, every unknown pixel
  ///  halves the number of entries the largest table here has 2^20 entries = 64MB x 4 = 256 MB,
  ///  after that the size goes down pretty fast.
  /// </summary>
  TLookupTable = record
  strict private
    procedure SetUnknownItems(E_offset, N_offset: TUnknownIndex; Index: uint32; const value: TSlice);
    function GetUnknownItems(E_offset, N_offset: TUnknownIndex; Index: uint32): TSlice;
    property UnknownItems[E_offset: TUnknownIndex; N_offset: TUnknownIndex; index: uint32]: TSlice read GetUnknownItems write SetUnknownItems;
  private
    /// <summary>
    ///  The lookup table is 'compressed' (i.e. deduplicated). The index contains 2^25 items
    ///  which points into the actual data.
    ///  We have 5 indexes, different indexes for different purposes
    ///  the stride is the distance to the next index.
    /// </summary>
    class var IndexStride: integer;
    /// <summary>
    ///  There are 48 unknown tables, this 2d array contains a pointer to the start-index of every table.
    /// </summary>
    class var UnknownOffset: array [TUnknownIndex, TUnknownIndex] of integer;
    /// <summary>
    ///  The number of entries in all unknown table
    /// </summary>
    class var UnknownSize: uint64;
    class constructor Init;
  private
    /// <summary>
    ///  Known data
    /// </summary>
    FData: TArray<TSlice>;
    FIndex: TArray<integer>;
    /// <summary>
    ///  Unknown data, note there is no index, because this table is 'small'
    /// </summary>
    FUnknownData: TArray<TSlice>;
    /// <summary>
    ///  A table containing the number of 7x7 ancestors for every 5x5 future bitmap.
    /// </summary>
    FCountData: TArray<uint64>;
    // FUnknownIndex: TArray<integer>; //not used, no index needed for a small table.
    /// <summary>
    ///  Because we start with a 7x7 bitmap and for all of these calculate the future 5x5,
    ///  we do not have to limit outselves to the center 3x3 slice, we can also extract an offsetted
    ///  3x3 slice from the original 7x7.
    ///  This allows us to further pre-prune the problem space.
    ///  Unfortunaty, this does not work correctly if there are unknown pixels in the 5x5.
    ///  For now this must NOT be used, until I can add code to check for unknown pixels.
    /// </summary>
    FCornerData: TArray<TSlice>;
    FCornerIndex: TArray<integer>;
    function GetItems(Offset: TOffset; Index: integer): TSlice;
    procedure SetItems(Offset: TOffset; Index: integer; const value: TSlice);
    /// <summary>
    ///  Load the known slice data from disk
    /// </summary>
    procedure LoadSliceData(const Filename: string; var SliceData: TArray<TSlice>);
    /// <summary>
    ///  Load the known slice index data from disk
    /// </summary>
    procedure LoadIndex(const Filename: string; var IndexData: TArray<integer>);
    /// <summary>
    ///  Load the known slice 7x7 ancestor count from disk
    ///  the result is a table with uint64's (some counts do not fit in a int32).
    /// </summary>
    procedure LoadCountData(const Filename: string);
    /// <summary>
    ///  We can recreate a close approximation of the slice data, save this to disk.
    /// </summary>
    procedure SaveSliceData(const Filename: string; const SliceData: TArray<TSlice>);
    /// <summary>
    ///  Takes a lookup table and reverses the left to right order of all items.
    /// </summary>
    procedure ReverseLeftToRight;
  public
    procedure LoadMainIndex(const Filename: string);
    procedure LoadMainData(const Filename: string);
    procedure LoadCornerIndex(const Filename: string);
    procedure LoadCornerData(const Filename: string);
    procedure LoadUnknownData(const Filename: string);
    /// <summary>
    ///  Used to check to we have already loaded data from disk.
    /// </summary>
    function HasCornerData: boolean;
    function HasSliceData: boolean;
    function HasCountData: boolean;
    function HasUnknownData: boolean;
    /// <summary>
    ///  Unified index property `[] operator in c++` for both known and unknown tables
    /// </summary>
    property Items[Offset: TOffset; index: integer]: TSlice read GetItems write SetItems; default;
  end;

  /// <summary>
  ///  esCheap, remove a constellation from a slice
  ///  esExpensive, take all valid grids and OR these grids together.
  ///  The expensive strategy removes a **lot** more states.
  /// </summary>
  TExplorationStrategy = (esCheap, esExpensive);

  /// <summary>
  ///  When pasting data, make sure we correctly clip stuff at the edges.
  ///  The TOutOfBounds helps to make sure only fragments that fit inside the region of a TGridBitmap
  ///  are processed. (see directly below)
  ///  An aliased TPoint structure. This is 0,0 if the fragment fits wholly inside
  ///  the bitmap, x or y is negative if it extends beyond the 0 point and
  ///  positive (starting with 1) if any part extends beyond pixel 15.
  /// </summary>
  TOutOfBounds = record
  private
    FData: TPoint;
  public
    class operator Implicit(const a: TPoint): TOutOfBounds;
    class operator Implicit(const a: TOutOfBounds): TPoint;
    class operator Add(const a: TRect; const b: TOutOfBounds): TRect;
    property x: integer read FData.x write FData.x;
    property y: integer read FData.y write FData.y;
  end;

  /// <summary>
  ///  Known and unknown Data for a TGridBitmap
  /// </summary>
  TGridData = record
  public
    class operator BitwiseAnd(const A, B: TGridData): TGridData;
    class operator BitwiseXor(const A, B: TGridData): TGridData;
    class operator BitwiseOr(const A, B: TGridData): TGridData;
    class operator LogicalNot(const A: TGridData): TGridData;
    function IsEmpty: boolean;
  private
    FData: array[0..3] of Uint64;
  end;

  {TODO -oJB -cTGridBitmap : Not yet implemented}
  /// <summary>
  ///  A TGridBitmap is a 16x16 pixel single bit per pixel sub-bitmap
  ///  It consists of either a single AVX2 qqword, or 2 AVX dqwords.
  ///  Every qword covers 16 pixels wide and 4 pixels tall.
  ///  The pixels are numbered like so
  ///  x=15                         x=0
  ///  0F0E0D0C0B0A09080706050403020100  y=0
  ///  1F1E1D1C1B1A19181716151413121110
  ///  2F2E2D2C2B2A29282726252423222120
  ///  3F3E3D3C3B3A39383736353433323130 y=3
  ///
  ///  The qwords are ordered like so
  ///  0  y=0
  ///  1
  ///  2
  ///  3  y=15
  ///  This is used to verify a potential past pattern against a known future.
  ///  The idea is that we calculate all past data, this generates a future bitmap
  ///  (Life is forward deterministic)
  ///  We then clip off the unknown parts (the future lightcone of the current unknown pixels)
  ///  Next we take the known future, perform the same clipping
  ///  and XOR the two. If the result = 0, All good, if not then that past is invalid.
  /// </summary>
  TGridBitmap = record
  public
    /// <summary>
    ///  PasteFragments inserts a 3x3 snippet with known and unknown data into the bitmap
    ///  The fragment looks the this
    ///  -------------210  LS 16 bits (only bottom 3 are relevant)
    ///  -------------543
    ///  -------------876
    ///  ----------------  MS 16 bits (none are relevant)
    /// </summary>
    /// <returns>
    ///  A aliased TPoint structure. This is 0,0 if the fragment fits wholly inside
    ///  the bitmap, x or y is negative if it extends beyond the 0 point and
    ///  positive (starting with 1) if any part extends beyond pixel 15.
    /// </returns>
    function PasteFragment(p: TPoint; KnownFragment, UnknownFragment: Uint64): TOutOfBounds;
    {TODO -oJB -cTGridBitmap.Validate : Not yet implemented}
    /// <summary>
    ///  1. Calculate the future of Self.
    ///  2. Clip off the unknown parts
    ///  3. Clip off the unknown parts of reference
    ///  4. XOR 2. and 3.
    ///  Result = true if 4.=0, or false otherwise.
    /// </summary>
    function Validate(const Reference: TGridBitmap): boolean;
    function UnknownFuture: TGridBitmap;

    class function Empty: TGridBitmap; static;
    class function Full: TGridBitmap; static;
    procedure MaskOffUnknown;
    function TestUnknownFuture: boolean;
  private
    case integer of
      8:  (FKnownData: array[0..3] of Uint64;
           FUnknownData: array[0..3] of Uint64);
      16: (FKnownUnit: array[0..1] of TUnit;
           FUnknownUnit: array[0..1] of TUnit);
      32: (FKnown: TGridData;
           FUnknown: TGridData);
  end;

  PActiveSlice = ^TActiveSlice;

  TActiveSliceReverseFactory = record
    FActiveSlice: PActiveSlice;
    constructor Create(Parent: PActiveSlice);
  end;

  TActiveSlice = packed record
  private type
    TActive = set of byte;
  public type
    TActiveEnumerator = record
    private
      FParent: PActiveSlice;
      FIndex: integer;
      FSizeX: integer;
      FMoveForward: boolean;
      function GetX: integer;
      function GetY: integer;
    public
      constructor Create(ActiveSlice: PActiveSlice; MoveForward: boolean; SizeX: integer);
      function MoveNext: boolean; inline;
      function GetCurrent: integer; inline;
      property Current: integer read GetCurrent;
      property X: integer read GetX;
      property Y: integer read GetY;
    end;
  public
    //Get the next bit set, starting from bit(previous+1)
    //Mask off the bits already processed
    //and from that point onward look for the next set bit.
    function GetEnumerator: TActiveEnumerator;
    function Reverse: TActiveSliceReverseFactory;
    function NextSetBit(previous: integer): integer;
    function PreviousSetBit(Next: integer): integer;
    procedure Activate(index: integer);
    procedure Reset(index: integer);
    procedure Update(index: integer; NewStatus: boolean);
    constructor Create(MaxX, MaxY: integer); overload;
    constructor Create(MinX, MinY, MaxX, MaxY, SizeX: integer); overload;
    procedure Limit(const Bounds: TRect);
  private
    case boolean of
      true: (
        FActive: TActive;
        FCount: integer;
        FSizeX: integer;
        ActiveCount: integer
      );
//    false: (
//        FBits: array[0..3] of UInt64
//      );
  end;

  TActiveSliceReverseFactoryHelper = record helper for TActiveSliceReverseFactory
    function GetEnumerator: TActiveSlice.TActiveEnumerator;
  end;

  TSliceDict = class;

  TSliceDictionaryComparer = class(TSingletonImplementation, IEqualityComparer<IntPair>)
    function Equals(const Left, Right: IntPair): Boolean; reintroduce;
    function GetHashCode(const Value: IntPair): Integer; reintroduce;
  end;


  TNoDupsDictComparer = class(TSingletonImplementation, IEqualityComparer<integer>)
  private
    FParent: TSliceDict;
  public
    constructor Create(Parent: TSliceDict);
    function Equals(const Left, Right: integer): Boolean; reintroduce;
    function GetHashCode(const Value: integer): Integer; reintroduce;
  end;

  TEmpty = record
  end;

  TSliceDict = class
  private
    const LineSize = 1024*1024;
  private
    const Zero = 512;
    const One = Zero+1;
    const Unknown = Zero+2;
    const Invalid = Zero+3;
  private
    NSDictionary: TSliceDictionary;
    EWDictionary: TSliceDictionary;
    FSliceDictionaryComparer: TSliceDictionaryComparer;
    FNoDupsComparer: TNoDupsDictComparer;
    SliceDB: TArray<TArray<TSlice>>;  //2D array, every element is 1024*1024 slices.
    NoDupsDict: TDictionary<integer, integer>; //Make sure we don't insert duplicate slices in the SliceDB
    //Store the Slices in a 2D array so that upon expansion we don't have to move data.
    FIndex: integer;
  public
    /// <summary>
    ///  Add the unity slices 1..512 and the yes/no slices.
    /// </summary>
    constructor Create;
    /// <summary>
    ///  Check to see if N+S is in the dict, if not, look it up and add it to
    ///  the dictionary.
    ///  Return the new North and South Indexes and report back on the changes.
    /// </summary>
    procedure NS(var North, South: integer; out Changes: TSliverChanges);
    /// <summary>
    ///  Check to see if E+W is in the dict, if not, look it up and add it to
    ///  the dictionary.
    ///  Return the new East and West Indexes and report back on the changes.
    /// </summary>
    procedure EW(var East, West: integer; out Changes: TSliverChanges);
  private
    function GetItem(index: integer): PSlice;
    function InsertSlice(const Slice: TSlice): integer;
    property Item[index: integer]: PSlice read GetItem; default;
  end;


  /// <summary>
  ///  A grid is a grid of slices.
  /// </summary>
  TGrid = record
  //Enumerator support, used in `for in` constructs
  public type
    PGrid = ^TGrid;
    TGridEnumerator = record
    private
      FParent: PGrid;
      FIndex, FCount: integer;
      function GetX: integer;
      function GetY: integer;
    public
      constructor Create(Grid: PGrid);
      function MoveNext: boolean; inline;
      function GetCurrent: PSlice; inline;
      property Current: PSlice read GetCurrent;
      property Index: integer read FIndex;
      property x: integer read GetX;
      property y: integer read GetY;
    end;
  private
    /// <summary>
    ///  array[FSizeX,FSizeY] of TSlice (remember, by design slices overlap)
    /// </summary>
    FActive: TActiveSlice;
    FSizeX, FSizeY: integer;
    /// <summary>
    ///  Is the grid free of invalid slices?
    /// </summary>
    FIsValid: boolean;
    FData: TArray<TSlice>;
    //FData: array[0..255] of TSlice;  //to be replaced by a variable size structure later
    function GetSlice(const Coordinate: TPoint): TSlice; overload; inline;
    procedure SetSlice(const Coordinate: TPoint; const Value: TSlice); overload; inline;
    function GetSlice(x,y: integer): TSlice; overload; inline;
    procedure SetSlice(x,y: integer; const Value: TSlice); overload; inline;
    function GetSlice(i: integer): TSlice; overload; inline;
    procedure SetSlice(i: integer; const Value: TSlice); overload; inline;
    /// <summary>
    ///  The core of the gridwalker algorithm
    ///  Take two slices (x,y) and (x+1,y) where all coordinates < MaxX <MaxY
    ///  and AND their overlapping slivers, expand the resuting sliver
    ///  back into two slices and AND that with the original slices.
    ///  Do the same for slices (x,y) and (x,y+1).
    ///  All measurements in the rect are inclusive, the routine will only
    ///  reject slices with coordinates outside those of the rect.
    /// </summary>
    function SliverSolve(x, y: integer; const MinMax: TRect): TSliverChanges;
    function SliverSolveReverse(x, y: integer; const MinMax: TRect): TSliverChanges;

    function SliverSolveOld(x, y: integer; const MinMax: TRect): TSliverChanges;
    function SliverSolveReverseOld(x, y: integer; const MinMax: TRect): TSliverChanges;
    /// <summary>
    ///  for (x=Min;x<Max;x++) {for (y=Min;y<Max;y++) {SliverSolve(x,y,Max)}}
    ///  for (x=Max;x>Min;x++) {for (y=Max;y<Min;y++) {SliverSolve(x,y,Max)}}
    ///  By working from topleft to bottomright and back again we propagate changes
    ///  through the grid much faster then just repeating topleft->bottomright over and over.
    ///  This speeds up things by a factor 4 or 5.
    /// </summary>
    function DoASliverRun: TSliverChanges;
    /// <summary>
    ///  Try and find an example of a satisfying assignment
    ///  making sure to avoid using MinSlice as a pivot.
    ///  Returns Invalid if the pattern is UNSAT, Valid if it is.
    /// </summary>
    function GetUniqueSolution: TSliverChanges;
    function GetUniqueSolutionOld: TSliverChanges; overload;
    function GetUniqueSolutionOld(var ValidSolutions: TArray<TGrid>; var ValidCount: integer): TSliverChanges; overload;
    /// <summary>
    ///  Find the slice with the lowest population count >= 2,
    ///  That does not equal MinSlice.
    ///  Pass nil in MinSlice if any slice will do.
    /// </summary>
    procedure GetMinSlice(out MinSlice: PSlice; out MinCount: integer);
    /// <summary>
    ///  Convert a unique grid (a grid where every slice only has a single allowed constellation)
    ///  To a bitmap that can be processed.
    ///  the output is an 2D array of TUnit, based on the size of the grid with 1 pixel around the
    ///  border.
    /// </summary>
    procedure GridToBitmap(out Bitmap: array of TUnit);
    function GetSliceIndex(Slice: PSlice): integer;
    procedure DisplayUniqueSolution(const SG: TStringGrid);
    /// <summary>
    ///  Return the count of bitmaps needed to convert a Grid into a bitmap
    /// </summary>
    function BitmapsNeeded: integer;
    /// <summary>
    ///  Transform a unique grid (a grid with only one allowed state for every slice in it)
    ///  to a past Grid. If AllowGrowth is true then the result grid will be larger than the
    ///  original grid.
    /// </summary>
    function GetPastGrid(AllowGrowth: boolean): TGrid;
  public type
    /// <summary>
    ///  After the basic overlapping of gridwalker has run out of steam
    ///  we start speculative exploring.
    ///  We select a pivot (using a heuristic)and split the grid into
    ///  popcount(pivot) subgrids.
    ///  We solve each subgrid recursively.
    ///  TSolveMe determines the strategy to follow after Splitting.
    ///  smNoSolving -> do nothing, just split.
    ///  smSolveFirst -> Solve all Sub grid, but do not remove anything
    ///  smSolveAndExcludeInvalids -> Solve all Sub grid, only return valid grids.
    /// </summary>
    TSolveMe = (smNoSolving, smSolveFirst, smSolveAndExcludeInvalids);
  public
    /// <summary>
    ///  initialize a new Grid with dimensions x and y.
    ///  All the slices will be fully unknown initially
    /// </summary>
    constructor Create(SizeX, SizeY: integer); overload;
    constructor Create(const Template: TGrid); overload;
    procedure Overwrite(var GridToBeOverwritten: TGrid);
    procedure Clear;
    function Clone: TGrid;
    /// <summary>
    ///  Or two slices together for every slice in the grid.
    ///  Note that the two grids must have the same dimensions.
    /// </summary>
    class operator BitwiseOr(const a,b: TGrid): TGrid;
    class operator Equal(const a,b: TGrid): boolean;
    class operator NotEqual(const a,b: TGrid): boolean;
    /// <summary>
    ///  Solve the grid until there are no more improvements to be made.
    ///  This is step 1 in the solver.
    /// </summary>
    function GridSolve: TSliverChanges; overload;
    function GridSolve(const Bounds: TRect): TSliverChanges; overload;
    function GridSolveOld(const Bounds: TRect): TSliverChanges; overload;
    function GridSolveOld(const Bounds: TRect; IndexStart: integer): TSliverChanges; overload;
    /// <summary>
    ///  Step 1: find the slice with the lowest popcount, starting from StartPoint
    ///  Step 2: explore every variantion thereof, only keep the grids that do not
    ///          contradict.
    /// </summary>
    /// <param name="Strategy">
    ///  esExpensive: perform an OR of all valid grids
    ///  esCheap: remove all invalid constellations from the pivotslice
    /// </param>
    /// <param name="SamplePoint">
    ///  Use this point to pivot the exploration on
    /// </param>
    /// <returns>
    ///  The status change of the pivot point.
    ///
    /// </returns>
    function SpeculativeExploration(Strategy: TExplorationStrategy; const SamplePoint: TPoint): TSliverChanges;

    /// <summary>
    ///  Get the first slice (Starting at the coordinate 0,0)
    ///  That has the minimum popcount > 1.
    ///  The search is performed from left to right first and turn up down
    ///  in a scanline like approach
    /// </summary>
    function GetFirstMinimalSlice(var PopCount: integer): TPoint;
    /// <summary>
    ///  Get the next slice with the minimum popcount
    ///  The search is performed from left to right first and turn up down
    ///  in a scanline like approach
    ///  This functions returns TPoint(-1,-1) if no other slice with popcount > 1 can be found
    ///  or if there is a slice with popcount 0 in the grid.
    /// </summary>
    function GetNextMinimalSlice(const Previous: TPoint; var PopCount: integer): TPoint;
    /// <summary>
    ///  False if any slice in the grid has a popcount of 0, true otherwise.
    /// </summary>
    function IsValid: boolean;
    function IsInvalid: boolean;
    /// <summary>
    ///  After the basic overlapping of gridwalker has run out of steam
    ///  we start speculative exploring.
    ///  We select a pivot (using a heuristic)and split the grid into
    ///  popcount(pivot) subgrids.
    ///  We solve each subgrid recursively.
    ///  TSolveMe determines the strategy to follow after Splitting.
    ///  smNoSolving -> do nothing, just split.
    ///  smSolveFirst -> Solve all Sub grid, but do not remove anything
    ///  smSolveAndExcludeInvalids -> Solve all Sub grid, only return valid grids.
    /// </summary>
    function Split(const Coordinate: TPoint; SolveMe: TSolveMe): TArray<TGrid>;
    /// <summary>
    ///  bitwise OR all the given grids together in a single grid
    /// </summary>
    function Join(const Grids: TArray<TGrid>): TGrid;
    /// <summary>
    ///  The sum of all popcounts in all slices in the grid.
    ///  (Yes the total number of states is the PRODUCT of all popcounts in the grid,
    ///   but that's a BigNum and this is a quick-and-dirty approximation).
    /// </summary>
    function PopCountSum: integer;
    function GetEnumerator: TGridEnumerator;
    /// <summary>
    ///  Creates a 16x16 bitmap starting at the given coordinates.
    /// </summary>
    /// <param name="StartX">
    ///  The rightmost corner of the rect to export.
    ///  X-coorninates increase from right to left like so: high- 9..0 -low
    /// </param>
    /// <param name="StartY">
    ///  The topmost corner of the rect to export.
    ///  Y-coorninates increase from top to bottom like so: top- 0..9 -bottom
    /// </param>
    /// <param name="Spillage">
    ///  A TRect denoting the points where the fragments put into the TGridBitmap
    ///  spilled over the 16x16 bounds.
    ///  Any result other than Rect(0,0,0,0) means that additional GridBitmaps
    ///  need to be constructed.
    ///  Spillage is usually to the left and bottom, so a typical overflow looks like
    ///  Rect(2,0,0,2), which means you need to add Bitmaps to the left and bottom
    ///  and probably an additional one to the bottom-left as well.
    /// </param>
    /// <returns>
    ///  The bitmap consisting two parts: a known and unknown part
    ///  The known part contains ON pixels for every known ON pixel
    ///  in the input, and OFF pixels for all unknown pixels as well as unknown pixels
    ///  The unknown mask contains ON pixels for all the unknown parts, this
    ///  can be used to mask off the parts of the map.
    ///  The format can be inputted into the AVX Life generation code.
    ///  Care must be taken to interpret the output thereof, because of the
    ///  stagger stepping that takes place.
    ///  This is less of a problem then it seems though, because the future light cone
    ///  of the pattern 16x16 pixels at t=0 is 14x14 pixels at t=1.
    /// </returns>
    function AsBitmap(StartX, StartY: integer; out Spillage: TRect): TGridBitmap;
    property SizeX: integer read FSizeX;
    property SizeY: integer read FSizeY;
    /// <summary>
    ///  Get the slice at the given coordinate.
    ///  !!Will generate an exception if the coordinate is out of bounds!!.
    /// </summary>
    property Item[const Coordinate: TPoint]: TSlice read GetSlice write SetSlice; default;
    property Item[x,y: integer]: TSlice read GetSlice write SetSlice; default;
    property Item[i: integer]: TSlice read GetSlice write SetSlice; default;
  end;

    TDictGrid = record
  private
    FSizeX, FSizeY: integer;
    FData: TArray<integer>;   //Every slice in the structure is represented by an index
    FDict: TSliceDict;
    FIsValid: boolean;
    //FDebugGrid: TGrid;
    function GetSlices(index: integer): PSlice; inline;
    function GridSolveOld(const Bounds: TRect; IndexStart: integer = -1): TSliverChanges;
    procedure SetItems(x, y: integer; const Value: integer);
    function GetSlicesXY(x, y: integer): PSlice; inline;
  public
    /// <summary>
    ///  initialize a new Grid with dimensions x and y.
    ///  All the slices will be fully unknown initially
    /// </summary>
    constructor Create(SizeX, SizeY: integer); overload;
    constructor Create(const Template: TDictGrid); overload;
    procedure Overwrite(var GridToBeOverwritten: TDictGrid);
    procedure Clear;
    function Clone: TDictGrid;
    function SliverSolveOld(x, y: integer; const MinMax: TRect): TSliverChanges;
    function SliverSolveReverseOld(x, y: integer; const MinMax: TRect): TSliverChanges;
    function GetUniqueSolutionOld: TSliverChanges;
    function GetMinSlice(out MinCount: integer): integer;
    property Slices[index: integer]: PSlice read GetSlices;
    property SlicesXY[x,y: integer]: PSlice read GetSlicesXY;
    property Items[x,y: integer]: integer write SetItems; default; //write-only property
    property SizeX: integer read FSizeX;
    property SizeY: integer read FSizeY;
  end;

  /// <summary>
  ///A cake is a 5x5 part of the future grid.
  ///It is stored in two parts, 1 part with known pixels
  ///and 1 part with unknown pixels.
  ///A slice is a part of a cake, and the cake is the whole :-)
  ///This is only used as input for the lookup table.
  /// </summary>
  TCake = record
  private
    //2 x 25 bits of storage.
    FKnownPart, FUnknownPart: integer;
  public
    constructor Create(Known, Unknown: integer);
    /// <summary>
    ///  True if there are no unknown pixels.
    /// </summary>
    function IsKnown: boolean;
    /// <summary>
    ///  Shift bits in and out of view
    ///  Used to combine two cakes
    /// </summary>
    class operator RightShift(const A: TCake; B: cardinal): TCake;
    class operator LeftShift(const A: TCake; B: cardinal): TCake;
    /// <summary>
    ///  Super simple trick to set pixels
    /// </summary>
    procedure SetKnown;
    procedure SetUnknown;
    /// <summary>
    ///  Is the LSB set?
    /// </summary>
    function OddKnown: boolean;
    function OddUnknown: boolean;
    property Known: integer read FKnownPart;
    property Unknown: integer read FUnknownPart;
  end;

  /// <summary>
  ///  Convert a point(x,y) to a single index number.
  /// </summary>
  TPointHelper = record helper for TPoint
    function Index(XSize: integer): integer; inline;
    /// <summary>
    ///  Get the index number of a West slice of a EW pair
    /// </summary>
    function West(XSize: integer): integer; inline;
    /// <summary>
    ///  Get the index number of the East slice of a EW pair
    /// </summary>
    function East(XSize: integer): integer; inline;
    /// <summary>
    ///  Get the index number of the North slice of a NS pair
    /// </summary>
    function North(XSize: integer): integer; inline;
    /// <summary>
    ///  Get the index number of the South slice of a NS pair
    /// </summary>
    function South(XSize: integer): integer; inline;
  end;


type
  /// <summary>
  ///  Our everything and the kitchen sink form.
  ///  This will need to be cleaned up for the final version.
  ///  For now it is just a canvas to run experiments and do validations.
  /// </summary>
  TForm2 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelSolutions: TLabel;
    Label5: TLabel;
    BtnProcessSliceLookup: TButton;
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
    ActionManager1: TActionManager;
    Action_SliverSolveRound: TAction;
    BtnRunSingleTest: TButton;
    ProgressBar1: TProgressBar;
    BtnCreateLookup5x5to3x3UsingSpeculativeExploration: TButton;
    BtnInitWithGoE2: TButton;
    Button1: TButton;
    BtnValidateCompressedLookupTable: TButton;
    BtnOld_SolveAndTime: TButton;
    TabSheet6: TTabSheet;
    BtnSearchGoE: TButton;
    MemoGoE_solution: TMemo;
    BtnReverseLookup: TButton;
    Btn5x5To3x3_Lookup: TButton;
    BtnOldNewSolveInLockstep: TButton;
    BtnDictSolveAndTime: TButton;
    procedure Action_SliverSolveRoundExecute(Sender: TObject);
    /// <summary>
    ///  We can create a lookup table by enumerating all 7x7 bitmaps.
    ///  This is working forward in time, relying on the forward deterministicness of Life.
    ///  But this same lookup table can also be created by the solver by reasoning backwards.
    ///  The two tables are nearly identical (0.5% difference).
    /// </summary>
    procedure BtnCreateLookupUsingSolverClick(Sender: TObject);
    procedure BtnRotateCounterClick(Sender: TObject);
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
    procedure BtnProcessSliceLookupClick(Sender: TObject);
    procedure BtnProcess_7x7_CountLookupClick(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure BtnAppyLookupTableClick(Sender: TObject);
    procedure BtnSolveRoundUsingChunksClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnClearGridClick(Sender: TObject);
    procedure BtnCreateLookup5x5to3x3UsingSpeculativeExplorationClick(Sender:
        TObject);
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
    procedure BtnInitWithGoE2Click(Sender: TObject);
    procedure BtnRunSingleTestClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnValidateCompressedLookupTableClick(Sender: TObject);
    procedure BtnOld_SolveAndTimeClick(Sender: TObject);
    procedure BtnSearchGoEClick(Sender: TObject);
    procedure BtnReverseLookupClick(Sender: TObject);
    procedure Btn5x5To3x3_LookupClick(Sender: TObject);
    procedure BtnOldNewSolveInLockstepClick(Sender: TObject);
    procedure BtnDictSolveAndTimeClick(Sender: TObject);
  private
    Buffer: TArray<TSlice>;
    ChunkLookup: array [boolean] of TArray<TSuperSlice>;
    NewLayout: TArray<integer>;
    procedure LoadLookupTable(var LookupTable: TArray<TSlice>);
    procedure InitNewLayout;
    procedure ShowNewLayout;
    function GetCounterLayout: TArray<integer>;
    procedure InitWithGoE;
    procedure Display5x5(Cake: TCake; const SG: TStringGrid);
    function GetPixelChar(x, y: integer; SG: TStringGrid): Char;
    class procedure LeadingTrailingPopCount(UnknownMask: integer; out Leading,
      Trailing, Popcnt: integer); static;
    function UnknownSlice(Cake: TCake): TSlice;
    class function GetNextBitSet(previous: integer; i: Uint64): integer; static;
    {$HINTS OFF}
    procedure InitWithGoE2;
    {$HINTS ON}
    procedure CreateLookupUsingGridSolver(ThreadIndex, ThreadCount: integer; var data: TArray<TSlice>);
    procedure InitWithPattern(const Pattern: string);
    procedure EnableAllCPUCores;
    procedure ReportSolution(NW, NE, SW, SE: integer);
    procedure GridSolveLockstep(const Old, New: TGrid);
  public
    LookupTable: TLookupTable;
    MiniLookup: TArray<TSlice>;
    CountsTable: TArray<Int64>;
    CountsIndex: TArray<integer>;
    GoESolutions: TStringList;
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
function Random64: Uint64;
function GetCurrentProcessorNumber: DWORD; stdcall;
procedure Move(const source; var Dest; Size: NativeInt);

   //{$L 'C:\Users\Johan\Documents\Embarcadero\Studio\Projects\Life64\Lazarus\lib\x86_64-win64\AVXGenerate.o'}
   //{$L 'C:\Users\Johan\Documents\Embarcadero\Studio\Projects\Life64\Lazarus\AVX_Lib.dll'}
const
  AVXlib = 'AVX_lib.dll';

function AVXGENERATE_TSLIVERHELPER_NS(const [ref] North, South: TSlice; out Status: TSliverChanges): TSliver; external AVXlib name 'TSliverHelper_NS';
function AVXGENERATE_TSLIVERHELPER_EW(const [ref] North, South: TSlice; out Status: TSliverChanges): TSliver; external AVXlib name 'TSliverHelper_EW';
function AVXGENERATE_TEST: boolean; external AVXlib name 'Test';
procedure TSlice_ForceSingleBit(Self: pointer; index: integer); external AVXlib name 'TSlice_ForceSingleBit';
function TSliverHelper_West(Self: pointer): TSlice; external AVXlib name 'TSliverHelper_West';
function TSliverHelper_East(Self: pointer): TSlice; external AVXlib name 'TSliverHelper_East';

implementation

{//$DEFINE GpProfile}

uses{$IFDEF GpProfile U} GpProf, {$ENDIF GpProfile U}
  StrUtils,
  System.Types,
  Generics.Collections,
  Generics.Defaults,
  System.UITypes,
  System.Diagnostics,
  HIResStopWatch,
  TestInsight.Client, TestInsight.DUnitX, UnitTests, Math;

{$R *.dfm}
{$POINTERMATH on}

var
  GlobalDict: TSliceDict;

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

  OffSlice: TSlice = (Data8:($E881E9978117977F, $FEE8FEE9E881E997, $FEE8FEE9E881E997, $FFFEFFFEFEE8FEE9,
                             $FEE8FEE9E881E997, $FFFEFFFEFEE8FEE9, $FFFEFFFEFEE8FEE9, $FFFFFFFFFFFEFFFE));


const
   cUnknownSliceData = 'Filename_UnknownSlice';
   cMainSliceData = 'Filename_MainSlice';
   cCornerSliceData = 'Filename_CornerSlice';
   cAncestorCounts = 'Filename_AncestorCount';
   cMainIndex = 'Filename_MainIndex';
   cCornerIndex = 'Filename_CornerIndex';
   cCountData = 'Filename_CountData';

//type
//  TSlices11 = array [0 .. 15, 0 .. 15] of TSlice;

var
  MySlices: TGrid;
  CloneSlices: TGrid;
  OldSlices: TGrid;

type
  TSliceComparer = class(TInterfacedObject, IComparer<TSlice>)
  class var
    FComparer: TSliceComparer;

    function Compare(const Left, Right: TSlice): integer;
  private
    class function Comparer: IComparer<TSlice>; static;
  end;

  TRotation = (rNone, rCounter, rClock, r180);

{$WARNINGS OFF}
// Meiyan means Beauty, Charming Eyes or most precisely: SOULFUL EYES.
function PascalFNV1A_Hash_Meiyan(const HashData; Len: integer; Seed: integer = 0): integer;
const
  //prime = 709607;
  prime = 16777619;
  offset_basis = 2166136261;
  n = $E6546B64;
var
  Hash32: integer;
  p: PInteger;
  Tail: integer;
  Temp: integer;
begin
  p:= @HashData;
  Hash32:= Seed + offset_basis;
  Tail:= Hash32;
  while Len >= 2 * SizeOf(cardinal) do begin
    Hash32:= (Hash32 xor (((p[0] shl 5) or (p[0] shr (32 - 5))) xor p[1]));
    Tail:= Tail xor Hash32;
    Hash32:= Hash32 * prime;
    Inc(p, 2);
    dec(Len, SizeOf(integer)* 2)
  end;
  if (Len and SizeOf(integer)) <> 0 then begin
    Temp:= p^;
    Tail:= Tail xor p^;
    Hash32:= (Hash32 xor (temp and $FFFF)) * prime;
    Tail:= Tail xor Hash32;
    Hash32:= (Hash32 xor (temp shr 16)) * prime;
    Inc(p);
  end;
  if (Len and SizeOf(word)) <> 0 then begin
    Temp:= PWord(p)^;
    Tail:= Tail xor Temp;
    Hash32:= (Hash32 xor Temp) * prime;
    Inc(PWord(p));
  end;
  if (Len and 1) <> 0 then begin
    Temp:= PByte(p)^;
    Tail:= Tail xor Temp;
    Hash32:= (Hash32 xor Temp) * prime;
  end;
  //Extra shuffle
  Tail:= (Tail * 17) + n;
  //Hash32:= Hash32 + Tail;
  Result:= (Hash32 + Tail) xor (Hash32 shr 16);
end;
{$POINTERMATH off}

function FNV1A_Hash_Meiyan(const HashData; Len: integer; Seed: integer = 0): integer;
const
  //prime = 709607;
  prime = 16777619;
  n = $E6546B64;
  offset_basis = 2166136261;
  {$REGION 'assembly'}
  {$IFDEF CPUX86}
  asm
    // EAX = STR
    // EDX = len
    // ECX = seed
    push  EBX
    push  ESI
    push  EDI
    add   ECX, offset_basis
    add   EAX,EDX
    lea   ESI,[EDX-8]
    neg   ESI
    jg @remaining
  @Loop8:
    mov   EBX,[EAX+ESI-8]
    rol   EBX,5

    xor   EBX,[EAX+ESI-8+4]
    xor   ECX,EBX
    mov   EDI,ECX
    imul  ECX,ECX,prime
    add   ESI,$08
    jle @loop8
  @remaining:
    lea   ESI,[ESI+EAX-8]
    test  DL,$4
    jz @wordsize

    mov   EBX,[ESI]
    mov   EDI,EBX
    mov   EAX,EBX
    and   EBX,$ffff
    xor   ECX,EBX
    imul  ECX,ECX,prime

    shr   EAX,16
    xor   ECX,EAX
    imul  ECX,ECX,prime

    add   ESI,$04
  @wordsize:
    test  DL,2
    jz @bytESIze

    movzx EBX, word ptr [ESI]
    mov   EDI,EBX
    xor   ECX,EBX
    imul  ECX,ECX,prime

    add   ESI,$02
  @bytesize:
    test  DL,1
    jz @wrapup

    movzx EBX, byte ptr [ESI]
    mov   EDI,EBX
    xor   ECX,EBX
    imul  ECX,ECX,prime
@wrapup:
//  @wrapup:
//    //Reduce collisions for short keys.
//    //The extra instructions are hidden in the latency of imul
    lea   EAX,[EDI*8]
    lea   EBX,[EAX*2+EDI+n]
    lea   EAX,[EBX+ECX]
    shr   ECX,16
    xor   EAX,ECX
    pop   EDI
    pop   ESI
    pop   EBX
end;
{$ENDIF}
{$IFDEF CPUX64}
asm
.NOFRAME
    // ECX = STR
    // EDX = len
    // R8 = seed
    add   RCX,RDX
    add   R8d,offset_basis
    lea   R11,[RDX-8]
    neg   R11
    mov   R10d,R8d
    jg @remaining
@Loop8:
    //Hash32:= (Hash32 xor (p[0] rol 5) xor p[1])) * prime;
    //Inc(p, 2);
    //dec(Len, SizeOf(integer)* 2)
    mov   RAX,[RCX+R11-8]
    mov   R9,RAX
    rol   R9d,5
    shr   RAX,32
    xor   EAX,R9d
    xor   R8d,EAX
    //Tail:= Tail xor Hash32
    xor   R10d,R8d
    imul  R8d,R8d,prime
    add   R11,$08
    jle @loop8
@remaining:
    //xor   R10,R10
    lea   R11,[R11+RCX-8]
    test  DL,$4
    jz @wordsize
//    Tail:= p^;
    mov   EAX,[R11]
//    Hash32:= (Hash32 xor (Tail and $FFFF)) * prime;
    xor   R10d, EAX
    mov   R9d,EAX
    and   R9d,$ffff
    xor   R8d,R9d
    imul  R8d,R8d,prime
//    Hash32:= (Hash32 xor (Tail shr 16)) * prime;
    //mov   R10d,EAX
    xor   R10d,R8d
    shr   EAX,16
    xor   R8d,EAX
    imul  R8d,R8d,prime
//    Inc(PWord(p));
    add   R11,$04
@wordsize:
    test  DL,2
    jz @bytesize

    movzx R9d, word ptr [R11]
    xor   R8d,R9d
    xor   R10d,R9d
    imul  R8d,R8d,prime

    add   R11,$02
@bytesize:
    test DL,1
    jz @wrapup

    movzx R9d, byte ptr [R11]
    xor   R8d,R9d
    xor   R10d,R9d
    imul  R8d,R8d,prime
@wrapup:
    lea   EAX,[R10d*8]
    lea   ECX,[EAX*2+R10d+n]
    lea   EAX,[ECX+R8d]
    shr   R8d,16
    xor   EAX,R8d
end;
{$ENDIF}
{$ENDREGION}

procedure Move(const source; var Dest; Size: NativeInt);
//RCX = @source
//RDX = @dest
//r8 = size
asm
  mov r9,rdi         //save regs
  mov r10,rsi        //save regs
  mov rsi,rcx        //source
  mov rdi,rdx        //dest
  mov rcx,r8         //size
  shr rcx,3          //divide by 8
  rep movsq
  mov rsi,r10
  mov rdi,r9
end;

function RorX(Shift: integer; const input: Uint64): Uint64;
asm
  mov rax,rdx
  ror ax,cl
  ror rax,16
  ror ax,cl
  ror rax,16
  ror ax,cl
  ror rax,16
  ror ax,cl
  ror rax,16
end;

function RorY(Shift: integer; const input: Uint64): Uint64;
asm
  mov rax,rdx
  shl ecx,4    //shift by 16 bits at a time
  ror rax,cl
end;

function RolX(Shift: integer; const input: Uint64): Uint64;
asm
  mov rax,rdx
  rol ax,cl
  rol rax,16
  rol ax,cl
  rol rax,16
  rol ax,cl
  rol rax,16
  rol ax,cl
  rol rax,16
end;

function RolY(Shift: integer; const input: Uint64): Uint64;
asm
  mov rax,rdx
  shl ecx,4    //shift by 16 bits at a time
  rol rax,cl
end;

//Some code to make sure that all cores on the processor are
//enabled. Without this code the program will only run on a single
//core, killing multi-threaded performance.
type
  TGetCurrentProcessorNumber = function: dword;


function GetProcedureAddress(const ModuleName, ProcName: AnsiString): pointer;
var
  ModuleHandle: HMODULE;
begin
  ModuleHandle := GetModuleHandleA(PAnsiChar(AnsiString(ModuleName)));
  if ModuleHandle = 0 then begin
    ModuleHandle := LoadLibraryA(PAnsiChar(ModuleName));
    if ModuleHandle = 0 then raise Exception.Create('Oops');
  end;
  Result := Pointer(GetProcAddress(ModuleHandle, PAnsiChar(ProcName)));
  if not Assigned(Result) then raise Exception.Create('oops');
end;

var
  _GetCurrentProcessorNumber:TGetCurrentProcessorNumber;

function GetCurrentProcessorNumber: dword;
begin
  _GetCurrentProcessorNumber:= TGetCurrentProcessorNumber(GetProcedureAddress(kernel32, 'GetCurrentProcessorNumber'));
  Result:= _GetCurrentProcessorNumber;
end;

/// <summary>
///  Remove a single bit from the middle of a int.
///  e.g. DeleteBit(01010101, 4) converts 01010101 to 0100101
///  zeros are always added to the MSB, even if the number is negative.
/// </summary>
function DeleteBit(input, BitToDelete: integer): integer;
var
  before, after: integer;
begin
  before:= input and not(-1 shl BitToDelete);
  after:= (input shr 1) and (-1 shl BitToDelete);
  Result:= before or after;
end;

/// <summary>
///  Delete multiple bits from `input`.
///  See DeleteBit above
/// </summary>
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

/// <summary>
///  Extract the x and y offset.
///  Note that positive offsets are to the East and North and negative offsets to the West and South
/// </summary>
procedure TOffsetHelper.XY(out E_offset, N_offset: integer);
var
  UnknownOffset: integer;
begin
  UnknownOffset:= ABS(Ord(Self));
  Dec(UnknownOffset);
  N_offset:= (UnknownOffset mod 7) - 3;
  E_offset:= (UnknownOffset div 7) - 3;
end;

/// <summary>
///   Insert a zero bit at the given position.
/// </summary>
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

/// <summary>
///   Transform every byte in the input into single bit in the result
/// </summary>
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

/// <summary>
///   Fix for older versions of Delphi that have a bug
///   when reading files > 2 GB.
/// </summary>
type
  TFileStream = class(System.Classes.TFileStream)
    function Read64(Buffer: TBytes; Offset, Count: int64): int64; reintroduce;
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


procedure TForm2.BtnProcessSliceLookupClick(Sender: TObject);
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
    BtnProcessSliceLookup.Caption:= 'Sort done';
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

procedure TForm2.BtnRunSingleTestClick(Sender: TObject);
begin
  //Put a failing test and breakpoint here if you need to debug it
  //but do not want to wait for all other the tests to run.
  //Right now we have no failing tests.
end;

procedure TForm2.BtnInitWithGoE2Click(Sender: TObject);
begin
  InitWithGoE2;
end;

//we must accept negative numbers, because these are used in the UI for pretty colors.
function TSlice.GetBit(Index: integer): boolean;
asm
//ABS(index)
  mov eax,edx
  cdq
  xor eax, edx
  sub eax, edx
//ABS done, get the bit.
  bt [rcx],eax
  mov eax,0    //make sure a cast from int to boolean does not add random cruft.
  setc al
end;

/// <summary>
///  Transpose a slice using the input array to transpose the bits
/// </summary>
class function TSlice.GetReordering(const Order: TArray<integer>; const Input: TArray<integer>): TArray<integer>;
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

//collect statistics on the counts lookup table.
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

/// <summary>
///   Take the bitmap from the on-screen image
///   and transform it into a format the solver can understand.
/// </summary>
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

function GetFutureMini(const SG: TStringGrid; col, row: integer): integer;
var
  x, y: integer;
begin
  Result:= 0;
  for y:= 0 to 2 do begin
    for x:= 2 downto 0 do begin
      Result:= Result shl 1;
      if ((col + x) < SG.ColCount) and ((row + y) < SG.RowCount) and ((col + x) >= 0) and ((row + y) >= 0) then begin
        if (SG.Cells[col + x, row + y] = 'X') then Inc(Result);
      end;
    end; {for x}
  end; {for y}
end;

/// <summary>
///   Deprecated, part of a failed approach.
/// </summary>
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

/// <summary>
///   Deprecated, part of a failed approach.
/// </summary>
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


/// <summary>
///   Take the 5x5 on-screen bitmap and transform the resulting cake into a slice.
/// </summary>
function FutureGridToPastSlice(const SG: TStringGrid; col, row: integer; const LookupTable: TLookupTable; KnownOffset: TOffset): TSlice;
var
  Cake: TCake;
begin
  Cake:= GetFutureCake(SG, col, row);
  if (Cake.IsKnown) then begin  //do we have no unknown pixels?
    Result:= LookupTable[KnownOffset, Cake.Known];
  end else begin  //Oops, unknown pixels
    Result:= Form2.UnknownSlice(Cake);
  end;
end;

function FutureGridToPastSliceMini(const SG: TStringGrid; col, row: integer; const LookupTable: TLookupTable; KnownOffset: TOffset): TSlice;
var
  Mini: integer;
begin
  Mini:= GetFutureMini(SG, col+1, row+1);
  Result:= Form2.MiniLookup[Mini];
end;

/// <summary>
///   Take a single on-screen pixel and transform it into a slice.
///   This means there can be only 3 possible outputs
///   ON slice 140 states, OFF slice 372 states, unknown slice 512 states
/// </summary>
function FutureGridToPastSliceSimple(const SG: TStringGrid; col, row: integer; const LookupTable: TLookupTable): TSlice;
var
  ZeroSlice, OneSlice: TSlice;
begin
  ZeroSlice:= LookupTable[oCenter, 0];
  OneSlice:= not(ZeroSlice);
  if (SG.Cells[col + 2, row + 2] = 'X') then Result:= OneSlice
  else if (SG.Cells[col + 2, row + 2] = '?') then Result:= TSlice.FullyUnknown
  else Result:= ZeroSlice;
end;

function FutureGridToPastDictSliceSimple(const SG: TStringGrid; col, row: integer; const LookupTable: TLookupTable): integer;
begin
  if (SG.Cells[col + 2, row + 2] = 'X') then Result:= 1
  else if (SG.Cells[col + 2, row + 2] = '?') then Result:= -1
  else Result:= 0;
end;

/// <summary>
///   Display the constellation-counts of a slice on-screen.
/// </summary>
procedure DisplaySlices(SG, SGDiff: TStringGrid; Slices: TGrid; ForceRefresh: boolean = false); overload;
var
  x, y: integer;
  Slice: TSlice;
  Old: integer;
  Known: boolean;
  TotalStates: double;
  Population: integer;
  Solutions: string;
  TooManyStates: boolean;
  Dummy: double;
  Exponent: integer;
begin
  var StartX:= (16-Slices.SizeX) div 2;
  var StartY:= (16-Slices.SizeY) div 2;

  TooManyStates:= false;
  TotalStates:= 1;
  for x:= StartX to StartX+Slices.SizeX-1 do begin
    for y:= StartY to StartY+Slices.SizeY-1 do begin
      Known:= Form2.StringGrid1.Cells[x+2,y+2] <> '?';
      Slice:= Slices[x-StartX, y-StartY];
      if (SG.Cells[x, y] <> '') then Old:= SG.Cells[x, y].ToInteger
      else begin
        case Form2.GetPixelChar(x + 2, y + 2, Form2.StringGrid1) of
          'X': Old:= 140;
          '?': Old:= 512;
        else Old:= 140;
        end;
      end;
      Population:= Slice.PopCount;
      SG.Cells[x, y]:= Population.ToString;
      if (Known) and not(TooManyStates) then begin
        TotalStates:= TotalStates * Population;
        Frexp(TotalStates, Dummy, Exponent);
        TooManyStates:= (Exponent > 100);
      end;
      SGDiff.Cells[x, y]:= (Old - Slice.PopCount).ToString;
    end; { for y }
  end; { for x }
  if (ForceRefresh) then begin
    SG.Refresh;
    SGDiff.Refresh;
  end;
  if (TotalStates > 1000000000) then Solutions:= Format('%2.6e',[TotalStates])
  else Solutions:= Format('%.0n',[TotalStates]);
  Solutions:= ReplaceStr(Solutions,'E+00','E+');
  Solutions:= ReplaceStr(Solutions,'E+0','E+');
  Solutions:= ReplaceStr(Solutions,'E+','e+');
  Form2.LabelSolutions.Caption:= 'Solutions: '+Solutions;
end;

/// <summary>
///   Display the constellation-counts of a slice on-screen.
/// </summary>
procedure DisplaySlices(SG, SGDiff: TStringGrid; Slices: TDictGrid; ForceRefresh: boolean = false); overload;
var
  x, y: integer;
  Old: integer;
  Known: boolean;
  TotalStates: double;
  Population: integer;
  Solutions: string;
  TooManyStates: boolean;
  Dummy: double;
  Exponent: integer;
begin
  var StartX:= (16-Slices.SizeX) div 2;
  var StartY:= (16-Slices.SizeY) div 2;

  TooManyStates:= false;
  TotalStates:= 1;
  for x:= StartX to StartX+Slices.SizeX-1 do begin
    for y:= StartY to StartY+Slices.SizeY-1 do begin
      Known:= Form2.StringGrid1.Cells[x+2,y+2] <> '?';
      var Slice:= Slices.SlicesXY[x-StartX, y-StartY];
      if (SG.Cells[x, y] <> '') then Old:= SG.Cells[x, y].ToInteger
      else begin
        case Form2.GetPixelChar(x + 2, y + 2, Form2.StringGrid1) of
          'X': Old:= 140;
          '?': Old:= 512;
        else Old:= 140;
        end;
      end;
      Population:= Slice.PopCount;
      SG.Cells[x, y]:= Population.ToString;
      if (Known) and not(TooManyStates) then begin
        TotalStates:= TotalStates * Population;
        Frexp(TotalStates, Dummy, Exponent);
        TooManyStates:= (Exponent > 100);
      end;
      SGDiff.Cells[x, y]:= (Old - Slice.PopCount).ToString;
    end; { for y }
  end; { for x }
  if (ForceRefresh) then begin
    SG.Refresh;
    SGDiff.Refresh;
  end;
  if (TotalStates > 1000000000) then Solutions:= Format('%2.6e',[TotalStates])
  else Solutions:= Format('%.0n',[TotalStates]);
  Solutions:= ReplaceStr(Solutions,'E+00','E+');
  Solutions:= ReplaceStr(Solutions,'E+0','E+');
  Solutions:= ReplaceStr(Solutions,'E+','e+');
  Form2.LabelSolutions.Caption:= 'Solutions: '+Solutions;
end;

function PopCount(Input: int64): integer;
asm
  popcnt rax,rcx
end;

/// <summary>
///   Fast int64 random numbers.
/// </summary>
function Random64: Uint64;
const
  a: uint64 = 2862933555777941757;
  b: uint64 = 3037000493;
begin
  //x[n]:= a*x[n-1]+b
  TSlice.RandomSeed:= (a * TSlice.RandomSeed) + b;
  Result:= TSlice.RandomSeed;
end;

/// <summary>
///   Deprecated!
///   Perform a single run of gridwalker on the grid
///   using SuperSlices (the union of two slices).
/// </summary>
procedure DoARun(Rotation: TRotation = rNone);
var
  SS: TSuperSlice;
  Slices: TGrid;

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
// Returns the status of any changes
// The board is divided into black and white squares, like a checkerboard.
// In any given odd sweep only the white squares are processed.
// An even sweep processes only black squares.
// The square to process is the center at (x,y). It is paired with its
// neighbors to the N,S,E,W, which conviniently fall on different colors.
// When there is a change to any neighbor, that neighbor will be marked as active
// meaning it will be processed in the next sweep.
// Changes to the center trigger updates in those neighbors that have already been
// processed.
// Solve the given sliver with its neighbors to the East and South
// Returns the status of any changes
function TGrid.SliverSolve(x, y: integer; const MinMax: TRect): TSliverChanges;
var
  Sliver: TSliver;
  IndexCenter: integer;
  CenterChanged: boolean;

  function DoEW: TSliverChanges;
  begin
    if (x < MinMax.Right) then begin
      var IndexEast:= IndexCenter+1;
      //If there is a problem the sliver will be invalid.
      Sliver:= TSliver.EW(Self[IndexEast], Self[IndexCenter], Result);
      if Result.WestChanged then begin
        Self[IndexCenter]:= Self[IndexCenter] and Sliver.West;
        CenterChanged:= true;
      end;
      if Result.EastChanged then begin
        Self[IndexEast]:= Self[IndexEast] and Sliver.East;
        FActive.Activate(IndexEast); //Process the changes forward.
      end;
    end else begin
      var IndexWest:= IndexCenter - 1;
      Sliver:= TSliver.EW(Self[IndexCenter], Self[IndexWest], Result);
      if Result.WestChanged then begin
        Self[IndexWest]:= Self[IndexWest] and Sliver.West;
        FActive.Activate(IndexWest);
      end;
      if Result.EastChanged then begin
        Self[IndexCenter]:= Self[IndexCenter] and Sliver.East;
        CenterChanged:= true;
      end;
    end;
  end;

  function DoNS: TSliverChanges;
  begin
    if (y < MinMax.Bottom) then begin
      var IndexSouth:= IndexCenter + FSizeX;
      Sliver:= TSliver.NS(Self[IndexCenter], Self[IndexSouth], Result);
      if Result.SouthChanged then begin
        Self[IndexSouth]:= Self[IndexSouth] and Sliver.South;
        FActive.Activate(IndexSouth); //we need to also add south.west to the active list.
        if (x > MinMax.Left) then FActive.Activate(IndexSouth-1); //but only if it is in bounds
      end;
      if Result.NorthChanged then begin
        Self[IndexCenter]:= Self[IndexCenter] and Sliver.North;
        CenterChanged:= true;
      end;
    end else begin
      var IndexNorth:= IndexCenter - FSizeX;
      Sliver:= TSliver.NS(Self[IndexNorth], Self[IndexCenter], Result);
      if Result.SouthChanged then begin
        Self[IndexCenter]:= Self[IndexCenter] and Sliver.South;
        CenterChanged:= true;
      end;
      if Result.NorthChanged then begin
        Self[IndexNorth]:= Self[IndexNorth] and Sliver.North;
        FActive.Activate(IndexNorth); //we need to also add south.west to the active list.
        if (x > MinMax.Left) then FActive.Activate(IndexNorth-1); //but only if it is in bounds
      end;
    end;
  end;


begin
  CenterChanged:= false;
  IndexCenter:= (y * FSizeX) + x;
  //EW
  var ResultEW:= DoEW;
  if ResultEW.IsInvalid then Exit(ResultEW);
  var ResultNS:= DoNS;
  //If the center has never changed, mark it as inactive.
  if ResultNS.IsInvalid then exit(ResultNS);
  if (ResultEW.IsUnchanged and ResultNS.IsUnchanged) then begin
    FActive.Reset(IndexCenter);
    Result:= (ResultNS or ResultEW);
  end else if (CenterChanged) then while True do begin
    CenterChanged:= false;
    //The center keeps changing, loop until it stabilizes
    //Looping until changes stabilize makes a small difference +/- 5% savings.
    ResultEW:= DoEW;
    if not(CenterChanged) or ResultEW.IsInvalid then Exit(ResultEW or ResultNS);
    //We hardly ever reach this point
    CenterChanged:= false;
    ResultNS:= DoNS;
    if not(CenterChanged) or ResultNS.IsInvalid then Exit(ResultNS or ResultEW);
  end; {else while}
end;

// Solve the given sliver with its neighbors to the East and South
// Returns the status of any changes
function TGrid.SliverSolveReverse(x, y: integer; const MinMax: TRect): TSliverChanges;
var
  Sliver: TSliver;
  IndexCenter: integer;
  CenterChanged: boolean;

  function DoEW: TSliverChanges;
  begin
    if (x > MinMax.Left) then begin
      var IndexWest:= IndexCenter-1;
      //If there is a problem the sliver will be invalid.
      Sliver:= TSliver.EW(Self[IndexCenter], Self[IndexWest], Result);
      if Result.WestChanged then begin
        Self[IndexWest]:= Self[IndexWest] and Sliver.West;
        FActive.Activate(IndexWest);
      end;
      if Result.EastChanged then begin
        Self[IndexCenter]:= Self[IndexCenter] and Sliver.East;
        CenterChanged:= true;
      end;
    end else begin
      var IndexEast:= IndexCenter+1;
      //If there is a problem the sliver will be invalid.
      Sliver:= TSliver.EW(Self[IndexEast], Self[IndexCenter], Result);
      if Result.WestChanged then begin
        Self[IndexCenter]:= Self[IndexCenter] and Sliver.West;
        CenterChanged:= true;
      end;
      if Result.EastChanged then begin
        Self[IndexEast]:= Self[IndexEast] and Sliver.East;
        FActive.Activate(IndexEast);
      end;
    end;
  end;

  function DoNS: TSliverChanges;
  begin
    if (y > MinMax.top) then begin
      var IndexNorth:= IndexCenter - FSizeX;
      Sliver:= TSliver.NS(Self[IndexNorth], Self[IndexCenter], Result);
      if Result.SouthChanged then begin
        Self[IndexCenter]:= Self[IndexCenter] and Sliver.South;
        CenterChanged:= true;
      end;
      if Result.NorthChanged then begin
        Self[IndexNorth]:= Self[IndexNorth] and Sliver.North;
        FActive.Activate(IndexNorth);
        if (x < MinMax.Right) then FActive.Activate(IndexNorth+1);
      end;
    end else begin
      var IndexSouth:= IndexCenter + FSizeX;
      Sliver:= TSliver.NS(Self[IndexCenter], Self[IndexSouth], Result);
      if Result.SouthChanged then begin
        Self[IndexSouth]:= Self[IndexSouth] and Sliver.South;
        FActive.Activate(IndexSouth);
        if (x < MinMax.Right) then FActive.Activate(IndexSouth+1);
      end;
      if Result.NorthChanged then begin
        Self[IndexCenter]:= Self[IndexCenter] and Sliver.North;
        CenterChanged:= true;
      end;
    end;
  end;

begin
  CenterChanged:= false;
  IndexCenter:= (y * FSizeX) + x;
  //EW
  var ResultEW:= DoEW;
  if ResultEW.IsInvalid then Exit(ResultEW);
  var ResultNS:= DoNS;
  if ResultNS.IsInvalid then exit(ResultNS);
  if (ResultEW.IsUnchanged and ResultNS.IsUnchanged) then begin
    FActive.Reset(IndexCenter);
    Result:= (ResultNS or ResultEW);
  end else if (CenterChanged) then while True do begin //Looping now (rather than later) saves about 5%.
    CenterChanged:= false;
    //The center keeps changing, loop until it stabilizes
    //Looping until changes stabilize makes a small difference +/- 5% savings.
    ResultEW:= DoEW;
    if not(CenterChanged) or ResultEW.IsInvalid then Exit(ResultEW or ResultNS);
    //We hardly ever reach this point
    CenterChanged:= false;
    ResultNS:= DoNS;
    if not(CenterChanged) or ResultNS.IsInvalid then Exit(ResultNS or ResultEW);
  end;
end;

// Solve the given sliver with its neighbors to the East and South
// Returns the status of any changes
function TGrid.SliverSolveOld(x, y: integer; const MinMax: TRect): TSliverChanges;
var
  Sliver: TSliver;
  IndexCenter: integer;

  function DoEW: TSliverChanges;
  begin
    if (x < MinMax.Right) then begin
      var IndexEast:= IndexCenter+1;
      //If there is a problem the sliver will be invalid.
      Sliver:= TSliver.EW(Self[IndexEast], Self[IndexCenter], Result);
      if Result.WestChanged then Self[IndexCenter]:= Self[IndexCenter] and Sliver.West;
      if Result.EastChanged then Self[IndexEast]:= Self[IndexEast] and Sliver.East;
    end else Result:= TSliverChanges.UnChanged; { handle EW }
  end;

  function DoNS: TSliverChanges;
  begin
    if (y < MinMax.Bottom) then begin
      var IndexSouth:= IndexCenter + FSizeX;
      Sliver:= TSliver.NS(Self[IndexCenter], Self[IndexSouth], Result);
      if Result.SouthChanged then Self[IndexSouth]:= Self[IndexSouth] and Sliver.South;
      if Result.NorthChanged then Self[IndexCenter]:= Self[IndexCenter] and Sliver.North;
    end else Result:= TSliverChanges.UnChanged; { handle NS }
  end;

begin
  IndexCenter:= (y * FSizeX) + x;
  //EW
  var ResultEW:= DoEW;
  if ResultEW.IsInvalid then Exit(ResultEW);
  var ResultNS:= DoNS;
  if not(ResultNS.NorthChanged) or ResultNS.IsInvalid then Exit(ResultNS or ResultEW);
  //The center keeps changing, loop until it stabilizes
  while True do begin
    //Looping until changes stabilize makes a small difference +/- 5% savings.
    ResultEW:= DoEW;
    if not(ResultEW.WestChanged) or ResultEW.IsInvalid then Exit(ResultEW or ResultNS);
    //We hardly ever reach this point
    ResultNS:= DoNS;
    if not(ResultNS.NorthChanged) or ResultNS.IsInvalid then Exit(ResultNS or ResultEW);
  end;
end;

// Solve the given sliver with its neighbors to the East and South
// Returns the status of any changes
function TGrid.SliverSolveReverseOld(x, y: integer; const MinMax: TRect): TSliverChanges;
var
  Sliver: TSliver;
  IndexCenter: integer;

  function DoEW: TSliverChanges;
  begin
    if x > (MinMax.Left) then begin
      var IndexWest:= IndexCenter-1;
      //If there is a problem the sliver will be invalid.
      Sliver:= TSliver.EW(Self[IndexCenter], Self[IndexWest], Result);
      if Result.WestChanged then Self[IndexWest]:= Self[IndexWest] and Sliver.West;
      if Result.EastChanged then Self[IndexCenter]:= Self[IndexCenter] and Sliver.East;
    end else Result:= TSliverChanges.UnChanged; { handle EW }
  end;

  function DoNS: TSliverChanges;
  begin
    if y > (MinMax.top) then begin
      var IndexNorth:= IndexCenter - FSizeX;
      Sliver:= TSliver.NS(Self[IndexNorth], Self[IndexCenter], Result);
      if Result.SouthChanged then Self[IndexCenter]:= Self[IndexCenter] and Sliver.South;
      if Result.NorthChanged then Self[IndexNorth]:= Self[IndexNorth] and Sliver.North;
    end else Result:= TSliverChanges.UnChanged; { handle NS }
  end;

begin
  IndexCenter:= (y * FSizeX) + x;
  //EW
  var ResultEW:= DoEW;
  if ResultEW.IsInvalid then Exit(ResultEW);
  var ResultNS:= DoNS;
  if not(ResultNS.SouthChanged) or ResultNS.IsInvalid then Exit(ResultNS or ResultEW);
  //The center keeps changing, loop until it stabilizes
  while True do begin
    //Looping until changes stabilize makes a small difference +/- 5% savings.
    ResultEW:= DoEW;
    if not(ResultEW.EastChanged) or ResultEW.IsInvalid then Exit(ResultEW or ResultNS);
    //We hardly ever reach this point
    ResultNS:= DoNS;
    if not(ResultNS.SouthChanged) or ResultNS.IsInvalid then Exit(ResultNS or ResultEW);
  end;
end;




function TGrid.DoASliverRun: TSliverChanges;
var
  x, y: integer;
  ChangeCount: integer;
label
  Done;
begin
  //OldSlices:= MySlices.Clone; // Save the previous state for comparison.
  ChangeCount:= 0;
  for x:= 0 to FSizeX-1 do begin
    for y:= 0 to FSizeY-1 do begin
      Result:= Self.SliverSolveOld(x, y, Rect(0,0,15,15));
      ChangeCount:= ChangeCount + Result;
      if (Result.IsInvalid) then Exit;
    end; { for y }
  end; { for x }
  if (ChangeCount = 0) then exit(TSliverChanges.UnChanged);
  ChangeCount:= 0;
  for x:= FSizeX-1 downto 0 do begin
    for y:= FSizeY-1 downto 0 do begin
      Result:= Self.SliverSolveOld(x, y, Rect(0,0,15,15));
      ChangeCount:= ChangeCount + Result;
      if (Result.IsInvalid) then Exit;
    end; { for y }
  end; { for x }
  if (ChangeCount = 0) then Result:= TSliverChanges.UnChanged
  else Result:= TSliverChanges.Changed;
end;

class operator TGrid.Equal(const a, b: TGrid): boolean;
begin
  if (a.FSizeX <> b.FSizeX) or (a.FSizeY <> b.FSizeY) then exit(false);
  for var i := 0 to (a.FSizeX * a.FSizeY)-1 do begin
    if a.FData[i] <> b.FData[i] then begin
      Exit(false);
    end;
  end;
  Result:= true;
end;

class operator TGrid.NotEqual(const a, b: TGrid): boolean;
begin
  Result:= not(A = B);
end;

//From the two slices for a single pixel, reason over every possible
//5x5 bitmap to get a lookup table with ancestors for every possible 5x5.
//Running this takes about 4 to 8 hours depending on your CPU.
//This routine uses speculative exploration.
procedure TForm2.CreateLookupUsingGridSolver(ThreadIndex, ThreadCount: integer; var data: TArray<TSlice>);
type
  TImprovementDetails = array[0..10] of integer;
const
  CalcPoints: array[0..4] of TPoint = ((X: 2; Y: 2), (X:1; Y:1), (X:3; Y:1), (X:3; Y:3), (X:1; Y:3));
var
  Start, Finish: integer;
  Grid: TGrid;
  i,a: integer;
  x,y: integer;
  ZeroSlice: TSlice;
  OneSlice: TSlice;
  Five: integer;
  Target: integer;
  SumSoll, SumIst: uint64; //cumulative popcount of the norm and calculated lookup tables
  //Improvement: TArray<TImprovementDetails>;
  Population: integer;
  InitTimer, SliverTimer, ExploreTimer: THiResStopWatch;
label
  Done;
begin
  InitTimer:= THiResStopWatch.StartPaused;
  SliverTimer:= THiResStopWatch.StartPaused;
  ExploreTimer:= THiResStopWatch.StartPaused;
  ZeroSlice:= LookupTable[oCenter,0];
  OneSlice:= not(ZeroSlice);
  Grid:= TGrid.Create(5,5);
  //SetLength(Improvement, (1 shl 25));    //will zero initialize the array.
  SumSoll:= 0; SumIst:= 0;
  //Loop over all possible 5x5 grids
  //for i:= 0 to (1 shl 25)-1 do begin
  Start:= (((1 shl 25) div ThreadCount) * ThreadIndex);
  Finish:= Start + ((1 shl 25) div ThreadCount)-1;
  //Finish:= Start + 1000;
  for i:= Start to Finish do begin
    //Initialize the grid based on i
    InitTimer.UnPause;
    Five:= i;
    for y:= 0 to 4 do begin
      for x:= 4 downto 0 do begin
        if Odd(Five) then Grid[x,y]:= OneSlice
        else Grid[x,y]:= ZeroSlice;
        Five:= Five shr 1;
      end; { for x }
    end; { for y }
    InitTimer.Pause;
    //The gold standard for the center Slice
    Target:= LookupTable[oCenter, i].PopCount;
    Population:= Grid[2,2].PopCount;
    if (Target = 372) then goto Done;
    SliverTimer.Unpause;
    Inc(SumSoll, target);
    //Improvement[i][0]:= Target;
    //First solve the grid using normal sliversolve
    Grid.GridSolve;
    Population:= Grid[2,2].Popcount;
    //Improvement[i][1]:= Population;
    SliverTimer.Pause;
    if (Target >= Population) then goto Done;
    ExploreTimer.Unpause;
//    for y:= 0 to 2 do begin
//      for x:= 0 to 2 do begin
//        Grid.SpeculativeExploration(esExpensive, point(x+1,y+1));
//        Population:= Grid[2,2].Popcount;
//        Improvement[i][2+(x + (y*3))]:= Population;
//        if (Target >= Population) then begin
//          ExploreTimer.Pause;
//          goto Done;
//        end;
//      end;
    for a:= 0 to 0 do begin
      Grid.SpeculativeExploration(esExpensive, CalcPoints[a]);
      Population:= Grid[2,2].Popcount;
      //Improvement[i][2+(a)]:= Population;
      if (Target >= Population) then begin
        ExploreTimer.Pause;
        goto Done;
      end;
    end;
    ExploreTimer.Pause;
Done:
    Data[i]:= Grid[2,2];
    Inc(SumIst, Population);
    if ((Data[i] and LookupTable[oCenter, i]) <> LookupTable[oCenter, i]) and (ThreadIndex = 0) then begin
      Memo2.Lines.Add('Oops '+i.ToString+' does not conform');
      Application.ProcessMessages;
    end;
    if ((i mod (32767)) = 0) and (ThreadIndex = 0) then begin
      ProgressBar1.Position:= ProgressBar1.Position + 1;
      Application.ProcessMessages;
    end;
  end; {for i}
  if (ThreadIndex = 0) then begin
    Memo2.Lines.Add('InitTimer: '+InitTimer.ElapsedTicks.ToString);
    Memo2.Lines.Add('SliverTimer '+SliverTimer.ElapsedTicks.ToString);
    Memo2.Lines.Add('ExploreTimer '+ExploreTimer.ElapsedTicks.ToString);
    Memo2.Lines.Add('Total popcount of gold standard');
    Memo2.Lines.Add(SumSoll.ToString);
    Memo2.Lines.Add('Total popcount of calculated data');
    Memo2.Lines.Add(SumIst.ToString);
    Memo2.Lines.Add('Difference');
    Memo2.Lines.Add((SumIst - SumSoll).ToString);
  end;
end;

//From the two slices for a single pixel, reason over every possible
//5x5 bitmap to get a lookup table with ancestors for every possible 5x5.
//Running this takes about 4 to 8 hours depending on your CPU.
//This routine DOES NOT use speculative exploration.
procedure TForm2.BtnCreateLookupUsingSolverClick(Sender: TObject);
const
  MaxXY = 4;
  Middle = MaxXY div 2;
  ItemsToTestCount = 32 * 1024 * 1024;
type
  TSlices5 = array [0 .. MaxXY, 0 .. MaxXY] of TSlice;
var
  i: integer;
  Grid: TGrid;
  OldGrid: TGrid;
  Five: integer;
  x, y: integer;
  ZeroSlice: TSlice;
  OneSlice: TSlice;

  procedure SolveWithSliversDown(var Slices: TGrid; var Result: TSliverChanges);
  var
    x, y: integer;
  begin
    for x:= 0 to MaxXY do begin
      for y:= 0 to MaxXY do begin
        Result:= Slices.SliverSolve(x, y, Rect(0,0,MaxXY, MaxXY));
        if not(Result.IsValid) then Exit;
      end; { for y }
    end; {for x}
  end;

  procedure SolveWithSliversUp(var Slices: TGrid; var Result: TSliverChanges);
  var
    x, y: integer;
  begin
    for x:= MaxXY downto 0 do begin
      for y:= MaxXY downto 0 do begin
        Result:= Slices.SliverSolve(x, y, Rect(0,0,MaxXY, MaxXY));
        if not(Result.IsValid) then Exit;
      end; { for y }
    end; {for x}
  end;

var
  TotalDiff, TotalCount: uint64;
  Changed: boolean;
  a: integer;
  //Status: TSliverChanges;
  ChangeCount: integer;
  Soll, Ist: TSlice;
  Oops: integer;
  StopWatch: TStopWatch;
  HoleCount: integer;
  NewLookup: TArray<TSlice>;
  FS: TFileStream;
  Status: TSliverChanges;
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
        SolveWithSliversDown(Grid, Status);
        Changed:= Status.KeepGoing;
        Inc(ChangeCount, integer(Changed));
        if (Status.KeepGoing) then begin
          SolveWithSliversUp(Grid, Status);
          Changed:= Status.KeepGoing;
        end;
      until not(Changed);
      // Now solve for every allowed state in the center slice
      OldGrid:= Grid;
      for a:= 0 to 511 do begin
        if (Grid[Middle, Middle].GetBit(a)) then begin
          // Reset the grid back to the start
          Grid:= OldGrid;
          // Force the grid to the state.
          Grid[Middle, Middle].ForceSingleBit(a);
          repeat
            SolveWithSliversDown(Grid, Status);
            if (Status.IsValid) then SolveWithSliversUp(Grid, Status);
          until not(Status.KeepGoing);
          if (Status.IsInvalid) then OldGrid[Middle, Middle].SetBit(a, false);
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

procedure TForm2.BtnReverseLookupClick(Sender: TObject);
begin
  Self.LookupTable.ReverseLeftToRight;
end;

//deprecated
//Rotating stuff, does not work correctly
//No longer needed
procedure TForm2.BtnRotateCounterClick(Sender: TObject);
var
  Slice: PSlice;
begin
  GetCounterLayout;
  for Slice in MySlices do begin
    Slice.ReorderSlice(NewLayout);
  end;
end;

//Start with the hard GoE.
procedure TForm2.BtnInitWith_GoEClick(Sender: TObject);
begin
  InitWithGoE;
end;

//Deprecated.
//No longer used.
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

//Test code, not sure what this does
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

//Early test code, should be removed, now that we have a full set of unit tests.
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

//Starting with the 2GB known 7x7->5x5->3x3 lookup table
//Make 48 lookup tables for all the different ways in with a
//known 5x5 rect and an unknown 5x5 rect can overlap.
//Some overlaps are excluded, because we already know the result
//will be a table filled with fully unknown `512` slices.
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


//Slow version of DeleteBit for unit testing
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

//Deprecated
//Old test code, should be removed.
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

//TestInsight is a IDE plugin that automatically runs tests in the background
//if the source code has changed.
function IsTestInsightRunning: Boolean;
var
  client: ITestInsightClient;
begin
  client := TTestInsightRestClient.Create;
  client.StartedTesting(0);
  Result := not client.HasError;
end;

//Perform a double round of solving.
procedure TForm2.Action_SliverSolveRoundExecute(Sender: TObject);
const
  ForceRefresh = true;
var
  Changes: TSliverChanges;
begin
  OldSlices:= MySlices.Clone; // Save the previous state for comparison.
  repeat
    Changes:= MySlices.DoASliverRun;
  until (Changes.IsInvalid) or (Changes.IsUnchanged);
  //DoASliverRun;
  DisplaySlices(StringGrid2, StringGrid3, MySlices, ForceRefresh);
end;

//Run any tests that failed previously, so we can debug.
procedure TForm2.BtnDoFailingTestsClick(Sender: TObject);
begin
  UnknownSlice(TCake.Create(524560,32742631));
end;

//Force unit tests to run when debugging the application in the IDE.
procedure TForm2.BtnStartUnitTestsClick(Sender: TObject);
begin
  if IsTestInsightRunning then TestInsight.DUnitX.RunRegisteredTests
  else ShowMessage('Run the tests inside the IDE using TestInsight');
end;

//Create unknown slices by folding all different possible permutations
//of known slices that fit the unknown describtion together.
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

//Create any unknown slice on the fly.
//First select the unknown lookuptable that fits the unknown pixels listed.
//If there are any unknown pixels left, fold states as needed.
//Unless we have a checkerboard pattern, the number of additional
//foldings should be small. In the worst case this can take a long time.
//Luckily this operation needs to be performed only once in per pattern.
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

//Get some statistics, no longer relevant
class procedure TForm2.LeadingTrailingPopCount(UnknownMask: integer; out Leading, Trailing, Popcnt: integer);
asm
  //ecx = UnknownMask
  //[rdx] = Leading
  //[r8] = Trailing
  //[r9] = popcount
  //rep bsr eax,ecx//lzcnt [rdx],rcx
  mov eax,32         //BSR eax,ecx If the ecx= 0, BSR sets ZF to 1 and does not change eax.
  bsr eax,ecx
  mov [rdx],eax
  //rep bsf eax,ecx//tzcnt [r8],rcx
  mov eax,32
  bsf eax,ecx        //BSF eax,ecx If the ecx= 0, BSF sets ZF to 1 and does not change eax.
  mov [r8],eax
  popcnt eax,ecx
  mov [r9],eax
end;

//If we want to iterate all set bits in a int64, we can do a for loop
//however that is slow. Better to use the intrinsics in the CPU to
//mask off the parts we have already scanned and then scan for the next
//bit. This is esp. efficient if there are few bits set.
class function TForm2.GetNextBitSet(previous: integer; i: Uint64): integer;
asm
  //ecx = previous
  //rdx = i
  inc ecx          //make sure we get the NEXT bit.
  shr rdx,cl      //remove the previous bits
  //rep bsf eax,edx //tzcnt eax,edx //Count the number of LSB that are zero
  mov eax,64
  bsf rax,rdx     //BSF eax,ecx If the ecx= 0, BSF sets ZF to 1 and does not change eax.
  add rax,rcx     //add the previous count back in.
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

//deprecated
//This code is no longer relevant, because this approach was dumped.
procedure TForm2.BtnSolveWithChunkLookupClick(Sender: TObject);
var
  x, y: integer;
  i: integer;
  Slices: TGrid;
  Chunk: TSuperSlice;
begin
  // Load the lookup table entries
  if Length(ChunkLookup[cFlat]) = 0 then BtnLoadSmallLookups.Click;
  Slices:= TGrid.Create(16,16);
  for i:= 0 to (16*16)-1 do Slices[i]:= TSlice.FullyUnknown;
  for x:= 0 to 15 do begin
    for y:= 0 to 15 do begin
      Chunk:= ChunkLookup[cStanding, GetFutureStandingChunk(StringGrid1, x + 1, y + 1)];
      Slices[x, y]:= Slices[x, y] and Chunk.South;
      if (y < (15)) then Slices[x, y + 1]:= Slices[x, y + 1] and Chunk.North;

      Chunk:= ChunkLookup[cFlat, GetFutureFlatChunk(StringGrid1, x + 1, y + 1)];
      Slices[x, y]:= Slices[x, y] and Chunk.East;
      if (x < (15)) then Slices[x + 1, y]:= Slices[x + 1, y] and Chunk.West;
    end;
  end;
  DisplaySlices(StringGrid2, StringGrid3, Slices);
  MySlices:= Slices;
end;

//Recreate a corner lookup table to make sure the GPU generated
//these tables correctly.
//takes a long time to run.
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

//display a 5x5 bitmap on-screen
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

//The total of the counts table ought to add up to 2^49, let's make sure
procedure TForm2.BtnValidateCountTableClick(Sender: TObject);
var
  Soll, Ist: uint64;
  MinOn, MinOff: uint64;
  MinOnIndex, MinOffIndex: integer;
  Counts: TArray<uint64>;
begin
  if not(FileOpenDialog1.Execute) then Exit;
  var FS:= TFileStream.Create(FileOpenDialog1.Filename, fmOpenRead);
  SetLength(Counts, FS.Size div SizeOf(uint64));
  FS.Read64(TBytes(Counts), 0, FS.Size);
  FS.Free;
  Ist:= 0;
  for var item in Counts do Inc(Ist, item);
  Soll:= 1;
  Soll:= Soll shl 49;
  System.Assert(Ist = Soll);
  MinOn:= Ist;
  MinOff:= Ist;
  // Find the minimum on and off slices
  MinOnIndex:= 0;
  MinOffIndex:= 0;
  for var i:= 0 to high(Counts) do begin
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
  end; {for i}
  Display5x5(TCake.Create(MinOnIndex,0), SGMinOn);
  Display5x5(TCake.Create(MinOffIndex,0), SGMinOff);
  BtnValidateCountTable.Caption:= 'MinOn=' + MinOn.ToString + ' MinOff=' + MinOff.ToString;
end;

//deprecated
//warning !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//Apply the corner lookup tables. This reuses the GPU exploration
//to reduce the number of states further than just the core
//lookup table can.
//warning does not work with unknown pixels yet!!!
procedure TForm2.BtnApplyNELookupTablesClick(Sender: TObject);
var
  x, y: integer;
  Slices: TGrid;
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


{TODO -oJB -cTForm2.BtnApplyNELookupTablesClick : Make the corner lookups work with unknown pixels}
//warning !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//Apply the corner lookup tables. This reuses the GPU exploration
//to reduce the number of states further than just the core
//lookup table can.
//warning does not work with unknown pixels yet!!!
procedure TForm2.Btn5x5To3x3_LookupClick(Sender: TObject);
var
  Lookup: TArray<TSlice>;
  Block: TCellBlock;
  Pattern: PUnit;
  Result: PUnit;
  Empty: TUnit;
begin
  SetLength(Lookup, 1 shl (3*3));
  Block:= TCellBlock.Create(0,0,nil);
  Block.Clear;
  Pattern:= @Block.p[10];
  Result:= @Block.q[10];
  Empty.Clear;
  for var i:= (1 shl (5*5)) -1 downto 0 do begin
    var BitToSet:= ((i and $1C0) shr 6) xor ((i and $3800) shr (11-3)) xor ((i and $70000) shr (16-6));
    Pattern.Clear;
    var a:= i;
    //a:= $739C0;
    //BitToSet:= ((a and $1C0) shr 6) xor ((a and $3800) shr (11-3)) xor ((a and $70000) shr (16-6));
    for var y:= 0 to 4 do begin
      for var x:= 0 to 4 do begin
        if odd(a) then Pattern.SetPixel(x+1,y+1);
        a:= a shr 1;
      end; {for x}
    end; {for y}
    Universe.GeneratePtoQ_AVX_32(Pattern, @Empty, @Empty, @Empty);
    var LookupEntry:= ((Result.w[3] and $0038) shr 3) xor ((Result.w[4] and $0038)) xor ((Result.w[5] and $0038) shl 3);

    Lookup[LookupEntry].SetBit(BitToSet);
    //Lookup[BitToSet].SetBit(LookupEntry);
  end; {for i}
  MiniLookup:= Lookup;
end;

procedure TForm2.BtnApplyCornerLookupTablesClick(Sender: TObject);
var
  x, y: integer;
  Slices: TGrid;
begin
  // Read the lookup tables
  if not(LookupTable.HasCornerData) then begin
    var Filename:= AppRegistry.ReadString(cCornerIndex);
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

/// <summary>
///   Load a lookuptable with slice data from disk
/// </summary>
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

/// <summary>
///  Create a grid from the bitmap on-screen
///  And apply the lookup table to all slices in that grid.
/// </summary>
procedure TForm2.BtnAppyLookupTableClick(Sender: TObject);
begin
  // Read the lookup table
  // if not FileOpenDialog1.Execute then Exit;
  // FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
  // SetLength(LookupTable, FS.Size div SizeOf(TSlice));
  // FS.Read64(TBytes(LookupTable), 0, FS.Size);
  // FS.Free;
  var Slices:= TGrid.Create(16,16);
  // Get the slice data from the grid, by looking it up in the lookup table
  for var x:= 0 to 15 do begin
    for var y:= 0 to 15 do begin
      //Slices[x, y]:= FutureGridToPastSlice(StringGrid1, x, y, LookupTable, oCenter);
      Slices[x, y]:= FutureGridToPastSliceMini(StringGrid1, x, y, LookupTable, oCenter);
      StringGrid3.Cells[x, y]:= '';
    end;
  end;
  DisplaySlices(StringGrid2, StringGrid3, Slices);
  // Take every slice and confront it with its neighbors to the west, east, north and south.
  MySlices:= Slices;
end;

//Do a single round of solving using chunks.
//Because this gives the exact same effect as using slivers,
//we can use it as a test to make sure we did not screw up the sliver
//solve code.
{TODO -oJB -cTForm2.BtnSolveRoundUsingChunksClick : Make sure chunk solve works the same as sliver solve (sliver is optimized, chunk is not)}
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

//Check to see which processor cores are enabled for this process.
function SingleProcessorMask(const ProcessorIndex: Integer): DWORD_PTR;
begin
  Result := 1 shl (ProcessorIndex);
end;

procedure TForm2.EnableAllCPUCores;
var
  ProcAffinityMask: DWORD_PTR;
  SystemAffinityMask: DWORD_PTR;
  a: integer;
begin
  var ThreadCount:= System.CpuCount;
  GetProcessAffinityMask(GetCurrentProcess, ProcAffinityMask, SystemAffinityMask);
  longbool(a):= SetProcessAffinityMask(GetCurrentProcess, (1 shl ThreadCount)-1);
  if (a=0) then RaiseLastOSError;
  GetProcessAffinityMask(GetCurrentProcess, ProcAffinityMask, SystemAffinityMask);
end;

//Start up as many threads as the CPU has cores
//And try and recreate the 7x7->5x5->3x3 lookup table using
//1x1          ->3x3   ->5x5                    ->3x3
//single pixel->slice, for every pixel in 5x5, eliminate impossible constellations from slices
//using speculative exploration.
procedure TForm2.BtnCreateLookup5x5to3x3UsingSpeculativeExplorationClick(Sender: TObject);
var
  FS: TFileStream;
  NewLookup: TArray<TSlice>;
  ThreadCount: integer;
  Threads: TArray<TThread>;
  CurrentProcessor: integer;
  i,a: integer;
  Done: boolean;
begin
  SetLength(NewLookup, 1 shl 25);
  ThreadCount:= System.CpuCount;
  SetLength(Threads, ThreadCount);
  CurrentProcessor:= GetCurrentProcessorNumber;
  a:= 0;
  for i:= 1 to ThreadCount-1 do begin
    Threads[i]:= TThread.CreateAnonymousThread(procedure begin
      CreateLookupUsingGridSolver(i, ThreadCount, NewLookup);
    end);
    Threads[i].FreeOnTerminate:= false;
    if (CurrentProcessor = a) then Inc(a);
    Inc(a);
    Threads[i].Start;
  end; {for i}
  CreateLookupUsingGridSolver(0, ThreadCount, NewLookup);
  //Wait for all threads to finish
  repeat
    Done:= true;
    for i:= 1 to ThreadCount-1 do begin
      if not(Threads[i].Finished) then Done:= false;
    end;
    Sleep(1000);
    Application.ProcessMessages;
  until done;
  if (not(FileSaveDialog1.Execute)) then exit;
  FS:= TFileStream.Create(FileSaveDialog1.Filename, fmCreate);
  try
    FS.Write(TBytes(NewLookup), Length(NewLookup) * SizeOf(TSlice));
  finally
    FS.Free;
  end;
end;

//Test the mini-lookups used for transforming slices to slivers
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

//calculate slices using only single pixels as information.
//does not really do any solving, just looks up single pixels.
procedure TForm2.BtnMinimalSolveClick(Sender: TObject);
var
  x, y: integer;
  Slices: TGrid; // array[0..10,0..10] of TSlice;
begin
  Slices:= TGrid.Create(16,16);
  for x:= 0 to 15 do begin
    for y:= 0 to 15 do begin
      Slices[x, y]:= FutureGridToPastSliceSimple(StringGrid1, x, y, LookupTable);
      StringGrid3.Cells[x, y]:= '';
    end;
  end;
  DisplaySlices(StringGrid2, StringGrid3, Slices);
  // Take every slice and confront it with its neighbors to the west, east, north and south.
  MySlices:= Slices;
end;

procedure TForm2.BtnOldNewSolveInLockstepClick(Sender: TObject);
begin
  var SlicesOld:= TGrid.Create(10,10);
  for var x:= 0 to 9 do begin
    for var y:= 0 to 9 do begin
      SlicesOld[x, y]:= FutureGridToPastSlice(StringGrid1, x+3, y+3, LookupTable, oCenter);
      //MySlices[x, y]:= FutureGridToPastSliceSimple(StringGrid1, x+3, y+3, LookupTable);
      //MySlices[x, y]:= FutureGridToPastSliceMini(StringGrid1, x+3, y+3, LookupTable);
      //DisplaySlices(Form2.StringGrid2, Form2.StringGrid3, MySlices, true);
      StringGrid3.Cells[x+3, y+3]:= '';
    end;
  end;
  var SlicesNew:= SlicesOld.Clone;
  GridSolveLockstep(SlicesOld, SlicesNew);
end;



procedure TForm2.BtnOld_SolveAndTimeClick(Sender: TObject);
begin
  // Read the lookup table
  // if not FileOpenDialog1.Execute then Exit;
  // FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
  // SetLength(LookupTable, FS.Size div SizeOf(TSlice));
  // FS.Read64(TBytes(LookupTable), 0, FS.Size);
  // FS.Free;
  var MySlices:= TGrid.Create(10,10);
  // Get the slice data from the grid, by looking it up in the lookup table
  for var x:= 0 to 9 do begin
    for var y:= 0 to 9 do begin
      //if (x in [0,1,8,9]) or (y in [0,1,8,9]) then begin
        MySlices[x, y]:= FutureGridToPastSliceSimple(StringGrid1, x+3, y+3, LookupTable);
//      end else begin
//        //MySlices[x, y]:= FutureGridToPastSliceMini(StringGrid1, x+3, y+3, LookupTable, oCenter);
//        MySlices[x, y]:= FutureGridToPastSlice(StringGrid1, x+3, y+3, LookupTable, oCenter);
//      end;
      //DisplaySlices(Form2.StringGrid2, Form2.StringGrid3, MySlices, true);
      StringGrid3.Cells[x+3, y+3]:= '';
    end;
  end;
  //Start the timer
  var Timer:= THiResStopWatch.StartNew;

  var Status:= MySlices.GridSolveOld(Rect(0,0,9,9));
  //var ValidSolutions: TArray<TGrid>;
  //var ValidCount:= 0;
  //SetLength(ValidSolutions, 100);
  //DisplaySlices(Form2.StringGrid2, Form2.StringGrid3, MySlices, true);
  if Status.IsValid then Status:= MySlices.GetUniqueSolutionOld;//(ValidSolutions, ValidCount);
  //Label1.Caption:= ValidCount.ToString;
  Timer.Stop;
  Memo1.Lines.Add(Timer.ElapsedTicks.ToString+' ticks until solution');
  Memo1.Lines.Add(Timer.ElapsedMilliseconds.ToString+' ms until solution');
  Memo1.Lines.add((Timer.ElapsedTicks / Timer.ElapsedMilliseconds).ToString+ ' ticks per ms');
  if Status.IsInvalid then Memo1.Lines.Add('UNSAT - Pattern is a GoE')
  else Memo1.Lines.Add('SAT - Pattern has a solution');
end;

procedure TForm2.BtnDictSolveAndTimeClick(Sender: TObject);
begin
  // Read the lookup table
  // if not FileOpenDialog1.Execute then Exit;
  // FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
  // SetLength(LookupTable, FS.Size div SizeOf(TSlice));
  // FS.Read64(TBytes(LookupTable), 0, FS.Size);
  // FS.Free;
  var MySlices:= TDictGrid.Create(10,10);
  // Get the slice data from the grid, by looking it up in the lookup table
  for var x:= 0 to 9 do begin
    for var y:= 0 to 9 do begin
      case FutureGridToPastDictSliceSimple(StringGrid1, x+3, y+3, LookupTable) of
        0: MySlices.Items[x,y]:= TSliceDict.Zero;
        1: MySlices.Items[x,y]:= TSliceDict.One;
        -1: MySlices.Items[x,y]:= TSliceDict.Unknown;
      end;
      //DisplaySlices(Form2.StringGrid2, Form2.StringGrid3, MySlices, true);
      StringGrid3.Cells[x+3, y+3]:= '';
    end;
  end;
  //Start the timer
  //var ValidSolutions: TArray<TGrid>;
  //var ValidCount:= 0;
  var Timer:= THiResStopWatch.StartNew;

  var Status:= MySlices.GridSolveOld(Rect(0,0,9,9));
  //SetLength(ValidSolutions, 100);
  //DisplaySlices(Form2.StringGrid2, Form2.StringGrid3, MySlices, true);
  if Status.IsValid then Status:= MySlices.GetUniqueSolutionOld;//(ValidSolutions, ValidCount);
  Timer.Stop;
  //Label1.Caption:= ValidCount.ToString;

  Memo1.Lines.Add(Timer.ElapsedTicks.ToString+' ticks until solution');
  Memo1.Lines.Add(Timer.ElapsedMilliseconds.ToString+' ms until solution');
  Memo1.Lines.add((Timer.ElapsedTicks / Timer.ElapsedMilliseconds).ToString+ ' ticks per ms');
  if Status.IsInvalid then Memo1.Lines.Add('UNSAT - Pattern is a GoE')
  else Memo1.Lines.Add('SAT - Pattern has a solution');
end;


// orig    counter   clock    diagonal other diag
// 012     258       630      036      852
// 345     147       741      147      741
// 678     036       852      258      630

// orig    counter
// 210     036
// 543     147
// 876     258

procedure TForm2.Button1Click(Sender: TObject);
begin
  // Read the lookup table
  // if not FileOpenDialog1.Execute then Exit;
  // FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
  // SetLength(LookupTable, FS.Size div SizeOf(TSlice));
  // FS.Read64(TBytes(LookupTable), 0, FS.Size);
  // FS.Free;
  var MySlices:= TGrid.Create(10,10);
  // Get the slice data from the grid, by looking it up in the lookup table
  for var x:= 0 to 9 do begin
    for var y:= 0 to 9 do begin
      //MySlices[x, y]:= FutureGridToPastSlice(StringGrid1, x+3, y+3, LookupTable, oCenter);
      MySlices[x, y]:= FutureGridToPastSliceSimple(StringGrid1, x+3, y+3, LookupTable);
      //MySlices[x, y]:= FutureGridToPastSliceMini(StringGrid1, x+3, y+3, LookupTable, oCenter);
      //DisplaySlices(Form2.StringGrid2, Form2.StringGrid3, MySlices, true);
      StringGrid3.Cells[x+3, y+3]:= '';
    end;
  end;
  //Start the timer
  var Timer:= THiResStopWatch.StartNew;
  MySlices.FActive.Limit(Rect(0,0,9,9));
  var Status:= MySlices.GridSolve(Rect(0,0,9,9));
  //DisplaySlices(Form2.StringGrid2, Form2.StringGrid3, MySlices, true);
  if Status.IsValid then Status:= MySlices.GetUniqueSolution;
  Timer.Stop;
  Memo1.Lines.Add(Timer.ElapsedTicks.ToString+' ticks until solution');
  Memo1.Lines.Add(Timer.ElapsedMilliseconds.ToString+' ms until solution');
  Memo1.Lines.add((Timer.ElapsedTicks / Timer.ElapsedMilliseconds).ToString+ ' ticks per ms');
  if Status.IsInvalid then Memo1.Lines.Add('UNSAT - Pattern is a GoE')
  else Memo1.Lines.Add('SAT - Pattern has a solution');
end;


procedure TForm2.BtnValidateCompressedLookupTableClick(Sender: TObject);
var
  UncompressedLookup: TArray<TSlice>;
begin
  FileOpenDialog1.Title:= 'Uncompressed lookup table';
  if not(FileOpenDialog1.Execute) then Exit;
  var FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
  SetLength(UncompressedLookup, FS.Size div SizeOf(TSlice));
  FS.Read64(TBytes(UncompressedLookup), 0, FS.Size);
  FS.Free;
  var Count:= 1 shl 25;
  for var i:= 0 to Count-1 do begin
    if LookupTable.Items[oCenter, i] <> UncompressedLookup[i] then begin
      Assert(LookupTable.Items[oCenter, i] = UncompressedLookup[i]);
    end;
  end; {for i}
end;

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

//deprecated
function TForm2.GetCounterLayout: TArray<integer>;
begin
  Button8Click(Form2);
  Result:= NewLayout;
end;

procedure TForm2.ReportSolution(NW, NE, SW, SE: integer);
begin
  MemoGoE_solution.Lines.Add('NWSESWNE:'+NW.ToString+SE.ToString+SW.ToString+NE.ToString);
  MemoGoE_solution.Lines.SaveToFile('GoE_solutions.txt');
end;

function Flip45(input: integer): integer;
begin
  Result:= ((input and (1 shl 24)) shr (24-0)) xor  //24-> 0
           ((input and (1 shl 19)) shr (19-1)) xor  //19-> 1
           ((input and (1 shl 14)) shr (14-2)) xor  //14-> 2
           ((input and (1 shl 9))  shr (9-3)) xor   //9 -> 3
           ((input and (1 shl 4))  shr (4-4)) xor   //4 -> 4

           ((input and (1 shl 23)) shr (23-5)) xor
           ((input and (1 shl 18)) shr (18-6)) xor
           ((input and (1 shl 13)) shr (13-7)) xor
           ((input and (1 shl 8))  shr (8-8)) xor
           ((input and (1 shl 3))  shl (9-3)) xor

           ((input and (1 shl 22)) shr (22-10)) xor
           ((input and (1 shl 17)) shr (17-11)) xor
           ((input and (1 shl 12)) shr (12-12)) xor
           ((input and (1 shl 7))  shl (13-7)) xor
           ((input and (1 shl 2))  shl (14-2)) xor

           ((input and (1 shl 21)) shr (21-15)) xor
           ((input and (1 shl 16)) shr (16-16)) xor
           ((input and (1 shl 11)) shl (17-11)) xor
           ((input and (1 shl 6))  shl (18-6)) xor
           ((input and (1 shl 1))  shl (19-1)) xor

           ((input and (1 shl 20)) shr (20-20)) xor
           ((input and (1 shl 15)) shl (21-15)) xor
           ((input and (1 shl 10)) shl (22-10)) xor
           ((input and (1 shl 5))  shl (23-5)) xor
           ((input and (1 shl 0))  shl (24-0));
end;

function Flip45Part(input: integer): integer;
begin
  Result:= ((input and (1 shl 3))  shl (9-3)) xor

           ((input and (1 shl 7))  shl (13-7)) xor
           ((input and (1 shl 2))  shl (14-2)) xor

           ((input and (1 shl 11)) shl (17-11)) xor
           ((input and (1 shl 6))  shl (18-6)) xor
           ((input and (1 shl 1))  shl (19-1)) xor

           ((input and (1 shl 15)) shl (21-15)) xor
           ((input and (1 shl 10)) shl (22-10)) xor
           ((input and (1 shl 5))  shl (23-5)) xor
           ((input and (1 shl 0))  shl (24-0));
  Result:= (input and $119DFF) xor Result;
end;

procedure TForm2.BtnSearchGoEClick(Sender: TObject);
begin
  Assert(Flip45($1FFFFFF) = $1FFFFFF);
  Assert(Flip45Part($119DFF) = $1FFFFFF);
  EnableAllCPUCores;
  BtnSearchGoE.Enabled:= false;


  if Length(CountsTable) = 0 then begin
    if not(FileOpenDialog1.Execute) then exit;
    var FS:= TFileStream.Create(FileOpenDialog1.Filename, fmOpenRead);
    Assert(FS.Size mod SizeOf(Int64) = 0);
    Assert(FS.Size = (1 shl 25)*SizeOf(int64));
    SetLength(CountsTable, 1 shl 25);
    FS.Read64(TBytes(CountsTable),0,FS.Size);
    SetLength(CountsIndex, 1 shl 25);
    for var i:= 0 to (1 shl 25)-1 do begin
      CountsIndex[i]:= i;
    end;
    TArray.Sort<integer>(CountsIndex, TDelegatedComparer<integer>.Construct(
      function(const Left, Right: integer): integer
      begin
        if CountsTable[Left] > CountsTable[Right] then Exit(1);
        if CountsTable[Left] < CountsTable[Right] then Exit(-1);
        Result:= 0;
      end
    ));
  end;
  Parallel.Async(procedure begin
    var ZeroSlice:= LookupTable[0,oCenter];
    var OneSlice:= not(ZeroSlice);
    Parallel.For(0,100).NoWait.NumTasks(System.CPUCount).Execute(procedure(NW: integer) begin
      var Grid:= TGrid.Create(10,10);
    //for var NW:= 0 to 100 do begin
      for var SW:= 0 to 100 do begin
        TThread.Synchronize(nil, procedure begin MemoGoE_solution.Lines[0]:= 'NW,SW:'+NW.ToString+','+SW.ToString end);
        for var SE:= 0 to 100 do begin
          for var NE:= 0 to 100 do begin

            var iNW:= CountsIndex[NW];
            var iSE:= Flip45(iNW);
            var iSW:= Flip45Part(CountsIndex[SW]);
            var iNE:= Flip45Part(CountsIndex[NE]);

            //Now get the pixels from the i's
            for var y:= 0 to 4 do begin
              for var x:= 4 downto 0 do begin
                var Mask:= (1 shl (4-x)) shl (y * 5);
                if ((Mask and iNW) <> 0) then Grid[x,y]:= OneSlice else Grid[x,y]:= ZeroSlice;
                if ((Mask and iNE) <> 0) then Grid[x+5,y]:= OneSlice else Grid[x+5,y]:= ZeroSlice;
                if ((Mask and iSW) <> 0) then Grid[x,y+5]:= OneSlice else Grid[x,y+5]:= ZeroSlice;
                if ((Mask and iSE) <> 0) then Grid[x+5,y+5]:= OneSlice else Grid[x+5,y+5]:= ZeroSlice;
              end; {for x}
            end; {for y}
            //The grid is set, let's solve it.
            if Grid.GridSolveOld(Rect(0,0,9,9)).IsInvalid then begin
              //We have found a GoE.
              TThread.Synchronize(nil, procedure begin ReportSolution(NW, NE, SW, SE); end);
            end else begin
              //Not yet, keep looking
              if Grid.GetUniqueSolutionOld.IsInvalid then begin
                //We have found a GoE
                TThread.Synchronize(nil, procedure begin ReportSolution(NW, NE, SW, SE); end);
              end;
            end;
          end; {for NE}//end); {parallel.for}
        end; {for SE}
      end; {for SW}
    end); {for parallel NW}
    TThread.Queue(nil, procedure begin BtnSearchGoE.Enabled:= true end);
  end); //TTask.run
end;

//deprecated
procedure TForm2.BtnSolveCounterClick(Sender: TObject);
begin
  DoARun(rCounter);
  DisplaySlices(StringGrid2, StringGrid3, MySlices);
end;

//display all allowed constellations in a slice.
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

//Init the on-screen display with a given pattern.
//'X' = on
//'.' = end of data
//',' = new line
//' ' = off
//'?' = unknown
procedure TForm2.InitWithPattern(const Pattern: string);
begin
  var SG:= StringGrid1;
  for var x:= 0 to SG.ColCount-1 do for var y:= 0 to SG.RowCount-1 do begin
    SG.Cells[x,y]:= ' ';
  end;
  var x:= 5;
  var y:= 5;
  var i:= 1;
  while Pattern[i] <> '.' do begin
    if Pattern[i] = 'X' then SG.Cells[x, y]:= 'X'
    else if (Pattern[i] = '-') then SG.Cells[x,y]:= ' ';
    if Pattern[i] = ',' then begin
      x:= 5;
      Inc(y);
    end
    else Inc(x);
    Inc(i);
  end;
end;

//Easy GoE, can be solved trivially
procedure TForm2.InitWithGoE2;
const
  Pattern: string = '-X-XXX-X--,'
                  + '--X-X-X--X,'
                  + 'X-XXX--XX-,'
                  + '-X-XXXXX-X,'
                  + 'X--X--XXXX,'
                  + 'XXXX--X--X,'
                  + 'X-XXXXX-X-,'
                  + '-XX--XXX-X,'
                  + 'X--X-X-X--,'
                  + '--X-XXX-X-.';
begin
  InitWithPattern(Pattern);
end;

//Hard GoE
procedure TForm2.InitWithGoE;
const
  Pattern: string = '-X-XXX-X--,' +
                    '--X-X-X--X,' +
                    'X-XXX--XX-,' +
                    '-X-XXXXX-X,' +
                    'X--X--XXXX,' +
                    'XXXX--X--X,' +
                    'X-XXXXX-X-,' +
                    '-XX--XXX-X,' +
                    'X--X-X-X--,' +
                    '--X-XXX-X-.';
begin
  InitWithPattern(Pattern);
end;

//Called when the app,ication starts up.
procedure TForm2.FormCreate(Sender: TObject);
begin
  InitWithGoE;
  InitNewLayout;
end;

//Called whenever the form receives focus
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
    GlobalDict:= TSliceDict.Create;

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

//Initializes a list of all allowed constellations in a slice
//with numbers 0..511, i.e. a fully unknown `512` slice.
procedure TForm2.InitNewLayout;
var
  i: integer;
begin
  SetLength(NewLayout, 512);
  for i:= 0 to 511 do NewLayout[i]:= i;
end;


/// <summary>
///  Performs speculative exploration starting the given slice
///  For every single constellation in the slice at x,y
///  Make a copy of the grid
///  for slice(x,y) force it to a single constellation
///  Run Gridwalker, until no more improvement, or contradiction
///  if valid, then add that constellation to the result.
///  return result
/// </summary>
function CheckSlicesStates(x,y: integer): TArray<integer>;
var
  StartStates: TArray<integer>;
  Slice: TSlice;
  i: integer;
  ValidCount: integer;
  GridClone: TGrid;
  Changes: TSliverChanges;
begin
  Slice:= MySlices[x,y];
  StartStates:= Slice.GetStates;
  ValidCount:= 0;
  SetLength(Result, Length(StartStates));
  for i:= 0 to Length(StartStates)-1 do begin
    GridClone:= MySlices.Clone;
    GridClone[x,y].ForceSingleBit(StartStates[i]);
    repeat
      Changes:= GridClone.DoASliverRun;
    until (Changes.IsInvalid) or (Changes.IsUnchanged);
    if Changes.IsValid then begin
      Result[ValidCount]:= StartStates[i];
      Inc(ValidCount);
    end;
  end; {for i}
  SetLength(Result, ValidCount);
end;

/// <summary>
///  StringGrid5 (I should rename this) contains a list of all valid constellations
///  for a previously selected slice
///  If we click on a constellation, see if this constellation is actually valid.
/// </summary>
procedure TForm2.StringGrid5DblClick(Sender: TObject);
var
  ForceState: integer;
  SG: TStringGrid;
  SourceSG: TStringGrid;
  ForceSlice: TSlice;
begin
  MySlices:= CloneSlices.Clone;
  // Get the index of the clicked item
  SG:= StringGrid5;
  SourceSG:= StringGrid2;
  try
    ForceState:= SG.Cells[SG.col, SG.row].ToInteger;
    ForceSlice:= MySlices[SourceSG.col, SourceSG.row];
    ForceSlice.ForceSingleBit(ForceState);
    MySlices[SourceSG.col, SourceSG.row]:= ForceSlice;
    // DisplaySlices(StringGrid2, StringGrid3, MySlices);
    Action_SliverSolveRoundExecute(Self);
  except
    { ignore }
  end;
end;

//Return 'X','?',' ' if pixel is ON, UNKNOWN, or OFF respectively.
function TForm2.GetPixelChar(x, y: integer; SG: TStringGrid): Char;
begin
  Result:= (SG.Cells[SG.col, SG.row] + ' ')[1];
end;

//Change a pixel (or many pixels) in the pattern bitmap.
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

//Allow us to draw a rect, changing a bunch of pixels in one go.
procedure TForm2.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  StringGrid1DblClick(Sender);
end;

//Activate the slice under to cursor and display its allowed
//constellation in the list of valid constellations
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
    SGDead.Cells[2-(i mod 3), i div 3]:= CurrentSlice.CountDead(i).ToString;
    SGAlive.Cells[2-(i mod 3), i div 3]:= CurrentSlice.CountAlive(i).ToString;
  end;
  States:= CurrentSlice.GetStates;
  DisplayStates(StringGrid4);
  DisplayStates(StringGrid5);
  CloneSlices:= MySlices.Clone;
end;

//Draw a visuallisation of the slices
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
  {$WARN COMBINING_SIGNED_UNSIGNED OFF}
  if (gdSelected in State) then C.Brush.Color:= C.Brush.Color - RGB(40,40,0);
  {$WARN COMBINING_SIGNED_UNSIGNED ON}
  CellText:= TStringGrid(Sender).Cells[ACol, ARow];
  if (CellText.Contains('-')) then C.Font.Color:= clRed
  else C.Font.Color:= clBlack;
  if (CellText = '0') then begin
  {$WARN COMBINING_SIGNED_UNSIGNED OFF}
    C.Brush.Color:= C.Brush.Color - RGB(80,80,80);
  {$WARN COMBINING_SIGNED_UNSIGNED ON}
  end;
  C.FillRect(Rect);
  OldAlignment:= SetTextAlign(C.Handle, TA_CENTER);
  // C.TextOut( Rect.Left+2, Rect.Top+2, CellText );
  C.TextRect(Rect, Rect.Left + (Rect.Width div 2) - 1, Rect.Top + 4, CellText);
  SetTextAlign(C.Handle, OldAlignment);
end;

//Display the number of allowable states for each of the 9 pixels in the slice.
//When the number of allowed constellations becomes low, some pixels in the
//3x3 grid that form a clice becomes forced.
//I one thought this would be useful, but gridwalker already forwards
//this knowledge perfectly.
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

//Draw the allowed constellations of the currently selected slice.
procedure TForm2.StringGrid4DrawCell(Sender: TObject; ACol, ARow: integer; Rect: TRect; State: TGridDrawState);
var
  SG: TStringGrid;
  value: integer;
  S: string;
  x, y: integer;
  C: TCanvas;
  PixelRect: TRect;
  OldColor: TColor;
  Colors: array [boolean, 0 .. 3] of TColor;
  Selected: boolean;
  MyLabel: string;
begin
  // Darker colors for selection
  Colors[false, 1]:= clNavy;
  Colors[true, 1]:= RGB(0, 0, 85);
  Colors[false, 0]:= clOlive;
  Colors[true, 0]:= RGB(85, 85, 0);
  Colors[false, 2]:= clAqua;
  Colors[true, 2]:= RGB(0, 200, 200);
  Colors[false,3]:= RGB(230,230,230); //clWhite;
  Colors[true,3]:= RGB(210,210,210);

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
    MyLabel:= {Value.ToString+' '+}Char(Ord('A')+(value div 24)) + Char(Ord('A')+(value mod 24));
    C.Brush.Color:= Colors[Selected, 3];
    C.FillRect(Rect);
    C.Brush.Color:= Colors[Selected, 2];
    C.FillRect(TRect.Create(Rect.Left, Rect.Top, Rect.Right, Rect.Top+36));
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
    //C.moveTo(Rect.Left, Rect.Top+36); C.LineTo(Rect.Right, Rect.Top+36);
    Rect:= TRect.Create(Rect.Left, Rect.Top+36, Rect.Right, Rect.Bottom);
    DrawTextEx(C.handle,PWideChar(MyLabel),-1,Rect,DT_CENTER or DT_VCENTER or DT_SINGLELINE,nil);

  end; { if PaintNeeded }
end;

{ TSlice }

//Operator overloading >
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

//generated by rolling a dice 6 times, guarenteed to be random.
class constructor TSlice.InitRandomSeed;
begin
  TSlice.RandomSeed:= 546546;
end;

//we no longer test bits in a slice one by one.
//function TSlice.IsBitSet(Bit: integer): boolean;
////begin
////  System.Assert((Bit >= 0) and (Bit <= 511));
////  Result:= (Self.Data4[Bit shr 5] and (1 shl (31 and Bit)) <> 0);
////end;
//asm
//  //RCX = @self
//  //edx = bit index
//  xor eax,eax
//  bt [rcx],edx
//  setc al
//end;

function TSlice.IsZero: boolean;
asm
//We should really get rid of all SSE code and only use AVX.
  //test to see if every bit in the slice is zero
  //RCX = self
//  pcmpeqq xmm0,xmm0   //FFFFFFF...
//  ptest xmm0,[rcx]
//  jnz @Done
//  ptest xmm0,[rcx+16]
//  jnz @Done
//  ptest xmm0,[rcx+32]
//  jnz @Done
//  ptest xmm0,[rcx+48]
//@Done:
//  setz al
//  rep ret

db  $c4,$e2,$79,$29,$c0           //vpcmpeqq xmm0,xmm0,xmm0   //xmm0=0
db  $c4,$e2,$79,$17,$01           //vptest xmm0,[rcx]         //[rcx]=0?
db  $75,$16                       //jne     <Done>            //no, -> done
db  $c4,$e2,$79,$17,$41,$10       //vptest xmm0,[rcx+$10]
db  $75,$0e                       //jne     <Done>
db  $c4,$e2,$79,$17,$41,$20       //vptest xmm0,[rcx+$20]
db  $75,$06                       //jne     <Done>
db  $c4,$e2,$79,$17,$41,$30       //vptest xmm0,[rcx+$30]
@Done:
db  $0f,$94,$c0                   //sete   al
db  $f3,$c3                       //repz ret                  //rep ret for AMD
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

//Get the next bit set, starting from bit(previous+1)
//Mask off the bits already processed
//and from that point onward look for the next set bit.
function TSlice.NextSetBit(previous: integer): integer;
asm
  // RCX: self
  // EDX: previous
  // Return result in EAX (0..511 is bit position of next set bit) 512=not found
  // r10 = copy of self
  // r8 = qword index
  // r9 = population count
  mov eax,512           // assume failure
  cmp edx,511           // are we all done?
  jge @done             // yes, done
  mov r10,rcx           // r10=self, We need cl for shifting
  inc edx               // we're looking for the next bit
  mov ecx,edx           // shift out the bits already processed
  and ecx,63            // only take 0..63 into account so we don't upset the offset calc
  // Get the relevant int64
  shr edx,6             // Get the revelant 64 bit section (div 64)
  lea r8,[rdx*8]        // but note that that 64 bits = 8 bytes (rounded down).
@repeat:
  mov r9,[r10+r8]       // get the next section to investigate
  shr r9,cl             // shift out the bits we've already looked at
  mov eax,64            // BSF DEST, SOURCE: if SOURCE=0 then DEST will be unchanged.
  bsf rax,r9            // get the next set bit, CF=0 if we found it
  lea eax,[eax+ecx]     // add the bitcount shifted out back in
  // if eax=64 then all bits set in our section are clear.
  // If r8d = 7 then we are done, if not we need to look further.
  lea rax,[rax+r8*8]    // Add the section offset back in.
  jnz @done             // We found a bit
  // Oops, ZF=1, meaning the section is empty, investigate the next section.
  xor ecx,ecx           // reset at the start of the next session
  cmp r8d,64-8          // Did we investigate all sections? ZF=1 if true
  lea r8,[r8+8]         // Let's prepare for a new round
  jne @repeat
@done:
  //mov edx,512
  //cmp eax,512
  //cmovnc eax,edx
  rep ret
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

//used for printing to a memo for debug purposes
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

//print to string for debug purposes
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

//did we not code this somewhere else already?
class function TSlice.Random: TSlice;
const
  a: uint64 = 2862933555777941757;
  b: uint64 = 3037000493;
var
  i: integer;
begin
  //x[n]:= a*x[n-1]+b
  for i:= 0 to 7 do begin
    TSlice.RandomSeed:= (a * TSlice.RandomSeed) + b;
    Result.Data8[i]:= TSlice.RandomSeed;
  end;
end;

//Return the allowed constellations as an array of int.
function TSlice.GetStates: TArray<integer>;
var
  i, a: integer;
  Count: integer;
begin
  Count:= Self.PopCount;
  SetLength(Result, Count);
  a:= -1;
  for i:= 0 to Count -1 do begin
    a:= Self.NextSetBit(a);
    if ((a and (1 shl 4)) <> 0) then Result[i]:= -a
    else Result[i]:= a;
  end;
end;

//deprecated
//Tensor transpose of a slice, we no longer do transposing
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

function TSlice.ReverseLeftRight: TSlice;
begin
  //The are 512 bits in a slice, thus every bit has a 9 bit index
  //Of that index, take bits 0 and 2 and swap them, same for bits 3 and 5 as well as 6 and 8
  Result:= TSlice.FullyEmpty;
  var Index:= -1;
  while Index < 512 do begin
    Index:= Self.NextSetBit(Index);
    var NewIndex:= Index;
    //Now pick the index apart and get the transformed index
    if (NewIndex and (1+4)) in [1,4] then NewIndex:= NewIndex xor (1+4);
    if (NewIndex and (8+32)) in [8,32] then NewIndex:= NewIndex xor (8+32);
    var a:= (NewIndex and (64+256));
    if (a = 64) or (a = 256) then NewIndex:= NewIndex xor (64+256);
    Result.SetBit(NewIndex);
  end;
end;

//Set a bit ON or OFF, note that the index may be negative
procedure TSlice.SetBit(Index: integer; value: boolean = true);
//var
//  One: uint64;
//  Mask: uint64;
//  element: integer;
//begin
//  One:= 1;
//  index:= ABS(index);
//  Mask:= (One shl (index and 63));
//  element:= index div 64;
//  if (value = false) then Data8[element]:= Data8[element] and not Mask
//  else Data8[element]:= Data8[element] or Mask;
//end;
asm
  //RCX = @self
  //edx = index
  //r8b = value
//abs(x), see wikipedia
  mov eax,edx
  cdq
  xor eax,edx
  sub eax,edx
//BitTest
  test r8b,r8b
  jz @clear
@Set:
  bts [rcx],eax
  rep ret
@clear:
  btc [rcx],eax
  rep ret
end;

//Clear the slice and force a single allowed constellation
procedure TSlice.ForceSingleBit(Index: integer);
//RCX = @self
//edx = index
asm
  jmp TSlice_ForceSingleBit
  //TSlice_ForceSingleBit(@self, index);
//  pxor xmm0,xmm0
//  movdqu [rcx],xmm0
//  movdqu [rcx+16],xmm0
//  movdqu [rcx+32],xmm0
//  movdqu [rcx+48],xmm0
//  bts [rcx],edx
//  FillChar(Self, SizeOf(TSlice), 0);
//  Self.SetBit(index);
end;


//Try to see if we can extract a known pixel from a slice.
//If the popcount is low, any ofn the 9 pixels might just be in OFF or ON
//in every constellation.
//We use this to piece together a bigger bitmap to be used in Speculative evolution.
function TSlice.GetBitmap(out KnownBitmap, UnknownBitmap: UInt64): TSliverChanges;
var
  OnBitmap: integer;
  OffBitmap: integer;
  i: integer;
  Index: integer;
begin
  OnBitmap:= 0;
  OffBitmap:= -1;
  Index:= NextSetBit(-1);

  //We follow a multi-pronged strategy.
  //After a simple sanity check to see if the slice is valid
  //We check the popcount to see how much work it is to enumerate all constellations.
  //If its cheap we do that, and split the accumulation thereof into Known and unknown bitmaps

  //If more than half of the positions are filled, then no single pixel can be known

  //Else we do a check against a bitmask per pixel.
  //There are 9 pixels, so 9 checks.
  case Self.PopCount of
    0: Exit(TSliverChanges.Invalid);    //Do not stress about the fact that our out parameters are undefined.
    1..16: begin
      while (Index < 512) do begin
        OnBitmap:= OnBitmap or Index;       //OR only adds pixels
        OffBitmap:= OffBitmap and Index;    //AND only subtracts pixels
        if (OnBitmap xor OffBitmap) = 511 then break; //stop sampling if all pixels are unknown
      end; {while}
      KnownBitmap:= (OnBitmap and OffBitmap);
      UnknownBitmap:= (OnBitmap xor OffBitmap);
    end; {1..16:}
    257..512: begin
      UnknownBitmap:= 511;
      KnownBitmap:= 0;
    end
    else begin
      for i:= 0 to 8 do begin
        if ((Self and BitMask[i]) = Self) then KnownBitmap:= KnownBitmap or (1 shl i)
        else UnknownBitmap:= UnknownBitmap or (1 shl i);
      end;
    end;
  end; {case}
  KnownBitmap:= (KnownBitmap and 7) xor ((KnownBitmap and $38) shl 13) xor ((KnownBitmap and $1C0) shl (10+16));
  UnknownBitmap:= (UnknownBitmap and 7) xor ((UnknownBitmap and $38) shl 13) xor ((UnknownBitmap and $1C0) shl (10+16))
end;

//result := a AND b
class operator TSlice.BitwiseAnd(const a, b: TSlice): TSlice;
//var
//  i: integer;
//begin
//  for i:= 0 to 7 do begin
//    Result.Data8[i]:= a.Data8[i] and b.Data8[i];
//  end;
asm
  //RCX = @Result
  //RDX = @A
  //R8 = @B
//  movdqu xmm0,[rdx]
//  movdqu xmm1,[r8]
//  pand xmm0,xmm1
//  movdqu [rcx],xmm0
//  movdqu xmm2,[rdx+16]
//  movdqu xmm3,[r8+16]
//  pand xmm2,xmm3
//  movdqu [rcx+16],xmm2
//  movdqu xmm4,[rdx+32]
//  movdqu xmm5,[r8+32]
//  pand xmm4,xmm5
//  movdqu [rcx+32],xmm4
//  movdqu xmm6,[rdx+48]
//  movdqu xmm7,[r8+48]
//  pand xmm6,xmm7
//  movdqu [rcx+48],xmm6
db  $c5,$fa,$6f,$02              //vmovdqu xmm0,[rdx]
db  $c4,$c1,$79,$db,$00          //vpand  xmm0,xmm0,[r8]
db  $c5,$fa,$7f,$01              //vmovdqu [rcx],xmm0
db  $c5,$fa,$6f,$4a,$10          //vmovdqu xmm1,[rdx+$10]
db  $c4,$c1,$71,$db,$48,$10      //vpand  xmm1,xmm1,[r8+$10]
db  $c5,$fa,$7f,$49,$10          //vmovdqu [rcx+$10],xmm1
db  $c5,$fa,$6f,$52,$20          //vmovdqu xmm2,[rdx+$20]
db  $c4,$c1,$69,$db,$50,$20      //vpand  xmm2,xmm2,[r8+$20]
db  $c5,$fa,$7f,$51,$20          //vmovdqu [rcx+$20],xmm2
db  $c5,$fa,$6f,$5a,$30          //vmovdqu xmm3,[rdx+$30]
db  $c4,$c1,$61,$db,$58,$30      //vpand  xmm3,xmm3,[r8+$30]
db  $c5,$fa,$7f,$59,$30          //vmovdqu [rcx+$30],xmm3
end;


class operator TSlice.LogicalNot(const a: TSlice): TSlice;
//var
//  i: integer;
//begin
//  for i:= 0 to high(a.Data8) do begin
//    Result.Data8[i]:= not(a.Data8[i]);
//  end;
asm
  //RCX = @Result
  //RDX = @A
//  movdqu xmm0,[rdx]
//  pcmpeqq xmm15,xmm15     //invert mask
//  pxor xmm0,xmm15
//  movdqu [rcx],xmm0
//  movdqu xmm1,[rdx+16]
//  pxor xmm1,xmm15
//  movdqu [rcx+16],xmm1
//  movdqu xmm2,[rdx+32]
//  pxor xmm2,xmm15
//  movdqu [rcx+32],xmm2
//  movdqu xmm3,[rdx+48]
//  pxor xmm3,xmm15
//  movdqu [rcx+48],xmm3
db  $c4,$42,$01,$29,$ff          //vpcmpeqq xmm15,xmm15,xmm15
db  $c5,$81,$ef,$02              //vpxor  xmm0,xmm15,[rdx]
db  $c5,$fa,$7f,$01              //vmovdqu [rcx],xmm0
db  $c5,$81,$ef,$4a,$10          //vpxor  xmm1,xmm15,[rdx+$10]
db  $c5,$fa,$7f,$49,$10          //vmovdqu [rcx+$10],xmm1
db  $c5,$81,$ef,$52,$20          //vpxor  xmm2,xmm15,[rdx+$20]
db  $c5,$fa,$7f,$51,$20          //vmovdqu [rcx+$20],xmm2
db  $c5,$81,$ef,$5a,$30          //vpxor  xmm3,xmm15,[rdx+$30]
db  $c5,$fa,$7f,$59,$30          //vmovdqu [rcx+$30],xmm3
end;


class operator TSlice.BitwiseOr(const a, b: TSlice): TSlice;
//var
//  i: integer;
//begin
//  for i:= 0 to 7 do begin
//    Result.Data8[i]:= a.Data8[i] or b.Data8[i];
//  end;
asm
//  movdqu xmm0,[rdx]
//  movdqu xmm1,[r8]
//  por xmm0,xmm1
//  movdqu [rcx],xmm0
//  movdqu xmm2,[rdx+16]
//  movdqu xmm3,[r8+16]
//  por xmm2,xmm3
//  movdqu [rcx+16],xmm2
//  movdqu xmm4,[rdx+32]
//  movdqu xmm5,[r8+32]
//  por xmm4,xmm5
//  movdqu [rcx+32],xmm4
//  movdqu xmm6,[rdx+48]
//  movdqu xmm7,[r8+48]
//  por xmm6,xmm7
//  movdqu [rcx+48],xmm6
db  $c5,$fa,$6f,$02              //vmovdqu xmm0,[rdx]
db  $c4,$c1,$79,$eb,$00          //vpor   xmm0,xmm0,[r8]
db  $c5,$fa,$7f,$01              //vmovdqu [rcx],xmm0
db  $c5,$fa,$6f,$4a,$10          //vmovdqu xmm1,[rdx+$10]
db  $c4,$c1,$71,$eb,$48,$10      //vpor   xmm1,xmm1,[r8+$10]
db  $c5,$fa,$7f,$49,$10          //vmovdqu [rcx+$10],xmm1
db  $c5,$fa,$6f,$52,$20          //vmovdqu xmm2,[rdx+$20]
db  $c4,$c1,$69,$eb,$50,$20      //vpor   xmm2,xmm2,[r8+$20]
db  $c5,$fa,$7f,$51,$20          //vmovdqu [rcx+$20],xmm2
db  $c5,$fa,$6f,$5a,$30          //vmovdqu xmm3,[rdx+$30]
db  $c4,$c1,$61,$eb,$58,$30      //vpor   xmm3,xmm3,[r8+$30]
db  $c5,$fa,$7f,$59,$30          //vmovdqu [rcx+$30],xmm3
end;

class operator TSlice.BitwiseXor(const a, b: TSlice): TSlice;
//var
//  i: integer;
//begin
//  for i:= 0 to 7 do begin
//    Result.Data8[i]:= a.Data8[i] xor b.Data8[i];
//  end;
asm
//  movdqu xmm0,[rdx]
//  movdqu xmm1,[r8]
//  pxor xmm0,xmm1
//  movdqu [rcx],xmm0
//  movdqu xmm2,[rdx+16]
//  movdqu xmm3,[r8+16]
//  pxor xmm2,xmm3
//  movdqu [rcx+16],xmm2
//  movdqu xmm4,[rdx+32]
//  movdqu xmm5,[r8+32]
//  pxor xmm4,xmm5
//  movdqu [rcx+32],xmm4
//  movdqu xmm6,[rdx+48]
//  movdqu xmm7,[r8+48]
//  pxor xmm6,xmm7
//  movdqu [rcx+48],xmm6
db  $c5,$fa,$6f,$02              //vmovdqu xmm0,[rdx]
db  $c4,$c1,$79,$ef,$00          //vpxor  xmm0,xmm0,[r8]
db  $c5,$fa,$7f,$01              //vmovdqu [rcx],xmm0
db  $c5,$fa,$6f,$4a,$10          //vmovdqu xmm1,[rdx+0x10]
db  $c4,$c1,$71,$ef,$48,$10      //vpxor  xmm1,xmm1,[r8+0x10]
db  $c5,$fa,$7f,$49,$10          //vmovdqu [rcx+0x10],xmm1
db  $c5,$fa,$6f,$52,$20          //vmovdqu xmm2,[rdx+0x20]
db  $c4,$c1,$69,$ef,$50,$20      //vpxor  xmm2,xmm2,[r8+0x20]
db  $c5,$fa,$7f,$51,$20          //vmovdqu [rcx+0x20],xmm2
db  $c5,$fa,$6f,$5a,$30          //vmovdqu xmm3,[rdx+0x30]
db  $c4,$c1,$61,$ef,$58,$30      //vpxor  xmm3,xmm3,[r8+0x30]
db  $c5,$fa,$7f,$59,$30          //vmovdqu [rcx+0x30],xmm3
end;

procedure TSlice.Clear;
begin
  FillChar(Self, SizeOf(TSlice), #0);
end;

//Count the number of constellations that predict a given pixel to be ON
function TSlice.CountAlive(Pixel: integer): integer;
begin
  System.Assert((Pixel >= 0) and (Pixel <= 8));
  Result:= (Self and not(OffMask[Pixel])).PopCount;
end;

//Count the number of constellations that predict a given pixel to be OFF
function TSlice.CountDead(Pixel: integer): integer;
begin
  System.Assert((Pixel >= 0) and (Pixel <= 8));
  Result:= (Self and (OffMask[Pixel])).PopCount;
end;

class function TSlice.FullyEmpty: TSlice;
begin
  FillChar(Result, SizeOf(Result), #0);
end;

class function TSlice.FullyUnknown: TSlice;
begin
  FillChar(Result, SizeOf(Result), $FF);
end;


class operator TSlice.Equal(const a, b: TSlice): boolean;
//var
//  i: integer;
//begin
//  for i:= 0 to high(a.Data8) do begin
//    if a.Data8[i] <> b.Data8[i] then Exit(false);
//  end;
//  Result:= true;
//end;
asm
  //RCX = @A
  //RDX = @B
  //al = result
//  pcmpeqq xmm15,xmm15
//  movdqu xmm0,[rcx]
//  movdqu xmm1,[rdx]
//  pxor xmm0,xmm1          //xmm0 = 0 if equal
//  ptest xmm0,xmm15        //CF = diff(0,1) and $FFFFFFF
//  jne @done
//  movdqu xmm0,[rcx+16]
//  movdqu xmm1,[rdx+16]
//  pxor xmm0,xmm1
//  ptest xmm0,xmm15
//  jne @done
//  movdqu xmm0,[rcx+32]
//  movdqu xmm1,[rdx+32]
//  pxor xmm0,xmm1
//  ptest xmm0,xmm15
//  jne @done
//  movdqu xmm0,[rcx+48]
//  movdqu xmm1,[rdx+48]
//  pxor xmm0,xmm1
//  ptest xmm0,xmm15
//@done:
//  sete al
//  rep ret
db  $c4,$42,$01,$29,$ff        //vpcmpeqq xmm15,xmm15,xmm15  ;XMM15 =-1
db  $c5,$fa,$6f,$01            //vmovdqu xmm0,[rcx]
db  $c5,$f9,$ef,$02            //vpxor  xmm0,xmm0,[rdx]
db  $c4,$c2,$79,$17,$c7        //vptest xmm0,xmm15
db  $75,$31                    //jne    45 <done>
db  $c5,$fa,$6f,$41,$10        //vmovdqu xmm0,[rcx+$10]
db  $c5,$f9,$ef,$42,$10        //vpxor  xmm0,xmm0,[rdx+$10]
db  $c4,$c2,$79,$17,$c7        //vptest xmm0,xmm15
db  $75,$20                    //jne    45 <done>
db  $c5,$fa,$6f,$41,$20        //vmovdqu xmm0,[rcx+$20]
db  $c5,$f9,$ef,$42,$20        //vpxor  xmm0,xmm0,[rdx+$20]
db  $c4,$c2,$79,$17,$c7        //vptest xmm0,xmm15
db  $75,$0f                    //jne    45 <done>
db  $c5,$fa,$6f,$41,$30        //vmovdqu xmm0,[rcx+$30]
db  $c5,$f9,$ef,$42,$30        //vpxor  xmm0,xmm0,[rdx+$30]
db  $c4,$c2,$79,$17,$c7        //vptest xmm0,xmm15
@Done:
db  $0f,$94,$c0                //sete   al
db  $f3,$c3                    //repz ret
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

class operator TSliver.Implicit(const a: Uint64): TSliver;
begin
  Result.Data8:= a;
end;

function TSliver.IsValid: boolean;
begin
  Result:= (Data8 <> 0);
end;

class operator TSliver.NotEqual(const a, b: TSliver): boolean;
begin
  Result:= a.Data8 <> b.Data8;
end;

function TSliver.PopCount: integer;
//RCX = Self
asm
  popcnt rax,rcx
end;

class function TSliverHelper.NSSlow(const North, South: TSlice; out Changed: TSliverChanges): TSliver;
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
  Changed:= TSliverChanges.Changes(N,S, Result);
  //Changed:= (N <> S); // or 1 if changes
  //Changed:= Changed and Result; // or 2 if invalid
end;

class function TSliverHelper.NS(const North, South: TSlice; out Changed: TSliverChanges): TSliver;
  //RCX =North: PSlice
  //RDX =South: PSlice
  //R8 = @Changed: PSliverChanges ((scUnchanged=0, scChanged=1, scInvalid=3));
  //RAX = Result: TSliver (as Int64)
//begin
//  Result:= AVXGENERATE_TSLIVERHELPER_NS(North, South, Changed);
//end;
asm
  JMP AVXGENERATE_TSLIVERHELPER_NS
end;
////  // First take the north slice and remove pixels 0,1,2
////  for i:= 0 to 7 do begin
////    // Remove pixel 012 by collapsing every byte into a bit.
////    N.bytes[i]:= Remove012(North.Data8[i]);
////  end;
//        pxor xmm0,xmm0        //xmm0 = 0
//@CollapsePart1:
//        movdqu xmm1,[rcx]
//        pcmpeqb xmm1,xmm0     //xmm1 = $FF if 0, $00 if not zero, per byte
//        pmovmskb r11d,xmm1     //ax = 1 bit per byte, 1 if 0, 0 if 1.
//@CollapsePart2:
//        movdqu xmm1,[rcx+16]
//        pcmpeqb xmm1,xmm0     //xmm1 = $FF if 0, $00 if not zero, per byte
//        pmovmskb eax,xmm1     //ax = 1 bit per byte, 1 if 0, 0 if 1.
//        shl eax,16
//        or r11,rax            //save part2
//@CollapsePart3:
//        movdqu xmm1,[rcx+32]
//        pcmpeqb xmm1,xmm0     //xmm1 = $FF if 0, $00 if not zero, per byte
//        pmovmskb eax,xmm1     //ax = 1 bit per byte, 1 if 0, 0 if 1.
//        shl rax,32
//        or r11,rax
//@CollapsePart4:
//        movdqu xmm1,[rcx+48]
//        pcmpeqb xmm1,xmm0     //xmm1 = $FF if 0, $00 if not zero, per byte
//        pmovmskb eax,xmm1     //ax = 1 bit per byte, 1 if 0, 0 if 1.
//        shl rax,48
//        or r11,rax
//@FinalizeCollapseBytesIntoBits:
//        not r11               //Invert, so that byte=0 -> bit=0, byte<>0-> bit=1
////  // Next remove pixels 678 from South
////  // Do this by folding 8 slivers
////  S:= South.Sliver[0];
////  for i:= 1 to 7 do begin
////    S:= S or South.Sliver[i];
////  end;
//@FoldS:
//        movdqu xmm0,[rdx]
//        movdqu xmm1,[rdx+16]
//        movdqu xmm2,[rdx+32]
//        movdqu xmm3,[rdx+48]
//        por xmm0,xmm1
//        por xmm2,xmm3
//        por xmm0,xmm2
//        movhlps xmm1,xmm0
//        por xmm0,xmm1
//        movq rax,xmm0
//        mov rcx,rax         //keep a copy for the status update
////  Result:= N and S;
//        xor edx,edx           //assume stats = scUnchanged
//        and rax,r11           //And the two slivers
//@StatusUpdates:
////  Changed:= Changed and Result; // or 2 if invalid
////r8 = @Changed
//        setz dl               //If the result = 0 (invalid, then mark it as such).
//        shl edx,1
////  Changed:= Changed or (N <> S); // or 1 if changes
//        xor rcx,r11           //Are the two slivers different?
//        setne cl              //yes, so there will be a change
//        or dl,cl              //merge the two change flags
//        mov [r8],dl           //save the status
//end;

class function TSliverHelper.EWSlow(const East, West: TSlice; out Changed: TSliverChanges): TSliver;
var
  E, W: TSliver;
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
  Result:= W and E;
  Changed:= TSliverChanges.Changes(E,W,Result);
  //Changed:= (W <> E); // or 1 if changed
  //Changed:= Changed and Result; // or 2 if invalid
end;



class function TSliverHelper.EW(const East, West: TSlice; out Changed: TSliverChanges): TSliver;
//begin
//  //AVXGENERATE_TEST;
//  Result:= AVXGENERATE_TSLIVERHELPER_EW(East, West, Changed);
//end;
asm
  jmp AVXGENERATE_TSLIVERHELPER_EW
end;
(*
const
  Mask: array[0..15] of byte = ($F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0);
  CollectBytes: array[0..15] of byte =  (0,2,4,6,8,10,12,14,0,2,4,6,8,10,12,14);
  CollectBytes2: array[0..15] of byte = (1,3,5,7,9,11,13,15,1,3,5,7,9,11,13,15);
  MortonDecode: array[0..15] of byte =   (0,1,1,1, 2,3,3,3, 2,3,3,3, 2,3,3,3);
  //RCX =North: PSlice
  //RDX =South: PSlice
  //R8 = Changed: PSliverChanges ((scUnchanged=0, scChanged=1, scInvalid=3));
  //RAX = Result: TSliver (as Int64)
  //available
  //rax, r9,r10,r11
asm
//  // West: drop pixels 2,5,8
//  // first eliminate pixel 5
//  // This reduces the size from 64 bytes = 16ints to 32 bytes = 8 ints
//  for i:= 0 to 7 do begin
//    Temp1.Data4[i]:= West.Data4[i * 2] or West.Data4[i * 2 + 1];
//  end;
//Removing pixel 5 means folding every int32.
        movdqu xmm0,[rdx]        //0123
        movdqu xmm1,[rdx+16]     //4567
        movdqu xmm2,[rdx+32]     //89AB
        movdqu xmm3,[rdx+48]     //CDEF
//Now we need to fold 0 or 1 into 0, 2 to 3 into 2
//So we need to change the order to A: 0246, B: 1357, C: 8ACE, D: 9BDF
//We need to do some shuffling.
//Lets get the first halves
        pshufd xmm0,xmm0,(0 shl 0)+(2 shl 2)+(1 shl 4)+(3 shl 6);   //0213  02--
        pshufd xmm1,xmm1,(0 shl 0)+(2 shl 2)+(1 shl 4)+(3 shl 6);   //4657  --57
        pshufd xmm2,xmm2,(0 shl 0)+(2 shl 2)+(1 shl 4)+(3 shl 6);   //8A9B  8A--
        pshufd xmm3,xmm3,(0 shl 0)+(2 shl 2)+(1 shl 4)+(3 shl 6);   //CEDF  --DF
        movdqu xmm15,[rip + mask]
        movdqa xmm14,xmm15
        psrlw xmm14,4
        movdqa xmm4,xmm0
        movlhps xmm4,xmm1        //0246
        movdqa xmm5,xmm1
        movhlps xmm5,xmm0        //1357
        movdqa xmm6,xmm2
        movlhps xmm6,xmm3        //8ACE
        movdqa xmm7,xmm3
        movhlps xmm7,xmm2        //9BDF
//Now we can OR the data
        por xmm4,xmm5            //0or1 + 2or3 + 4or5 + 6or7
        por xmm6,xmm7            //8or9 + AorB + CorD + EorF
//  // Next eliminate pixel 2.
//  // This means folding every nibble with its neighbor
//  // this reduces the size from 32 bytes into 16 bytes
//  for i:= 0 to 15 do begin
//    Temp2.bytes[i]:= TSuperSlice.LookupRemove2[Temp1.Data2[i]];
//  end;
        movdqa xmm0,xmm4       //AAAABBBBCCCCDDDDEEEEFFFF
        psrlq xmm0,4           //BBBBCCCCDDDDEEEEFFFF
        por xmm0,xmm4          //aaaa----cccc----eeee----  OR nibbles together.
        movdqa xmm1,xmm6       //KKKKLLLLMMMMNNNNOOOOPPPP
        psrlq xmm1,4           //LLLLMMMMNNNNOOOOPPPP
        por xmm1,xmm6          //kkkk----mmmm----oooo---- OR nibbles together.
        //                            --byte0|--byte1|--byte2|....
        //we now have nibbles like so AAAA----BBBB----CCCC----DDDD...
        //This needs to be translated to AAAABBBBCCCCDDDD.....
                                //       ---0---|---1---|---2---|---3---|---4---|
        pand xmm0,xmm14         //xmm4 = aaaa....cccc....eeee....gggg....iiii.... //mask off irrevant parts
        movdqa xmm4,xmm0        //xmm4 = aaaa....cccc....eeee....gggg....iiii....
        psllq xmm4,4            //xmm4 = ....cccc....eeee....gggg....iiii....kkkk
        //now we need to combine the bytes, collect byte 0,2,4,6,8,10,12,14 into 0,1,2,3,4,5,6,7
        movdqu xmm13,[rip + CollectBytes]
        movdqu xmm12,[rip + CollectBytes2]
        pshufb xmm0,xmm13       //xmm0 = aaaa....eeee....iiii....
        pshufb xmm4,xmm12       //xmm4 = ....cccc....gggg....kkkk
        por xmm0,xmm4
        //Do the same for the next 16 bytes
        pand xmm1,xmm14         //xmm4 = aaaa....cccc....eeee....gggg....iiii.... //mask off irrevant parts
        movdqa xmm4,xmm1        //xmm4 = aaaa....cccc....eeee....gggg....iiii....
        psllq xmm4,4            //xmm4 = ....cccc....eeee....gggg....iiii....kkkk
        //now we need to combine the bytes, collect byte 0,2,4,6,8,10,12,14 into 0,1,2,3,4,5,6,7
        //movdqu xmm13,[rip + CollectBytes]
        pshufb xmm1,xmm13       //xmm1 = aaaa....eeee....iiii....
        pshufb xmm4,xmm12       //xmm4 = ....cccc....gggg....kkkk
        por xmm1,xmm4
        //Mix the low part of xmm0 with the high part of xmm1
        //the low part of xmm0 contains part1
        //the low part of xmm1 contains part2
//  // Eliminate pixel 8
//  // Finally fold the two remaining slivers into one.
//  // This reduces the size from 16 bytes to 8 bytes
//  w:= Temp2.Sliver[0] or Temp2.Sliver[1];
        por xmm0,xmm1
        movq rdx,xmm0

        //push rdx            //make sure we keep sane
//Now process the East part (contained in RCX)
//  // Next create E by dropping pixels 0,3,6
//  // First remove pixel6
//  // This or's every int64 with its neighbor
//  // reducing 64 (8 int64's) to 32 (4 int64's) bytes.
//  for i:= 0 to 3 do begin
//    Temp1.Data8[i]:= East.Data8[i * 2] or East.Data8[i * 2 + 1];
//  end;
        mov rax,[rcx]
        or rax,[rcx+8]      //Data8[0 or 1]
        mov r9,[rcx+16]
        or r9,[rcx+24]      //Data8[2 or 3]
        mov r10,[rcx+32]
        or r10,[rcx+40]     //Data8[4 or 5]
        mov r11,[rcx+48]
        or r11,[rcx+56]     //Data8[6 or 7]
//  // first remove pixel3
//  // This or's every byte with its neighbor
//  // We start with 32 bytes and reduce it to 16.
//  for i:= 0 to 15 do begin
//    Temp2.bytes[i]:= Temp1.bytes[i * 2] or Temp1.bytes[i * 2 + 1]
//  end;
        movq xmm0,rax
        movq xmm1,r9
        movlhps xmm0,xmm1    //xmm0 = part1
        movdqa xmm1,xmm0     //make a copy
        movq xmm2,r10
        movq xmm3,r11
        movlhps xmm2,xmm3    //xmm1 = part2
        movdqa xmm3,xmm2     //make a copy
//Now shuffle the bytes, so that part0 contains 0,2,4,6,8,10,12,14
        pshufb xmm0,xmm13
        pshufb xmm2,xmm13
        pshufb xmm1,xmm12     //part1b contains bytes 1,3,5,7,9,...
        pshufb xmm3,xmm12
        por xmm0,xmm1         //combine bytes 0or1, 2or3, etc
        por xmm2,xmm3         //The data is now in the low parts of xmm0,xmm2
//  // Finally remove pixel0.
//  // This reduces from 16 bytes to 8
//  for i:= 0 to 7 do begin
//    E.bytes[i]:= TSuperSlice.LookupRemove0[Temp2.Data2[i]];
//  end;
        movdqu xmm11,[rip+MortonDecode]
        movdqa xmm10,xmm11    //Keep a copy
        movlhps xmm0,xmm2     //xmm0 now holds all the data
        movdqa xmm1,xmm0      //make a copy
        pand xmm0,xmm14       //Keep only the low nibbles
        pand xmm1,xmm15       //Keep only the high nibbles
        psrlw xmm1,4          //Shift into position for the lookup table
        pshufb xmm10,xmm0     //lookup the low nibbles AAAA----CCCC----EEEE -> aa------cc------ee------
        pshufb xmm11,xmm1     //Lookup the hi nibbles  BBBB----DDDD----FFFF -> bb------dd------ff------
        psllw xmm11,2         //bb------dd------ff------ -> ==bb----==dd----==ff----
        por xmm10,xmm11       //aabb----ccdd----eeff----
        //Now we need to split the even from the odd bytes and do some moving around
        movdqa xmm0,xmm10     //make a copy
        pshufb xmm0,xmm13     //receive the even bytes: aabb----eeff----iijj----
        pshufb xmm10,xmm12    //receive the odd bytes:  ccdd----gghh----kkll----
        psllw xmm10,4         //shift, so that it is in range
        por xmm0,xmm10        //combine the two: aabbccdd|eeffiijj
        movq rax,xmm0

        mov r11,rdx           //Get back West
        mov rcx,rax           //save east for status updates
//  Result:= W and E;
        xor edx,edx           //assume stats = scUnchanged
        and rax,r11           //And the two slivers
@StatusUpdates:
//  Changed:= Changed and Result; // or 4 if invalid
//r8 = @Changed
        setz dl               //If the result = 0 (invalid, then mark it as such).
        //shl edx,2
//Check if E has changed
        mov r9,rcx            //save the original
        and rcx,rax           //Does E stay the same?
        cmp rcx,r9
        setne r9b
        lea edx,[r9d+edx*4]

//Check if W has changed
        xor ecx,ecx           //break false dependency in setcc cl
        mov r9,r11            //save the original
        and r11,rax
        cmp r11,r9
        setne cl
        lea edx,[edx+ecx*2]   //Add W to the status.
        mov [r8],dl           //save the status
end;
(**)


function TSliverHelper.SlowWest: TSlice;
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

function TSliverHelper.West: TSlice;
asm
  jmp TSliverHelper_West
end;
//begin
//  result:= TSliverHelper_West(@Self);//: TSlice;
(*
const
  DoubleNibbles: array[0..15] of byte = (0,1*17,2*17,3*17,4*17,5*17,6*17,7*17,8*17,9*17,10*17,11*17,12*17,13*17,14*17,15*17);
  ShuffleMask: array[0..15] of byte = (0,8,1,9,2,10,3,11,4,12,5,13,6,14,7,15);
asm
  //RCX = @self
  //RDX = @Result
  mov r8,[rcx]
  movdqu xmm0,[rip+DoubleNibbles]
  movdqu xmm15,[rip+ShuffleMask]
  movdqa xmm1,xmm0
  mov r9,$0F0F0F0F0F0F0F0F          //extract the low nibbles
  mov r10,r8
  and r8,r9                         //r8 = even nibbles
  xor r10,r8                        //r10 = odd nibbles
  shr r10,4                         //Align the odd nibbles with the mask
  movq xmm2,r8
  movq xmm3,r10
  pshufb xmm0,xmm2                  //xmm0_low = even nibbles doubled
  pshufb xmm1,xmm3                  //xmm1_low = odd nibbles doubled
  movlhps xmm0,xmm1
  pshufb xmm0,xmm15                 //put them in the correct order.
  //Now double all the dwords into qwords
  pshufd xmm1,xmm0,(0 shl 0)+(0 shl 2)+(1 shl 4)+(1 shl 6);
  movdqu [rdx],xmm1
  pshufd xmm2,xmm0,(2 shl 0)+(2 shl 2)+(3 shl 4)+(3 shl 6);
  //Now just write out the result twice.
  movdqu [rdx+16],xmm2
  movdqu [rdx+32],xmm1
  movdqu [rdx+48],xmm2

end;
  (**)

function TSliverHelper.SlowNorth: TSlice;
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

function TSliverHelper.North: TSlice;
//const
//  SliverToSliceMask: array[0..7] of byte = ($01,$02,$04,$08,$10,$20,$40,$80);
asm
  //RCX = @Self    (a pointer to an Int64)
  //RDX = @Result  (a pointer to an array[0..63] of byte)
//  movq xmm0,[rcx]                       //Get the sliver
//  mov r9,$8040201008040201
//  movq xmm15,r9 //[rip+SliverToSliceMask] //Get the mask
//  movlhps xmm15,xmm15                   //extend it
//  mov r8,$0101010101010101              //Shuffle mask
//  movq xmm14,r8                         //00 00 00 00 00 00 00 00 01 01 01 01 01 01 01 01
//  pslldq xmm14,8                        //01 01 01 01 01 01 01 01 00 00 00 00 00 00 00 00
//  movdqa xmm1,xmm0                      //make a copy of the sliver
//  //bytes 0,1
//  pshufb xmm1,xmm14                     //copy the first two bytes across
//  pand xmm1,xmm15                       //Mask off the relevant bits
//  pcmpeqb xmm1,xmm15                    //Expand a bit into a byte
//  movdqu [rdx],xmm1
//  //bytes 2,3
//  psrldq xmm0,2                         //shift in the next two bytes
//  movdqa xmm2,xmm0
//  pshufb xmm2,xmm14                     //copy the next two bytes across
//  pand xmm2,xmm15                       //Mask off the relevant bits
//  pcmpeqb xmm2,xmm15                    //Expand a bit into a byte
//  movdqu [rdx+16],xmm2
//  //bytes 4,5
//  psrldq xmm0,2                         //shift in the next two bytes
//  movdqa xmm3,xmm0
//  pshufb xmm3,xmm14                     //copy the next two bytes across
//  pand xmm3,xmm15                       //Mask off the relevant bits
//  pcmpeqb xmm3,xmm15                    //Expand a bit into a byte
//  movdqu [rdx+32],xmm3
//  //bytes 6,7
//  psrldq xmm0,2                         //shift in the next two bytes
//  movdqa xmm4,xmm0
//  pshufb xmm4,xmm14                     //copy the final two bytes across
//  pand xmm4,xmm15                       //Mask off the relevant bits
//  pcmpeqb xmm4,xmm15                    //Expand a bit into a byte
//  //Store the data
//  movdqu [rdx+48],xmm4
db  $c5,$fa,$7e,$01              //vmovq  xmm0,QWORD PTR [rcx]
db  $49,$b9,$01,$02,$04,$08,$10,$20,$40,$80  //movabs r9,0x8040201008040201

db  $c4,$41,$f9,$6e,$f9          //vmovq  xmm15,r9
db  $c4,$41,$00,$16,$ff          //vmovlhps xmm15,xmm15,xmm15
db  $49,$b8,$01,$01,$01,$01,$01,$01,$01,$01  //movabs r8,0x101010101010101

db  $c4,$41,$f9,$6e,$f0          //vmovq  xmm14,r8
db  $c4,$c1,$09,$73,$fe,$08      //vpslldq xmm14,xmm14,0x8
db  $c5,$f9,$6f,$c8              //vmovdqa xmm1,xmm0
db  $c4,$c2,$71,$00,$ce          //vpshufb xmm1,xmm1,xmm14
db  $c4,$c1,$71,$db,$cf          //vpand  xmm1,xmm1,xmm15
db  $c4,$c1,$71,$74,$cf          //vpcmpeqb xmm1,xmm1,xmm15
db  $c5,$fa,$7f,$0a              //vmovdqu XMMWORD PTR [rdx],xmm1
db  $c5,$f9,$73,$d8,$02          //vpsrldq xmm0,xmm0,0x2
db  $c5,$f9,$6f,$d0              //vmovdqa xmm2,xmm0
db  $c4,$c2,$69,$00,$d6          //vpshufb xmm2,xmm2,xmm14
db  $c4,$c1,$69,$db,$d7          //vpand  xmm2,xmm2,xmm15
db  $c4,$c1,$69,$74,$d7          //vpcmpeqb xmm2,xmm2,xmm15
db  $c5,$fa,$7f,$52,$10          //vmovdqu XMMWORD PTR [rdx+0x10],xmm2
db  $c5,$f9,$73,$d8,$02          //vpsrldq xmm0,xmm0,0x2
db  $c5,$f9,$6f,$d8              //vmovdqa xmm3,xmm0
db  $c4,$c2,$61,$00,$de          //vpshufb xmm3,xmm3,xmm14
db  $c4,$c1,$61,$db,$df          //vpand  xmm3,xmm3,xmm15
db  $c4,$c1,$61,$74,$df          //vpcmpeqb xmm3,xmm3,xmm15
db  $c5,$fa,$7f,$5a,$20          //vmovdqu XMMWORD PTR [rdx+0x20],xmm3
db  $c5,$f9,$73,$d8,$02          //vpsrldq xmm0,xmm0,0x2
db  $c5,$f9,$6f,$e0              //vmovdqa xmm4,xmm0
db  $c4,$c2,$59,$00,$e6          //vpshufb xmm4,xmm4,xmm14
db  $c4,$c1,$59,$db,$e7          //vpand  xmm4,xmm4,xmm15
db  $c4,$c1,$59,$74,$e7          //vpcmpeqb xmm4,xmm4,xmm15
db  $c5,$fa,$7f,$62,$30          //vmovdqu XMMWORD PTR [rdx+0x30],xmm4
end;

function TSliverHelper.SlowEast: TSlice;
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
  // Finally add pixel 6, this doubles every int64.
  // Expanding the slice from 32 (= 4 int64) into 64 bytes (=8int64)
  for i:= 0 to 3 do begin
    Result.Data8[i * 2]:= Temp2.Data8[i];
    Result.Data8[i * 2 + 1]:= Temp2.Data8[i];
  end;
end;

function TSliverHelper.East: TSlice;
asm
  jmp TSliverHelper_East;
end;
(*const
  DoubleBytes: array[0..15] of byte = (0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7);
asm
  //RCX = @self
  //RDX = @Result
  //First double every bit
  movq xmm0,[rcx]
  movdqu xmm15,[rip+DoubleBytes]
  pclmulqdq xmm0,xmm0,0        //...11111 -> ....10101010
  movdqa xmm1,xmm0
  psllw xmm1,1
  por xmm0,xmm1               //The bits are now doubled.
  //Next double every byte, this is a simple shuffle
  movhlps xmm2,xmm0           //xmm0_low = low part, xmm1_low = high part
  pshufb xmm0,xmm15
  pshufb xmm2,xmm15
  //Now double every qword
  movhlps xmm1,xmm0
  movlhps xmm0,xmm0           //double xmm0_low
  movlhps xmm1,xmm1           //double xmm1_low
  movhlps xmm3,xmm2
  movlhps xmm2,xmm2           //double xmm2_low
  movlhps xmm3,xmm3           //double xmm3_low
  //write the data
  movdqu [rdx],xmm0
  movdqu [rdx+16],xmm1
  movdqu [rdx+32],xmm2
  movdqu [rdx+48],xmm3
end; (**)

function TSliverHelper.SlowSouth: TSlice;
var
  i: integer;
begin
  // Add pixels 678
  // This means copying the pattern 8 times
  for i:= 0 to 7 do begin
    Result.Data8[i]:= Self.Data8;
  end;
end;

function TSliverHelper.South: TSlice;
asm
  // Add pixels 678
  // This means copying the pattern 8 times
  //RCX = @self
  //RDX = @Result
//  movq xmm0,[rcx]
//  movlhps xmm0,xmm0
//  movdqu [rdx],xmm0
//  movdqu [rdx+16],xmm0
//  movdqu [rdx+32],xmm0
//  movdqu [rdx+48],xmm0
db  $c5,$fa,$7e,$01             //vmovq  xmm0,QWORD PTR [rcx]
db  $c5,$f8,$16,$c0             //vmovlhps xmm0,xmm0,xmm0
db  $c5,$fa,$7f,$02             //vmovdqu XMMWORD PTR [rdx],xmm0
db  $c5,$fa,$7f,$42,$10         //vmovdqu XMMWORD PTR [rdx+0x10],xmm0
db  $c5,$fa,$7f,$42,$20         //vmovdqu XMMWORD PTR [rdx+0x20],xmm0
db  $c5,$fa,$7f,$42,$30         //vmovdqu XMMWORD PTR [rdx+0x30],xmm0
end;

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

procedure TLookupTable.ReverseLeftToRight;
type
  PLookupTable = ^TLookupTable;
var
  NewIndex: TArray<integer>;
  a: array[0..4] of integer;
  S: PLookupTable;
begin
  S:= @Self;
  Parallel.For(0, Length(FData)-1).Execute(procedure(i: integer) begin
    S.FData[i]:= S.FData[i].ReverseLeftRight;
  end); {for i}
  Parallel.For(0, Length(FUnknownData)-1).Execute(procedure(i: integer) begin
    S.FUnknownData[i]:= S.FUnknownData[i].ReverseLeftRight;
  end); {for i}
  //Next work on the index.
  //The elements of the index must also be reversed left to right.
  SetLength(NewIndex, 1 shl 25);
  for var i := 0 to ((1 shl 25)-1) do begin
    var b:= 0;
    for var j:= 0 to 4 do begin
      a[j]:= (i shr (j*5)) and 31;
      if (a[j] and (1+16)) in [1,16] then a[j]:= a[j] xor (1+16);
      if (a[j] and (2+8)) in [2,8] then a[j]:= a[j] xor (2+8);
      b:= b or (a[j] shl(j*5));
    end;
    NewIndex[b]:= FIndex[i];
  end; {for i}
  var TestIndex: TArray<integer>;
  SetLength(TestIndex, 1 shl 25);
  for var i := 0 to ((1 shl 25)-1) do begin
    var b:= 0;
    for var j:= 0 to 4 do begin
      a[j]:= (i shr (j*5)) and 31;
      if (a[j] and (1+16)) in [1,16] then a[j]:= a[j] xor (1+16);
      if (a[j] and (2+8)) in [2,8] then a[j]:= a[j] xor (2+8);
      b:= b or (a[j] shl(j*5));
    end;
    TestIndex[b]:= NewIndex[i];
  end; {for i}
  for var i := 0 to ((1 shl 25)-1) do begin
    if TestIndex[i] <> FIndex[i] then begin
      Assert(TestIndex[i] = FIndex[i], 'element '+i.ToString+':soll='+FIndex[i].ToString+',ist='+TestIndex[i].ToString);
    end;
  end;
  FIndex:= NewIndex;
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

{ TSliverChanges }

class operator TSliverChanges.BitwiseOr(const a, b: TSliverChanges): TSliverChanges;
begin
  Result.Raw:= a.Raw or b.Raw;
end;

class operator TSliverChanges.Add(a: integer; const b: TSliverChanges): integer;
begin
  Result:= a + integer(b.KeepGoing);
end;

class operator TSliverChanges.BitwiseAnd(const a: TSliverChanges; const b: TSliver): TSliverChanges;
begin
  Result:= a;
  if (b.Data8 = 0) then Include(Result.Data, scInvalid);
end;

class function TSliverChanges.Changed: TSliverChanges;
begin
  Result.Data:= [scNEChanged,scSWChanged];
end;

class function TSliverChanges.Changes(const NE, SW, Sliver: TSliver): TSliverChanges;
begin
  //Do not optimize an early out Result:=7 here, because it will not match
  //The behaviour in the optimized code.
  //Sliver being zero is the rare condition anyway and this code is only called
  //in the slow versions of the code.
  if Sliver = 0 then Result.Raw:= (1 shl Ord(scInvalid))
  else Result.Data:= [];
  if (Sliver and NE) <> NE then Include(Result.Data, scNEChanged);
  if (Sliver and SW) <> SW then Include(Result.Data, scSWChanged);
end;

function TSliverChanges.EastChanged: boolean;
begin
  Result:= (scNEChanged in Self.Data);
end;

class function TSliverChanges.Invalid: TSliverChanges;
begin
  Result.Data:= [scNEChanged,scSWChanged,scInvalid];
end;

function TSliverChanges.IsValid: boolean;
begin
  Result:= not(scInvalid in Self.Data);
end;

function TSliverChanges.KeepGoing: boolean;
begin
  Result:= ((Self.Data * [scNEChanged,scSWChanged,scSkipped]) <> []);
end;

function TSliverChanges.IsChanged: boolean;
begin
  Result:= ((Self.Data * [scInValid, scNEChanged, scSWChanged]) <> []);
end;

function TSliverChanges.IsInvalid: boolean;
begin
  Result:= (scInvalid in Self.Data);
end;

function TSliverChanges.IsSkipped: boolean;
begin
  Result:= (scSkipped in Self.Data);
end;

function TSliverChanges.IsUnchanged: boolean;
begin
  Result:= (Self.Data = []);
end;


//class operator TSliverChanges.LogicalOr(const a: TSliverChanges; const b: boolean): TSliverChanges;
//begin
//  Result.AsByte:= a.AsByte or byte(b <> false);
//end;

class function TSliverChanges.UnChanged: TSliverChanges;
begin
  Result.Data:= [];
end;

function TSliverChanges.NorthChanged: boolean;
begin
  Result:= (scNEChanged in Self.Data);
end;

function TSliverChanges.WestChanged: boolean;
begin
  Result:= (scSWChanged in Self.Data);
end;

class operator TSliverChanges.Equal(a, b: TSliverChanges): boolean;
begin
  Result:= (a.Raw = b.Raw);
end;

class operator TSliverChanges.NotEqual(a, b: TSliverChanges): boolean;
begin
  Result:= (a.Raw <> b.Raw);
end;

class function TSliverChanges.Skipped: TSliverChanges;
begin
  Result.Data:= [scSkipped];
end;

function TSliverChanges.SouthChanged: boolean;
begin
  Result:= (scSWChanged in Self.Data);
end;

{ TMaskedBits }

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

{ TGrid }

function TGrid.AsBitmap(StartX, StartY: integer; out Spillage: TRect): TGridBitmap;
var
  x,y: integer;
  MaxX, MaxY: integer;
  KnownFragment, UnknownFragment: Uint64;
  FragmentStatus: TSliverChanges;
  OutOfBounds: TOutOfBounds;
begin
  MaxX:= Min(StartX + (16-1), (FSizeX-1));
  MaxY:= Min(StartX + (4+1), (FSizeY-1));
  //A bitmap is a row of 16 pixels numbered like so FEDCBA9876543210
  //The least significant bytes are on top and the rows gets more significant going down.
  //Every pixel is one bit.
  //TSlice.GetBitmap returns a bitmap that looks like this:
  //-------------210
  //-------------543
  //-------------876
  //----------------
  Y:= StartY;
  X:= StartX;
  Spillage:= Rect(0,0,0,0);   //Assume no spillage
  while Y <= MaxY do begin
    while X <= MaxX do begin
      FragmentStatus:= Item[x,y].GetBitmap(KnownFragment, UnknownFragment);
      if (FragmentStatus.IsInvalid) then Exit(TGridBitmap.Empty); //It is pointless to calculate
      OutOfBounds:= Result.PasteFragment(Point(x-StartX, y-Starty), KnownFragment, UnknownFragment);
      Spillage:= Spillage + OutOfBounds;
      //Every fragment covers a 3x3 area, therefor we can step through the bitmap
      //using big steps.
      {TODO -oJB -cTGrid.AsBitmap : See if stepsize needs to be 2 pixels, because a middle slice is completely defined by its direct neighbors}
      Inc(x,3);
    end; {while x}
    Inc(y,3);
  end; {while y}
end;

class operator TGrid.BitwiseOr(const a, b: TGrid): TGrid;
var
  i: integer;
begin
  Assert((a.FSizeX = b.FSizeX) and (a.FSizeY = b.FSizeY));
  Result:= TGrid.Create(a);
  if (a.IsInvalid) or (b.IsInvalid) then begin
    Result.FIsValid:= false;
    exit;
  end;
  for i:= 0 to (a.FSizeX*a.FSizeY)-1 do begin
    Result[i]:= a[i] or b[i];
  end; {for i}
end;

constructor TGrid.Create(SizeX, SizeY: integer);
begin
  Assert((SizeX * SizeY) >= 1);
  FSizeX:= SizeX;
  FSizeY:= SizeY;
  SetLength(FData, SizeX * SizeY);
  {TODO -oJB -cTGrid.Create : Remove the zero fill}
  //FillChar(FData[0], SizeOf(TSlice) * SizeX * SizeY, #0);
  FActive:= TActiveSlice.Create(SizeX, SizeY);

  FIsValid:= true;
end;

procedure TGrid.Clear;
begin
  FillChar(FData[0], Length(FData) * SizeOf(TSlice), #0);
end;

function TGrid.Clone: TGrid;
var
  Count: integer;
begin
  Result:= TGrid.Create(Self);
  Count:= FSizeX * FSizeY;
  if (Count = 0) then exit;
  Move(Self.FData[0], Result.FData[0], Count * SizeOf(TSlice));
  Result.FActive:= Self.FActive;
end;

procedure TGrid.Overwrite(var GridToBeOverwritten: TGrid);
begin
  var Count:= FSizeX * FSizeY;
  Move(Self.FData[0], GridToBeOverwritten.FData[0], Count * SizeOf(TSlice));
  GridToBeOverwritten.FActive:= Self.FActive;
  //System.Move(Self.FData[0], GridToBeOverwritten.FData[0], Count * SizeOf(TSlice));
end;

constructor TGrid.Create(const Template: TGrid);
begin
  Self:= TGrid.Create(Template.SizeX, Template.SizeY);
end;

function TGrid.GetEnumerator: TGridEnumerator;
begin
  Result:= TGridEnumerator.Create(@Self);
end;

function TGrid.GetFirstMinimalSlice(var PopCount: integer): TPoint;
begin
  Result:= GetNextMinimalSlice(point(-1,0), PopCount);
end;

function TGrid.GetNextMinimalSlice(const Previous: TPoint; var PopCount: integer): TPoint;
var
  StartX: integer;
  MinVal: integer;
  MinPoint: TPoint;
  pc: integer;
  p: TPoint;
  x,y: integer;
begin
  Assert(Previous.x >= -1);
  Assert(Previous.x < FSizeX);
  Assert(Previous.y >= 0);
  Assert(Previous.y < FSizeY);
  MinVal:= 512;
  pc:= 512;
  MinPoint:= Previous;
  StartX:= Previous.X;
  for y:= Previous.y to FSizeY-1 do begin
    for x:= StartX+1 to FSizeX-1 do begin
      p:= point(x,y);
      pc:= Item[p].Popcount;
      if (pc < MinVal) then begin
        MinVal:= pc;
        MinPoint:= p;
      end;
    end; {for x}
    StartX:= 0;
  end; {for y}
  PopCount:= pc;
  Result:= MinPoint;
end;

function TGrid.GetSlice(x, y: integer): TSlice;
begin
  Result:= GetSlice(point(x,y));
end;

function TGrid.GetSlice(const Coordinate: TPoint): TSlice;
begin
  Result:= FData[Coordinate.Index(FSizeY)];
end;

function TGrid.IsValid: boolean;
begin
  Result:= FIsValid;
end;

function TGrid.IsInValid: boolean;
begin
  Result:= not(FIsValid);
end;

function TGrid.Join(const Grids: TArray<TGrid>): TGrid;
var
  i: integer;
begin
  Assert(Length(Grids) <> 0);
  Result:= Grids[0];
  for i:= 1 to Length(Grids) -1 do begin
    Result:= Result or Grids[i];
  end;
end;

function TGrid.PopCountSum: integer;
var
  i: integer;
begin;
  Result:= 0;
  for i:= 0 to Length(FData)-1 do begin
    Inc(Result, FData[i].PopCount);
  end;
end;

procedure TGrid.SetSlice(const Coordinate: TPoint; const Value: TSlice);
begin;
  FData[Coordinate.Index(FSizeY)]:= Value;
end;

procedure TGrid.SetSlice(x, y: integer; const Value: TSlice);
begin
  SetSlice(point(x,y), Value);
end;

procedure TGrid.SetSlice(i: integer; const Value: TSlice);
begin
  Assert(i < Length(FData));
  FData[i]:= Value;
end;

function TGrid.GridSolve: TSliverChanges;
begin
  Result:= GridSolve(Rect(0,0,FSizeX-1, FSizeY-1));
end;

function TGrid.GridSolve(const Bounds: TRect): TSliverChanges;
label
  Done;
begin
  repeat
    for var Index in FActive do begin
      var x:= Index mod FSizeX;
      var y:= Index div FSizeX;
      Result:= SliverSolve(x, y, Bounds);
      if (Result.IsInvalid) then goto Done;
    end; { for Index }
    if (FActive.ActiveCount = 0) then goto Done;
    for var Index in FActive.Reverse do begin
      var x:= Index mod FSizeX;
      var y:= Index div FSizeX;
      Result:= SliverSolveReverse(x, y, Bounds);
      if (Result.IsInvalid) then goto Done;
    end; { for Index }
  until (FActive.ActiveCount = 0);
Done:
  Self.FIsValid:= Result.IsValid;
end;

function TGrid.GridSolveOld(const Bounds: TRect): TSliverChanges;
var
  x, y: integer;
  ChangeCount: integer;
label
  Done;
begin
  repeat
    ChangeCount:= 0;
    for y:= Bounds.Top to Bounds.Bottom do begin
      for x:= Bounds.Left to Bounds.Right do begin
        Result:= SliverSolveOld(x, y, Bounds);
        if (Result.IsInvalid) then goto Done;
        ChangeCount:= ChangeCount + Result; //+1 if changed, +0 if not changed
      end; { for y }
    end; { for x }
    if (ChangeCount = 0) then goto Done;
    ChangeCount:= 0;
    for y:= Bounds.Bottom downto Bounds.Top do begin
      for x:= Bounds.Right downto Bounds.Left do begin
        Result:= SliverSolveReverseOld(x, y, Bounds);
        if (Result.IsInvalid) then goto Done;
        ChangeCount:= ChangeCount + Result
      end; { for y }
    end; { for x }
  until (ChangeCount = 0);
Done:
  Self.FIsValid:= Result.IsValid;
end;

procedure TForm2.GridSolveLockstep(const Old, New: TGrid);
var
  x, y: integer;
  ChangeCount: integer;
  ResultOld, ResultNew: TSliverChanges;
label
  Done;
begin
  var Bounds:= Rect(0,0,9,9);
  repeat
    var Iterator:= New.FActive.GetEnumerator;
    Iterator.MoveNext;
    ChangeCount:= 0;
    for y:= 0 to 9 do begin
      for x:= 0 to 9 do begin
        ResultOld:= Old.SliverSolveOld(x, y, Bounds);
        if (Iterator.x = x) and (Iterator.y = y) then begin
          ResultNew:= New.SliverSolve(x,y, Bounds);
          Iterator.MoveNext;
        end;
        if (Old <> New) then begin
          Assert(Old = New);
        end;
        if (ResultOld.IsInvalid) then goto Done;
        ChangeCount:= ChangeCount + ResultOld; //+1 if changed, +0 if not changed
      end; { for y }
    end; { for x }
    if (ChangeCount = 0) then goto Done;
    Iterator:= New.FActive.Reverse.GetEnumerator;
    Iterator.MoveNext;
    ChangeCount:= 0;
    for y:= 9 downto 0 do begin
      for x:= 9 downto 0 do begin
        ResultOld:= Old.SliverSolveReverseOld(x, y, Bounds);
        if (Iterator.x = x) and (Iterator.y = y) then begin
          New.SliverSolveReverse(x,y, Bounds);
          Iterator.MoveNext;
        end;
        if (Old <> New) then begin
          Assert(Old = New);
        end;
        if (ResultOld.IsInvalid) then goto Done;
        ChangeCount:= ChangeCount + ResultOld;
      end; { for y }
    end; { for x }
  until (ChangeCount = 0);
Done:
  //Old.FIsValid:= ResultOld.IsValid;
end;

//Solve the grid starting at the given index.
function TGrid.GridSolveOld(const Bounds: TRect; IndexStart: integer): TSliverChanges;
var
  x, y: integer;
  ChangeCount: integer;
label
  Done,
  Start;
begin
  x:= IndexStart mod FSizeX;
  y:= IndexStart div FSizeX;
  ChangeCount:= 0;
  goto Start;
  repeat
    ChangeCount:= 0;
//    y:= Bounds.Top;
//    while y <= Bounds.Bottom do begin
    for y := Bounds.Top to Bounds.Bottom do begin
      for x:= Bounds.Left to Bounds.Right do begin
//      x:= Bounds.Left;
//      while x <= Bounds.Right do begin
//Start: - this is slower than the other position
        Result:= SliverSolveOld(x, y, Bounds);
        if (Result.IsInvalid) then goto Done;
        ChangeCount:= ChangeCount + Result; //+1 if changed, +0 if not changed
        //inc(x);
      end; { while x }
      //inc(y);
    end; { while y }
    if (ChangeCount = 0) then goto Done;
    ChangeCount:= 0;
    y:= Bounds.Bottom;
    while y >= Bounds.Top do begin
      x:= Bounds.Right;
      while x >= Bounds.Left do begin
Start: //32ms (above) vs 29 ms (here), keep the start here.
        Result:= SliverSolveReverseOld(x, y, Bounds);
        if (Result.IsInvalid) then goto Done;
        ChangeCount:= ChangeCount + Result;
        Dec(x);
      end; { while x }
      Dec(y);
    end; { while y }
  until (ChangeCount = 0);
Done:
  Self.FIsValid:= Result.IsValid;
end;

function TGrid.BitmapsNeeded: integer;
begin
  var xB:= ((FSizeX+2) div 16) + integer(((FSizeX+2) mod 16) > 0);
  var yB:= ((FSizeY+2) div 8)  + integer(((FSizeY+2) mod 8) > 0);
  Result:= xB * yB;
end;

procedure TGrid.GridToBitmap(out Bitmap: array of TUnit);
var
  x,y: integer;
begin
  Assert(Length(Bitmap) = BitmapsNeeded);
  var xB:= ((FSizeX+2) div 16) + integer(((FSizeX+2) mod 16) > 0);
  var MaxY:= ((FSizeY) div 3);
  var MaxX:= ((FSizeX) div 3);
  for var y2:= 0 to MaxY do begin  //not MaxY-1, because we have to process the border as well
    if y2 = MaxY then y:= FSizeY-2 else y:= y2*3;
    for var x2:= 0 to MaxX do begin //not MaxX-1
      if x2 = MaxX then x:= FSizeX-2 else x:= x2*3;
      var Index:= Self[x,y].NextSetBit(-1);
      for var y1:= 0 to 2 do begin
        for var x1:= 0 to 2 do begin
          var x3:= (x+x1);
          var y3:= (y+y1);
          if Odd(Index) then Bitmap[(x3 div 16) + ((y3 div 8)*xB)].SetPixel(x3 mod 16,y3 mod 8);
        end; {for x1}
      end; {for y1}
    end; {for x2}
  end; {for y2}
end;

function TGrid.GetPastGrid(AllowGrowth: boolean): TGrid;
var
  Slice: array[boolean] of TSlice;
begin
  Assert(Integer(AllowGrowth) in [0,1]);
  Result:= TGrid.Create(FSizeX + Integer(AllowGrowth) *2, FSizeY+Integer(AllowGrowth)*2);
  Slice[false]:= Form2.LookupTable[0,oCenter];
  Slice[true]:= not(Slice[false]);
  //if we AllowGrowth then start at 1,1 else start at 0,0
  var StartX:= Integer(AllowGrowth);
  var StartY:= StartX;
  for var y:= 0 to FSizeX-1 do begin
    for var x:= 0 to FSizeX-1 do begin
      var Index:= Self[x,y].NextSetBit(-1);
      Result[x+StartX, y+StartY]:= Slice[(Index and (1 shl 4)) <> 0];
      if AllowGrowth then begin
        if y = 0 then begin //process the top row and top two corners
          if x = 0 then Result[0,0]:= Slice[(Index and (1 shl 2)) <> 0];
          if x = (FSizeX-1) then Result[FSizeX+1,0]:= Slice[(Index and (1 shl 0)) <> 0];
          Result[x+1,0]:= Slice[(Index and (1 shl 1)) <> 0];
        end;
        if y=(FSizeY-1) then begin //process the bottom row and bottom two corners
          if x = 0 then Result[0,FSizeY+1]:= Slice[(Index and (1 shl 8)) <> 0];
          if x = (FSizeX-1) then Result[FSizeX+1,FSizeY+1]:= Slice[(Index and (1 shl 6)) <> 0];
          Result[x+StartX,FSizeY+1]:= Slice[(Index and (1 shl 7)) <> 0];
        end;
      end; {if AllowGrowth}
    end; {for x}
    if AllowGrowth then begin
      var Index:= Self[0,y].NextSetBit(-1);
      Result[0, y+1]:= Slice[(Index and (1 shl 5)) <> 0];
      Index:= Self[FSizeX-1,y].NextSetBit(-1);
      Result[FSizeX+1, y+1]:= Slice[(Index and (1 shl 3)) <> 0];
    end; {if AllowGrowth}
  end; {for y}
end;

procedure TGrid.GetMinSlice(out MinSlice: PSlice; out MinCount: integer);
begin
  MinCount:= 513;
  MinSlice:= nil;
  for var Slice in Self do begin
    var Count:= Slice.PopCount;
    if (Count < MinCount) and (Count > 1) then begin
      MinSlice:= Slice;
      MinCount:= Count;
      if (MinCount = 2) then exit;
    end;
  end;
end;

function TGrid.GetSliceIndex(Slice: PSlice): integer;
begin
  Result:= (NativeUInt(Slice) - NativeUInt(@FData[0])) div SizeOf(TSlice);
end;

//function TGrid.GetUniqueSolution: TSliverChanges;
//const
//  HasUniqueSolution = 513;
//var
//  MinCount: integer;
//  MinSlice: PSlice;
//begin
//  GetMinSlice(MinSlice, MinCount);
//  //If we cannot find a count other than 1, then we have reached a unique solution.
//  if (MinCount = HasUniqueSolution) then Exit(TSliverChanges.Changed);
//  var MinSliceIndex:= GetSliceIndex(MinSlice);
//  FActive.Activate(MinSliceIndex);
//  if (MinSliceIndex - FSizeY) >= (0) then FActive.Activate(MinSliceIndex-FSizeY);
//  if (MinSliceIndex + FSizeY) < (FSizeX * FSizeY) then FActive.Activate(MinSliceIndex+FSizeY);
//  if ((MinSliceIndex) mod FSizeX) > 0 then FActive.Activate(MinSliceIndex-1);
//  if ((MinSliceIndex) mod FSizeX) < (FSizeX-1) then FActive.Activate(MinSliceIndex+1);
//  //Explore each of the alternatives recursively
//  var Clone:= Self.Clone;
//  var Index:= -1;
//  for var i:= 0 to MinCount -1 do begin
//    Index:= MinSlice.NextSetBit(Index); //Get the next constellation
//    MinSlice.ForceSingleBit(Index); //Is this constellation valid?
//    //solve the grid
//    var Changes:= Self.GridSolve(Rect(0,0,9,9)); //if we're lucky then there is no solution
//    if Changes.IsValid then begin
//      //There is no quick contradiction, is there perhaps a satisfying assignment here?
//      Result:= GetUniqueSolution; //Depth first search for a solution.
//      if Result.IsValid then Exit;   //Early out when we have a unique solution
//      //Add the grid to the list of valid grids
//    end;
//    //No single solution? then reset the grid and try the next constellation
//    if (i < (MinCount-1)) then Clone.Overwrite(Self)
//  end; {for i}
//    //We have now reduced our grid to only those states that are valid upon first inspection.
//    //All alternatives investigated are invalid, there is no solution, return UNSAT.
//    Result:= TSliverChanges.Invalid;
//  //end;
//end;

procedure TGrid.DisplayUniqueSolution(const SG: TStringGrid);
begin
  Assert(SG.RowCount >= FSizeY+2);
  Assert(SG.ColCount >= FSizeX+2);
  for var y1:= 0 to FSizeY -1 do begin
    for var x1:= 0 to FSizeX-1 do begin
      var Value:= Self[x1,y1].NextSetBit(-1);
      for var y:= 0 to 2 do begin
        for var x:= 2 downto 0 do begin
          if (Odd(value)) then SG.Cells[x1+x,y1+y]:= 'X'
          else SG.Cells[x1+x,y1+y]:= ' ';
          value:= value shr 1;
        end; { for x }
      end; { for y }
    end; {for x1}
  end; {for y1}

end;

function TGrid.GetUniqueSolution: TSliverChanges;
const
  HasUniqueSolution = 513;
var
  MinCount: integer;
  MinSlice: PSlice;
begin
  GetMinSlice(MinSlice, MinCount);
  //If we cannot find a count other than 1, then we have reached a unique solution.
  if (MinCount = HasUniqueSolution) then begin
    DisplayUniqueSolution(Form2.StringGrid2);
    Exit(TSliverChanges.Changed);
  end;
  //FActive.Activate(GetSliceIndex(MinSlice));
  //Explore each of the alternatives recursively
  var Clone:= Self.Clone;
  var Index:= -1;
  for var i:= 0 to MinCount -1 do begin
    Index:= MinSlice.NextSetBit(Index); //Get the next constellation
    MinSlice.ForceSingleBit(Index); //Is this constellation valid?
    var MinSliceIndex:= GetSliceIndex(MinSlice);
    FActive.Activate(MinSliceIndex);
    if (MinSliceIndex - FSizeY) >= (0) then FActive.Activate(MinSliceIndex-FSizeY);
    if (MinSliceIndex + FSizeY) < (FSizeX * FSizeY) then FActive.Activate(MinSliceIndex+FSizeY);
    if ((MinSliceIndex) mod FSizeX) > 0 then FActive.Activate(MinSliceIndex-1);
    if ((MinSliceIndex) mod FSizeX) < (FSizeX-1) then FActive.Activate(MinSliceIndex+1);
    //solve the grid
    //starting at the pivot (this saves about 10%).
    var Changes:= Self.GridSolve(Rect(0,0,9,9)); //if we're lucky then there is no solution
    if Changes.IsValid then begin
      //There is no quick contradiction, is there perhaps a satisfying assignment here?
      Result:= GetUniqueSolution; //Depth first search for a solution.
      if Result.IsValid then Exit;   //Early out when we have a unique solution
    end;
    //No single solution? then reset the grid and try the next constellation
    if (i < (MinCount-1)) then Clone.Overwrite(Self)
  end; {for i}
    //We have now reduced our grid to only those states that are valid upon first inspection.
    //All alternatives investigated are invalid, there is no solution, return UNSAT.
  Result:= TSliverChanges.Invalid;
end;

function TGrid.GetUniqueSolutionOld(var ValidSolutions: TArray<TGrid>; var ValidCount: integer): TSliverChanges;
const
  HasUniqueSolution = 513;
var
  MinCount: integer;
  MinSlice: PSlice;
begin
  GetMinSlice(MinSlice, MinCount);
  //If we cannot find a count other than 1, then we have reached a unique solution.
  if (MinCount = HasUniqueSolution) then begin
    Inc(ValidCount);
    if Length(ValidSolutions) < ValidCount then SetLength(ValidSolutions, ValidCount);
    ValidSolutions[ValidCount-1]:= Self.Clone;
    Exit(TSliverChanges.Changed);
  end;
  //FActive.Activate(GetSliceIndex(MinSlice));
  //Explore each of the alternatives recursively
  var Clone:= Self.Clone;
  var Index:= -1;
  for var i:= 0 to MinCount -1 do begin
    Index:= MinSlice.NextSetBit(Index); //Get the next constellation
    MinSlice.ForceSingleBit(Index); //Is this constellation valid?
    //solve the grid
    //starting at the pivot (this saves about 10%).
    var Changes:= Self.GridSolveOld(Rect(0,0,9,9), GetSliceIndex(MinSlice)); //if we're lucky then there is no solution
    if Changes.IsValid then begin
      //There is no quick contradiction, is there perhaps a satisfying assignment here?
      Result:= GetUniqueSolutionOld(ValidSolutions, ValidCount); //Depth first search for a solution.
      //if Result.IsValid then Exit;   //Early out when we have a unique solution
    end;
    //No single solution? then reset the grid and try the next constellation
    if (i < (MinCount-1)) then Clone.Overwrite(Self)
  end; {for i}
    //We have now reduced our grid to only those states that are valid upon first inspection.
    //All alternatives investigated are invalid, there is no solution, return UNSAT.
  if (ValidCount = 0) then Result:= TSliverChanges.Invalid
  else Result:= TSliverChanges.Changed;
end;

function TGrid.GetUniqueSolutionOld: TSliverChanges;
const
  HasUniqueSolution = 513;
var
  MinCount: integer;
  MinSlice: PSlice;
begin
  GetMinSlice(MinSlice, MinCount);
  //If we cannot find a count other than 1, then we have reached a unique solution.
  if (MinCount = HasUniqueSolution) then begin
    Exit(TSliverChanges.Changed);
  end;
  //FActive.Activate(GetSliceIndex(MinSlice));
  //Explore each of the alternatives recursively
  var Clone:= Self.Clone;
  var Index:= -1;
  for var i:= 0 to MinCount -1 do begin
    Index:= MinSlice.NextSetBit(Index); //Get the next constellation
    MinSlice.ForceSingleBit(Index); //Is this constellation valid?
    //solve the grid
    //starting at the pivot (this saves about 10%).
    var Changes:= Self.GridSolveOld(Rect(0,0,9,9), GetSliceIndex(MinSlice)); //if we're lucky then there is no solution
    if Changes.IsValid then begin
      //There is no quick contradiction, is there perhaps a satisfying assignment here?
      Result:= GetUniqueSolutionOld; //Depth first search for a solution.
      if Result.IsValid then Exit;   //Early out when we have a unique solution
    end;
    //No single solution? then reset the grid and try the next constellation
    if (i < (MinCount-1)) then Clone.Overwrite(Self)
  end; {for i}
    //We have now reduced our grid to only those states that are valid upon first inspection.
    //All alternatives investigated are invalid, there is no solution, return UNSAT.
  Result:= TSliverChanges.Invalid
end;


function TGrid.SpeculativeExploration(Strategy: TExplorationStrategy; const SamplePoint: TPoint): TSliverChanges;
var
  Pivot: TPoint;
  Count: integer;
  PivotSlice: TSlice;
  TrailSlice: TSlice;
  PivotStartStatus: TSlice;
  TrailGrid: TGrid;
  i: integer;
  NextBit: integer;
  PivotStatus: TSliverChanges;
  ValidCount: integer;
  NewGrid: TGrid;
begin
  //step 1: find the (next) slice with the lowest popcount.
  Pivot:= SamplePoint;
  //step 2: solve for a single configuration of that slice.
  //We do this by forcing the chosen pivot slice to a single constellation and
  //observing if this leads to conflicts.
  PivotSlice:= Item[Pivot];
  PivotStartStatus:= PivotSlice;
  Count:= PivotSlice.PopCount;
  NextBit:= -1;     //Start with the first set bit in the slice
  ValidCount:= 0;   //keep track of the number of valid constellations
  Result:= TSliverChanges.UnChanged;
  for i:= 0 to Count -1 do begin
    TrailGrid:= Self.Clone;
    NextBit:= PivotSlice.NextSetBit(NextBit);  //guarenteed to not overflow, because we keep within the number of bits set.
    System.Assert(NextBit <= 511,'NextSetBit should always be a valid, expected < 511, got '+NextBit.ToString);
    //The trail grid is the same as the original, but with the pivot set to a single constellation
    TrailSlice.ForceSingleBit(NextBit);
    TrailGrid[Pivot]:= TrailSlice;
    //Now we just do a normal solve until we can get no further improvement, or until a conflict occurs.
    PivotStatus:= TrailGrid.GridSolve;
    if (PivotStatus.IsChanged) then Result:= TSliverChanges.Changed;
    //////////////////////
    ///  At this point we could choose to use a recursive approach, but let's keep
    ///  it simple for now.
    //////////////////////
    //If the constellation is (in)valid, we have two options.
    //A: keep the valid grids and OR them all together.
    //B: remove the invalid slice constellation from the grid.
    case Strategy of
      esExpensive: begin
        //The expensive strategy is to OR all the valid grids together
        //Thereby combining all the elimination work that has been done
        if (PivotStatus.Isvalid) then begin
          Inc(ValidCount);
          if (ValidCount = 1) then NewGrid:= TrailGrid.Clone
          else NewGrid:= NewGrid or TrailGrid;
        end;
      end;
      esCheap: begin
        //The cheap strategy is to remove the invalid constellations from the
        //slice under investigation, leaving only valid constellations.
        //This is very cheap, but does not retain all the information gained
        //whilst solving for the individual constellations.
        if (PivotStatus.IsInvalid) then begin
          Item[Pivot].SetBit(NextBit, false);
        end else Inc(ValidCount);
      end;
    end;
  end; {for i}

  case Strategy of
    esCheap: begin
      //If our pruning worked, then
      //Forward all the implications of the pruning.
       if (ValidCount < Count) and (ValidCount > 0) then Self.GridSolve;
    end;
    esExpensive: begin
      //Replace the starting grid with merged valid grids
      //We want to to this even if we did not eliminate any grids, because we very
      //likely (hopefully) will have removed a few constellations from our grid.
      if (ValidCount > 0) then Self:= NewGrid
      else Self:= TrailGrid;
    end;
  end; {for i}
  if (ValidCount = 0) then Result:= TSliverChanges.Invalid;
end;

function TGrid.Split(const Coordinate: TPoint; SolveMe: TSolveMe): TArray<TGrid>;
var
  Count: integer;
  i,a: integer;
  Constellation: integer;
  Pivot: TSlice;
  ForcedSlice: TSlice;
  CloneGrid: TGrid;
  SolveResult: TSliverChanges;
begin
  Pivot:= Item[Coordinate];
  Count:= Pivot.PopCount;
  SetLength(Result, Count);
  if (Count = 0) then Exit;
  Constellation:= -1;
  a:= 0;
  for i:= 0 to Count-1 do begin
    //Iterate
    Constellation:= Pivot.NextSetBit(Constellation);
    ForcedSlice.ForceSingleBit(Constellation);
    CloneGrid:= Self.Clone;
    CloneGrid.Item[Coordinate]:= ForcedSlice;
    case SolveMe of
      smNoSolving: SolveResult:= TSliverChanges.Changed; //Force the clone to be valid
      smSolveFirst, smSolveAndExcludeInvalids: begin
        SolveResult:= CloneGrid.GridSolve;
      end;
    end;
    if (SolveMe <> smSolveAndExcludeInvalids) or SolveResult.IsValid then begin
      Result[a]:= CloneGrid;
      Inc(a);
    end;
  end; {for i}
  //SetLength is not cheap
  if (SolveMe = smSolveAndExcludeInvalids) and ((a+1) <> Count) then SetLength(Result,a+1);
end;

function TGrid.GetSlice(i: integer): TSlice;
begin
  Assert(i < Length(FData));
  Result:= FData[i];
end;

{ TPointHelper }

function TPointHelper.Index(XSize: integer): integer;
begin
  Result:= (y * XSize) + x;
end;

function TPointHelper.North(XSize: integer): integer;
begin
  Result:= (y * XSize) + x;
end;

function TPointHelper.West(XSize: integer): integer;
begin
  Result:= (y * XSize) + x;
end;

function TPointHelper.East(XSize: integer): integer;
begin
  Result:= (y * XSize) + (x+1);
end;

function TPointHelper.South(XSize: integer): integer;
begin
  Result:= ((y+1) * XSize) + x;
end;

{ TGrid.TGridEnumerator }

constructor TGrid.TGridEnumerator.Create(Grid: PGrid);
begin
  FParent:= Grid;
  FIndex:= -1;
  FCount:= Grid.SizeX * Grid.SizeY;
end;

function TGrid.TGridEnumerator.GetCurrent: PSlice;
begin
  Result:= @FParent^.FData[FIndex];
end;

function TGrid.TGridEnumerator.GetX: integer;
begin
  Result:= FIndex mod FParent.FSizeX;
end;

function TGrid.TGridEnumerator.GetY: integer;
begin
  Result:= FIndex div FParent.FSizeX;
end;

function TGrid.TGridEnumerator.MoveNext: boolean;
begin
  Inc(FIndex);
  Result:= (FIndex < FCount);
end;

{ TSliceHelper }

class constructor TSliceHelper.Init;
var
  i,a: integer;
begin
  FillChar(Bitmask, SizeOf(Bitmask), #0);
  for i:= 0 to 511 do begin
    for a:= 0 to 8 do begin
      if (i and (1 shl a)) <> 0 then Bitmask[a].SetBit(i);
    end;
  end;
end;

{ TGridBitmap }

class function TGridBitmap.Empty: TGridBitmap;
begin
  FillChar(Result, SizeOf(Result), #0);
end;

class function TGridBitmap.Full: TGridBitmap;
begin
  FillChar(Result, SizeOf(Result), $FF);
end;

procedure TGridBitmap.MaskOffUnknown;
begin
  FKnownUnit[0]:= FKnownUnit[0] and not(FUnknownUnit[0]);
  FKnownUnit[1]:= FKnownUnit[1] and not(FUnknownUnit[1]);
end;

function TGridBitmap.PasteFragment(p: TPoint; KnownFragment, UnknownFragment: Uint64): TOutOfBounds;
const
  Mask: array[-2..2,-2..2] of Uint64 =
   //-2,-2      -1,-2      0,-2       1,-2       2,-2
   (($400000000,$600000000,$700000000,$300000000,$100000000),
   //-2,-1      -1,-1      0,-1       1,-1       2,-1
    ($400040000,$600060000,$700070000,$300030000,$100010000),
   //-2,0      -1,0        0,0        1,0        2,0
    ($400040004,$600060006,$700070007,$300030003,$100010001),
   //-2,1      -1,1        0,1        1,1        2,1
    ($000040004,$000060006,$000070007,$000030003,$000010001),
   //-2,2      -1,2        0,2        1,2        2,2
    ($000000004,$000000006,$000000007,$000000003,$000000001));

var
  //The fragment can cross 2 segments
  Part1, Part2: Uint64;

  procedure Split(var Part1, Part2: Uint64; input: Uint64);
  begin
    input:= input and Mask[Result.x, Result.y]; //Remove the out of bounds pixels
    //The X shift can never shift the input across a 64 bit boundary
    if (P.X > 0) then Input:= Input shl P.X else Input:= Input shr -P.X;
    if (P.Y >= 0) then begin
      Part1:= input shl ((P.Y mod 4) * 16);
      if ((P.Y mod 4) in [0,1]) then Part2:= 0
      else Part2:= input shr ((3- (P.Y mod 4)) * 16);
    end else begin
      //P.Y < 0
      Part1:= input shr ((3- (P.Y mod 4)) * 16);
      Part2:= 0;
    end; {handle Y}
  end;

begin
  Result:= P;
  if (P.X in [0..13]) then Result.X:= 0
  else if (P.X > 13) then Result.X:= P.X - 13;
  if (P.Y in [0..13]) then Result.Y:= 0
  else if (P.Y > 13) then Result.Y:= P.Y - 13;
  //Let's process the known fragment first and then do the same for the unknown fragment
  Split(Part1, Part2, KnownFragment);
  case P.Y of
    -2..1,4,5,8,9: FKnownData[(P.Y + 2) mod 4]:= FKnownData[(P.Y + 2) mod 4] or Part1;
    2..3,6..7,10..11: begin
      FKnownData[P.Y mod 4]:= FKnownData[P.Y mod 4] or Part1;
      FKnownData[(P.Y mod 4)+1]:= FKnownData[(P.Y mod 4)+1] or Part2;
    end;
    12,13,14,15: FKnownData[3]:= FKnownData[3] or Part1;
  end; {case}

  Split(Part1, Part2, UnknownFragment);
  {TODO -oJB -cTGridBitmap.PasteFragment : Fix duplication of case statement}
  case P.Y of
    -2..1,4,5,8,9: FUnknownData[(P.Y + 2) mod 4]:= FUnknownData[(P.Y + 2) mod 4] or Part1;
    2..3,6..7,10..11: begin
      FUnknownData[P.Y mod 4]:= FUnknownData[P.Y mod 4] or Part1;
      FUnknownData[(P.Y mod 4)+1]:= FUnknownData[(P.Y mod 4)+1] or Part2;
    end;
    12,13,14,15: FUnknownData[3]:= FUnknownData[3] or Part1;
  end; {case}
end;

function TGridBitmap.TestUnknownFuture: boolean;
begin
  FillChar(Self, SizeOf(Self), $FF);
  var Test:= Self.UnknownFuture;
  Result:= (Test.FUnknownData[0] and Test.FUnknownData[1] and Test.FUnknownData[2] and Test.FUnknownData[3]) = -1;
end;

function TGridBitmap.UnknownFuture: TGridBitmap;
begin
  //RCX = Self
  //Self:
  //    FKnownData: array[0..3] of Uint64;
  //    FUnknownData: array[0..3] of Uint64; #Every 16 bits holds a row.
  var UnknownN:= FUnknownUnit[0];
  var UnknownS:= FUnknownUnit[1];
  //Offset to each of the 8 compass directions
  var UNN:= UnknownN.OffsetToN;
  var UNNE:= UnknownN.OffsetToNE;
  var UNE:= UnknownN.OffsetToE;
  var UNSE:= UnknownN.OffsetToSE;
  var UNS:= UnknownN.OffsetToS;
  var UNSW:= UnknownN.OffsetToSW;
  var UNW:= UnknownN.OffsetToW;
  var UNNW:= UnknownN.OffsetToNW;

  var USN:= UnknownS.OffsetToN;
  var USNE:= UnknownS.OffsetToNE;
  var USE:= UnknownS.OffsetToE;
  var USSE:= UnknownS.OffsetToSE;
  var USS:= UnknownS.OffsetToS;
  var USSW:= UnknownS.OffsetToSW;
  var USW:= UnknownS.OffsetToW;
  var USNW:= UnknownS.OffsetToNW;

  //Combine the two N offsets, we only keep the S parts
  UNN.A:= UNN.B xor USN.A;             //Xor is used to aid testing that no invalid overlaps occur.
  UNN.B:= USN.B;
  //Combine the two S offset, we only keep the N parts
  //UNS.A:= UNS.A
  UNS.B:= UNS.B xor USS.A;
  //For the E shift, we only keep the West part
  //UNE.A:= UNE.A
  UNE.B:= USE.A;
  //For the W shift, we keep only the East part
  //UNW.A:= UNW.A;
  UNW.B:= USW.A;
  //For the NW shift, we keep only the SE part
  UNNW.NE:= UNNW.SE xor USNW.NE;
  UNNW.SE:= USNW.SE;
  //for the NE shift, we keep only the SW part
  UNNE.NW:= UNNE.SW xor USNE.NW;
  UNNE.SW:= USNE.SW;
  //for the SE shift, we keep only the NW part
  //UNSE.NW:= UNSE.NW
  UNSE.SW:= UNSE.SW xor USSE.NW;
  //for the SW shift, we keep only the NE part
  //UNSW.NE:= UNSW.NE;
  UNSW.SE:= UNSW.SE xor USSW.NE;
  //Now add everything together
  ////////////////////////////////////  N        S        E        W
  Result.FUnknownUnit[0]:= UNN.A or UNS.a or UNE.a or UNW.a or UNNW.NE or UNNE.NW or UNSE.NW or UNSW.NE;
  Result.FUnknownUnit[0]:= UNN.b or UNS.b or UNE.b or UNW.b or UNNW.SE or UNNE.SW or UNSE.SW or UNSW.SE;
  Result.FKnownData[0]:= Self.FKnownData[0];
  Result.FKnownData[1]:= Self.FKnownData[1];
  Result.FKnownData[2]:= Self.FKnownData[2];
  Result.FKnownData[3]:= Self.FKnownData[3];
end;

function TGridBitmap.Validate(const Reference: TGridBitmap): boolean;
begin
  //First calculate the t=1 pattern from the t=0 current KnownData
  //A cellblock is a 64x64 pixel
  var Block:= TCellBlock.Create(0,0,nil);
  Block.p[5+0].q[0]:= Self.FKnownData[0];
  Block.p[5+0].q[1]:= Self.FKnownData[1];
  Block.p[5+4].q[0]:= Self.FKnownData[2];
  Block.p[5+4].q[1]:= Self.FKnownData[3];
  Block.ToDoList.Activate(5);
  Block.ToDoList.Activate(5+4);
  Block.GeneratePtoQ;
  //Now we have the result in Block.Q, but offset 1 pixel to the NorthWest (or was it SouthEast?)
  {TODO -oJB -cTGridBitmap.Validate : DoubleCheck offset direction of PtoQ}
  var QuadN:= Block.q[5+0].OffsetToNW; //I may need to change this to OffsetToSE if I guessed wrong
  var QuadS:= Block.q[5+4].OffsetToNW;
  //Reduce the 16x16 pixel block to 15x15
  //First we need to move the unknown bits 1 pixel to all 8 compass directions
  var Future:= Self.UnknownFuture;
  //Put the known future together with the unknown future
  Future.FKnownUnit[0]:= QuadN.SE xor QuadS.NE;
  Future.FKnownUnit[1]:= QuadS.SE;

  //Get the difference between the norm and the future
  Future.FKnown:= Future.FKnown xor Reference.FKnown;
  Future.MaskOffUnknown;
  //If the result <> 0, then we have a problem.
  Result:= Future.FKnown.IsEmpty;
end;

{ TOutOfBounds }

class operator TOutOfBounds.Add(const a: TRect; const b: TOutOfBounds): TRect;
begin
  if (TPoint(b) = Point(0,0)) then Exit(a)
  else begin
    Result:= Rect(Min(a.Left, b.x), Min(a.Top, b.y), Max(a.Right, b.x), Max(a.Bottom, b.y));
  end;
end;

class operator TOutOfBounds.Implicit(const a: TPoint): TOutOfBounds;
begin
  Result.FData:= a;
end;

class operator TOutOfBounds.Implicit(const a: TOutOfBounds): TPoint;
begin
  Result:= a.FData;
end;

{ TGridData }

class operator TGridData.BitwiseAnd(const A, B: TGridData): TGridData;
begin
  for var i:= 0 to 3 do begin
    Result.FData[i]:= A.FData[i] and B.FData[i];
  end;
end;

class operator TGridData.BitwiseOr(const A, B: TGridData): TGridData;
begin
  for var i:= 0 to 3 do begin
    Result.FData[i]:= A.FData[i] or B.FData[i];
  end;
end;

class operator TGridData.BitwiseXor(const A, B: TGridData): TGridData;
begin
  for var i:= 0 to 3 do begin
    Result.FData[i]:= A.FData[i] xor B.FData[i];
  end;
end;

function TGridData.IsEmpty: boolean;
begin
  for var i:= 0 to 3 do begin
    if FData[i] <> 0 then Exit(false);
  end;
  Result:= true;
end;

class operator TGridData.LogicalNot(const A: TGridData): TGridData;
begin
  for var i:= 0 to 3 do begin
    Result.FData[i]:= not(A.FData[i]);
  end;
end;

{ TActiveSlice }

procedure TActiveSlice.Activate(index: integer);
//rcx = self
//edx = index
asm
  bts [rcx],edx            //if CF=0, then we added a flag
  jc @done
  inc dword ptr [rcx.TActiveSlice.ActiveCount]   //increase the active counter
@done:
  rep ret
end;

procedure TActiveSlice.Reset(index: integer);
//rcx = self
//edx = index
asm
  btr [rcx],edx            //if CF=1, then we removed a flag
  jnc @done
  dec dword ptr [rcx.TActiveSlice.ActiveCount]   //decrease the active counter
@done:
  rep ret
end;

procedure TActiveSlice.Update(index: integer; NewStatus: boolean);
  //rcx = @self
  //edx = index
  //r8b = NewStatus
asm
  test r8b,r8b
  jnz @Activate
@Reset:
  btr [rcx],edx            //if CF=1, then we removed a flag
  jnc @done
  dec dword ptr [rcx.TActiveSlice.ActiveCount]   //decrease the active counter
@done:
  rep ret
@Activate:
  bts [rcx],edx            //if CF=0, then we added a flag
  jc @done
  inc dword ptr [rcx.TActiveSlice.ActiveCount]   //increase the active counter
  rep ret
end;

function TActiveSlice.Reverse: TActiveSliceReverseFactory;
begin
  Result:= TActiveSliceReverseFactory.Create(@Self);
end;

constructor TActiveSlice.Create(MaxX, MaxY: integer);
begin
  FSizeX:= MaxX;
  FCount:= MaxX * MaxY;
  FillChar(FActive, SizeOf(FActive), #0);
  for var i := 0 to (MaxX * MaxY)-1 do begin
    Activate(i);
  end;
  ActiveCount:= FCount;
end;

constructor TActiveSlice.Create(MinX, MinY, MaxX, MaxY, SizeX: integer);
begin
  Create(SizeX, MaxY);
  Limit(Rect(MinX, MinY, MaxX, MaxY));
end;

procedure TActiveSlice.Limit(const Bounds: TRect);
begin
  //block off all the bits that fall outside the valid range
  for var i := 0 to FCount-1 do begin
    var x:= i mod FSizeX;
    var y:= i div FSizeX;
    with Bounds do if (x < Left) or (x > Right) or (y < Top) or (y > Bottom) then Reset(i);
  end; {for i}
end;

function TActiveSlice.GetEnumerator: TActiveEnumerator;
begin
  Result:= TActiveSlice.TActiveEnumerator.Create(@Self, true, FSizeX);
end;

function TActiveSlice.NextSetBit(previous: integer): integer;
asm
  // RCX: self
  // EDX: previous
  // Return result in EAX (0..255 is bit position of next set bit) 256=not found
  // r10 = copy of self
  // r8 = qword index
  // r9 = population count
  mov eax,256           // assume failure
  cmp edx,255           // are we all done?
  jge @done             // yes, done
  mov r10,rcx           // r10=self, We need cl for shifting
  inc edx               // we're looking for the next bit
  mov ecx,edx           // shift out the bits already processed
  and ecx,63            // only take 0..63 into account so we don't upset the offset calc
  // Get the relevant int64
  shr edx,6             // Get the revelant 64 bit section (div 64)
  lea r8,[rdx*8]        // but note that that 64 bits = 8 bytes (rounded down).
@repeat:
  mov r9,[r10+r8]       // get the next section to investigate
  shr r9,cl             // shift out the bits we've already looked at
  //mov eax,64            // BSF DEST, SOURCE: if SOURCE=0 then DEST will be unchanged.
  bsf rax,r9            // get the next set bit, CF=0 if we found it
  lea eax,[eax+ecx]     // add the bitcount shifted out back in
  // if eax=64 then all bits set in our section are clear.
  // If r8d = 7 then we are done, if not we need to look further.
  lea rax,[rax+r8*8]    // Add the section offset back in.
  jnz @done             // We found a bit
  // Oops, ZF=1, meaning the section is empty, investigate the next section.
  xor ecx,ecx           // reset at the start of the next session
  cmp r8d,32-8          // Did we investigate all sections? ZF=1 if true
  lea r8,[r8+8]         // Let's prepare for a new round
  jne @repeat
@done:
  //mov edx,512
  //cmp eax,512
  //cmovnc eax,edx
  rep ret
end;

function TActiveSlice.PreviousSetBit(Next: integer): integer;
asm
  // RCX: self
  // EDX: previous
  // Return result in EAX (0..255 is bit position of next set bit) 256=not found
  // r10 = copy of self
  // r8 = qword index
  // r9 = population count
  mov eax,-1            // assume failure
  cmp edx,0             // are we all done?
  jle @done             // yes, done
  mov r10,rcx           // r10=self, We need cl for shifting
  mov ecx,edx           // shift out the bits already processed
  neg ecx
  dec edx               // we're looking for the previous bit
  //and ecx,63            // only take 0..63 into account so we don't upset the offset calc
  // Get the relevant int64
  shr edx,6             // Get the revelant 64 bit section (div 64)
  lea r8,[rdx*8]        // but note that that 64 bits = 8 bytes (rounded down).
@repeat:
  mov r9,[r10+r8]       // get the next section to investigate
  shl r9,cl             //
  shr r9,cl             // zero the bits we've already looked at.
  //mov eax,64            // BSF DEST, SOURCE: if SOURCE=0 then DEST will be unchanged.
  //neg ecx             // Subtract the shift, not add it.
  bsr rax,r9            // get the next set bit, CF=0 if we found it
  //lea eax,[eax+ecx]     // subtract the bitcount shifted out back in
  //and eax,63            // cap the max value at 63
  // if eax=64 then all bits set in our section are clear.
  // If r8d = 7 then we are done, if not we need to look further.
  lea rax,[rax+r8*8]    // Add the section offset back in.
  jnz @done             // We found a bit
  // Oops, ZF=1, meaning the section is empty, investigate the next section.
  xor ecx,ecx           // reset at the start of the next session
  cmp r8d,0             // Did we investigate all sections? ZF=1 if true
  lea r8,[r8-8]         // Let's prepare for a new round
  jne @repeat
@done:
  //mov edx,512
  //cmp eax,512
  //cmovnc eax,edx
  rep ret
end;

{ TActiveSlice.TGridEnumerator }

constructor TActiveSlice.TActiveEnumerator.Create(ActiveSlice: PActiveSlice; MoveForward: boolean; SizeX: integer);
begin
  FParent:= ActiveSlice;
  FMoveForward:= MoveForward;
  FSizeX:= SizeX;
  case FMoveForward of
    true: FIndex:= -1;
    false: FIndex:= 256;
  end;
end;

function TActiveSlice.TActiveEnumerator.GetCurrent: integer;
begin
  Result:= FIndex;
end;

function TActiveSlice.TActiveEnumerator.GetX: integer;
begin
  Result:= FIndex mod FSizeX;
end;

function TActiveSlice.TActiveEnumerator.GetY: integer;
begin
    Result:= FIndex div FSizeX;
end;

function TActiveSlice.TActiveEnumerator.MoveNext: boolean;
begin
  case FMoveForward of
    true: begin
      //var NewIndex:= FIndex;
      FIndex:= FParent.NextSetBit(FIndex);
      //Inc(FIndex);
      //while (FIndex < FParent.FCount) and not(FIndex in Self.FParent.FActive) do Inc(FIndex);
      //Assert((NewIndex = FIndex) or (FIndex >= FParent.FCount));
    end;
    false: begin
      //var OldIndex:= FIndex;
      //var NewIndex:= FIndex;
      FIndex:= FParent.PreviousSetBit(FIndex);
      //Dec(FIndex);
      //while (FIndex >= 0) and not(FIndex in Self.FParent.FActive) do Dec(FIndex);
      //if not((NewIndex = FIndex) or (FIndex < 0)) then begin
      //  NewIndex:= FParent.PreviousSetBit(OldIndex);
      //  Assert((NewIndex = FIndex) or (FIndex < 0));
      //end;
    end;
  end;
  Result:= (FIndex >= 0) and (FIndex < FParent.FCount);
end;

{ TActiveSliceReverseFactory }

function TActiveSliceReverseFactoryHelper.GetEnumerator: TActiveSlice.TActiveEnumerator;
begin
  Result:= TActiveSlice.TActiveEnumerator.Create(FActiveSlice, false, FActiveSlice.FSizeX);
end;

{ TActiveSliceReverseFactory }

constructor TActiveSliceReverseFactory.Create(Parent: PActiveSlice);
begin
  FActiveSlice:= Parent;
end;

{ TSliceDict }

constructor TSliceDict.Create;
begin
  inherited Create;
  FSliceDictionaryComparer:= TSliceDictionaryComparer.Create;
  Self.NSDictionary:= TSliceDictionary.Create(FSliceDictionaryComparer);
  Self.EWDictionary:= TSliceDictionary.Create(FSliceDictionaryComparer);
  FNoDupsComparer:= TNoDupsDictComparer.Create(Self);
  Self.NoDupsDict:= TDictionary<integer, integer>.Create(FNoDupsComparer);
  SetLength(SliceDB, 1024); //Reserve 1024* 1 million entries. (64 GB) max.
  SetLength(SliceDB[0], LineSize); //Allocate the first line.
  for var i := 0 to 511 do begin
    var Slice:= TSlice.FullyEmpty;
    Slice.ForceSingleBit(i);
    InsertSlice(Slice);
  end;
  InsertSlice(OffSlice); //Off slice
  InsertSlice(not(SliceDB[0][512])); //On Slice;
  InsertSlice(TSlice.FullyUnknown);
  InsertSlice(TSlice.FullyEmpty);
  Assert(FIndex = (Invalid+1));
end;

procedure TSliceDict.EW(var East, West: integer; out Changes: TSliverChanges);
begin
  var EW:= TPair<integer, integer>.Create(East,West);
  var Result: TSliceResult;
  if EWDictionary.TryGetValue(EW, Result) then begin
    Changes:= Result.Changes;
    East:= Result.NE;
    West:= Result.SW;
  end else begin
    var E:= Item[East];
    var W:= Item[West];
    var Sliver:= TSliver.EW(E^,W^, Changes);
    if Changes.EastChanged then begin
      var E2:= E^ and Sliver.East;
      East:= InsertSlice(E2);
    end;
    if Changes.WestChanged then begin
      var W2:= W^ and Sliver.West;
      West:= InsertSlice(W2);
    end;
    EWDictionary.Add(EW, TSliceResult.Create(East, West, Changes));
  end;
end;

procedure TSliceDict.NS(var North, South: integer; out Changes: TSliverChanges);
begin
  var NS:= TPair<integer, integer>.Create(North, South);
  var Result: TSliceResult;
  if NSDictionary.TryGetValue(NS, Result) then begin
    Changes:= Result.Changes;
    North:= Result.NE;
    South:= Result.SW;
  end else begin
    var N:= Item[North];
    var S:= Item[South];
    var Sliver:= TSliver.NS(N^,S^, Changes);
    if Changes.NorthChanged then begin
      var N2:= N^ and Sliver.North;
      North:= InsertSlice(N2);
    end;
    if Changes.SouthChanged then begin
      var S2:= S^ and Sliver.South;
      South:= InsertSlice(S2);
    end;
    NSDictionary.Add(NS, TSliceResult.Create(North, South, Changes));
  end;
end;

function TSliceDict.GetItem(index: integer): PSlice;
begin
  var Y:= Index mod LineSize;
  var X:= Index div LineSize;
  Result:= @SliceDB[x][y];
end;

function TSliceDict.InsertSlice(const Slice: TSlice): integer;
begin
  var Y:= FIndex mod LineSize;
  var X:= FIndex div LineSize;
  if (Y = 0) then begin //Grow the DB
    SetLength(SliceDB[X], LineSize); //Allocate a new line.
  end;
  SliceDB[X][Y]:= Slice;
  //Only commit the slice to the DB if it is unique.
  if NoDupsDict.TryAdd(FIndex, FIndex) then begin
    Result:= FIndex;
    Inc(FIndex);
  end else begin
    //The Slice already exists, return its index
    NoDupsDict.TryGetValue(FIndex, Result);
  end;
end;



{ TSliceResult }

constructor TSliceResult.Create(NE, SW: integer; Change: TSliverChanges);
begin
  Self.NE:= NE;
  Self.SW:= SW;
  Self.Changes:= Change;
end;

{ TDictGrid }

procedure TDictGrid.Clear;
begin
  for var i:= 0 to FSizeX*FSizeY-1 do begin
    FData[i]:= TSliceDict.Invalid;
  end;
end;

function TDictGrid.Clone: TDictGrid;
begin
  Result:= TDictGrid.Create(Self);
  Move(FData[0], Result.FData[0], SizeOf(integer) * FSizeX * FSizeY);
end;

constructor TDictGrid.Create(SizeX, SizeY: integer);
begin
  if GlobalDict = nil then GlobalDict:= TSliceDict.Create;
  FDict:= GlobalDict;
  FIsValid:= true;
  FSizeX:= SizeX;
  FSizeY:= SizeY;
  SetLength(FData, FSizeX*FSizeY);
  for var i:= 0 to FSizeX * FSizeY-1 do begin
    FData[i]:= TSliceDict.Unknown;
  end;
  //FDebugGrid:= TGrid.Create(SizeX, SizeY);
end;

constructor TDictGrid.Create(const Template: TDictGrid);
begin
  Create(Template.FSizeX, Template.FSizeY);
end;

procedure TDictGrid.Overwrite(var GridToBeOverwritten: TDictGrid);
begin
  Assert((GridToBeOverwritten.FSizeX = FSizeX) and (GridToBeOverwritten.FSizeY = FSizeY));
  Move(FData[0], GridToBeOverwritten.FData[0], SizeOf(integer) * FSizeX * FSizeY);
end;

procedure TDictGrid.SetItems(x, y: integer; const Value: integer);
begin
  var i:= x+y*FSizeX;
  FData[i]:= Value;
  //FDebugGrid.FData[i]:= Self.FDict.SliceDB[0][Value];
end;

function TDictGrid.SliverSolveOld(x, y: integer; const MinMax: TRect): TSliverChanges;
var
  IndexCenter: integer;

  function DoEW: TSliverChanges;
  begin
    if x < (MinMax.Right) then begin
      var IndexEast:= IndexCenter+1;
      //If there is a problem the sliver will be invalid.
      FDict.EW(FData[IndexEast], FData[IndexCenter], Result);
    end else Result:= TSliverChanges.UnChanged; { handle EW }
  end;

  function DoNS: TSliverChanges;
  begin
    if y < (MinMax.Bottom) then begin
      var IndexSouth:= IndexCenter + FSizeX;
      FDict.NS(FData[IndexCenter], FData[IndexSouth], Result);
    end else Result:= TSliverChanges.UnChanged; { handle NS }
  end;

begin
  IndexCenter:= (y * FSizeX) + x;
  //EW
  var ResultEW:= DoEW;
  if ResultEW.IsInvalid then Exit(ResultEW);
  var ResultNS:= DoNS;
  if not(ResultNS.NorthChanged) or ResultNS.IsInvalid then Exit(ResultNS or ResultEW);
  //The center keeps changing, loop until it stabilizes
  while True do begin
    //Looping until changes stabilize makes a small difference +/- 5% savings.
    ResultEW:= DoEW;
    if not(ResultEW.WestChanged) or ResultEW.IsInvalid then Exit(ResultEW or ResultNS);
    //We hardly ever reach this point
    ResultNS:= DoNS;
    if not(ResultNS.NorthChanged) or ResultNS.IsInvalid then Exit(ResultNS or ResultEW);
  end;
end;

function TDictGrid.SliverSolveReverseOld(x, y: integer; const MinMax: TRect): TSliverChanges;
var
  IndexCenter: integer;

  function DoEW: TSliverChanges;
  begin
    if x > (MinMax.Left) then begin
      var IndexWest:= IndexCenter-1;
      //If there is a problem the sliver will be invalid.
      FDict.EW(FData[IndexCenter], FData[IndexWest], Result);
    end else Result:= TSliverChanges.UnChanged; { handle EW }
  end;

  function DoNS: TSliverChanges;
  begin
    if y > (MinMax.top) then begin
      var IndexNorth:= IndexCenter - FSizeX;
      FDict.NS(FData[IndexNorth], FData[IndexCenter], Result);
    end else Result:= TSliverChanges.UnChanged; { handle NS }
  end;

begin
  IndexCenter:= (y * FSizeX) + x;
  //EW
  var ResultEW:= DoEW;
  if ResultEW.IsInvalid then Exit(ResultEW);
  var ResultNS:= DoNS;
  if not(ResultNS.SouthChanged) or ResultNS.IsInvalid then Exit(ResultNS or ResultEW);
  //The center keeps changing, loop until it stabilizes
  while True do begin
    //Looping until changes stabilize makes a small difference +/- 5% savings.
    ResultEW:= DoEW;
    if not(ResultEW.EastChanged) or ResultEW.IsInvalid then Exit(ResultEW or ResultNS);
    //We hardly ever reach this point
    ResultNS:= DoNS;
    if not(ResultNS.SouthChanged) or ResultNS.IsInvalid then Exit(ResultNS or ResultEW);
  end;
end;

function TDictGrid.GetUniqueSolutionOld: TSliverChanges;
const
  HasUniqueSolution = 513;
var
  MinCount: integer;
begin
  var MinSlice:= GetMinSlice(MinCount);
  //If we cannot find a count other than 1, then we have reached a unique solution.
  if (MinCount = HasUniqueSolution) then begin
    Exit(TSliverChanges.Changed);
  end;
  //var DebugMinSlice: PSlice;
  //var DebugMinCount: integer;
  //FDebugGrid.GetMinSlice(DebugMinSlice, DebugMinCount);
  //Assert(DebugMinCount = MinCount);
  //Explore each of the alternatives recursively
  var Clone:= Self.Clone;
  //var DebugClone:= Self.FDebugGrid.Clone;
  var Index:= -1;
  //var DebugIndex:= -1;
  for var i:= 0 to MinCount -1 do begin
    Index:= Slices[MinSlice].NextSetBit(Index); //Get the next constellation
    //DebugIndex:= DebugMinSlice.NextSetBit(DebugIndex);
    //Assert(Index = DebugIndex);
    FData[MinSlice]:= Index; //MinSlice.ForceSingleBit(Index); //Is this constellation valid?
    //DebugMinSlice.ForceSingleBit(DebugIndex);
    //solve the grid
    //starting at the pivot (this saves about 10%).
    var Changes:= Self.GridSolveOld(Rect(0,0,9,9), MinSlice); //if we're lucky then there is no solution
    if Changes.IsValid then begin
      //There is no quick contradiction, is there perhaps a satisfying assignment here?
      Result:= GetUniqueSolutionOld; //Depth first search for a solution.
      if Result.IsValid then Exit;   //Early out when we have a unique solution
    end;
    //No single solution? then reset the grid and try the next constellation
    //if (i < (MinCount-1)) then begin
      Clone.Overwrite(Self);
      //DebugClone.Overwrite(Self.FDebugGrid);
    //end;
  end; {for i}
    //We have now reduced our grid to only those states that are valid upon first inspection.
    //All alternatives investigated are invalid, there is no solution, return UNSAT.
  Result:= TSliverChanges.Invalid
end;

//Solve the grid starting at the given index.
function TDictGrid.GridSolveOld(const Bounds: TRect; IndexStart: integer = -1): TSliverChanges;
var
  x, y: integer;
  ChangeCount: integer;
label
  Done,
  Start;
begin
  ChangeCount:= 0;
  if IndexStart <> -1 then begin
    x:= IndexStart mod FSizeX;
    y:= IndexStart div FSizeX;
    goto Start;
  end;
  repeat
    ChangeCount:= 0;
//    y:= Bounds.Top;
//    while y <= Bounds.Bottom do begin
    for y := Bounds.Top to Bounds.Bottom do begin
      for x:= Bounds.Left to Bounds.Right do begin
//      x:= Bounds.Left;
//      while x <= Bounds.Right do begin
//Start: - this is slower than the other position
        Result:= SliverSolveOld(x, y, Bounds);
        //var Result2:= FDebugGrid.SliverSolveOld(x,y,Bounds);
        //if (Result <> Result2) then begin
        //  Assert(Result = Result2);
        //end;
        if (Result.IsInvalid) then goto Done;
        ChangeCount:= ChangeCount + Result; //+1 if changed, +0 if not changed
        //inc(x);
      end; { while x }
      //inc(y);
    end; { while y }
    if (ChangeCount = 0) then goto Done;
    ChangeCount:= 0;
    y:= Bounds.Bottom;
    while y >= Bounds.Top do begin
      x:= Bounds.Right;
      while x >= Bounds.Left do begin
Start: //32ms (above) vs 29 ms (here), keep the start here.
        Result:= SliverSolveReverseOld(x, y, Bounds);
        //var Result2:= FDebugGrid.SliverSolveReverseOld(x,y,Bounds);
        //if (Result <> Result2) then begin
        //  Assert(Result = Result2);
        //end;
        if (Result.IsInvalid) then goto Done;
        ChangeCount:= ChangeCount + Result;
        Dec(x);
      end; { while x }
      Dec(y);
    end; { while y }
  until (ChangeCount = 0);
Done:
  Self.FIsValid:= Result.IsValid;
end;



function TDictGrid.GetMinSlice(out MinCount: integer): integer;
begin
  MinCount:= 513; //Make sure we always get a hit.
  Result:= -1;
  for var i:= 0 to FSizeX * FSizeY-1 do begin
    var iCount:= Self.Slices[i].PopCount;
    if (iCount < MinCount) and (iCount > 1) then begin
      MinCount:= iCount;
      Result:= i;
      if (MinCount = 2) then exit;
    end;
  end; {for i}
end;

function TDictGrid.GetSlices(index: integer): PSlice;
begin
  Result:= Self.FDict.Item[FData[index]];
end;

function TDictGrid.GetSlicesXY(x, y: integer): PSlice;
begin
  Result:= Self.FDict.Item[FData[x + y*FSizeX]];
end;

{TNoDupsDictComparer<integer> }

constructor TNoDupsDictComparer.Create(Parent: TSliceDict);
begin
  inherited Create;
  FParent:= Parent;
end;

function TNoDupsDictComparer.Equals(const Left, Right: integer): Boolean;
begin
  Result:= FParent.Item[Left]^ = FParent.Item[Right]^
end;

function TNoDupsDictComparer.GetHashCode(const Value: integer): Integer;
begin
  Result:= FNV1A_Hash_Meiyan(FParent.Item[Value]^, SizeOf(TSlice), FParent.LineSize);
end;

{ TSliceDictionaryComparer }

function TSliceDictionaryComparer.Equals(const Left, Right: IntPair): Boolean;
begin
  Result:= Int64(Left) = Int64(Right);
end;

function TSliceDictionaryComparer.GetHashCode(const Value: IntPair): Integer;
begin
  Result:= FNV1A_Hash_Meiyan(Value, SizeOf(Value), 4515722);
end;


end.

