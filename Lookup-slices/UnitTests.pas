unit UnitTests;

interface

// {$I DUnitx.inc}

{$M+}

uses
  DUnitx.TestFramework;
type

  [TestFixture]
  TSliceTests = class(TObject)
  //By default only unit tests that are published are run.
  private
    //Inactive unit tests
    class procedure TestSliceNextBitSet; static;
    class procedure TimeSlowNSFolding; static;
    class procedure TimeFastNSFolding; static;
    class procedure TimeSlowEast; static;
    class procedure TimeFastEast; static;
    procedure DoesConstructedNESWMatchTheLookupTable;
    procedure DoesConstructedNESWMatchTheLookupTable_NESW;
    class procedure TestUnknownFolding;
    procedure MaskedBitsCreate;
    class procedure MaskedBitsFastTimer; static;
    class procedure MaskedBitsSlowTimer; static;
    class procedure FoldRemaining; static;
    class procedure TestTMaskBitsCreate; static;
  published
    //Active unit tests.
    procedure DoesTestingStart;
  private
//    class procedure TimeSlowNSFolding; static;
//    class procedure TimeFastNSFolding; static
    class procedure Time_TSliceAnd; static;
    class procedure Time_TSliceOr; static;
    class procedure TSlice_GetBit; static;
    class procedure TSlice_SetBit; static;
    class procedure TSlice_IsZero; static;
    class procedure TSlice_NextSetBit; static;
    class procedure TSlice_Popcount; static;
    class procedure TSlice_BitwiseAnd; static;
    class procedure TSlice_LogicalNot; static;
    class procedure TSlice_BitwiseOr; static;
    class procedure TSlice_BitwiseXor; static;
    class procedure TSlice_Equal; static;

    class procedure DoubleCheckUnknownTable; static;
    class procedure TimeMoveFast; static;
    class procedure TimeMoveSlow; static;
    class procedure TimeFastNorth; static;
    class procedure TimeSlowNorth; static;
  published
    class procedure ValidateUnknownFuture; static;
    class procedure ValidateFastEWFolding; static;
    class procedure ValidateFastNSFolding; static;

    class procedure VerifyFastEast; static;
    class procedure TimeFastWest; static;
    class procedure TimeSlowWest; static;
    class procedure VerifyFastWest; static;
    class procedure VerifyFastNorth; static;
    class procedure VerifyFastSouth; static;
    class procedure TimeFastEWFolding; static;
    class procedure TimeSlowEWFolding; static;
  end;



implementation

uses{$IFDEF GpProfile U} GpProf, {$ENDIF GpProfile U}
  System.SysUtils,
  Math,
  Unit2;

{ TSliceTests }

function ReflectNE_SW(i, width: integer): integer;
var
  a,b,c,d: integer;
begin
  Result:= 0;
  case width of
    4: begin
      a:= ((i and (1 shl  0)) shl 15) xor ((i and (1 shl  1)) shl 13) xor ((i and (1 shl  2)) shl 11) xor ((i and (1 shl  3)) shl  9);
      b:= ((i and (1 shl  4)) shl  7) xor ((i and (1 shl  5)) shl  5) xor ((i and (1 shl  6)) shl  3) xor ((i and (1 shl  7)) shl  1);
      c:= ((i and (1 shl  8)) shr  1) xor ((i and (1 shl  9)) shr  3) xor ((i and (1 shl 10)) shr  5) xor ((i and (1 shl 11)) shr  7);
      d:= ((i and (1 shl 12)) shr  9) xor ((i and (1 shl 13)) shr 11) xor ((i and (1 shl 14)) shr 13) xor ((i and (1 shl 15)) shr 15);
      Result:= (a xor b xor c xor d);
    end;
    3: begin
      a:= ((i and (1 shl 0)) shl 8) xor ((i and (1 shl 1)) shl 6) xor ((i and (1 shl 2)) shl 4);
      b:= ((i and (1 shl 3)) shl 2) xor ((i and (1 shl 4)) shl 0) xor ((i and (1 shl 5)) shr 2);
      c:= ((i and (1 shl 6)) shr 4) xor ((i and (1 shl 7)) shr 6) xor ((i and (1 shl 8)) shr 8);
      Result:= a xor b xor c;
    end;
  end;  {case}
  Assert.AreEqual(Popcount(Result), Popcount(i), 'Reflection does not preserve popcount at '+i.ToString);
end;

function ReflectEW(i, width: integer): integer;
var
  a,b,c,d: integer;
begin
  Result:= 0;
  case Width of
    4: begin
      a:= ((i and $88888) shr 3);
      b:= ((i and $44444) shr 1);
      c:= ((i and $22222) shl 1);
      d:= ((i and $11111) shl 3);
      Result:= (a or b or c or d);
    end;
    3: begin
      a:= ((i and $4924) shr 2);
      b:= ((i and $2492) shr 0);
      c:= ((i and $1249) shl 2);
      Result:= (a or b or c);
    end;
    2: begin
      a:= ((i and $2AA) shr 1);
      b:= ((i and $155) shl 1);
      Result:= (a or b);
    end;
  end;
end;

function ReflectNS(i, height: integer): integer;
var
  a,b,c,d: integer;
begin
  Result:= 0;
  case height of
    4: begin
      a:= ((i and $1F) shl 15);
      b:= ((i and $3E0) shl 5);
      c:= ((i and $7C00) shr 5);
      d:= ((i and $F8000) shr 15);
      Result:= (a or b or c or d);
    end;
    3: begin
      a:= ((i and $1F) shl 10);
      b:= ((i and $3E0) shl 0);
      c:= ((i and $7C00) shr 10);
      Result:= (a or b or c);
    end;
    2: begin
      a:= ((i and $0001F) shl 5);
      b:= ((i and $003E0) shr 5);
      Result:= (a or b);
    end;
  end;
  Assert.AreEqual(PopCount(i), Popcount(Result),'ReflectNS does not preserve popcount, input: '+i.ToString);
end;

procedure TSliceTests.DoesConstructedNESWMatchTheLookupTable;
var
  i,a: integer;
begin
  Assert.IsTrue(true, 'Testing starts');
  // build the unknwon lookup table.
  if not(Form2.LookupTable.HasUnknownData) then begin
    Assert.IsTrue(true, 'Building lookup data for unknown states, please wait...');
    Form2.BtnCreateUnknownLookupTableClick(Form2);
    Assert.IsTrue(Form2.LookupTable.HasUnknownData, 'Lookup data for unknown states is done');
  end;
  // test to see if reflecting the data yields the same popcounts
  for i:= 0 to (1 shl 20)-1 do begin
    a:= ReflectEW(i,4);
    Assert.AreEqual(Form2.LookupTable[TOffset.Create(E1,0),i].PopCount,Form2.LookupTable[TOffset.Create(W1,0),a].PopCount,'E1.Popcount <> W1.Popcount at '+i.ToString);
    a:= ReflectNS(i,4);
    Assert.AreEqual(Form2.LookupTable[TOffset.Create(0,N1),i].PopCount,Form2.LookupTable[TOffset.Create(0,S1),a].PopCount,'N1.Popcount <> S1.Popcount at '+i.ToString);
  end;
  for i:= 0 to (1 shl 15)-1 do begin
    a:= ReflectEW(i,3);
    Assert.AreEqual(Form2.LookupTable[TOffset.Create(E2,0),i].PopCount
                   ,Form2.LookupTable[TOffset.Create(W2,0),a].PopCount
                   ,'E2.Popcount <> W2.Popcount at '+i.ToString);
    a:= ReflectNS(i,3);
    Assert.AreEqual(Form2.LookupTable[TOffset.Create(0,N2),i].PopCount
                   ,Form2.LookupTable[TOffset.Create(0,S2),a].PopCount
                   ,'N2.Popcount <> S2.Popcount at '+i.ToString);
  end;
  for i:= 0 to (1 shl 10)-1 do begin
    a:= ReflectEW(i,2);
    Assert.AreEqual(Form2.LookupTable[TOffset.Create(E3,0),i].PopCount
                   ,Form2.LookupTable[TOffset.Create(W3,0),a].PopCount
                   ,'E3.Popcount <> W3.Popcount at '+i.ToString);
    a:= ReflectNS(i,2);
    Assert.AreEqual(Form2.LookupTable[TOffset.Create(0,N3),i].PopCount
                   ,Form2.LookupTable[TOffset.Create(0,S3),a].PopCount
                   ,'N3.Popcount <> S3.Popcount at '+i.ToString);
  end;
end;

procedure TSliceTests.DoesConstructedNESWMatchTheLookupTable_NESW;
var
  Soll, Ist: TSlice;
  i: integer;
begin
  if not(Form2.LookupTable.HasUnknownData) then begin
    Assert.IsTrue(true, 'Building lookup data for unknown states, please wait...');
    Form2.BtnCreateUnknownLookupTableClick(Form2);
    Assert.IsTrue(Form2.LookupTable.HasUnknownData, 'Lookup data for unknown states is done');
  end;
  //TestN1E1 vs S1W1
  for i:= (1 shl (4*4))-1 downto 0 do begin
    Soll:= Form2.LookupTable[TOffset.Create(E1,N1),i];
    Ist:= Form2.LookupTable[TOffset.Create(W1,S1), ReflectNE_SW(i,4)];
    Assert.AreEqual(Soll.PopCount,Ist.PopCount,'E1N1.Popcount <> W1S1.Popcount at '+i.ToString);
  end;
  for i:= (1 shl (4*4))-1 downto 0 do begin
    Soll:= Form2.LookupTable[TOffset.Create(E1,S1),i];
    Ist:= Form2.LookupTable[TOffset.Create(W1,N1), ReflectNE_SW(i,4)];
    Assert.AreEqual(Soll.PopCount,Ist.PopCount,'E1S1.Popcount <> W1N1.Popcount at '+i.ToString);
  end;

  //TestN2E2 vs S2W2
  for i:= (1 shl (3*3))-1 downto 0 do begin
    Soll:= Form2.LookupTable[TOffset.Create(E2,N2),i];
    Ist:= Form2.LookupTable[TOffset.Create(W2,S2), ReflectNE_SW(i,3)];
    Assert.AreEqual(Soll.PopCount,Ist.PopCount,'E2N2.Popcount <> W2S2.Popcount at '+i.ToString);
  end;
  for i:= (1 shl (3*3))-1 downto 0 do begin
    Soll:= Form2.LookupTable[TOffset.Create(E2,S2),i];
    Ist:= Form2.LookupTable[TOffset.Create(W2,N2), ReflectNE_SW(i,3)];
    Assert.AreEqual(Soll.PopCount,Ist.PopCount,'E2S2.Popcount <> W2N2.Popcount at '+i.ToString);
  end;
end;

procedure TSliceTests.DoesTestingStart;
begin
  Assert.IsTrue(true, 'Testing starts');
end;

class procedure TSliceTests.FoldRemaining;
var
  Soll, Ist: TSlice;
  KnownMask,UnknownMask: integer;
begin
  if not(Form2.LookupTable.HasUnknownData) then begin
    Assert.IsTrue(true, 'Building lookup data for unknown states, please wait...');
    Form2.BtnCreateUnknownLookupTableClick(Form2);
    Assert.IsTrue(Form2.LookupTable.HasUnknownData, 'Lookup data for unknown states is done');
  end;
  //Start with N3 vs S3
  for KnownMask:= (1 shl (2*5))-1 downto 0 do begin
    for UnknownMask:= (1 shl (2*5))-1 downto 0 do begin
      if PopCount(UnknownMask) > 5 then continue; //Skip everything that would be overfull anyway.
      Soll:= Form2.FoldRemaining(TOffset.Create(0,N3), UnknownMask, KnownMask);
      Ist:= Form2.FoldRemaining(TOffset.Create(0,S3), ReflectNS(UnknownMask,2), ReflectNS(KnownMask,2));
      Assert.AreEqual(Soll.Popcount, Ist.Popcount, 'FoldRemaining N3.Popcount <> S3.Popcount at KnownMask: '+KnownMask.ToString+' and UnknownMask: '+UnknownMask.ToString);
    end;
  end;

  //next up with N2 vs S2
  for KnownMask:= (1 shl (3*5))-1 downto 0 do begin
    for UnknownMask:= (1 shl (3*5))-1 downto 0 do begin
      if PopCount(UnknownMask) > 3 then continue; //Skip everything that would be overfull anyway.
      Soll:= Form2.FoldRemaining(TOffset.Create(0,N2), UnknownMask, KnownMask);
      Ist:= Form2.FoldRemaining(TOffset.Create(0,S2), ReflectNS(UnknownMask,3), ReflectNS(KnownMask,3));
      Assert.AreEqual(Soll.Popcount, Ist.Popcount, 'FoldRemaining N2.Popcount <> S2.Popcount at KnownMask: '+KnownMask.ToString+' and UnknownMask: '+UnknownMask.ToString);
    end;
  end;
end;

procedure TSliceTests.MaskedBitsCreate;
var
  i,a,j: integer;
  Ist, Soll: TMaskedBits;
begin
  for i:= 0 to (1 shl 25)-1 do begin
    Ist:= TMaskedBits.Create(i);
    Soll.Length:= PopCount(i);
    j:= 0;
    for a:= 0 to 24 do begin
      if (((1 shl a) and i) <> 0) then begin
        Soll.FData[j]:= a;
        Inc(j);
      end;
    end;
    Assert.IsTrue(Ist = Soll,'MaskedBits does not match at '+i.ToString);
  end; {for i}
end;

class procedure TSliceTests.MaskedBitsFastTimer;
var
  i: integer;
  Ist: TMaskedBits;
begin
  Assert.IsTrue(true);
  for i:= 0 to (1 shl 25)-1 do begin
    Ist:= TMaskedBits.Create(i);
  end; {for i}
end;

class procedure TSliceTests.MaskedBitsSlowTimer;
var
  i,a,j: integer;
  Soll: TMaskedBits;
begin
  Assert.IsTrue(true);
  for i:= 0 to (1 shl 25)-1 do begin
    Soll.Length:= PopCount(i);
    j:= 0;
    for a:= 0 to 24 do begin
      if (((1 shl a) and i) <> 0) then begin
        Soll.FData[j]:= a;
        Inc(j);
      end;
    end;
  end; {for i}
end;

class procedure TSliceTests.TestSliceNextBitSet;
var
  TestSlice: TSlice;
  i,a: integer;
  Test, Soll, Ist: integer;
begin
  TestSlice:= TSlice.FullyUnknown;
  for i:= -1 to 510 do begin
    if (i+1 <> TestSlice.NextSetBit(i)) then begin
      TestSlice.NextSetBit(i);
    end;
    Assert.AreEqual(i+1, TestSlice.NextSetBit(i),'Fully unknown');
  end;
  Assert.AreEqual(512, TestSlice.NextSetBit(511));
  TestSlice.Clear;
  for i:= -1 to 510 do begin
    if (TestSlice.NextSetBit(i) <= 511) then begin
      TestSlice.NextSetBit(i);
    end;
    Assert.IsTrue(TestSlice.NextSetBit(i) > 511,'Fully clear');
  end;
  for a:= 0 to 10000 do begin
    for i:= 0 to 511 do begin
      TestSlice.SetBit(i, Odd(Random(512)));
    end;
    Test:= Random(513)-1;
    Soll:= 512;
    for i:= Test+1 to 511 do begin
      if (TestSlice.GetBit(i)) then begin
        Soll:= i;
        break;
      end;
    end; {for i}
    Ist:= TestSlice.NextSetBit(Test);
    if (Soll <> Ist) and ((Soll = 512) and (Ist <= 511)) then begin
      Ist:= TestSlice.NextSetBit(Test);
    end;
    if (Soll = 512) then Assert.IsTrue((Ist > 511),'Random data')
    else Assert.AreEqual(Soll, Ist,'Random data');
  end;
end;

class procedure TSliceTests.TestTMaskBitsCreate;
var
  x,y,i: integer;
  MaskedBits: TMaskedBits;
  Part1, Part2: integer;
begin
  for x:= -3 to 3 do for y:= -3 to 3 do begin
    MaskedBits:= TMaskedBits.Create(x,y);
    Part1:= Max(ABS(x),ABS(y)) * 5;
    Part2:= ((25 - Part1) div 5) * Min(ABS(x), ABS(y));
    Assert.AreEqual(MaskedBits.Count, Part1 + Part2);
    for i:= 0 to MaskedBits.Count-2 do begin
      Assert.IsTrue(MaskedBits[i] < MaskedBits[i+1], 'MaskedBits.Create soll: a<b, but a= '+MaskedBits[i].ToString+' and b= '+MaskedBits[i+1].ToString);
    end;
  end;
end;

class procedure TSliceTests.TestUnknownFolding;
var
  Soll, Ist: TSlice;
  Offset: TOffset;
  i: integer;
  UnknownMask, KnownMask: integer;
begin
  if not(Form2.LookupTable.HasUnknownData) then begin
    Assert.IsTrue(true, 'Building lookup data for unknown states, please wait...');
    Form2.BtnCreateUnknownLookupTableClick(Form2);
    Assert.IsTrue(Form2.LookupTable.HasUnknownData, 'Lookup data for unknown states is done');
  end;
  for i:= 0 to (1 shl 20)-1 do begin
    KnownMask:= i;
    Soll:= Form2.LookupTable[TOffset.Create(0,N1),KnownMask];
    Offset:= TOffset(0);
    UnknownMask:= $1F; //North;
    Ist:= Form2.FoldRemaining(Offset, UnknownMask, (KnownMask shl 5));
    Assert.IsTrue(Soll = Ist,'Mismatch between lookup and FoldRemaining at '+i.ToString);
  end;
end;

const
  TimingRepetitions = (1000*1000)-1;//100000000-1;

class procedure TSliceTests.TSlice_NextSetBit;
var
  NextA, NextB: integer;
  S: TSlice;
  i,j: integer;
  a,b: integer;
  previousSet: integer;
begin
  for i:= 0 to (TimingRepetitions div 100) do begin
    S:= TSlice.Random;
    PreviousSet:= -1;
    for j:= 0 to 511 do begin
      if S.GetBit(j) then begin
        b:= S.NextSetBit(j-1);
        if (b <> j) then begin
          b:= S.NextSetBit(j-1);
        end;
        Assert.AreEqual(j,b);
        b:= S.NextSetBit(PreviousSet);
        if (b <> j) then begin
          b:= S.NextSetBit(PreviousSet);
        end;
        Assert.AreEqual(j,b);
        PreviousSet:= j;
      end;
    end; {for j}
  end; {for i}
end;

class procedure TSliceTests.TimeFastWest;
var
  A: TSliver;
  i: integer;
begin
  for i:= 0 to TimingRepetitions do A.West;
  Assert.IsTrue(true);
end;

class procedure TSliceTests.DoubleCheckUnknownTable;
var
  Soll, Ist, Diff: TSlice;
  i,a: integer;
  Offset: TOffset;
  UnknownBits: TMaskedBits;
begin
  //Test N1
  Offset:= TOffset.Create(0,N1);
  UnknownBits:= TMaskedBits.Create(0,N1);
  for i:= 0 to (1 shl (25 - UnknownBits.Count))-1 do begin
    Ist:= Form2.LookupTable[oCenter, (i shl UnknownBits.Count)];
    for a:= 1 to (1 shl UnknownBits.Count)-1 do begin
      Ist:= Ist or Form2.LookupTable[oCenter, (i shl UnknownBits.Count) + a];
    end; {for a}
    Soll:= Form2.LookupTable[Offset, i];
    if (Soll <> Ist) then begin
      Diff:= Soll xor Ist;
    end;
    Assert.AreEqual(Soll, Ist);
  end; {for i}

  //Test N2
  Offset:= TOffset.Create(0,N2);
  UnknownBits:= TMaskedBits.Create(0,N2);
  for i:= 0 to (1 shl (25 - UnknownBits.Count))-1 do begin
    Ist:= Form2.LookupTable[oCenter, (i shl UnknownBits.Count)];
    for a:= 1 to (1 shl UnknownBits.Count)-1 do begin
      Ist:= Ist or Form2.LookupTable[oCenter, (i shl UnknownBits.Count) + a];
    end; {for a}
    Soll:= Form2.LookupTable[Offset, i];
    if (Soll <> Ist) then begin
      Diff:= Soll xor Ist;
    end;
    Assert.AreEqual(Soll, Ist);
  end; {for i}

  //Test N3
  Offset:= TOffset.Create(0,N3);
  UnknownBits:= TMaskedBits.Create(0,N3);
  for i:= 0 to (1 shl (25 - UnknownBits.Count))-1 do begin
    Ist:= Form2.LookupTable[oCenter, (i shl UnknownBits.Count)];
    for a:= 1 to (1 shl UnknownBits.Count)-1 do begin
      Ist:= Ist or Form2.LookupTable[oCenter, (i shl UnknownBits.Count) + a];
    end; {for a}
    Soll:= Form2.LookupTable[Offset, i];
    if (Soll <> Ist) then begin
      Diff:= Soll xor Ist;
    end;
    Assert.AreEqual(Soll, Ist);
  end; {for i}

end;



var
  Data1: array[1..6400] of byte;
  Data2: array[1..6400] of byte;

class procedure TSliceTests.TimeMoveFast;
var
  i: integer;
begin
  for i:= 0 to TimingRepetitions div 1000 do Unit2.Move(Data1,Data2, SizeOf(Data1) div 100);
end;

class procedure TSliceTests.TimeMoveSlow;
var
  i: integer;
begin
  for i:= 0 to TimingRepetitions div 1000 do System.Move(Data1,Data2, SizeOf(Data1) div 100);
end;

class procedure TSliceTests.TimeFastEast;
var
  A: TSliver;
  i: integer;
begin
  for i:= 0 to TimingRepetitions do A.East;
  Assert.IsTrue(true);
end;

class procedure TSliceTests.TimeFastNorth;
var
  A: TSliver;
  i: integer;
begin
  for i:= 0 to TimingRepetitions do A.North;
  Assert.IsTrue(true);
end;

class procedure TSliceTests.TimeFastNSFolding;
var
  N,S: TSlice;
  Status: TSliverChanges;
  i: integer;
begin
  for i:= 0 to TimingRepetitions do TSliver.NS(N,S, status);
  Assert.IsTrue(true);
end;

class procedure TSliceTests.TimeSlowWest;
var
  A: TSliver;
  i: integer;
begin
  for i:= 0 to TimingRepetitions do A.SlowWest;
  Assert.IsTrue(true);
end;

class procedure TSliceTests.Time_TSliceAnd;
var
  A,B,C: TSlice;
  i,j: integer;
begin
  A:= TSlice.Random;
  B:= TSlice.Random;
  for i:= 0 to (TimingRepetitions * 100) do begin
    C:= A and B;
  end;
  Assert.AreEqual(not(C), not(B) or not(A));
end;

class procedure TSliceTests.Time_TSliceOr;
var
  A,B,C: TSlice;
  i,j: integer;
begin
  A:= TSlice.Random;
  B:= TSlice.Random;
  for i:= 0 to (TimingRepetitions * 100) do begin
    C:= A or B;
  end;
  Assert.AreEqual(not(C), not(B) and not(A));
end;

class procedure TSliceTests.TSlice_BitwiseAnd;
var
  A,B,C,D: TSlice;
  i,j: integer;
begin
  for i:= 0 to TimingRepetitions do begin
    A:= TSlice.Random;
    B:= TSlice.Random;
    C:= A and B;
    for j:= 0 to 7 do begin
      D.data8[j]:= A.data8[j] and B.data8[j];
    end;
    Assert.AreEqual(C,D);
  end;
end;

class procedure TSliceTests.TSlice_BitwiseOr;
var
  A,B,C,D: TSlice;
  i,j: integer;
begin
  for i:= 0 to TimingRepetitions do begin
    A:= TSlice.Random;
    B:= tSlice.Random;
    C:= A or B;
    for j:= 0 to 7 do begin
      D.data8[j]:= A.data8[j] or B.data8[j];
    end;
    Assert.AreEqual(C,D);
  end;
end;

class procedure TSliceTests.TSlice_BitwiseXor;
var
  A,B,C,D: TSlice;
  i,j: integer;
begin
  for i:= 0 to TimingRepetitions do begin
    A:= TSlice.Random;
    B:= tSlice.Random;
    C:= A xor B;
    for j:= 0 to 7 do begin
      D.data8[j]:= A.data8[j] xor B.data8[j];
    end;
    Assert.AreEqual(C,D);
  end;
end;

class procedure TSliceTests.TSlice_Equal;
var
  A,B,C,D: TSlice;
  i,j: integer;
  pc: integer;
begin
  for i:= 0 to TimingRepetitions do begin
    A:= TSlice.Random;
    B:= A;
    B.SetBit(Random(511));
    if (A = B) then for j:= 0 to 7 do begin
      Assert.IsTrue(A.Data8[j] = B.data8[j]);
    end else begin
      pc:= 0;
      for j:= 0 to 7 do begin
        Inc(pc, Popcount(A.Data8[j] xor B.data8[j]));
      end; {for j}
    end;
    Assert.IsTrue(A = A);
  end; {for i}
end;

class procedure TSliceTests.TSlice_GetBit;
var
  A,B: TSlice;
  i,j: integer;
  pc: integer;
  R: integer;
  Bit: UInt64;
  Index: integer;
  BitA, BitB: boolean;
begin
  for i:= 0 to TimingRepetitions do begin
    A:= TSlice.Random;
    R:= Random(511);
    Bit:= 1;
    Bit:= Bit shl (R and 63);
    Index:= (R shr 6);
    BitA:= A.GetBit(R);
    BitB:= ((A.Data8[Index] and Bit) <> 0);
    Assert.AreEqual(BitA, BitB);
  end; {for i}
end;

class procedure TSliceTests.TSlice_IsZero;
var
  A: TSlice;
  i,j: integer;
  Test: Uint64;
  Zero: Uint64;
begin
  Zero:= 0;
  for i:= 0 to TimingRepetitions do begin
    A:= TSlice.Random;
    Test:= 0;
    for j:= 0 to 7 do begin
      Test:= Test or A.Data8[j];
    end; {for j}
    if (A.IsZero) then begin
      if (Zero <> Test) then begin
        Test:= 0;
        for j:= 0 to 7 do begin
          Test:= Test or A.Data8[j];
        end; {for j}
      end;
      Assert.AreEqual(Zero, Test)
    end else Assert.IsTrue(0 <> Test);
  end; {for i}
end;

class procedure TSliceTests.TSlice_LogicalNot;
var
  A,B: TSlice;
  i,j: integer;
  Test: Uint64;
begin
  for i:= 0 to TimingRepetitions do begin
    A:= TSlice.Random;
    B:= not(A);
    Test:= 0;
    for j:= 0 to 7 do begin
      if (B.Data8[j] <> not(A.Data8[j])) then begin
        B:= A;
        A:= not(B);
        Assert.AreEqual(B.Data8[j], not(A.Data8[j]));
      end;
    end; {for j}
  end; {for i}
  Assert.IsTrue(true);
end;

class procedure TSliceTests.TSlice_Popcount;
var
  A,B: TSlice;
  i,j: integer;
  pc: integer;
begin
  for i:= 0 to TimingRepetitions do begin
    A:= TSlice.Random;
    pc:= 0;
    for j:= 0 to 7 do begin
      Inc(pc, PopCount(A.data8[j]));
    end; {for j}
    if (A.PopCount <> pc) then begin
      pc:= 0;
      for j:= 0 to 7 do begin
        Inc(pc, PopCount(A.data8[j]));
      end; {for j}
      pc:= pc - A.PopCount;
    end;
    Assert.AreEqual(A.PopCount, pc);
  end; {for i}
end;

class procedure TSliceTests.TSlice_SetBit;
var
  A,B: TSlice;
  i,j: integer;
  pc: integer;
  R: integer;
  Bit: UInt64;
  Index: integer;
begin
  for i:= 0 to TimingRepetitions do begin
    A:= TSlice.Random;
    R:= Random(511);
    Bit:= 1;
    Bit:= Bit shl (R and 63);
    Index:= (R shr 6);
    B:= A;
    B.Data8[index]:= B.Data8[index] or Bit;
    A.SetBit(R);
    if not(A = B) then begin
      A.SetBit(R);
    end;
    Assert.AreEqual(A,B);
    B.Data8[index]:= B.Data8[index] and not(Bit);
    A.SetBit(R, false);
    if not(A = B) then begin
      A.SetBit(R,false);
    end;
    Assert.AreEqual(A,B);
  end; {for i}
end;

class procedure TSliceTests.TimeSlowEast;
var
  A: TSliver;
  i: integer;
begin
  //for i:= 0 to TimingRepetitions do A.East;
  Assert.IsTrue(true);
end;


class procedure TSliceTests.VerifyFastWest;
var
  A,B: TSlice;
  S: TSliver;
  i: integer;
begin
  for i:= 0 to TimingRepetitions div 5 do begin
    S:= Random64;
    A:= S.SlowWest;
    B:= S.West;
    if (A <> B) then begin
      A:= S.SlowWest;
      B:= S.West;
    end;
    Assert.AreEqual(A,B,'slices do not match');
  end;
end;

class procedure TSliceTests.VerifyFastEast;
var
  A,B: TSlice;
  S: TSliver;
  i: integer;
begin
  for i:= 0 to TimingRepetitions div 5 do begin
    S:= Random64;
    A:= S.SlowEast;
    B:= S.East;
    if (A <> B) then begin
      A:= S.SlowEast;
      B:= S.East;
    end;
    Assert.AreEqual(A,B,'slices do not match');
  end;
end;

class procedure TSliceTests.VerifyFastNorth;
var
  A,B: TSlice;
  S: TSliver;
  i: integer;
begin
  for i:= 0 to TimingRepetitions div 5 do begin
    S:= Random64;
    A:= S.SlowNorth;
    B:= S.North;
    if (A <> B) then begin
      A:= S.SlowNorth;
      B:= S.North;
    end;
    Assert.AreEqual(A,B,'slices do not match');
  end;
end;

class procedure TSliceTests.VerifyFastSouth;
var
  A,B: TSlice;
  S: TSliver;
  i: integer;
begin
  for i:= 0 to TimingRepetitions div 5 do begin
    S:= Random64;
    A:= S.SlowSouth;
    B:= S.South;
    if (A <> B) then begin
      A:= S.SlowSouth;
      B:= S.South;
    end;
    Assert.AreEqual(A,B,'slices do not match');
  end;
end;

class procedure TSliceTests.TimeSlowNorth;
var
  A: TSliver;
  i: integer;
begin
  for i:= 0 to TimingRepetitions do A.SlowNorth;
  Assert.IsTrue(true);
end;

class procedure TSliceTests.TimeSlowNSFolding;
var
  N,S: TSlice;
  Status: TSliverChanges;
  i: integer;
begin
  for i:= 0 to TimingRepetitions do TSliver.NSSlow(N,S, status);
  Assert.IsTrue(true);
end;

class procedure TSliceTests.TimeFastEWFolding;
var
  N,S: TSlice;
  Status: TSliverChanges;
  i: integer;
begin
  for i:= 0 to (TimingRepetitions) do TSliver.EW(N,S, status);
  Assert.IsTrue(true);
end;

class procedure TSliceTests.TimeSlowEWFolding;
var
  N,S: TSlice;
  Status: TSliverChanges;
  i: integer;
begin
  for i:= 0 to (TimingRepetitions) do TSliver.EWSlow(N,S, status);
  Assert.IsTrue(true);
end;

class procedure TSliceTests.ValidateFastNSFolding;
var
  N,S: TSlice;
  a,b: TSliver;
  StatusA, StatusB: TSliverChanges;
  i: integer;
begin
  for i:= 0 to (TimingRepetitions * 100) do begin
    N:= TSlice.Random;
    S:= TSlice.Random;
    a:= TSliver.NSSlow(N,S, statusA);
    b:= TSliver.NS(N,S, statusB);
    if (a <> b) then begin
      TSliver.NSSlow(N,S, statusA);
      TSliver.NS(N,S, statusB);
    end;
    Assert.AreEqual(a,b,'TSliver.NSFast produces incorrect results');
    if (StatusA <> StatusB) then begin
      TSliver.NSSlow(N,S, statusA);
      TSliver.NS(N,S, statusB);
    end;
    Assert.AreEqual(StatusA,StatusB,'TSliver.NSFast produces incorrect status');
  end;
end;

class procedure TSliceTests.ValidateUnknownFuture;
begin
  var Ist:= TGridBitmap.Full;
  var Soll:= TGridBitmap.Full;
  Ist.UnknownFuture;
  Assert.AreEqual(Soll, Ist);
end;

class procedure TSliceTests.ValidateFastEWFolding;
var
  E,W: TSlice;
  a,b: TSliver;
  StatusA, StatusB: TSliverChanges;
  i: integer;
begin
  for i:= 0 to (TimingRepetitions*100) do begin
    E:= TSlice.Random;
    W:= TSlice.Random;
    a:= TSliver.EWSlow(E,W, statusA);
    b:= TSliver.EW(E,W, statusB);
    if (a <> b) then begin
      TSliver.EWSlow(E,W, statusA);
      TSliver.EW(E,W, statusB);
    end;
    Assert.AreEqual(a,b,'TSliver.EWFast produces incorrect results');
    if (StatusA <> StatusB) then begin
      a:= TSliver.EWSlow(E,W, statusA);
      b:= TSliver.EW(E,W, statusB);
    end;
    Assert.AreEqual(StatusA, StatusB,'TSliver.EW gives incorrect status');
  end;
end;

initialization
  TDunitx.RegisterTestFixture(TSliceTests);
end.
