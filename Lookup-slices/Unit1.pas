unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;


type
  TSyntaxNodeType = (ntAddr,ntDeref);



  SyntaxNodeNames = record //<<-------- fails

  //change record to class and it works.

  strict private

    class var FData: array[TSyntaxNodeType] of string;

    class function GetItem(const index: TSyntaxNodeType): string; static; inline;

    class constructor Init;

  public

    class property Items[const index: TSyntaxNodeType]: string read GetItem; default;

  end;


  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    FileOpenDialog1: TFileOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    Buffer: array of integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses TypInfo;



{ TSyntaxNodeNames }



class function SyntaxNodeNames.GetItem(const index: TSyntaxNodeType): string;

begin

  Result:= FData[index];

end;



class constructor SyntaxNodeNames.Init;

var

  value: TSyntaxNodeType;

begin

  for value := Low(TSyntaxNodeType) to High(TSyntaxNodeType) do

    FData[value] := Copy(LowerCase(GetEnumName(TypeInfo(TSyntaxNodeType), Ord(value))), 3);

end;



procedure TForm1.Button1Click(Sender: TObject);

begin

  Button1.Caption := SyntaxNodeNames[ntAddr];

end;


function PopCount(input: integer): integer;
asm
  //EdX = input
  //eax = output
  popcnt eax,eax
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  FS: TFileStream;
  i,a: Integer;
  Count: uint64;
  Len: integer;
  Max: uint64;
begin

  if FileOpenDialog1.Execute then begin
    FS:= TFileStream.Create(FileOpenDialog1.FileName, fmOpenRead);
    SetLength(Buffer, FS.Size div SizeOf(integer));
    FS.Read64(TBytes(Buffer), 0, FS.Size);
    Len:= FS.Size;
    FS.Free;
    Count:= 0;
    for i := 0 to (Len div SizeOf(integer))-1 do begin
      Inc(Count, Popcount(Buffer[i]));
    end;
    Max:= 512*1024*1024; Max:= Max * 8;
    Button2.Caption:= Count.ToString + ' ' + ((Len div (256 div 8))).ToString + ' '+((count / Max)*100).ToString+'%';
  end;
end;

end.

