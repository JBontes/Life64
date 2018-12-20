program Project2;

uses
  TestInsight.Client,
  TestInsight.DUnitx,
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  UnitTests in 'UnitTests.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
