program emvpcsc;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {Form1},
  PCSCConnector in 'PCSCConnector.pas',
  SCardErr in 'SCardErr.pas',
  WinSCard in 'WinSCard.pas',
  WinSmCrd in 'WinSmCrd.pas',
  defs in 'defs.pas',
  CardUtils in 'CardUtils.pas',
  EMVsys in 'EMVsys.pas',
  TLVsys in 'TLVsys.pas',
  EMVconst in 'EMVconst.pas',
  EMVkeys in 'EMVkeys.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
