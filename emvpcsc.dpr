program emvpcsc;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {fPOS},
  PCSCConnector in 'PCSCConnector.pas',
  SCardErr in 'SCardErr.pas',
  WinSCard in 'WinSCard.pas',
  WinSmCrd in 'WinSmCrd.pas',
  defs in 'defs.pas',
  CardUtils in 'CardUtils.pas',
  EMVsys in 'EMVsys.pas',
  TLVsys in 'TLVsys.pas',
  EMVconst in 'EMVconst.pas',
  EMVkeys in 'EMVkeys.pas',
  Chiphers in 'Chiphers.pas',
  LbCipher in 'lockbox2\LbCipher.pas',
  LbRSA in 'lockbox2\LbRSA.pas',
  LbString in 'lockbox2\LbString.pas',
  LbUtils in 'lockbox2\LbUtils.pas',
  LbBigInt in 'lockbox2\LbBigInt.pas',
  LbConst in 'lockbox2\LbConst.pas',
  LbAsym in 'lockbox2\LbAsym.pas',
  LbClass in 'lockbox2\LbClass.pas',
  LbProc in 'lockbox2\LbProc.pas',
  EMVRec in 'EMVRec.pas',
  VISAVirtualBank in 'VISAVirtualBank.pas',
  LbRandom in 'lockbox2\LbRandom.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfPOS, fPOS);
  Application.Run;
end.
