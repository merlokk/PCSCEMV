{******************************************************************}
{                                                                  }
{ PC/SC Interface component                                        }
{ Helps you access a cardreader through Microsofts SmartCard API   }
{                                                                  }
{ The Original Code is PCSCConnector.pas                           }
{                                                                  }
{ The Initial Developer of the Original Code is                    }
{ Norbert Huettisch (nobbi(at)nobbi.com)                           }
{                                                                  }
{ Any suggestions and improvements to the code are appreciated     }
{                                                                  }
{ This Code uses a modified   SCardErr.pas (included)              }
{ This Code uses a modified   WinSCard.pas (included)              }
{ This code uses the original WinSmCrd.pas (included)              }
{                                                                  }
{ All originally made by Chris Dickerson (chrisd(at)tsc.com),      }
{ available as 'Interface units for the Microsoft Smart Card API'  }
{ at the Project JEDI Homepage http://www.delphi-jedi.org          }
{                                                                  }
{ Version info:                                                    }
{ 021230 - initial version                                         }
{ 030101 - routed errors from 'init' to the OnError event          }
{                                                                  }
{                                                                  }
{******************************************************************}
{                                                                  }
{ The contents of this file are subject to the                     }
{                                                                  }
{       Mozilla Public License Version 1.1 (the "License")         }
{                                                                  }
{ You may not use this file except in compliance with the License. }
{ You may obtain a copy of the License at                          }
{ http://www.mozilla.org/MPL/                                      }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

unit PCSCConnector;

interface

uses
  Windows, Messages, Forms, Classes, SysUtils,
  SCardErr, WinSCard, WinSmCrd, CardUtils, defs;

type
  TErrSource         = (esInit, esConnect, esGetStatus, esTransmit);
  TNeededPIN         = (npPIN1, npPIN2, npPUK1, npPUK2);
  TDelimiters        = set of Char;

  TPCSCErrorEvent    = procedure(Sender: TObject; ErrSource: TErrSource; ErrCode: cardinal) of object;
  TPCSCPinEvent      = procedure(Sender: TObject; NeedPIN: TNeededPIN) of object;

const
  MAXAPDULENGTH      = 260; // CLA + INS + P1..3 + 255Bytes
  NOREADERSELECTED   = -1;
  SCARD_PCI_T0       : SCARD_IO_REQUEST = (dwProtocol:1; dbPciLength:8);
  SCARD_PCI_T1       : SCARD_IO_REQUEST = (dwProtocol:2; dbPciLength:8);
  SCARD_PROTOCOL_T0  = $00000001;
  SCARD_PROTOCOL_T1  = $00000002;
  SCARD_PROTOCOL_RAW = $00010000;
  SCARD_PROTOCOL_UNK = $00000000;

  WM_CARDSTATE     = WM_USER + 42;

  GSMStatusOK           = $9000;
  GSMStatusMemoryError  = $9240;
  GSMStatusNoEFSelected = $9400;
  GSMStatusOutOfRange   = $9402;
  GSMStatusNotFound     = $9404;
  GSMStatusFCDoNotMatch = $9408;
  GSMStatusCHVNeeded    = $9802;
  GSMStatusAuthFailed   = $9804;
  GSMStatusAuthFailedBl = $9840;
  GSMStatusTechProblem  = $6F00;
  GSMStatusResponseData = $9F;

  GSMFileTypeRFU = 0;
  GSMFileTypeMF  = 1;
  GSMFileTypeDF  = 2;
  GSMFileTypeEF  = 4;

  GSMEfTransp    = 0;
  GSMEfLinFixed  = 1;
  GSMEfCyclic    = 3;

type
  TPCSCConnector = class(TComponent)
  private
    function GetAttrATRHistBytes: string;
    function GetATR: ATRRec;

  protected
    FContext            : cardinal;
    FCardHandle         : integer;
    FConnected          : boolean;
    FNumReaders         : integer;
    FUseReaderNum       : integer;

    FReaderList         : TStringlist;

    FAPDULogging        : boolean;

    FAttrProtocol       : integer;
    FAttrICCType        : string;
    FAttrCardATR        : AnsiString;
    FAttrVendorName     : string;
    FAttrVendorSerial   : string;
    FAttrInterfaceStatus: integer;
    FAttrDefaultDataRate: integer;
    FAttrDefaultCLK     : integer;

    FGSMCurrentFile     : string;
    FGSMFileInfo        : string;
    FGSMDirInfo         : string;
    FGSMVoltage30       : boolean;
    FGSMVoltage18       : boolean;

    FOnReaderWaiting    : TNotifyEvent;
    FOnReaderListChange : TNotifyEvent;
    FOnCardInserted     : TNotifyEvent;
    FOnCardActive       : TNotifyEvent;
    FOnCardRemoved      : TNotifyEvent;
    FOnCardInvalid      : TNotifyEvent;
    FOnError            : TPCSCErrorEvent;
    FOnCHVNeeded        : TPCSCPinEvent;

    procedure SetReaderNum(Value: integer);
    procedure MessageWndProc(var Msg: TMessage);
    function  ConnectSelectedReader: boolean;
    procedure ProcessReaderState(const OldState,NewState: cardinal);
    procedure GetReaderAttributes;
    procedure GetCardAttributes;
    procedure ClearReaderAttributes;
    procedure ClearCardAttributes;
    function  IsReaderOpen: boolean;
    function  GetReaderState: cardinal;
    procedure CloseAndDisconnect;
    procedure CardInsertedAction;
    procedure CardActiveAction;
    procedure CardRemovedAction;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    Init: boolean;
    function    Open: boolean;
    procedure   Close;
    function    Connect: boolean;
    procedure   Disconnect;
    function    GetResponseFromCard(const apdu: AnsiString): AnsiString; overload;
    function    GetResponseFromCard(const command: AnsiString; var data: AnsiString; var sw: word): boolean; overload;

    function    NXPBoot(var sw: Word): boolean;
    function    NXPProtect(var sw: Word): boolean;
    function    NXPFuse(var sw: Word): boolean;
    function    NXPRead(addr: cardinal; len: byte; var sw: Word): AnsiString;
    function    NXPWrite(addr: cardinal; len: byte; data: AnsiString; var sw: Word): AnsiString;

    function    CardSelect(const aid: AnsiString; var sw: Word): AnsiString;
    function    ReadSFIRecord(sfi, rnum: byte; var sw: Word): AnsiString;
    function    GetData(id: AnsiString; var sw: Word): AnsiString;
    function    InternalAuthenticate(data: AnsiString; var sw: Word): AnsiString;
    function    ExternalAuthenticate(data: AnsiString; var sw: Word): AnsiString;
    function    VerifyPIN(data: AnsiString; refdata: byte;var sw: Word): AnsiString;
    function    GetChallenge(var sw: Word): AnsiString;
    function    GenerateAC(RefControl: byte; data: AnsiString; var sw: Word): AnsiString;

  published
    property UseReaderNum: integer    read FUseReaderNum    write SetReaderNum  default -1;
    property APDULogging: boolean     read FAPDULogging     write FAPDULogging  default false;

    property OnCardInserted:     TNotifyEvent    read FOnCardInserted     write FOnCardInserted;
    property OnCardActive:       TNotifyEvent    read FOnCardActive       write FOnCardActive;
    property OnCardRemoved:      TNotifyEvent    read FOnCardRemoved      write FOnCardRemoved;
    property OnCardInvalid:      TNotifyEvent    read FOnCardInvalid      write FOnCardInvalid;
    property OnReaderWaiting:    TNotifyEvent    read FOnReaderWaiting    write FOnReaderWaiting;
    property OnReaderListChange: TNotifyEvent    read FOnReaderListChange write FOnReaderListChange;
    property OnError:            TPCSCErrorEvent read FOnError            write FOnError;
    property OnCHVNeeded:        TPCSCPinEvent   read FOnCHVNeeded        write FOnCHVNeeded;

    property ReaderList:       TStringList read FReaderList;
    property NumReaders:       integer     read FNumReaders;
    property Connected:        boolean     read FConnected;
    property Opened:           boolean     read IsReaderOpen;
    property ReaderState:      cardinal    read GetReaderState;
    property AttrProtocol:     integer     read FAttrProtocol;
    property AttrICCType:      string      read FAttrICCType;
    property AttrCardATR:      AnsiString  read FAttrCardATR;
    property AttrATRHistBytes: string      read GetAttrATRHistBytes;
    property AttrATR:          ATRRec      read GetATR;
    property AttrVendorName:   string      read FAttrVendorName;
    property AttrVendorSerial: string      read FAttrVendorSerial;
    property AttrInterfaceStatus: integer  read FAttrInterfaceStatus;
    property AttrDefaultDataRate: integer  read FAttrDefaultDataRate;
    property AttrDefaultCLK: integer       read FAttrDefaultCLK;
  end;

procedure Register;

implementation

var
  ActReaderState  : cardinal;
  LastReaderState : cardinal;
  SelectedReader  : PWideChar;
  ReaderOpen      : boolean;
  NotifyHandle    : HWND;

const

  // GSM Commands
  GCGetStatus: AnsiString   = #$A0#$F2#$00#$00#$16;
  GCGetResponse:AnsiString = #$00#$C0#$00#$00;
  GCSelectFile:AnsiString  = #$A0#$A4#$00#$00#$02;
  GCReadBinary:AnsiString  = #$A0#$B0;

  GSMMasterFile:AnsiString  = #$3f#$00;
  DFgsm900:AnsiString       = #$7f#$20;
  DFgsm1800:AnsiString      = #$7f#$21;

procedure Register;
begin
  RegisterComponents('More...', [TPCSCConnector]);
end;

function SortOutSubstrings(const From:string; var t:array of string; const Delim:TDelimiters = [' ',';']; const ConcatDelim:boolean = true):integer;
var a,b,s,i : integer;
    sep     : boolean;
begin
a := 1;
b := Low(t);
s := 1;
i := 0;
sep := ConcatDelim;
t[b] := '';

while a <= Length(From) do
  begin
  if not (From[a] in Delim) then
     begin
     Inc(i);
     sep := false;
     end else
     begin
     if not sep then
        begin
        t[b] := Copy(From, s, i);
        Inc(b);
        if b > High(t) then Break;
        t[b] := '';
        end;
     if ConcatDelim then sep := true;
     s := a + 1;
     i := 0;
     end;
  Inc(a);
  end;
if (b <= High(t)) and (i > 0) then
   begin
   t[b] := Copy(From, s, i);
   Inc(b);
   end;
for a := b + 1 to High(t) do t[a] := '';
Result := b;
end;

function OrdD(const From: string; const Index: integer): integer;
begin
if Index <= Length(From) then Result := Ord(From[Index])
                         else Result := 0;
end;

function CardWatcherThread(PContext: pointer): integer;
var
  RetVar   : cardinal;
  RContext : cardinal;
  RStates  : array[0..1] of SCARD_READERSTATEW;
begin
  try
  RContext := cardinal(PContext^);
  FillChar(RStates,SizeOf(RStates),#0);
  RStates[0].szReader     := SelectedReader;
  RStates[0].pvUserData   := nil;
  RStates[0].dwEventState := ActReaderState;
  while ReaderOpen do
    begin
    RStates[0].dwCurrentState := RStates[0].dwEventState;
    RetVar := SCardGetStatusChangeW(RContext, -1, RStates, 1);
    ActReaderState := RStates[0].dwEventState;
    PostMessage(NotifyHandle, WM_CARDSTATE, RetVar, 0);
    end;
  finally
    Result := 0;
  end;
end;

procedure TPCSCConnector.MessageWndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_CARDSTATE) then
    begin
    if Msg.WParam <> SCARD_S_SUCCESS then
      if Assigned(FOnError) then FOnError(Self, esGetStatus, Msg.WParam);
    if ActReaderState <> LastReaderState then
      begin
      ProcessReaderState(LastReaderState, ActReaderState);
      end;
    end
    else Msg.Result := DefWindowProc(NotifyHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

function TPCSCConnector.NXPBoot(var sw: Word): boolean;
var
  res: AnsiString;
begin
  Result := false;
  if not GetResponseFromCard(#$00#$F0#$00#$00, res, sw) then exit;

  if sw = $9000 then Result := true;
end;

function TPCSCConnector.NXPFuse(var sw: Word): boolean;
var
  res: AnsiString;
begin
  Result := false;
  if not GetResponseFromCard(#$00#$00#$00#$00, res, sw) then exit;

  if sw = $9000 then Result := true;
end;

function TPCSCConnector.NXPProtect(var sw: Word): boolean;
var
  res: AnsiString;
begin
  Result := false;
  if not GetResponseFromCard(#$00#$10#$00#$00, res, sw) then exit;

  if sw = $9000 then Result := true;
end;

function TPCSCConnector.NXPRead(addr: cardinal; len: byte; var sw: Word): AnsiString;
var
  a0,a1,a2: AnsiChar;
begin
  Result := '';

  a0 := AnsiChar((addr and $0000FF));
  a1 := AnsiChar((addr and $00FF00) shr 8);
  a2 := AnsiChar((addr and $FF0000) shr 16);

  if not GetResponseFromCard(a2 + #$B0 + a1 + a0 + AnsiChar(len), Result, sw) then
    Result := '';
end;

function TPCSCConnector.NXPWrite(addr: cardinal; len: byte; data: AnsiString;
  var sw: Word): AnsiString;
var
  a0,a1,a2: AnsiChar;
begin
  Result := data;

  a0 := AnsiChar((addr and $0000FF));
  a1 := AnsiChar((addr and $00FF00) shr 8);
  a2 := AnsiChar((addr and $FF0000) shr 16);

  if not GetResponseFromCard(a2 + #$D6 + a1 + a0 + AnsiChar(len), Result, sw) then
    Result := '';
end;

constructor TPCSCConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReaderList   := TStringlist.Create;
  FContext      := 0;
  FCardHandle   := 0;
  FNumReaders   := 0;
  FUseReaderNum := -1;
  FConnected    := false;
  FAPDULogging  := false;
  ActReaderState  := SCARD_STATE_UNAWARE;
  LastReaderState := SCARD_STATE_UNAWARE;
  ReaderOpen      := false;
  ClearReaderAttributes;
  ClearCardAttributes;
  if not (csDesigning in ComponentState) then NotifyHandle := AllocateHWnd(MessageWndProc);
end;

destructor TPCSCConnector.Destroy;
begin
  CloseAndDisconnect;
  SCardReleaseContext(FContext);
  FReaderList.Free;
  if not (csDesigning in ComponentState) then DeallocateHWnd(NotifyHandle);
  inherited Destroy;
end;

function TPCSCConnector.Init: boolean;
var
  RetVar         : cardinal;
  ReaderList     : string;
  ReaderListSize : integer;
  v              : array[0..MAXIMUM_SMARTCARD_READERS] of string;
  i              : integer;

begin
  Result      := false;
  FNumReaders := 0;
  CloseAndDisconnect;
  if SCardIsValidContext(FContext) = SCARD_S_SUCCESS then SCardReleaseContext(FContext);
  RetVar := SCardEstablishContext(SCARD_SCOPE_USER, nil, nil, @FContext);
  if RetVar = SCARD_S_SUCCESS then
    begin
    ReaderListSize := 0;
    RetVar := SCardListReadersW(FContext, nil, nil, ReaderListSize);
    if RetVar = SCARD_S_SUCCESS then
      begin
      SetLength(ReaderList, ReaderListSize);
      SCardListReadersW(FContext, nil, Pointer(ReaderList), ReaderListSize);
      FReaderList.Clear;
      SortOutSubstrings(ReaderList,v,[#0]);
      for i := 0 to MAXIMUM_SMARTCARD_READERS do
        if v[i] <> '' then FReaderList.Add(v[i]);
      FNumReaders := FReaderList.Count;
      if FNumReaders > 0 then
        begin
        if Assigned(FOnReaderListChange) then FOnReaderListChange(Self);
        Result := true;
        end;
      end else if Assigned(FOnError) then FOnError(Self, esInit, RetVar);
    end else if Assigned(FOnError) then FOnError(Self, esInit, RetVar);
end;

function TPCSCConnector.InternalAuthenticate(data: AnsiString; var sw: Word): AnsiString;
var
  len: integer;
begin
  Result := '';
  len := length(data);
  if len > $FF then exit;

  Result := data;
  if not GetResponseFromCard(#$00#$88#$00#$00 + AnsiChar(len), Result, sw)
  then
    Result := '';
end;

function TPCSCConnector.Open: boolean;
var
  ThreadID    : LongWord;
begin
  CloseAndDisconnect;
  if (FUseReaderNum > NOREADERSELECTED) and
     (SCardIsValidContext(FContext) = SCARD_S_SUCCESS) then
    begin
    ReaderOpen      := true;
    ActReaderState  := SCARD_STATE_UNAWARE;
    LastReaderState := SCARD_STATE_UNAWARE;
    BeginThread(nil, 0, CardWatcherThread, @FContext, 0, ThreadID);
    Result := true;
    end else Result := false;
end;

procedure TPCSCConnector.Close;
begin
  ReaderOpen := false;
  SCardCancel(FContext);
  if FConnected then Disconnect;
end;

function TPCSCConnector.Connect: boolean;
begin
  if FConnected then Disconnect;
  if FUseReaderNum > NOREADERSELECTED then
    if ConnectSelectedReader then FConnected := true
                             else FConnected := false;
  Result := FConnected;
end;

procedure TPCSCConnector.Disconnect;
begin
  if FConnected then
    begin
    SCardDisconnect(FCardHandle, SCARD_RESET_CARD);
    FConnected  := false;
    FCardHandle := 0;
    end;
end;

function TPCSCConnector.ExternalAuthenticate(data: AnsiString;
  var sw: Word): AnsiString;
var
  len: integer;
begin
  Result := '';
  len := length(data);
  if len > $FF then exit;

  Result := data;
  if not GetResponseFromCard(#$00#$82#$00#$00 + AnsiChar(len), Result, sw)
  then
    Result := '';
end;

procedure TPCSCConnector.CloseAndDisconnect;
begin
  if FConnected then Disconnect;
  if ReaderOpen then Close;
end;

function TPCSCConnector.ConnectSelectedReader: boolean;
var
  RetVar : cardinal;
begin
  RetVar := SCardConnectW(FContext,
                          SelectedReader,
                          SCARD_SHARE_SHARED,
                          SCARD_PROTOCOL_Tx,
                          FCardHandle,
                          @FAttrProtocol);
  case RetVar of
    SCARD_S_SUCCESS      : begin
                           CardActiveAction;
                           Result := true;
                           end;
    SCARD_W_REMOVED_CARD : begin
                           Result := true;
                           end;
    else                   begin
                           Result := false;
                           if Assigned(FOnError) then FOnError(Self, esConnect, RetVar);
                           end;
    end;
end;

procedure TPCSCConnector.ProcessReaderState(const OldState,NewState: cardinal);
var
  CardInOld, CardInNew     : boolean;
  ReaderEmOld, ReaderEmNew : boolean;
  CardMuteOld, CardMuteNew : boolean;
  CardIgnore               : boolean;

begin
CardInOld   := (OldState and SCARD_STATE_PRESENT) > 0;
CardInNew   := (NewState and SCARD_STATE_PRESENT) > 0;
ReaderEmOld := (OldState and SCARD_STATE_EMPTY) > 0;
ReaderEmNew := (NewState and SCARD_STATE_EMPTY) > 0;
CardMuteOld := (OldState and SCARD_STATE_MUTE) > 0;
CardMuteNew := (NewState and SCARD_STATE_MUTE) > 0;
CardIgnore  := (NewState and SCARD_STATE_IGNORE) > 0;

if CardMuteNew     and
   not CardMuteold then if Assigned(FOnCardInvalid) then FOnCardInvalid(Self);

if CardInNew       and
   not CardInOld   and
   not CardMuteNew and
   not CardIgnore  then CardInsertedAction;

if CardInOld     and
   not CardInNew then CardRemovedAction;

if ReaderEmNew     and
   not ReaderEmOld then begin
                        if Assigned(FOnReaderWaiting) then FOnReaderWaiting(Self);
                        end;

LastReaderState := NewState;
end;

function TPCSCConnector.ReadSFIRecord(sfi, rnum: byte; var sw: Word): AnsiString;
begin
  Result := '';
  if (rnum > $10) then exit;

  if not GetResponseFromCard(
        #$00#$B2 + AnsiChar(rnum) + AnsiChar((sfi shl 3) or $04) + #$00, Result, sw)
  then
    Result := '';

  if Hi(sw) = $6C then
  begin
    Result := '';
    if not GetResponseFromCard(
          #$00#$B2 + AnsiChar(rnum) + AnsiChar((sfi shl 3) or $04) + AnsiChar(Lo(sw)), Result, sw)
    then
      Result := '';
  end;
end;

procedure TPCSCConnector.CardInsertedAction;
begin
  if Assigned(FOnCardInserted) then FOnCardInserted(Self);
  if FConnected then CardActiveAction;
end;

procedure TPCSCConnector.CardActiveAction;
//var
// RetVar: cardinal;
begin
//  RetVar := SCardConnectA(FContext,
//                          SelectedReader,
//                          SCARD_SHARE_SHARED,
//                          SCARD_PROTOCOL_Tx,
//                          FCardHandle,
//                          @FAttrProtocol);

  GetReaderAttributes;
  if FAttrProtocol <> SCARD_PROTOCOL_UNK then
    begin
    GetCardAttributes;
    if Assigned(FOnCardActive) then FOnCardActive(Self);
    end;
end;

procedure TPCSCConnector.CardRemovedAction;
begin
  ClearReaderAttributes;
  ClearCardAttributes;
  if Assigned(FOnCardRemoved) then FOnCardRemoved(Self);
  Disconnect;
end;

function TPCSCConnector.CardSelect(const aid: AnsiString; var sw: Word): AnsiString;
begin
  Result := '';
  if length(aid) > 250 then exit;
  Result := AnsiChar(byte(length(aid))) + aid;
  if GetResponseFromCard(#$00#$A4#$04#$00, Result, sw) <> true then
    Result := '';
end;

procedure TPCSCConnector.SetReaderNum(Value: Integer);
begin
  if Value <> FUseReaderNum then
    begin
    CloseAndDisconnect;
    if Value < FReaderList.Count then
      begin
      SelectedReader := PChar(FReaderList[Value]);
      FUseReaderNum   := Value;
      end else
      begin
      SelectedReader := '';
      FUseReaderNum   := -1;
      end;
    end;
end;

function TPCSCConnector.VerifyPIN(data: AnsiString; refdata: byte;
  var sw: Word): AnsiString;
var
  len: integer;
begin
  Result := data;
  len := length(data);
  if len > $FF then exit;

  if not GetResponseFromCard(
        #$00#$20#$00 + AnsiChar(refdata) + AnsiChar(len), Result, sw)
  then
    Result := '';
end;

function TPCSCConnector.IsReaderOpen: boolean;
begin
  Result := ReaderOpen;
end;

function TPCSCConnector.GetReaderState: cardinal;
begin
  Result := ActReaderState;
end;

procedure TPCSCConnector.GetReaderAttributes;
var
  RetVar : cardinal;
  ABuf   : AnsiString;
  AIBuf  : integer;
  ALen   : integer;
begin
  ABuf := StringOfChar(#0, 127);
  ALen := Length(ABuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_ATR_STRING, Pointer(ABuf), @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrCardATR := Copy(ABuf, 1, ALen)
                              else FAttrCardATR := '';

  ALen := Length(ABuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_VENDOR_NAME, Pointer(ABuf), @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrVendorName := Copy(ABuf, 1, ALen)
                              else FAttrVendorName := '';

  ALen := Length(ABuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_VENDOR_IFD_SERIAL_NO, Pointer(ABuf), @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrVendorSerial := Copy(ABuf, 1, ALen)
                              else FAttrVendorSerial := '';

  ALen := SizeOf(AIBuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_CURRENT_PROTOCOL_TYPE, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrProtocol := AIBuf
                              else FAttrProtocol := 0;

  ALen := SizeOf(AIBuf);
  AIBuf := 0;
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_ICC_TYPE_PER_ATR, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then begin
                                   case AIBuf of
                                     1  : FAttrICCType := 'ISO7816 Async';
                                     2  : FAttrICCType := 'ISO7816 Sync';
                                     else FAttrICCType := 'UNKNOWN';
                                     end;
                                   end
                              else FAttrICCType := '';

  ALen := SizeOf(AIBuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_ICC_INTERFACE_STATUS, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrInterfaceStatus := AIBuf
                              else FAttrInterfaceStatus := 0;

  ALen := SizeOf(AIBuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_DEFAULT_DATA_RATE, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrDefaultDataRate := AIBuf
                              else FAttrDefaultDataRate := 0;

  ALen := SizeOf(AIBuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_DEFAULT_CLK, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrDefaultCLK := AIBuf
                              else FAttrDefaultCLK := 0;


end;

function TPCSCConnector.GenerateAC(RefControl: byte; data: AnsiString; var sw: Word): AnsiString;
var
  len: integer;
begin
  Result := '';
  len := length(data);
  if len > $FF then exit;

  Result := data;
  if not GetResponseFromCard(#$80#$AE + AnsiChar(RefControl) + #$00 + AnsiChar(len), Result, sw)
  then
    Result := '';
end;

function TPCSCConnector.GetATR: ATRRec;
begin
  Result.Clear;
  if Length(FAttrCardATR) < 2 then exit;

  Result.Load(FAttrCardATR);
end;

function TPCSCConnector.GetAttrATRHistBytes: string;
var
  ATR: ATRrec;
begin
  Result := '';
  if Length(FAttrCardATR) < 2 then exit;

  ATR.Load(FAttrCardATR);
  if ATR.Valid then Result := ATR.HistoricalBytes;
end;

procedure TPCSCConnector.GetCardAttributes;
begin
end;

function TPCSCConnector.GetChallenge(var sw: Word): AnsiString;
begin
  Result := '';
  if not GetResponseFromCard(#$00#$84#$00#$00, Result, sw)
  then
    Result := '';

  if Hi(sw) = $6C then
  begin
    Result := '';
    if not GetResponseFromCard(
          #$00#$84#$00#$00 + AnsiChar(Lo(sw)), Result, sw)
    then
      Result := '';
  end;
end;

function TPCSCConnector.GetData(id: AnsiString; var sw: Word): AnsiString;
var
  res: AnsiString;
begin
  Result := '';
  if length(id) <> 2 then exit;

  res := '';
  if not GetResponseFromCard(
        #$80#$CA + id[1] + id[2] + #$00, res, sw)
  then
    res := '';

  if Hi(sw) = $6C then
  begin
    res := '';
    if not GetResponseFromCard(
          #$80#$CA + id[1] + id[2] + AnsiChar(Lo(sw)), res, sw)
    then
      res := '';
  end;

  if (length(res) < 3) or (res[1] <> id[1]) or (res[2] <> id[2]) then exit;

  Result := res;
end;

procedure TPCSCConnector.ClearReaderAttributes;
begin
  FAttrCardATR      := '';
  FAttrVendorName   := '';
  FAttrVendorSerial := '';
  FAttrProtocol     := 0;
  FAttrICCType      := '';
end;

procedure TPCSCConnector.ClearCardAttributes;
begin
  FGSMCurrentFile := '';
  FGSMFileInfo    := '';
  FGSMDirInfo     := '';
  FGSMVoltage30   := false;
  FGSMVoltage18   := false;
end;

function TPCSCConnector.GetResponseFromCard(const APdu: AnsiString): AnsiString;
var
  RetVar : cardinal;
  SBuf   : AnsiString;
  SLen   : cardinal;
  RBuf   : AnsiString;
  RLen   : cardinal;
  Ppci   : Pointer;
begin
  SBuf := apdu;
  if FAPDULogging then AddLog('--> ' + Bin2Hex(apdu));
  RBuf := AnsiString(StringOfChar(#0, MAXAPDULENGTH));
  if Length(SBuf) <= MAXAPDULENGTH then
  begin
    case FAttrProtocol of
      SCARD_PROTOCOL_T0: Ppci := @SCARD_PCI_T0;
      SCARD_PROTOCOL_T1: Ppci := @SCARD_PCI_T1;
    else
                         Ppci := nil;
    end;

    SLen := Length(apdu);
    RLen := Length(RBuf);
    RetVar := SCardTransmit(FCardHandle, Ppci, @SBuf[1], SLen, nil, @RBuf[1], @RLen);
    if RetVar = SCARD_S_SUCCESS then
    begin
      Result := Copy(RBuf, 1, RLen);
      if FAPDULogging then AddLog('<-- ' + Bin2Hex(Result));
    end
    else
    begin
      if FAPDULogging then AddLog('<-- error: ' + IntToHex(RetVar, 8));
      Result := '';
      if Assigned(FOnError) then FOnError(Self, esTransmit, RetVar);
    end;
  end;
end;

function TPCSCConnector.GetResponseFromCard(const Command: AnsiString; var Data: AnsiString; var sw: word): boolean;
var
  Answer  : AnsiString;
  AnswerL : integer;
begin
  sw := 0;
  Answer := GetResponseFromCard(Command + Data);
  AnswerL := Length(Answer);
  if AnswerL >= 2 then
  begin
    Data := Copy(Answer, 1, AnswerL - 2);
    sw  := Byte(Answer[AnswerL - 1]) shl 8;
    sw  := sw or Byte(Answer[AnswerL]);
    if Hi(sw) = $61 then
    begin
      Data := AnsiChar(sw and $FF);
      if not GetResponseFromCard(GCGetResponse, Data, sw) then
      begin
        Data := '';
        sw  := 0;
        Result := false;
      end
      else
        Result := true;
    end
    else
      Result := true;
  end
  else
  begin
    Data := '';
    sw  := 0;
    Result := false;
  end;
end;

end.
