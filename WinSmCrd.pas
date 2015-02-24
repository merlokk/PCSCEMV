{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ PCSC interface unit                                              }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1996 Microsoft Corporation.                        }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: WinSmCrd.h                                 }
{ The original Pascal code is: WinSmCrd.pas                        }
{ The initial developer of the Pascal code is Chris Dickerson      }
{ (chrisd@tsc.com).                                                }
{                                                                  }
{ Obtained through:                                                }
{                                                                  }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/MPL/MPL-1.1.html                          }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}
unit WinSmCrd;

interface

uses
   Windows;

(*

Copyright (c) 1996 - 1999  Microsoft Corporation

Module Name:

    winsmcrd.h

Abstract:
    Smart Card class/port IOCTL codes. This file is required for all code
    user mode and kernel mode, using Smart Card IOCTL's, defines,
    data structures

Revision History:

*)

(*
#ifdef _WINSCARD_H_
typedef DWORD ULONG;
typedef WORD UWORD;
typedef BYTE UCHAR;
#else
typedef ULONG DWORD;
// typedef UWORD WORD;
typedef UCHAR BYTE;
#endif *)

const
   FILE_DEVICE_SMARTCARD = $00000031;
   {$EXTERNALSYM FILE_DEVICE_SMARTCARD}

//
// Various constants
//

   SCARD_ATR_LENGTH = 33;  { ISO 7816-3 spec. }
   {$EXTERNALSYM SCARD_ATR_LENGTH}

//
///////////////////////////////////////////////////////////////////////////////
//
//  Protocol Flag definitions
//

   SCARD_PROTOCOL_UNDEFINED = $00000000;  // There is no active protocol.
   {$EXTERNALSYM SCARD_PROTOCOL_UNDEFINED}
   SCARD_PROTOCOL_T0        = $00000001;  // T=0 is the active protocol.
   {$EXTERNALSYM SCARD_PROTOCOL_T0}
   SCARD_PROTOCOL_T1        = $00000002;  // T=1 is the active protocol.
   {$EXTERNALSYM SCARD_PROTOCOL_T1}
   SCARD_PROTOCOL_RAW       = $00010000;  // Raw is the active protocol.
   {$EXTERNALSYM SCARD_PROTOCOL_RAW}
//
// This is the mask of ISO defined transmission protocols
//
   SCARD_PROTOCOL_Tx: LongInt = SCARD_PROTOCOL_T0 or SCARD_PROTOCOL_T1;
   {$EXTERNALSYM SCARD_PROTOCOL_Tx}
//
// Use the default transmission parameters / card clock freq.
//
   SCARD_PROTOCOL_DEFAULT = $80000000;
   {$EXTERNALSYM SCARD_PROTOCOL_DEFAULT}
//
// Use optimal transmission parameters / card clock freq.
// Since using the optimal parameters is the default case no bit is defined to be 1
//
   SCARD_PROTOCOL_OPTIMAL = $00000000;
   {$EXTERNALSYM SCARD_PROTOCOL_OPTIMAL}

//
// Ioctl parameters 1 for IOCTL_SMARTCARD_POWER
//
   SCARD_POWER_DOWN = 0;          // Power down the card.
   {$EXTERNALSYM SCARD_POWER_DOWN}
   SCARD_COLD_RESET = 1;          // Cycle power and reset the card.
   {$EXTERNALSYM SCARD_COLD_RESET}
   SCARD_WARM_RESET = 2;          // Force a reset on the card.
   {$EXTERNALSYM SCARD_WARM_RESET}

//
///////////////////////////////////////////////////////////////////////////////
//
//  Reader Action IOCTLs
//

function SCARD_CTL_CODE(code: Integer): DWORD;
{$EXTERNALSYM SCARD_CTL_CODE}

function IOCTL_SMARTCARD_POWER: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_POWER}
function IOCTL_SMARTCARD_GET_ATTRIBUTE: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_GET_ATTRIBUTE}
function IOCTL_SMARTCARD_SET_ATTRIBUTE: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_SET_ATTRIBUTE}
function IOCTL_SMARTCARD_CONFISCATE: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_CONFISCATE}
function IOCTL_SMARTCARD_TRANSMIT: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_TRANSMIT}
function IOCTL_SMARTCARD_EJECT: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_EJECT}
function IOCTL_SMARTCARD_SWALLOW: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_SWALLOW}
function IOCTL_SMARTCARD_IS_PRESENT: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_IS_PRESENT}
function IOCTL_SMARTCARD_IS_ABSENT: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_IS_ABSENT}
function IOCTL_SMARTCARD_SET_PROTOCOL: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_SET_PROTOCOL}
function IOCTL_SMARTCARD_GET_STATE: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_GET_STATE}
function IOCTL_SMARTCARD_GET_LAST_ERROR: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_GET_LAST_ERROR}
function IOCTL_SMARTCARD_GET_PERF_CNTR: DWORD;
{$EXTERNALSYM IOCTL_SMARTCARD_GET_PERF_CNTR}


//
///////////////////////////////////////////////////////////////////////////////
//
// Tags for requesting card and reader attributes
//

const
   MAXIMUM_ATTR_STRING_LENGTH = 32;   // Nothing bigger than this from getAttr
   {$EXTERNALSYM MAXIMUM_ATTR_STRING_LENGTH}
   MAXIMUM_SMARTCARD_READERS = 10;   // Limit the readers on the system
   {$EXTERNALSYM MAXIMUM_SMARTCARD_READERS}

function SCARD_ATTR_VALUE(ulClass, ulTag: ULONG): ULONG;
{$EXTERNALSYM SCARD_ATTR_VALUE}

const
   SCARD_CLASS_VENDOR_INFO = 1;     // Vendor information definitions
   {$EXTERNALSYM SCARD_CLASS_VENDOR_INFO}
   SCARD_CLASS_COMMUNICATIONS = 2;  // Communication definitions
   {$EXTERNALSYM SCARD_CLASS_COMMUNICATIONS}
   SCARD_CLASS_PROTOCOL = 3;        // Protocol definitions
   {$EXTERNALSYM SCARD_CLASS_PROTOCOL}
   SCARD_CLASS_POWER_MGMT = 4;      // Power Management definitions
   {$EXTERNALSYM SCARD_CLASS_POWER_MGMT}
   SCARD_CLASS_SECURITY = 5;        // Security Assurance definitions
   {$EXTERNALSYM SCARD_CLASS_SECURITY}
   SCARD_CLASS_MECHANICAL = 6;      // Mechanical characteristic definitions
   {$EXTERNALSYM SCARD_CLASS_MECHANICAL}
   SCARD_CLASS_VENDOR_DEFINED = 7;  // Vendor specific definitions
   {$EXTERNALSYM SCARD_CLASS_VENDOR_DEFINED}
   SCARD_CLASS_IFD_PROTOCOL = 8;    // Interface Device Protocol options
   {$EXTERNALSYM SCARD_CLASS_IFD_PROTOCOL}
   SCARD_CLASS_ICC_STATE = 9;       // ICC State specific definitions
   {$EXTERNALSYM SCARD_CLASS_ICC_STATE}
   SCARD_CLASS_PERF = $7ffe;        // performace counters
   {$EXTERNALSYM SCARD_CLASS_PERF}
   SCARD_CLASS_SYSTEM = $7fff;      // System-specific definitions
   {$EXTERNALSYM SCARD_CLASS_SYSTEM}

function SCARD_ATTR_VENDOR_NAME: ULONG;
{$EXTERNALSYM SCARD_ATTR_VENDOR_NAME}
function SCARD_ATTR_VENDOR_IFD_TYPE: ULONG;
{$EXTERNALSYM SCARD_ATTR_VENDOR_IFD_TYPE}
function SCARD_ATTR_VENDOR_IFD_VERSION: ULONG;
{$EXTERNALSYM SCARD_ATTR_VENDOR_IFD_VERSION}
function SCARD_ATTR_VENDOR_IFD_SERIAL_NO: ULONG;
{$EXTERNALSYM SCARD_ATTR_VENDOR_IFD_SERIAL_NO}
function SCARD_ATTR_CHANNEL_ID: ULONG;
{$EXTERNALSYM SCARD_ATTR_CHANNEL_ID}
function SCARD_ATTR_PROTOCOL_TYPES: ULONG;
{$EXTERNALSYM SCARD_ATTR_PROTOCOL_TYPES}
function SCARD_ATTR_DEFAULT_CLK: ULONG;
{$EXTERNALSYM SCARD_ATTR_DEFAULT_CLK}
function SCARD_ATTR_MAX_CLK: ULONG;
{$EXTERNALSYM SCARD_ATTR_MAX_CLK}
function SCARD_ATTR_DEFAULT_DATA_RATE: ULONG;
{$EXTERNALSYM SCARD_ATTR_DEFAULT_DATA_RATE}
function SCARD_ATTR_MAX_DATA_RATE: ULONG;
{$EXTERNALSYM SCARD_ATTR_MAX_DATA_RATE}
function SCARD_ATTR_MAX_IFSD: ULONG;
{$EXTERNALSYM SCARD_ATTR_MAX_IFSD}
function SCARD_ATTR_POWER_MGMT_SUPPORT: ULONG;
{$EXTERNALSYM SCARD_ATTR_POWER_MGMT_SUPPORT}
function SCARD_ATTR_USER_TO_CARD_AUTH_DEVICE: ULONG;
{$EXTERNALSYM SCARD_ATTR_USER_TO_CARD_AUTH_DEVICE}
function SCARD_ATTR_USER_AUTH_INPUT_DEVICE: ULONG;
{$EXTERNALSYM SCARD_ATTR_USER_AUTH_INPUT_DEVICE}
function SCARD_ATTR_CHARACTERISTICS: ULONG;
{$EXTERNALSYM SCARD_ATTR_CHARACTERISTICS}
function SCARD_ATTR_CURRENT_PROTOCOL_TYPE: ULONG;
{$EXTERNALSYM SCARD_ATTR_CURRENT_PROTOCOL_TYPE}
function SCARD_ATTR_CURRENT_CLK: ULONG;
{$EXTERNALSYM SCARD_ATTR_CURRENT_CLK}
function SCARD_ATTR_CURRENT_F: ULONG;
{$EXTERNALSYM SCARD_ATTR_CURRENT_F}
function SCARD_ATTR_CURRENT_D: ULONG;
{$EXTERNALSYM SCARD_ATTR_CURRENT_D}
function SCARD_ATTR_CURRENT_N: ULONG;
{$EXTERNALSYM SCARD_ATTR_CURRENT_N}
function SCARD_ATTR_CURRENT_W: ULONG;
{$EXTERNALSYM SCARD_ATTR_CURRENT_W}
function SCARD_ATTR_CURRENT_IFSC: ULONG;
{$EXTERNALSYM SCARD_ATTR_CURRENT_IFSC}
function SCARD_ATTR_CURRENT_IFSD: ULONG;
{$EXTERNALSYM SCARD_ATTR_CURRENT_IFSD}
function SCARD_ATTR_CURRENT_BWT: ULONG;
{$EXTERNALSYM SCARD_ATTR_CURRENT_BWT}
function SCARD_ATTR_CURRENT_CWT: ULONG;
{$EXTERNALSYM SCARD_ATTR_CURRENT_CWT}
function SCARD_ATTR_CURRENT_EBC_ENCODING: ULONG;
{$EXTERNALSYM SCARD_ATTR_CURRENT_EBC_ENCODING}
function SCARD_ATTR_EXTENDED_BWT: ULONG;
{$EXTERNALSYM SCARD_ATTR_EXTENDED_BWT}
function SCARD_ATTR_ICC_PRESENCE: ULONG;
{$EXTERNALSYM SCARD_ATTR_ICC_PRESENCE}
function SCARD_ATTR_ICC_INTERFACE_STATUS: ULONG;
{$EXTERNALSYM SCARD_ATTR_ICC_INTERFACE_STATUS}
function SCARD_ATTR_CURRENT_IO_STATE: ULONG;
{$EXTERNALSYM SCARD_ATTR_CURRENT_IO_STATE}
function SCARD_ATTR_ATR_STRING: ULONG;
{$EXTERNALSYM SCARD_ATTR_ATR_STRING}
function SCARD_ATTR_ICC_TYPE_PER_ATR: ULONG;
{$EXTERNALSYM SCARD_ATTR_ICC_TYPE_PER_ATR}
function SCARD_ATTR_ESC_RESET: ULONG;
{$EXTERNALSYM SCARD_ATTR_ESC_RESET}
function SCARD_ATTR_ESC_CANCEL: ULONG;
{$EXTERNALSYM SCARD_ATTR_ESC_CANCEL}
function SCARD_ATTR_ESC_AUTHREQUEST: ULONG;
{$EXTERNALSYM SCARD_ATTR_ESC_AUTHREQUEST}
function SCARD_ATTR_MAXINPUT: ULONG;
{$EXTERNALSYM SCARD_ATTR_MAXINPUT}
function SCARD_ATTR_DEVICE_UNIT: ULONG;
{$EXTERNALSYM SCARD_ATTR_DEVICE_UNIT}
function SCARD_ATTR_DEVICE_IN_USE: ULONG;
{$EXTERNALSYM SCARD_ATTR_DEVICE_IN_USE}

function SCARD_ATTR_DEVICE_FRIENDLY_NAME_W: ULONG;
{$EXTERNALSYM SCARD_ATTR_DEVICE_FRIENDLY_NAME_W}
function SCARD_ATTR_DEVICE_SYSTEM_NAME_W: ULONG;
{$EXTERNALSYM SCARD_ATTR_DEVICE_SYSTEM_NAME_W}

function SCARD_ATTR_DEVICE_FRIENDLY_NAME_A: ULONG;
{$EXTERNALSYM SCARD_ATTR_DEVICE_FRIENDLY_NAME_A}
function SCARD_ATTR_DEVICE_SYSTEM_NAME_A: ULONG;
{$EXTERNALSYM SCARD_ATTR_DEVICE_SYSTEM_NAME_A} 

function SCARD_ATTR_SUPRESS_T1_IFS_REQUEST: ULONG;
{$EXTERNALSYM SCARD_ATTR_SUPRESS_T1_IFS_REQUEST}
function SCARD_PERF_NUM_TRANSMISSIONS: ULONG;
{$EXTERNALSYM SCARD_PERF_NUM_TRANSMISSIONS}
function SCARD_PERF_BYTES_TRANSMITTED: ULONG;
{$EXTERNALSYM SCARD_PERF_BYTES_TRANSMITTED}
function SCARD_PERF_TRANSMISSION_TIME: ULONG;
{$EXTERNALSYM SCARD_PERF_TRANSMISSION_TIME}

//
// T=0 Protocol Defines
//

const
   SCARD_T0_HEADER_LENGTH = 7;
   {$EXTERNALSYM SCARD_T0_HEADER_LENGTH}
   SCARD_T0_CMD_LENGTH = 5;
   {$EXTERNALSYM SCARD_T0_CMD_LENGTH}


//
// T=1 Protocol Defines
//

   SCARD_T1_PROLOGUE_LENGTH = 3;
   {$EXTERNALSYM SCARD_T1_PROLOGUE_LENGTH}
   SCARD_T1_EPILOGUE_LENGTH = 2;
   {$EXTERNALSYM SCARD_T1_EPILOGUE_LENGTH}
   SCARD_T1_MAX_IFS = 254;
   {$EXTERNALSYM SCARD_T1_MAX_IFS}


//
///////////////////////////////////////////////////////////////////////////////
//
//  Reader states
//

   SCARD_UNKNOWN    = 0;           // This value implies the driver is unaware
   {$EXTERNALSYM SCARD_UNKNOWN}    // of the current state of the reader.
   SCARD_ABSENT     = 1;           // This value implies there is no card in
   {$EXTERNALSYM SCARD_ABSENT}     // the reader.
   SCARD_PRESENT    = 2;           // This value implies there is a card is
   {$EXTERNALSYM SCARD_PRESENT}    // present in the reader, but that it has
                                   // not been moved into position for use.
   SCARD_SWALLOWED  = 3;           // This value implies there is a card in the
   {$EXTERNALSYM SCARD_SWALLOWED}  // reader in position for use.  The card is
                                   // not powered.
   SCARD_POWERED    = 4;           // This value implies there is power is
   {$EXTERNALSYM SCARD_POWERED}    // being provided to the card, but the
                                   // Reader Driver is unaware of the mode of
                                   // the card.
   SCARD_NEGOTIABLE = 5;           // This value implies the card has been
   {$EXTERNALSYM SCARD_NEGOTIABLE} // reset and is awaiting PTS negotiation.
   SCARD_SPECIFIC   = 6;           // This value implies the card has been
   {$EXTERNALSYM SCARD_SPECIFIC}   // reset and specific communication
                                   // protocols have been established.

////////////////////////////////////////////////////////////////////////////////
//
//  I/O Services
//
//      The following services provide access to the I/O capabilities of the
//      reader drivers.  Services of the Smart Card are requested by placing the
//      following structure into the protocol buffer:
//
type
   PSCARD_IO_REQUEST = ^SCARD_IO_REQUEST;
   {$EXTERNALSYM PSCARD_IO_REQUEST}
   SCARD_IO_REQUEST = record
      dwProtocol: DWORD;    { Protocol identifier }
      dbPciLength: DWORD;   { Protocol Control Information Length }
   end;
   {$EXTERNALSYM SCARD_IO_REQUEST}

//
// T=0 protocol services.
//
type
   PSCARD_T0_COMMAND = ^SCARD_T0_COMMAND;
   {$EXTERNALSYM PSCARD_T0_COMMAND}
   SCARD_T0_COMMAND = record
      bCla: Byte;   // The instruction class
      bIns: Byte;   // The instruction code within the instruction class
      bP1: Byte;
      bP2: Byte;    // Parameters to the instruction
      bP3: Byte;    // Size of I/O Transfer
   end;
   {$EXTERNALSYM SCARD_T0_COMMAND}


   PSCARD_T0_REQUEST = ^SCARD_T0_REQUEST;
   {$EXTERNALSYM PSCARD_T0_REQUEST}
   SCARD_T0_REQUEST = record
      ioRequest: SCARD_IO_REQUEST;
      bSw1: Byte;
      bSw2: Byte;    // Return codes from the instruction
      case Integer of
         0: (CmdBytes: SCARD_T0_COMMAND);
         1: (rgbHeader: array[0..4] of Byte);
   end;
   {$EXTERNALSYM SCARD_T0_REQUEST}

//
//  T=1 Protocol Services
//
type
   PSCARD_T1_REQUEST = ^SCARD_T1_REQUEST;
   {$EXTERNALSYM PSCARD_T1_REQUEST}
   SCARD_T1_REQUEST = record
      ioRequest: SCARD_IO_REQUEST;
   end;
   {$EXTERNALSYM SCARD_T1_REQUEST}

//
////////////////////////////////////////////////////////////////////////////////
//
//  Driver attribute flags
//
const
   SCARD_READER_SWALLOWS     = $00000001;  // Reader has a card swallowing
   {$EXTERNALSYM SCARD_READER_SWALLOWS}    // mechanism.
   SCARD_READER_EJECTS       = $00000002;  // Reader has a card ejection
   {$EXTERNALSYM SCARD_READER_EJECTS}      // mechanism.
   SCARD_READER_CONFISCATES  = $00000004;  // Reader has a card capture
   {$EXTERNALSYM SCARD_READER_CONFISCATES} // mechanism.

//
///////////////////////////////////////////////////////////////////////////////
//
// Type of reader
//
const
   SCARD_READER_TYPE_SERIAL   = $01;
   {$EXTERNALSYM SCARD_READER_TYPE_SERIAL}
   SCARD_READER_TYPE_PARALELL = $02;
   {$EXTERNALSYM SCARD_READER_TYPE_PARALELL}
   SCARD_READER_TYPE_KEYBOARD = $04;
   {$EXTERNALSYM SCARD_READER_TYPE_KEYBOARD}
   SCARD_READER_TYPE_SCSI     = $08;
   {$EXTERNALSYM SCARD_READER_TYPE_SCSI}
   SCARD_READER_TYPE_IDE      = $10;
   {$EXTERNALSYM SCARD_READER_TYPE_IDE}
   SCARD_READER_TYPE_USB      = $20;
   {$EXTERNALSYM SCARD_READER_TYPE_USB}
   SCARD_READER_TYPE_PCMCIA   = $40;
   {$EXTERNALSYM SCARD_READER_TYPE_PCMCIA}
   SCARD_READER_TYPE_VENDOR   = $F0;
   {$EXTERNALSYM SCARD_READER_TYPE_VENDOR}

implementation

function CTL_CODE(DeviceType, Func, Method, Access: WORD): DWORD;
begin
  Result := (DeviceType shl 16) or (Access shl 14) or (Func shl 2) or Method;
end;

function SCARD_CTL_CODE(code: Integer): DWORD;
begin
   Result := CTL_CODE(FILE_DEVICE_SMARTCARD, code, 0{METHOD_BUFFERED}, 0{FILE_ANY_ACCESS});
end;

function IOCTL_SMARTCARD_POWER: DWORD;
begin
   Result := SCARD_CTL_CODE(1);
end;

function IOCTL_SMARTCARD_GET_ATTRIBUTE: DWORD;
begin
   Result := SCARD_CTL_CODE(2);
end;

function IOCTL_SMARTCARD_SET_ATTRIBUTE: DWORD;
begin
   Result := SCARD_CTL_CODE(3);
end;

function IOCTL_SMARTCARD_CONFISCATE: DWORD;
begin
   Result := SCARD_CTL_CODE(4);
end;

function IOCTL_SMARTCARD_TRANSMIT: DWORD;
begin
   Result := SCARD_CTL_CODE(5);
end;

function IOCTL_SMARTCARD_EJECT: DWORD;
begin
   Result := SCARD_CTL_CODE(6);
end;

function IOCTL_SMARTCARD_SWALLOW: DWORD;
begin
   Result := SCARD_CTL_CODE(7);
end;

function IOCTL_SMARTCARD_IS_PRESENT: DWORD;
begin
   Result := SCARD_CTL_CODE(10);
end;

function IOCTL_SMARTCARD_IS_ABSENT: DWORD;
begin
   Result := SCARD_CTL_CODE(11);
end;

function IOCTL_SMARTCARD_SET_PROTOCOL: DWORD;
begin
   Result := SCARD_CTL_CODE(12);
end;

function IOCTL_SMARTCARD_GET_STATE: DWORD;
begin
   Result := SCARD_CTL_CODE(14);
end;

function IOCTL_SMARTCARD_GET_LAST_ERROR: DWORD;
begin
   Result := SCARD_CTL_CODE(15);
end;

function IOCTL_SMARTCARD_GET_PERF_CNTR: DWORD;
begin
   Result := SCARD_CTL_CODE(16);
end;

function SCARD_ATTR_VALUE(ulClass, ulTag: ULONG): ULONG;
begin
   Result := (ulClass shl 16) or ulTag;
end;

function SCARD_ATTR_VENDOR_NAME: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_INFO, $0100);
end;

function SCARD_ATTR_VENDOR_IFD_TYPE: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_INFO, $0101);
end;

function SCARD_ATTR_VENDOR_IFD_VERSION: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_INFO, $0102);
end;

function SCARD_ATTR_VENDOR_IFD_SERIAL_NO: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_INFO, $0103);
end;

function SCARD_ATTR_CHANNEL_ID: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_COMMUNICATIONS, $0110);
end;

function SCARD_ATTR_PROTOCOL_TYPES: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PROTOCOL, $0120);
end;

function SCARD_ATTR_DEFAULT_CLK: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PROTOCOL, $0121);
end;

function SCARD_ATTR_MAX_CLK: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PROTOCOL, $0122);
end;

function SCARD_ATTR_DEFAULT_DATA_RATE: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PROTOCOL, $0123);
end;

function SCARD_ATTR_MAX_DATA_RATE: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PROTOCOL, $0124);
end;

function SCARD_ATTR_MAX_IFSD: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PROTOCOL, $0125);
end;

function SCARD_ATTR_POWER_MGMT_SUPPORT: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_POWER_MGMT, $0131);
end;

function SCARD_ATTR_USER_TO_CARD_AUTH_DEVICE: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SECURITY, $0140);
end;

function SCARD_ATTR_USER_AUTH_INPUT_DEVICE: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SECURITY, $0142);
end;

function SCARD_ATTR_CHARACTERISTICS: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_MECHANICAL, $0150);
end;

function SCARD_ATTR_CURRENT_PROTOCOL_TYPE: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0201);
end;

function SCARD_ATTR_CURRENT_CLK: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0202);
end;

function SCARD_ATTR_CURRENT_F: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0203);
end;

function SCARD_ATTR_CURRENT_D: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0204);
end;

function SCARD_ATTR_CURRENT_N: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0205);
end;

function SCARD_ATTR_CURRENT_W: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0206);
end;

function SCARD_ATTR_CURRENT_IFSC: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0207);
end;

function SCARD_ATTR_CURRENT_IFSD: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0208);
end;

function SCARD_ATTR_CURRENT_BWT: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0209);
end;

function SCARD_ATTR_CURRENT_CWT: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $020a);
end;

function SCARD_ATTR_CURRENT_EBC_ENCODING: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $020b);
end;

function SCARD_ATTR_EXTENDED_BWT: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $020c);
end;

function SCARD_ATTR_ICC_PRESENCE: ULONG;
begin    
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_ICC_STATE, $0300);
end;

function SCARD_ATTR_ICC_INTERFACE_STATUS: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_ICC_STATE, $0301);
end;

function SCARD_ATTR_CURRENT_IO_STATE: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_ICC_STATE, $0302) ;
end;

function SCARD_ATTR_ATR_STRING: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_ICC_STATE, $0303);
end;

function SCARD_ATTR_ICC_TYPE_PER_ATR: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_ICC_STATE, $0304);
end;

function SCARD_ATTR_ESC_RESET: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_DEFINED, $A000);
end;

function SCARD_ATTR_ESC_CANCEL: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_DEFINED, $A003);
end;

function SCARD_ATTR_ESC_AUTHREQUEST: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_DEFINED, $A005);
end;

function SCARD_ATTR_MAXINPUT: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_DEFINED, $A007);
end;

function SCARD_ATTR_DEVICE_UNIT: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0001);
end;

function SCARD_ATTR_DEVICE_IN_USE: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0002);
end;

function SCARD_ATTR_DEVICE_FRIENDLY_NAME_A: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0003);
end;

function SCARD_ATTR_DEVICE_SYSTEM_NAME_A: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0004);
end;

function SCARD_ATTR_DEVICE_FRIENDLY_NAME_W: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0005);
end;

function SCARD_ATTR_DEVICE_SYSTEM_NAME_W: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0006);
end;

function SCARD_ATTR_SUPRESS_T1_IFS_REQUEST: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0007);
end;

function SCARD_PERF_NUM_TRANSMISSIONS: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PERF, $0001);
end;

function SCARD_PERF_BYTES_TRANSMITTED: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PERF, $0002);
end;

function SCARD_PERF_TRANSMISSION_TIME: ULONG;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PERF, $0003);
end;

end.
