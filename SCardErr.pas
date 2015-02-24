{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ PCSC interface unit                                              }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1996 Microsoft Corporation.                        }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: SCardErr.h                                 }
{ The original Pascal code is: SCardErr.pas                        }
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
unit SCardErr;

interface

uses
   Windows;

const

(*
 scarderr.mc

   Error message codes from the Smart Card Resource Manager
   These messages must be reconciled with winerror.w
   They exist here to provide error messages on pre-Win2K systems.

*)

//
// =============================
// Facility SCARD Error Messages
// =============================
//
   SCARD_S_SUCCESS = 0;
   {$EXTERNALSYM SCARD_S_SUCCESS}

//
//  Values are 32 bit values layed out as follows:
//
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//  +---+-+-+-----------------------+-------------------------------+
//  |Sev|C|R|     Facility          |               Code            |
//  +---+-+-+-----------------------+-------------------------------+
//
//  where
//
//      Sev - is the severity code
//
//          00 - Success
//          01 - Informational
//          10 - Warning
//          11 - Error
//
//      C - is the Customer code flag
//
//      R - is a reserved bit
//
//      Facility - is the facility code
//
//      Code - is the facility's status code
//
//
// Define the facility codes
//
   FACILITY_SYSTEM = $0;
   {$EXTERNALSYM FACILITY_SYSTEM}
   FACILITY_SCARD  = $10;
   {$EXTERNALSYM FACILITY_SCARD}

//
// Define the severity codes
//
   STATUS_SEVERITY_WARNING       = $2;
   {$EXTERNALSYM STATUS_SEVERITY_WARNING}
   STATUS_SEVERITY_INFORMATIONAL = $1;
   {$EXTERNALSYM STATUS_SEVERITY_INFORMATIONAL}
   STATUS_SEVERITY_ERROR         = $3;
   {$EXTERNALSYM STATUS_SEVERITY_ERROR}

//
// MessageId: SCARD_F_INTERNAL_ERROR
//
// MessageText:
//
//  An internal consistency check failed.
//
   SCARD_F_INTERNAL_ERROR           = DWORD($80100001);
   {$EXTERNALSYM SCARD_F_INTERNAL_ERROR}

//
// MessageId: SCARD_E_CANCELLED
//
// MessageText:
//
//  The action was cancelled by an SCardCancel request.
//
   SCARD_E_CANCELLED                = DWORD($80100002);
   {$EXTERNALSYM SCARD_E_CANCELLED}

//
// MessageId: SCARD_E_INVALID_HANDLE
//
// MessageText:
//
//  The supplied handle was invalid.
//
   SCARD_E_INVALID_HANDLE           = DWORD($80100003);
   {$EXTERNALSYM SCARD_E_INVALID_HANDLE}

//
// MessageId: SCARD_E_INVALID_PARAMETER
//
// MessageText:
//
//  One or more of the supplied parameters could not be properly interpreted.
//
   SCARD_E_INVALID_PARAMETER        = DWORD($80100004);
   {$EXTERNALSYM SCARD_E_INVALID_PARAMETER}

//
// MessageId: SCARD_E_INVALID_TARGET
//
// MessageText:
//
//  Registry startup information is missing or invalid.
//
   SCARD_E_INVALID_TARGET           = DWORD($80100005);
   {$EXTERNALSYM SCARD_E_INVALID_TARGET}

//
// MessageId: SCARD_E_NO_MEMORY
//
// MessageText:
//
//  Not enough memory available to complete this command.
//
   SCARD_E_NO_MEMORY                = DWORD($80100006);
   {$EXTERNALSYM SCARD_E_NO_MEMORY}

//
// MessageId: SCARD_F_WAITED_TOO_LONG
//
// MessageText:
//
//  An internal consistency timer has expired.
//
   SCARD_F_WAITED_TOO_LONG          = DWORD($80100007);
   {$EXTERNALSYM SCARD_F_WAITED_TOO_LONG}

//
// MessageId: SCARD_E_INSUFFICIENT_BUFFER
//
// MessageText:
//
//  The data buffer to receive returned data is too small for the returned data.
//
   SCARD_E_INSUFFICIENT_BUFFER      = DWORD($80100008);
   {$EXTERNALSYM SCARD_E_INSUFFICIENT_BUFFER}

//
// MessageId: SCARD_E_UNKNOWN_READER
//
// MessageText:
//
//  The specified reader name is not recognized.
//
   SCARD_E_UNKNOWN_READER           = DWORD($80100009);
   {$EXTERNALSYM SCARD_E_UNKNOWN_READER}

//
// MessageId: SCARD_E_TIMEOUT
//
// MessageText:
//
//  The user-specified timeout value has expired.
//
   SCARD_E_TIMEOUT                  = DWORD($8010000A);
   {$EXTERNALSYM SCARD_E_TIMEOUT}

//
// MessageId: SCARD_E_SHARING_VIOLATION
//
// MessageText:
//
//  The smart card cannot be accessed because of other connections outstanding.
//
   SCARD_E_SHARING_VIOLATION        = DWORD($8010000B);
   {$EXTERNALSYM SCARD_E_SHARING_VIOLATION}

//
// MessageId: SCARD_E_NO_SMARTCARD
//
// MessageText:
//
//  The operation requires a Smart Card, but no Smart Card is currently in the device.
//
   SCARD_E_NO_SMARTCARD             = DWORD($8010000C);
   {$EXTERNALSYM SCARD_E_NO_SMARTCARD}

//
// MessageId: SCARD_E_UNKNOWN_CARD
//
// MessageText:
//
//  The specified smart card name is not recognized.
//
   SCARD_E_UNKNOWN_CARD             = DWORD($8010000D);
   {$EXTERNALSYM SCARD_E_UNKNOWN_CARD}

//
// MessageId: SCARD_E_CANT_DISPOSE
//
// MessageText:
//
//  The system could not dispose of the media in the requested manner.
//
   SCARD_E_CANT_DISPOSE             = DWORD($8010000E);
   {$EXTERNALSYM SCARD_E_CANT_DISPOSE}

//
// MessageId: SCARD_E_PROTO_MISMATCH
//
// MessageText:
//
//  The requested protocols are incompatible with the protocol currently in use with the smart card.
//
   SCARD_E_PROTO_MISMATCH           = DWORD($8010000F);
   {$EXTERNALSYM SCARD_E_PROTO_MISMATCH}

//
// MessageId: SCARD_E_NOT_READY
//
// MessageText:
//
//  The reader or smart card is not ready to accept commands.
//
   SCARD_E_NOT_READY                = DWORD($80100010);
   {$EXTERNALSYM SCARD_E_NOT_READY}

//
// MessageId: SCARD_E_INVALID_VALUE
//
// MessageText:
//
//  One or more of the supplied parameters values could not be properly interpreted.
//
   SCARD_E_INVALID_VALUE            = DWORD($80100011);
   {$EXTERNALSYM SCARD_E_INVALID_VALUE}

//
// MessageId: SCARD_E_SYSTEM_CANCELLED
//
// MessageText:
//
//  The action was cancelled by the system, presumably to log off or shut down.
//
   SCARD_E_SYSTEM_CANCELLED         = DWORD($80100012);
   {$EXTERNALSYM SCARD_E_SYSTEM_CANCELLED}

//
// MessageId: SCARD_F_COMM_ERROR
//
// MessageText:
//
//  An internal communications error has been detected.
//
   SCARD_F_COMM_ERROR               = DWORD($80100013);
   {$EXTERNALSYM SCARD_F_COMM_ERROR}

//
// MessageId: SCARD_F_UNKNOWN_ERROR
//
// MessageText:
//
//  An internal error has been detected, but the source is unknown.
//
   SCARD_F_UNKNOWN_ERROR            = DWORD($80100014);
   {$EXTERNALSYM SCARD_F_UNKNOWN_ERROR}

//
// MessageId: SCARD_E_INVALID_ATR
//
// MessageText:
//
//  An ATR obtained from the registry is not a valid ATR string.
//
   SCARD_E_INVALID_ATR              = DWORD($80100015);
   {$EXTERNALSYM SCARD_E_INVALID_ATR}

//
// MessageId: SCARD_E_NOT_TRANSACTED
//
// MessageText:
//
//  An attempt was made to end a non-existent transaction.
//
   SCARD_E_NOT_TRANSACTED           = DWORD($80100016);
   {$EXTERNALSYM SCARD_E_NOT_TRANSACTED}

//
// MessageId: SCARD_E_READER_UNAVAILABLE
//
// MessageText:
//
//  The specified reader is not currently available for use.
//
   SCARD_E_READER_UNAVAILABLE       = DWORD($80100017);
   {$EXTERNALSYM SCARD_E_READER_UNAVAILABLE}

//
// MessageId: SCARD_P_SHUTDOWN
//
// MessageText:
//
//  The operation has been aborted to allow the server application to exit.
//
   SCARD_P_SHUTDOWN                 = DWORD($80100018);
   {$EXTERNALSYM SCARD_P_SHUTDOWN}

//
// MessageId: SCARD_E_PCI_TOO_SMALL
//
// MessageText:
//
//  The PCI Receive buffer was too small.
//
   SCARD_E_PCI_TOO_SMALL            = DWORD($80100019);
   {$EXTERNALSYM SCARD_E_PCI_TOO_SMALL}

//
// MessageId: SCARD_E_READER_UNSUPPORTED
//
// MessageText:
//
//  The reader driver does not meet minimal requirements for support.
//
   SCARD_E_READER_UNSUPPORTED       = DWORD($8010001A);
   {$EXTERNALSYM SCARD_E_READER_UNSUPPORTED}

//
// MessageId: SCARD_E_DUPLICATE_READER
//
// MessageText:
//
//  The reader driver did not produce a unique reader name.
//
   SCARD_E_DUPLICATE_READER         = DWORD($8010001B);
   {$EXTERNALSYM SCARD_E_DUPLICATE_READER}

//
// MessageId: SCARD_E_CARD_UNSUPPORTED
//
// MessageText:
//
//  The smart card does not meet minimal requirements for support.
//
   SCARD_E_CARD_UNSUPPORTED         = DWORD($8010001C);
   {$EXTERNALSYM SCARD_E_CARD_UNSUPPORTED}

//
// MessageId: SCARD_E_NO_SERVICE
//
// MessageText:
//
//  The Smart card resource manager is not running.
//
   SCARD_E_NO_SERVICE               = DWORD($8010001D);
   {$EXTERNALSYM SCARD_E_NO_SERVICE}

//
// MessageId: SCARD_E_SERVICE_STOPPED
//
// MessageText:
//
//  The Smart card resource manager has shut down.
//
   SCARD_E_SERVICE_STOPPED          = DWORD($8010001E);
   {$EXTERNALSYM SCARD_E_SERVICE_STOPPED}

//
// MessageId: SCARD_E_UNEXPECTED
//
// MessageText:
//
//  An unexpected card error has occurred.
//
   SCARD_E_UNEXPECTED               = DWORD($8010001F);
   {$EXTERNALSYM SCARD_E_UNEXPECTED}

//
// MessageId: SCARD_E_ICC_INSTALLATION
//
// MessageText:
//
//  No Primary Provider can be found for the smart card.
//
   SCARD_E_ICC_INSTALLATION         = DWORD($80100020);
   {$EXTERNALSYM SCARD_E_ICC_INSTALLATION}

//
// MessageId: SCARD_E_ICC_CREATEORDER
//
// MessageText:
//
//  The requested order of object creation is not supported.
//
   SCARD_E_ICC_CREATEORDER          = DWORD($80100021);
   {$EXTERNALSYM SCARD_E_ICC_CREATEORDER}

//
// MessageId: SCARD_E_UNSUPPORTED_FEATURE
//
// MessageText:
//
//  This smart card does not support the requested feature.
//
   SCARD_E_UNSUPPORTED_FEATURE      = DWORD($80100022);
   {$EXTERNALSYM SCARD_E_UNSUPPORTED_FEATURE}

//
// MessageId: SCARD_E_DIR_NOT_FOUND
//
// MessageText:
//
//  The identified directory does not exist in the smart card.
//
   SCARD_E_DIR_NOT_FOUND            = DWORD($80100023);
   {$EXTERNALSYM SCARD_E_DIR_NOT_FOUND}

//
// MessageId: SCARD_E_FILE_NOT_FOUND
//
// MessageText:
//
//  The identified file does not exist in the smart card.
//
   SCARD_E_FILE_NOT_FOUND           = DWORD($80100024);
   {$EXTERNALSYM SCARD_E_FILE_NOT_FOUND}

//
// MessageId: SCARD_E_NO_DIR
//
// MessageText:
//
//  The supplied path does not represent a smart card directory.
//
   SCARD_E_NO_DIR                   = DWORD($80100025);
   {$EXTERNALSYM SCARD_E_NO_DIR}

//
// MessageId: SCARD_E_NO_FILE
//
// MessageText:
//
//  The supplied path does not represent a smart card file.
//
   SCARD_E_NO_FILE                  = DWORD($80100026);
   {$EXTERNALSYM SCARD_E_NO_FILE}

//
// MessageId: SCARD_E_NO_ACCESS
//
// MessageText:
//
//  Access is denied to this file.
//
   SCARD_E_NO_ACCESS                = DWORD($80100027);
   {$EXTERNALSYM SCARD_E_NO_ACCESS}

//
// MessageId: SCARD_E_WRITE_TOO_MANY
//
// MessageText:
//
//  The smartcard does not have enough memory to store the information.
//
   SCARD_E_WRITE_TOO_MANY           = DWORD($80100028);
   {$EXTERNALSYM SCARD_E_WRITE_TOO_MANY}

//
// MessageId: SCARD_E_BAD_SEEK
//
// MessageText:
//
//  There was an error trying to set the smart card file object pointer.
//
   SCARD_E_BAD_SEEK                 = DWORD($80100029);
   {$EXTERNALSYM SCARD_E_BAD_SEEK}

//
// MessageId: SCARD_E_INVALID_CHV
//
// MessageText:
//
//  The supplied PIN is incorrect.
//
   SCARD_E_INVALID_CHV              = DWORD($8010002A);
   {$EXTERNALSYM SCARD_E_INVALID_CHV}

//
// MessageId: SCARD_E_UNKNOWN_RES_MNG
//
// MessageText:
//
//  An unrecognized error code was returned from a layered component.
//
   SCARD_E_UNKNOWN_RES_MNG          = DWORD($8010002B);
   {$EXTERNALSYM SCARD_E_UNKNOWN_RES_MNG}

//
// MessageId: SCARD_E_NO_SUCH_CERTIFICATE
//
// MessageText:
//
//  The requested certificate does not exist.
//
   SCARD_E_NO_SUCH_CERTIFICATE      = DWORD($8010002C);
   {$EXTERNALSYM SCARD_E_NO_SUCH_CERTIFICATE}

//
// MessageId: SCARD_E_CERTIFICATE_UNAVAILABLE
//
// MessageText:
//
//  The requested certificate could not be obtained.
//
   SCARD_E_CERTIFICATE_UNAVAILABLE  = DWORD($8010002D);
   {$EXTERNALSYM SCARD_E_CERTIFICATE_UNAVAILABLE}

//
// MessageId: SCARD_E_NO_READERS_AVAILABLE
//
// MessageText:
//
//  Cannot find a smart card reader.
//
   SCARD_E_NO_READERS_AVAILABLE     = DWORD($8010002E);
   {$EXTERNALSYM SCARD_E_NO_READERS_AVAILABLE}

//
// MessageId: SCARD_E_COMM_DATA_LOST
//
// MessageText:
//
//  A communications error with the smart card has been detected.  Retry the operation.
//
   SCARD_E_COMM_DATA_LOST           = DWORD($08010002F);
   {$EXTERNALSYM SCARD_E_COMM_DATA_LOST}

//
// MessageId: SCARD_E_NO_KEY_CONTAINER
//
// MessageText:
//
//  The requested key container does not exist on the smart card.
//
   SCARD_E_NO_KEY_CONTAINER         = DWORD($080100030);
   {$EXTERNALSYM SCARD_E_NO_KEY_CONTAINER}

//
// These are warning codes.
//
//
// MessageId: SCARD_W_UNSUPPORTED_CARD
//
// MessageText:
//
//  The reader cannot communicate with the smart card, due to ATR configuration conflicts.
//
   SCARD_W_UNSUPPORTED_CARD         = DWORD($80100065);
   {$EXTERNALSYM SCARD_W_UNSUPPORTED_CARD}

//
// MessageId: SCARD_W_UNRESPONSIVE_CARD
//
// MessageText:
//
//  The smart card is not responding to a reset.
//
   SCARD_W_UNRESPONSIVE_CARD        = DWORD($80100066);
   {$EXTERNALSYM SCARD_W_UNRESPONSIVE_CARD}

//
// MessageId: SCARD_W_UNPOWERED_CARD
//
// MessageText:
//
//  Power has been removed from the smart card, so that further communication is not possible.
//
   SCARD_W_UNPOWERED_CARD           = DWORD($80100067);
   {$EXTERNALSYM SCARD_W_UNPOWERED_CARD}

//
// MessageId: SCARD_W_RESET_CARD
//
// MessageText:
//
//  The smart card has been reset, so any shared state information is invalid.
//
   SCARD_W_RESET_CARD               = DWORD($80100068);
   {$EXTERNALSYM SCARD_W_RESET_CARD}

//
// MessageId: SCARD_W_REMOVED_CARD
//
// MessageText:
//
//  The smart card has been removed, so that further communication is not possible.
//
   SCARD_W_REMOVED_CARD              = DWORD($80100069);
   {$EXTERNALSYM SCARD_W_REMOVED_CARD}

//
// MessageId: SCARD_W_SECURITY_VIOLATION
//
// MessageText:
//
//  Access was denied because of a security violation.
//
   SCARD_W_SECURITY_VIOLATION       = DWORD($8010006A);
   {$EXTERNALSYM SCARD_W_SECURITY_VIOLATION}

//
// MessageId: SCARD_W_WRONG_CHV
//
// MessageText:
//
//  The card cannot be accessed because the wrong PIN was presented.
//
   SCARD_W_WRONG_CHV                = DWORD($8010006B);
   {$EXTERNALSYM SCARD_W_WRONG_CHV}

//
// MessageId: SCARD_W_CHV_BLOCKED
//
// MessageText:
//
//  The card cannot be accessed because the maximum number of PIN entry attempts has been reached.
//
   SCARD_W_CHV_BLOCKED              = DWORD($8010006C);
   {$EXTERNALSYM SCARD_W_CHV_BLOCKED}

//
// MessageId: SCARD_W_EOF
//
// MessageText:
//
//  The end of the smart card file has been reached.
//
   SCARD_W_EOF                      = DWORD($8010006D);
   {$EXTERNALSYM SCARD_W_EOF}

//
// MessageId: SCARD_W_CANCELLED_BY_USER
//
// MessageText:
//
//  The action was cancelled by the user.
//
   SCARD_W_CANCELLED_BY_USER        = DWORD($8010006E);
   {$EXTERNALSYM SCARD_W_CANCELLED_BY_USER}

//
// MessageId: SCARD_W_CARD_NOT_AUTHENTICATED
//
// MessageText:
//
//  No PIN was presented to the smart card.
//
   SCARD_W_CARD_NOT_AUTHENTICATED   = DWORD($08010006F);
   {$EXTERNALSYM SCARD_W_CARD_NOT_AUTHENTICATED}

implementation

end.
