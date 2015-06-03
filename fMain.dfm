object fPOS: TfPOS
  Left = 0
  Top = 0
  Caption = 'Test POS terminal'
  ClientHeight = 974
  ClientWidth = 1105
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1105
    Height = 337
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 47
      Height = 16
      Caption = 'Readers'
    end
    object Label2: TLabel
      Left = 459
      Top = 125
      Width = 19
      Height = 16
      Caption = 'PIN'
    end
    object Label3: TLabel
      Left = 583
      Top = 19
      Width = 95
      Height = 16
      Caption = 'Transaction type'
    end
    object Label4: TLabel
      Left = 584
      Top = 141
      Width = 81
      Height = 16
      Caption = 'Issuer scripts:'
    end
    object Label5: TLabel
      Left = 584
      Top = 46
      Width = 198
      Height = 16
      Caption = 'Contactless transactions accepted:'
    end
    object cbReaders: TComboBox
      Left = 69
      Top = 16
      Width = 409
      Height = 24
      AutoComplete = False
      Style = csDropDownList
      TabOrder = 0
    end
    object btRefresh: TButton
      Left = 484
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 1
      OnClick = btRefreshClick
    end
    object btRunContact: TButton
      Left = 16
      Top = 236
      Width = 129
      Height = 25
      Caption = 'Run contact'
      TabOrder = 15
      OnClick = btRunContactClick
    end
    object cbATR: TCheckBox
      Left = 16
      Top = 56
      Width = 233
      Height = 17
      Caption = 'Show ATR and reader params'
      TabOrder = 2
    end
    object cbTLV: TCheckBox
      Left = 16
      Top = 79
      Width = 233
      Height = 17
      Caption = 'Show TLV decoding'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object cbCheckExpired: TCheckBox
      Left = 255
      Top = 102
      Width = 198
      Height = 17
      Caption = 'Check expired objects'
      TabOrder = 7
    end
    object cbVerifyPIN: TCheckBox
      Left = 255
      Top = 125
      Width = 97
      Height = 17
      Caption = 'Verify PIN!!!!'
      TabOrder = 9
    end
    object edPIN: TEdit
      Left = 484
      Top = 121
      Width = 49
      Height = 24
      TabOrder = 8
      Text = '1234'
    end
    object Button2: TButton
      Left = 1022
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Test'
      TabOrder = 19
      OnClick = Button2Click
    end
    object cbAPDULogging: TCheckBox
      Left = 16
      Top = 102
      Width = 97
      Height = 17
      Caption = 'APDU logging'
      TabOrder = 4
    end
    object cbGoodTVR: TCheckBox
      Left = 255
      Top = 171
      Width = 130
      Height = 17
      Caption = 'Use "all OK" TVR'
      Checked = True
      State = cbChecked
      TabOrder = 11
    end
    object btSaveLog: TButton
      Left = 158
      Top = 308
      Width = 75
      Height = 25
      Caption = 'Save log'
      TabOrder = 17
      OnClick = btSaveLogClick
    end
    object edLogName: TEdit
      Left = 16
      Top = 308
      Width = 136
      Height = 24
      TabOrder = 18
    end
    object cbUpToAC1: TCheckBox
      Left = 255
      Top = 194
      Width = 194
      Height = 17
      Caption = 'Do not execute AC1 and next'
      TabOrder = 12
    end
    object cbIgnoreCVM: TCheckBox
      Left = 255
      Top = 148
      Width = 186
      Height = 17
      Caption = 'Ignore false CVM result'
      TabOrder = 10
    end
    object cbTransactionType: TComboBox
      Left = 684
      Top = 16
      Width = 145
      Height = 24
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 20
      Text = 'Online'
      Items.Strings = (
        'Offline'
        'Online')
    end
    object cbPSEForce: TCheckBox
      Left = 255
      Top = 56
      Width = 222
      Height = 17
      Caption = 'Force select via AID list (wo PSE)'
      TabOrder = 5
    end
    object cbSPINUnblock: TCheckBox
      Left = 584
      Top = 186
      Width = 137
      Height = 17
      Caption = 'PIN unblock'
      TabOrder = 21
    end
    object cbSAppUnblock: TCheckBox
      Left = 584
      Top = 163
      Width = 145
      Height = 17
      Caption = 'Application unblock'
      TabOrder = 22
    end
    object cbSPINChange: TCheckBox
      Left = 584
      Top = 209
      Width = 137
      Height = 17
      Caption = 'PIN change'
      TabOrder = 23
    end
    object cbSUpdateRecord: TCheckBox
      Left = 584
      Top = 232
      Width = 145
      Height = 17
      Caption = 'Update Record'
      TabOrder = 24
    end
    object edSFI: TEdit
      Left = 584
      Top = 255
      Width = 33
      Height = 24
      Hint = 'SFI'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 25
      Text = '03'
    end
    object edRECN: TEdit
      Left = 623
      Top = 255
      Width = 34
      Height = 24
      Hint = 'Record number'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 26
      Text = '03'
    end
    object edRecord: TEdit
      Left = 663
      Top = 255
      Width = 434
      Height = 24
      Hint = 'Record data'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 27
      Text = '70 12 8E 10 00 00 00 00 00 00 00 00 41 03 1E 03 42 03 1F 02'
    end
    object cbSPutData: TCheckBox
      Left = 584
      Top = 285
      Width = 105
      Height = 17
      Caption = 'Put data'
      TabOrder = 28
    end
    object edTag: TEdit
      Left = 584
      Top = 308
      Width = 73
      Height = 24
      Hint = 'SFI'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 29
      Text = '9F 53'
    end
    object edValue: TEdit
      Left = 663
      Top = 308
      Width = 434
      Height = 24
      Hint = 'Record data'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 30
      Text = 'FF'
    end
    object btRunContactless: TButton
      Left = 16
      Top = 267
      Width = 129
      Height = 25
      Caption = 'Run contactless'
      TabOrder = 16
      OnClick = btRunContactlessClick
    end
    object cbqVSDC: TCheckBox
      Left = 585
      Top = 91
      Width = 257
      Height = 17
      Caption = 'qVSDC (Quick Visa Smart Debit/Credit)'
      Checked = True
      State = cbChecked
      TabOrder = 31
    end
    object cbVSDC: TCheckBox
      Left = 586
      Top = 114
      Width = 244
      Height = 17
      Caption = 'VSDC (Visa Smart Debit/Credit)'
      TabOrder = 32
    end
    object cbMSD: TCheckBox
      Left = 584
      Top = 68
      Width = 234
      Height = 17
      Caption = 'MSD (Magnetic Stripe Data)'
      Checked = True
      State = cbChecked
      TabOrder = 33
    end
    object cbCheckAIDinPSE: TCheckBox
      Left = 255
      Top = 79
      Width = 278
      Height = 17
      Caption = 'Check PSE AID in terminal AID list (EMV)'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object cbCDAEnabled: TCheckBox
      Left = 255
      Top = 217
      Width = 278
      Height = 17
      Caption = 'Enable CDA in AC'
      TabOrder = 13
    end
    object cbMSDCVN17: TCheckBox
      Left = 255
      Top = 259
      Width = 304
      Height = 17
      Caption = 'TTQ Online cryptogram required (MSD CVN17)'
      Checked = True
      State = cbChecked
      TabOrder = 14
    end
  end
  object meLog: TMemo
    Left = 0
    Top = 337
    Width = 1105
    Height = 637
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
