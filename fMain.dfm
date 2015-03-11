object fPOS: TfPOS
  Left = 0
  Top = 0
  Caption = 'Test POS terminal'
  ClientHeight = 720
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
    Height = 195
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
      Top = 79
      Width = 19
      Height = 16
      Caption = 'PIN'
    end
    object Label3: TLabel
      Left = 600
      Top = 55
      Width = 95
      Height = 16
      Caption = 'Transaction type'
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
    object btRun: TButton
      Left = 16
      Top = 128
      Width = 75
      Height = 25
      Caption = 'Run'
      TabOrder = 11
      OnClick = btRunClick
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
      Top = 56
      Width = 223
      Height = 17
      Caption = 'Check expired objects'
      TabOrder = 5
    end
    object cbVerifyPIN: TCheckBox
      Left = 255
      Top = 79
      Width = 97
      Height = 17
      Caption = 'Verify PIN!!!!'
      TabOrder = 7
    end
    object edPIN: TEdit
      Left = 484
      Top = 76
      Width = 49
      Height = 24
      TabOrder = 6
      Text = '1234'
    end
    object Button2: TButton
      Left = 992
      Top = 159
      Width = 75
      Height = 25
      Caption = 'Test'
      TabOrder = 14
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
      Top = 125
      Width = 130
      Height = 17
      Caption = 'Use "all OK" TVR'
      Checked = True
      State = cbChecked
      TabOrder = 9
    end
    object btSaveLog: TButton
      Left = 16
      Top = 159
      Width = 75
      Height = 25
      Caption = 'Save log'
      TabOrder = 12
      OnClick = btSaveLogClick
    end
    object edLogName: TEdit
      Left = 97
      Top = 159
      Width = 136
      Height = 24
      TabOrder = 13
    end
    object cbUpToAC1: TCheckBox
      Left = 255
      Top = 148
      Width = 194
      Height = 17
      Caption = 'Do not execute AC1 and next'
      TabOrder = 10
    end
    object cbIgnoreCVM: TCheckBox
      Left = 255
      Top = 102
      Width = 186
      Height = 17
      Caption = 'Ignore false CVM result'
      TabOrder = 8
    end
    object cbTransactionType: TComboBox
      Left = 701
      Top = 52
      Width = 145
      Height = 24
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 15
      Text = 'Offline'
      Items.Strings = (
        'Offline'
        'Online')
    end
  end
  object meLog: TMemo
    Left = 0
    Top = 195
    Width = 1105
    Height = 525
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitTop = 201
    ExplicitHeight = 519
  end
end
