object Form1: TForm1
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
    Height = 201
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
    object Button1: TButton
      Left = 16
      Top = 128
      Width = 75
      Height = 25
      Caption = 'Run'
      TabOrder = 2
      OnClick = Button1Click
    end
    object cbATR: TCheckBox
      Left = 16
      Top = 56
      Width = 233
      Height = 17
      Caption = 'Show ATR and reader params'
      TabOrder = 3
    end
    object cbTLV: TCheckBox
      Left = 16
      Top = 79
      Width = 233
      Height = 17
      Caption = 'Show TLV decoding'
      Checked = True
      State = cbChecked
      TabOrder = 4
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
      TabOrder = 6
    end
    object edPIN: TEdit
      Left = 484
      Top = 76
      Width = 49
      Height = 24
      TabOrder = 7
      Text = '1234'
    end
    object Button2: TButton
      Left = 904
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Button2'
      TabOrder = 8
      OnClick = Button2Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 201
    Width = 1105
    Height = 519
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
