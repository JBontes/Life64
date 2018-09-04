object Form2: TForm2
  Left = 0
  Top = 0
  ActiveControl = PageControl1
  Caption = 'Form2'
  ClientHeight = 721
  ClientWidth = 1514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1514
    Height = 721
    ActivePage = TabSheet1
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object Label1: TLabel
        Left = 88
        Top = 104
        Width = 31
        Height = 13
        Caption = 'Label1'
      end
      object Label2: TLabel
        Left = 88
        Top = 85
        Width = 31
        Height = 13
        Caption = 'Label2'
      end
      object Label3: TLabel
        Left = 296
        Top = 53
        Width = 31
        Height = 13
        Caption = 'Label3'
      end
      object Label4: TLabel
        Left = 689
        Top = 181
        Width = 149
        Height = 23
        Caption = 'No of solutions'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Rosewood Std Regular'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 1098
        Top = 175
        Width = 123
        Height = 23
        Caption = 'Improvement'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Rosewood Std Regular'
        Font.Style = []
        ParentFont = False
      end
      object Label6: TLabel
        Left = 959
        Top = 100
        Width = 25
        Height = 13
        Caption = 'Dead'
      end
      object Label7: TLabel
        Left = 1035
        Top = 100
        Width = 23
        Height = 13
        Caption = 'Alive'
      end
      object Label8: TLabel
        Left = 1263
        Top = 100
        Width = 25
        Height = 13
        Caption = 'Dead'
      end
      object Label9: TLabel
        Left = 1339
        Top = 100
        Width = 23
        Height = 13
        Caption = 'Alive'
      end
      object Button1: TButton
        Left = 3
        Top = 48
        Width = 169
        Height = 25
        Caption = 'Process slice lookup'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Memo1: TMemo
        Left = 8
        Top = 136
        Width = 169
        Height = 457
        Lines.Strings = (
          'Memo1')
        TabOrder = 1
      end
      object Button2: TButton
        Left = 3
        Top = 17
        Width = 169
        Height = 25
        Caption = 'Process 7x7 count lookup'
        TabOrder = 2
        OnClick = Button2Click
      end
      object StringGrid1: TStringGrid
        Left = 182
        Top = 154
        Width = 503
        Height = 503
        ColCount = 20
        Ctl3D = True
        DefaultColWidth = 24
        DrawingStyle = gdsClassic
        FixedCols = 0
        RowCount = 20
        FixedRows = 0
        ParentCtl3D = False
        TabOrder = 3
        OnClick = StringGrid1DblClick
        OnDblClick = StringGrid1DblClick
        ColWidths = (
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24)
        RowHeights = (
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24)
      end
      object Button3: TButton
        Left = 487
        Top = 31
        Width = 418
        Height = 25
        Caption = 'Appy lookup table'
        TabOrder = 4
        OnClick = Button3Click
      end
      object StringGrid2: TStringGrid
        Left = 690
        Top = 204
        Width = 403
        Height = 403
        ColCount = 16
        Ctl3D = True
        DefaultColWidth = 24
        DrawingStyle = gdsClassic
        FixedCols = 0
        RowCount = 16
        FixedRows = 0
        ParentCtl3D = False
        TabOrder = 5
        OnClick = StringGrid2Click
        OnDrawCell = StringGrid2DrawCell
        ColWidths = (
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24)
        RowHeights = (
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24)
      end
      object Button4: TButton
        Left = 487
        Top = 62
        Width = 418
        Height = 25
        Caption = 'Solve a round using chunks'
        TabOrder = 6
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 487
        Top = 126
        Width = 418
        Height = 25
        Caption = 'Clear grid'
        TabOrder = 7
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 512
        Top = 0
        Width = 337
        Height = 25
        Caption = 'Test lookup0, 012'
        TabOrder = 8
        OnClick = Button6Click
      end
      object StringGrid3: TStringGrid
        Left = 1098
        Top = 204
        Width = 403
        Height = 403
        ColCount = 16
        Ctl3D = True
        DefaultColWidth = 24
        DrawingStyle = gdsClassic
        FixedCols = 0
        RowCount = 16
        FixedRows = 0
        ParentCtl3D = False
        TabOrder = 9
        OnClick = StringGrid3Click
        OnDrawCell = StringGrid2DrawCell
        ColWidths = (
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24)
        RowHeights = (
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24)
      end
      object Button7: TButton
        Left = 248
        Top = 95
        Width = 233
        Height = 25
        Caption = 'Minimal solve'
        TabOrder = 10
        OnClick = Button7Click
      end
      object Button9: TButton
        Left = 944
        Top = 31
        Width = 233
        Height = 25
        Caption = 'Solve counter'
        TabOrder = 11
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 944
        Top = 0
        Width = 233
        Height = 25
        Caption = 'Rotate counter'
        TabOrder = 12
        OnClick = Button10Click
      end
      object Button11: TButton
        Left = 487
        Top = 95
        Width = 418
        Height = 25
        Caption = 'Solve a round using slivers'
        TabOrder = 13
        OnClick = Button11Click
      end
      object Button12: TButton
        Left = 248
        Top = 126
        Width = 233
        Height = 25
        Caption = 'Init with GoE'
        TabOrder = 14
        OnClick = Button12Click
      end
      object BtnLoadSmallLookups: TButton
        Left = 248
        Top = 3
        Width = 129
        Height = 25
        Caption = 'Load small lookups'
        TabOrder = 15
        OnClick = BtnLoadSmallLookupsClick
      end
      object Button13: TButton
        Left = 248
        Top = 34
        Width = 187
        Height = 25
        Caption = 'Solve with chunk-lookup'
        TabOrder = 16
        OnClick = Button13Click
      end
      object Button16: TButton
        Left = 1201
        Top = 0
        Width = 300
        Height = 25
        Caption = 'Apply N/E lookup tables'
        TabOrder = 17
        OnClick = Button16Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      object SGSliceLayout: TStringGrid
        Left = 136
        Top = 96
        Width = 1059
        Height = 403
        ColCount = 32
        DefaultColWidth = 32
        FixedCols = 0
        RowCount = 16
        FixedRows = 0
        TabOrder = 0
        ColWidths = (
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32
          32)
        RowHeights = (
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24)
      end
      object Button8: TButton
        Left = 432
        Top = 32
        Width = 137
        Height = 25
        Caption = 'Rotate clockwise'
        TabOrder = 1
        OnClick = Button8Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'TabSheet3'
      ImageIndex = 2
      object StringGrid4: TStringGrid
        Left = 212
        Top = 143
        Width = 659
        Height = 413
        ColCount = 16
        DefaultColWidth = 40
        DefaultRowHeight = 40
        FixedCols = 0
        RowCount = 10
        FixedRows = 0
        TabOrder = 0
        OnDrawCell = StringGrid4DrawCell
        ColWidths = (
          40
          40
          40
          40
          40
          40
          40
          40
          40
          40
          40
          40
          40
          40
          40
          40)
        RowHeights = (
          40
          40
          40
          40
          40
          40
          40
          40
          40
          40)
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'TabSheet4'
      ImageIndex = 3
      object Button14: TButton
        Left = 248
        Top = 176
        Width = 377
        Height = 25
        Caption = 'ValidateN1E1 lookup table'
        TabOrder = 0
        OnClick = Button14Click
      end
      object Button15: TButton
        Left = 248
        Top = 224
        Width = 377
        Height = 25
        Caption = 'ValidateCountTable'
        TabOrder = 1
        OnClick = Button15Click
      end
      object SGMinOn: TStringGrid
        Left = 272
        Top = 276
        Width = 128
        Height = 128
        Ctl3D = True
        DefaultColWidth = 24
        DrawingStyle = gdsClassic
        FixedCols = 0
        FixedRows = 0
        ParentCtl3D = False
        TabOrder = 2
        OnClick = StringGrid2Click
        OnDrawCell = StringGrid2DrawCell
        ColWidths = (
          24
          24
          24
          24
          24)
        RowHeights = (
          24
          24
          24
          24
          24)
      end
      object SGMinOff: TStringGrid
        Left = 472
        Top = 276
        Width = 128
        Height = 128
        Ctl3D = True
        DefaultColWidth = 24
        DrawingStyle = gdsClassic
        FixedCols = 0
        FixedRows = 0
        ParentCtl3D = False
        TabOrder = 3
        OnClick = StringGrid2Click
        OnDrawCell = StringGrid2DrawCell
        ColWidths = (
          24
          24
          24
          24
          24)
        RowHeights = (
          24
          24
          24
          24
          24)
      end
    end
  end
  object SGAliveDiff: TStringGrid
    Left = 1318
    Top = 147
    Width = 81
    Height = 78
    ColCount = 3
    Ctl3D = True
    DefaultColWidth = 25
    DrawingStyle = gdsClassic
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    ParentCtl3D = False
    TabOrder = 1
    ColWidths = (
      25
      25
      25)
    RowHeights = (
      24
      24
      24)
  end
  object SGDeadDiff: TStringGrid
    Left = 1231
    Top = 147
    Width = 81
    Height = 78
    ColCount = 3
    Ctl3D = True
    DefaultColWidth = 25
    DrawingStyle = gdsClassic
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    ParentCtl3D = False
    TabOrder = 2
    ColWidths = (
      25
      25
      25)
    RowHeights = (
      24
      24
      24)
  end
  object SGDead: TStringGrid
    Left = 928
    Top = 147
    Width = 81
    Height = 78
    ColCount = 3
    Ctl3D = True
    DefaultColWidth = 25
    DrawingStyle = gdsClassic
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    ParentCtl3D = False
    TabOrder = 3
    OnDblClick = SGDeadDblClick
    ColWidths = (
      25
      25
      25)
    RowHeights = (
      24
      24
      24)
  end
  object SGAlive: TStringGrid
    Left = 1015
    Top = 146
    Width = 81
    Height = 78
    ColCount = 3
    Ctl3D = True
    DefaultColWidth = 25
    DrawingStyle = gdsClassic
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    ParentCtl3D = False
    TabOrder = 4
    OnDblClick = SGDeadDblClick
    ColWidths = (
      25
      25
      25)
    RowHeights = (
      24
      24
      24)
  end
  object FileOpenDialog1: TFileOpenDialog
    DefaultExtension = 'bin'
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPathMustExist, fdoFileMustExist, fdoNoTestFileCreate]
    Left = 336
    Top = 40
  end
end
