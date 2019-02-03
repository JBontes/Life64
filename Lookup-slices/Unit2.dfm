object Form2: TForm2
  Left = 0
  Top = 0
  ActiveControl = PageControl1
  Caption = 'Form2'
  ClientHeight = 729
  ClientWidth = 1514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1514
    Height = 729
    ActivePage = TabSheet1
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object Label1: TLabel
        Left = 178
        Top = 20
        Width = 31
        Height = 13
        Caption = 'Label1'
      end
      object Label2: TLabel
        Left = 178
        Top = 39
        Width = 31
        Height = 13
        Caption = 'Label2'
      end
      object Label3: TLabel
        Left = 178
        Top = 58
        Width = 31
        Height = 13
        Caption = 'Label3'
      end
      object LabelSolutions: TLabel
        Left = 690
        Top = 155
        Width = 82
        Height = 24
        Caption = 'Solutions'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Roboto Lt'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 1098
        Top = 155
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
        Top = 79
        Width = 25
        Height = 13
        Caption = 'Dead'
      end
      object Label7: TLabel
        Left = 1035
        Top = 81
        Width = 23
        Height = 13
        Caption = 'Alive'
      end
      object Label8: TLabel
        Left = 1263
        Top = 81
        Width = 25
        Height = 13
        Caption = 'Dead'
      end
      object Label9: TLabel
        Left = 1339
        Top = 81
        Width = 23
        Height = 13
        Caption = 'Alive'
      end
      object BtnProcessSliceLookup: TButton
        Left = 3
        Top = 34
        Width = 169
        Height = 25
        Caption = 'Process slice lookup'
        TabOrder = 0
        OnClick = BtnProcessSliceLookupClick
      end
      object Memo1: TMemo
        Left = 6
        Top = 200
        Width = 169
        Height = 460
        Lines.Strings = (
          'Memo1')
        TabOrder = 1
      end
      object BtnProcess_7x7_CountLookup: TButton
        Left = 3
        Top = 3
        Width = 169
        Height = 25
        Caption = 'Process 7x7 count lookup'
        TabOrder = 2
        OnClick = BtnProcess_7x7_CountLookupClick
      end
      object StringGrid1: TStringGrid
        Left = 181
        Top = 157
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
        OnDblClick = StringGrid1DblClick
        OnDrawCell = StringGrid2DrawCell
        OnMouseUp = StringGrid1MouseUp
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
      object BtnAppyLookupTable: TButton
        Left = 487
        Top = 34
        Width = 418
        Height = 25
        Caption = 'Appy lookup table'
        TabOrder = 4
        OnClick = BtnAppyLookupTableClick
      end
      object StringGrid2: TStringGrid
        Left = 690
        Top = 177
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
      object BtnSolveRoundUsingChunks: TButton
        Left = 487
        Top = 64
        Width = 418
        Height = 25
        Caption = 'Solve a round using chunks'
        TabOrder = 6
        OnClick = BtnSolveRoundUsingChunksClick
      end
      object BtnClearGrid: TButton
        Left = 487
        Top = 126
        Width = 418
        Height = 25
        Caption = 'Clear grid'
        TabOrder = 7
        OnClick = BtnClearGridClick
      end
      object BtnTestLookup0_012: TButton
        Left = 487
        Top = 3
        Width = 138
        Height = 25
        Caption = 'Test lookup0, 012'
        TabOrder = 8
        OnClick = BtnTestLookup0_012Click
      end
      object StringGrid3: TStringGrid
        Left = 1098
        Top = 177
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
      object BtnMinimalSolve: TButton
        Left = 248
        Top = 64
        Width = 233
        Height = 25
        Caption = 'Minimal solve'
        TabOrder = 10
        OnClick = BtnMinimalSolveClick
      end
      object BtnSolveCounter: TButton
        Left = 944
        Top = 31
        Width = 233
        Height = 25
        Caption = 'Solve counter'
        TabOrder = 11
        OnClick = BtnSolveCounterClick
      end
      object BtnRotateCounter: TButton
        Left = 944
        Top = 0
        Width = 233
        Height = 25
        Caption = 'Rotate counter'
        TabOrder = 12
        OnClick = BtnRotateCounterClick
      end
      object BtnSliverSolveRound: TButton
        Left = 487
        Top = 95
        Width = 418
        Height = 25
        Action = Action_SliverSolveRound
        TabOrder = 13
      end
      object BtnInitWith_GoE: TButton
        Left = 248
        Top = 95
        Width = 233
        Height = 25
        Caption = 'Init with GoE'
        TabOrder = 14
        OnClick = BtnInitWith_GoEClick
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
      object BtnSolveWithChunkLookup: TButton
        Left = 248
        Top = 34
        Width = 187
        Height = 25
        Caption = 'Solve with chunk-lookup'
        TabOrder = 16
        OnClick = BtnSolveWithChunkLookupClick
      end
      object BtnApplyNELookupTables: TButton
        Left = 1201
        Top = 0
        Width = 300
        Height = 25
        Caption = 'Apply N/E lookup tables'
        TabOrder = 17
        OnClick = BtnApplyNELookupTablesClick
      end
      object BtnApplyCornerLookupTables: TButton
        Left = 1201
        Top = 31
        Width = 300
        Height = 25
        Caption = 'Apply corner lookup tables'
        TabOrder = 18
        OnClick = BtnApplyCornerLookupTablesClick
      end
      object StringGrid5: TStringGrid
        Left = 690
        Top = 583
        Width = 811
        Height = 77
        ColCount = 80
        DefaultColWidth = 40
        DefaultRowHeight = 56
        FixedCols = 0
        RowCount = 1
        FixedRows = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 19
        OnClick = StringGrid5DblClick
        OnDblClick = StringGrid5DblClick
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
          40
          40
          40
          40
          40)
        RowHeights = (
          56)
      end
      object BtnInitWithGoE2: TButton
        Left = 248
        Top = 126
        Width = 233
        Height = 25
        Caption = 'Init with GoE2'
        TabOrder = 20
        OnClick = BtnInitWithGoE2Click
      end
      object Button1: TButton
        Left = 3
        Top = 126
        Width = 170
        Height = 25
        Caption = 'SolveAndTime'
        TabOrder = 21
        OnClick = Button1Click
      end
      object BtnOld_SolveAndTime: TButton
        Left = 3
        Top = 95
        Width = 169
        Height = 25
        Caption = 'Old_SolveAndTime'
        TabOrder = 22
        OnClick = BtnOld_SolveAndTimeClick
      end
      object BtnReverseLookup: TButton
        Left = 631
        Top = 3
        Width = 274
        Height = 25
        Caption = 'Reverse lookup left-right'
        TabOrder = 23
        OnClick = BtnReverseLookupClick
      end
      object BtnOldNewSolveInLockstep: TButton
        Left = 3
        Top = 64
        Width = 169
        Height = 25
        Caption = 'Old_new in lockstep'
        TabOrder = 24
        OnClick = BtnOldNewSolveInLockstepClick
      end
      object BtnDictSolveAndTime: TButton
        Left = 3
        Top = 157
        Width = 169
        Height = 25
        Caption = 'Dictionary Solve && Time'
        TabOrder = 25
        OnClick = BtnDictSolveAndTimeClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      object SGSliceLayout: TStringGrid
        Left = 136
        Top = 180
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
      object BtnValidateN1E1LookupTable: TButton
        Left = 488
        Top = 184
        Width = 377
        Height = 25
        Caption = 'ValidateN1E1 lookup table'
        TabOrder = 0
        OnClick = BtnValidateN1E1LookupTableClick
      end
      object BtnValidateCountTable: TButton
        Left = 488
        Top = 232
        Width = 377
        Height = 25
        Caption = 'ValidateCountTable'
        TabOrder = 1
        OnClick = BtnValidateCountTableClick
      end
      object SGMinOn: TStringGrid
        Left = 512
        Top = 284
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
        Left = 712
        Top = 284
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
      object BtnCreateUnknownLookupTable: TButton
        Left = 488
        Top = 136
        Width = 377
        Height = 25
        Caption = 'Create unknown lookup table'
        TabOrder = 4
        OnClick = BtnCreateUnknownLookupTableClick
      end
      object BtnCreateLookupUsingSolver: TButton
        Left = 488
        Top = 488
        Width = 377
        Height = 25
        Caption = 'Create lookup 5x5 -> 3x3'
        TabOrder = 5
        OnClick = BtnCreateLookupUsingSolverClick
      end
      object Memo2: TMemo
        Left = 3
        Top = 16
        Width = 462
        Height = 529
        Lines.Strings = (
          'Memo2')
        ScrollBars = ssVertical
        TabOrder = 6
      end
      object BtnTest_TSliceNextSetBit: TButton
        Left = 488
        Top = 48
        Width = 233
        Height = 25
        Caption = 'Test TSlice.NextSetBit'
        TabOrder = 7
        OnClick = BtnTest_TSliceNextSetBitClick
      end
      object BtnTestCalcSouth: TButton
        Left = 488
        Top = 88
        Width = 233
        Height = 25
        Caption = 'TestCalcSouth'
        TabOrder = 8
        OnClick = BtnTestCalcSouthClick
      end
      object BtnTestDeleteBit: TButton
        Left = 792
        Top = 16
        Width = 75
        Height = 25
        Caption = 'TestDeleteBit'
        TabOrder = 9
        OnClick = BtnTestDeleteBitClick
      end
      object ProgressBar1: TProgressBar
        Left = 3
        Top = 584
        Width = 1026
        Height = 33
        Max = 1023
        Smooth = True
        MarqueeInterval = 1
        Step = 1
        TabOrder = 10
      end
      object BtnCreateLookup5x5to3x3UsingSpeculativeExploration: TButton
        Left = 488
        Top = 528
        Width = 377
        Height = 25
        Caption = 'Create lookup 5x5 -> 3x3 using Speculative Exploration'
        TabOrder = 11
        OnClick = BtnCreateLookup5x5to3x3UsingSpeculativeExplorationClick
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Unit Tests'
      ImageIndex = 4
      object Memo3: TMemo
        Left = 16
        Top = 24
        Width = 425
        Height = 393
        Lines.Strings = (
          'Memo3')
        TabOrder = 0
      end
      object BtnStartUnitTests: TButton
        Left = 528
        Top = 176
        Width = 153
        Height = 25
        Caption = 'Start unit tests'
        TabOrder = 1
        OnClick = BtnStartUnitTestsClick
      end
      object BtnDoFailingTests: TButton
        Left = 528
        Top = 232
        Width = 153
        Height = 25
        Caption = 'Do failing tests'
        TabOrder = 2
        OnClick = BtnDoFailingTestsClick
      end
      object BtnRunSingleTest: TButton
        Left = 528
        Top = 320
        Width = 153
        Height = 25
        Caption = 'Run a single test-If needed'
        TabOrder = 3
        OnClick = BtnRunSingleTestClick
      end
      object BtnValidateCompressedLookupTable: TButton
        Left = 528
        Top = 368
        Width = 153
        Height = 25
        Caption = 'Validate compressed lookup'
        TabOrder = 4
        OnClick = BtnValidateCompressedLookupTableClick
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Search'
      ImageIndex = 5
      object BtnSearchGoE: TButton
        Left = 104
        Top = 24
        Width = 225
        Height = 25
        Caption = 'BtnSearchGoE'
        TabOrder = 0
        OnClick = BtnSearchGoEClick
      end
      object MemoGoE_solution: TMemo
        Left = 56
        Top = 80
        Width = 377
        Height = 513
        Lines.Strings = (
          'MemoGoE_solution')
        TabOrder = 1
      end
      object Btn5x5To3x3_Lookup: TButton
        Left = 592
        Top = 24
        Width = 137
        Height = 25
        Caption = 'Make 5x5->3x3 lookup'
        TabOrder = 2
        OnClick = Btn5x5To3x3_LookupClick
      end
    end
  end
  object SGAliveDiff: TStringGrid
    Left = 1318
    Top = 122
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
    Top = 122
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
    Top = 122
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
    Top = 121
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
    Left = 1192
    Top = 24
  end
  object FileSaveDialog1: TFileSaveDialog
    DefaultExtension = 'bin'
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 1196
    Top = 75
  end
  object AppRegistry: TJvAppRegistryStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    StorageOptions.BooleanAsString = False
    Root = 'Software\LifeSATSearch'
    SubStorages = <>
    Left = 924
    Top = 27
  end
  object Taskbar1: TTaskbar
    TaskBarButtons = <>
    ProgressMaxValue = 100
    TabProperties = []
    Left = 424
    Top = 16
  end
  object ActionManager1: TActionManager
    Left = 472
    Top = 16
    StyleName = 'Platform Default'
    object Action_SliverSolveRound: TAction
      Caption = 'Solve a round using slivers'
      ShortCut = 112
      OnExecute = Action_SliverSolveRoundExecute
    end
  end
end
