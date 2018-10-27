DEFINT I-N
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Programable HandHeld Calculator        Quick Calc II (c)                 ³
' ³                                                                          ³
' ³ Written by David Packard   July 1990  Original 1.00.00                   ³
' ³                            Feb  1991  Revision 1.00.03                   ³
' ³                                                                          ³
' ³ Compatible with:                                                         ³
' ³               Microsoft Quick Basic Version 4.5                          ³
' ³               Crescent Software QuickPak Professional Version 3.16       ³
' ³                                                                          ³
' ³               Microsoft Basic Professional Development System Version 7  ³
' ³                                                                          ³
' ³ Requires EGA or VGA video hardware, mouse optional                       ³
' ³                                                                          ³
' ³ Three files are required to run this program:                            ³
' ³                                                                          ³
' ³        HandHeld.Exe  -  the program proper                               ³
' ³        UsrFnct.Crd   -  the user defined function key definitions        ³
' ³                         80 byte random access file                       ³
' ³        HHHelp.Txt    -  the instruction manual                           ³
' ³                         80 byte random access file                       ³
' ³                                                                          ³
' ³ Compile instructions:                                                    ³
' ³        BC HandHeld,,NUL /E/O/S;                                          ³
' ³      LINK HandHeld,,NUL,PRO.LIB  (or PRO7.LIB if using BASIC PDS 7)      ³
' ³                                                                          ³
' ³    Where: PRO.LIB Crescent Software QuickPak Professional Library        ³
' ³                                                                          ³
' ³ Note: The /EXEPACK option may be used during the linking option, this    ³
' ³       option will reduce the executable file size from 132K to 124K      ³
' ³       bytes.  However several after market compression routines are      ³
' ³       available that will further reduce the packed executable to 80K    ³
' ³       bytes and provide automatic un-packing.  Tests should be run with  ³
' ³       the /E[XEPACK] option and the packing program to find the mimimum  ³
' ³       file size.  Linking with the PDS7.1 stub files NoCGA, NoCOM,       ³
' ³       NoEMS, NoEVENT, NoHERC, NoISAM, NoOGA, NoVGA, and SmallERR appears ³
' ³       to further reduce the executable file size by 6K.                  ³
' ³                                                                          ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Programable HandHeld Calculator        Quick Calc II (c)   Feburary 1991 ³
' ³ Copyrighted by  David G. Packard       All rights reserved               ³
' ³                 11 Lavery Lane                                           ³
' ³                 Milford, CT. 06460                                       ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Quick Basic and BASIC Professional Development System are copyrighted    ³
' ³ products of Microsoft Corporation                                        ³
' ³                                                                          ³
' ³ QuickPak Professional is a copyrighted product of                        ³
' ³ Crescent Software Inc.                                                   ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      'Subroutines from QuickPak Professional

      DECLARE SUB APrint0 (BYVAL IAddress, INumEls, IMidFirst, INumChars, IColr)
      DECLARE SUB Calendar (IYear, IMonth, IDay, INew)                         '**** Modified ****
      DECLARE SUB GetVMode (IMode, IPage, IPageSize, IRows, IColumns)
      DECLARE SUB Mouse (IA, IB, IC, ID)
      DECLARE SUB PaintBox0 (IULRow, IULCol, ILRRow, ILRCol, IColor)
      DECLARE SUB PrtSc (ILPTNum, IPage)
      DECLARE SUB QPrint0 (X$, IColr)
      DECLARE SUB ScrnSave0 (IULRow, IULCol, ILRRow, ILRCol, SEG IAddress)
      DECLARE SUB ScrnRest0 (IULRow, IULCol, ILRRow, ILRCol, SEG IAddress)
      DECLARE SUB VertMenu (IChoice, IMaxLen, IBoxBot, Ky$, IAction)           '**** Modified ****

      'Functions from QuickPak Professional

      DECLARE FUNCTION ASCII% (Strng$)
      DECLARE FUNCTION Compact$ (Spaced$)
      DECLARE FUNCTION Date2Num% (Dat$)
      DECLARE FUNCTION DOSVer% ()
      DECLARE FUNCTION Evaluate# (Equation$)                                   '**** Modified ****
      DECLARE FUNCTION Exist% (FileName$)
      DECLARE FUNCTION Factorial# (IZeroTo170)
      DECLARE FUNCTION FileSize& (FileName$)
      DECLARE FUNCTION FindLast% (BYVAL IAddress, ISize)
      DECLARE FUNCTION InCount% (Source$, Search$)
      DECLARE FUNCTION MaxInt% (I, J)
      DECLARE FUNCTION MinInt% (I, J)
      DECLARE FUNCTION Monitor% ()
      DECLARE FUNCTION QInstrB% (IStart, Text$, Search$)
      DECLARE FUNCTION QPLen% (Strng$)
      DECLARE FUNCTION PMatch% (Work$)

      'Subroutines unique to HandHeld

      DECLARE SUB CardEdit80Col (FileName$, INumLines, IFormat, IRow, INormalColor, IEditColor, IEditChr, INumOnly, Message$, IError)
      DECLARE SUB DefineUser ()
      DECLARE SUB DigitalTime ()
      DECLARE SUB DrawHandHeld ()
      DECLARE SUB DrawHands ()
      DECLARE SUB EditSyntax (Temp$)
      DECLARE SUB EraseHands ()
      DECLARE SUB FormatOutput (IRow, IFormat, IDecimal, IFix)
      DECLARE SUB GetKey (IKey, IDisplay, IExtraTrap, Ky$)
      DECLARE SUB HHTurboDuck ()
      DECLARE SUB InfoBox (Info, Message$)
      DECLARE SUB Instructions ()
      DECLARE SUB Logo ()
      DECLARE SUB Mask ()
      DECLARE SUB PanCalendar ()
      DECLARE SUB ReadEqFile ()
      DECLARE SUB ReadHHData ()
      DECLARE SUB ScrnOut (FileName$, IMaxChar)
      DECLARE SUB VertMenuVars ()



' Common Block

      TYPE Config
                MonTyp AS INTEGER           'Monitor Type
                PulBar AS INTEGER           'Horizontal Bar Color
                MenBox AS INTEGER           'Pulled down Box Color
                ActivCh AS INTEGER          'Active menu choice color
                InActCh AS INTEGER          'Inactive (Non-selectable) Choice Color
                HiLite AS INTEGER           'Menu Bar Color for active choices
                AltHiLite AS INTEGER        'Menu Bar Color for active choices  Alternate character
                InActHiLt AS INTEGER        'Dito for inactive choices
                NonMen AS INTEGER           'Normal screen color
                CurSize AS INTEGER          'Cursor Scan Lines
                Mouse AS INTEGER            'Mouse in residence Flag
                MouseButtons AS INTEGER     'Number of buttons

      END TYPE

      COMMON SHARED Cnf AS Config

      COMMON SHARED IBoxULRow
      COMMON SHARED IBoxULCol
      COMMON SHARED IBoxLRRow
      COMMON SHARED IBoxLRCol

      COMMON SHARED IBlack
      COMMON SHARED IBlue
      COMMON SHARED IGreen
      COMMON SHARED ICyan
      COMMON SHARED IRed
      COMMON SHARED IMagenta
      COMMON SHARED IBrown
      COMMON SHARED IWhite
      COMMON SHARED IHBlack
      COMMON SHARED IHBlue
      COMMON SHARED IHGreen
      COMMON SHARED IHCyan
      COMMON SHARED IHRed
      COMMON SHARED IHMagenta
      COMMON SHARED IHBrown
      COMMON SHARED IHWhite

      COMMON SHARED IXMouseLast
      COMMON SHARED IYMouseLast
     
      CLEAR , , 4096


' Set initial values

      NChoice = 0                           'Dimension VertMenu variables
      REDIM VChoice$(NChoice)
      REDIM VAltChoice$(NChoice)
      REDIM IVAltChoice(NChoice)
      REDIM VHelp$(NChoice)

      IBlack = 0
      IBlue = 1
      IGreen = 2
      ICyan = 3
      IRed = 4
      IMagenta = 5
      IBrown = 6
      IWhite = 7
      IHBlack = 8
      IHBlue = 9
      IHGreen = 10
      IHCyan = 11
      IHRed = 12
      IHMagenta = 13
      IHBrown = 14
      IHWhite = 15

      IBoxULRow = 11
      IBoxULCol = 1
      IBoxLRRow = 24
      IBoxLRCol = 56

      IEditChr = 32

      IBoxColor = IWhite * 16 + IBlue
      IEditColor = IBlack * 16 + IHWhite
      IFillColor = IWhite * 16 + IBlack
      INormalColor = IWhite * 16 + IBlack
      ITextColor = IWhite * 16 + IBlack
      IWarningColor = IWhite * 16 + IHWhite
      IErrorColor = IWhite * 16 + IHBrown
      ISundayColor = IWhite * 16 + IRed

      IVertMenuBackGroundColor = 52         'Color 52 is orange

      CALL InitMouse(Cnf.Mouse)

      IF Cnf.Mouse THEN
             IA = 0
             CALL Mouse(IA, IB, IC, ID)
             Cnf.Mouse = IA
             Cnf.MouseButtons = IB
      END IF

      Cnf.MonTyp = Monitor%
      SELECT CASE Cnf.MonTyp
             CASE 3, 5, 7       'CGA, EGA/Color or VGA/Color monitors
                    Cnf.PulBar = IWhite * 16 + IBlack
                    Cnf.MenBox = IGreen * 16 + IHWhite
                    Cnf.ActivCh = IGreen * 16 + IBlack
                    Cnf.InActCh = IGreen * 16 + IHBrown
                    Cnf.HiLite = IWhite * 16 + IBlack
                    Cnf.AltHiLite = IWhite * 16 + IHWhite
                    Cnf.InActHiLt = IGreen * 16 + IBlack
                    Cnf.NonMen = IWhite * 16 + IBlack
                    Cnf.CurSize = 13
                    IF Cnf.MonTyp = 5 THEN Cnf.CurSize = 7
             CASE ELSE
                    Cnf.PulBar = IWhite * 16 + IBlack
                    Cnf.MenBox = IWhite * 16 + IHWhite
                    Cnf.ActivCh = IWhite * 16 + IBlack
                    Cnf.InActCh = IWhite * 16 + IBlack
                    Cnf.HiLite = IBlack * 16 + IHWhite
                    Cnf.AltHiLite = IBlack * 16 + IHWhite
                    Cnf.InActHiLt = IBlack * 16 + IHWhite
                    Cnf.NonMen = IWhite * 16 + IBlack
                    Cnf.CurSize = 13
      END SELECT

      WarningSound$ = "T150 L16 MS O4 GGGG GGEE AAAA"


      REDIM Month$(12)
      RESTORE CalendarMonths
      FOR I = 1 TO 12
             READ Month$(I)
      NEXT I

      PALETTE                             'Default color values
      ISizeofText = 3                     'Default size of text       in routine ScrnOut
      IHighLighterColor = 7               'Default high lighter color in routine ScrnOut
      IF Cnf.Mouse THEN
             CALL GetCursor(IXMouseLast, IYMouseLast, IButton)
      END IF

      CurrentDate$ = DATE$                    'Read system date
      IYear = VAL(RIGHT$(CurrentDate$, 4))    'Two copies of the current values
      IMonth = VAL(LEFT$(CurrentDate$, 2))
      IDay = VAL(MID$(CurrentDate$, 4, 2))
      ISystemYear = IYear
      ISystemMonth = IMonth
      ISystemDay = IDay

      IPaletteColor = IBlue                   'Palette color
                                              'NOTE: Color 2 (Green) is replaced by this color
      CALL Mask                               'Display universal mask                  
      CALL Logo                               'Display program logo


' Display start up information in the Information Box

      CALL InfoBox(0, "Q u i c k   C a l c   I I   (c)")
      LOCATE IBoxULRow + 3, 2
      CALL QPrint0("Author  -  D.Packard", ITextColor)
      LOCATE IBoxULRow + 4, 2
      CALL QPrint0("Version 01.00.03   February 1991", ITextColor)

      IF Cnf.MonTyp < 5 THEN

             LOCATE IBoxULRow + 7, 4
             CALL QPrint0("   Program requires EGA or VGA video . . . .", IErrorColor)
             PLAY WarningSound$
             LOCATE IBoxULRow + 11, 4
             CALL QPrint0("Depress any key to end . . . ", ITextColor)
             CALL GetKey(3, 1, 0, Ky$)
             END
      END IF

      LOCATE IBoxULRow + 6, 2
      CALL QPrint0("Compiled with:", ITextColor)
      LOCATE IBoxULRow + 7, 2
      CALL QPrint0("Microsoft        - Professional Development System", ITextColor)
      LOCATE IBoxULRow + 8, 2
      CALL QPrint0("Cresent Software - QuickPak Professional          ", ITextColor)
      LOCATE IBoxULRow + 10, 2
      CALL QPrint0("All rights reserved", ITextColor)
      LOCATE IBoxULRow + 12, 2
      CALL QPrint0("Depress any key to continue . . . . ", ITextColor)

      CALL GetKey(3, 1, 0, Ky$)

      IRows = 6: ICols = 10: IKeys = 59: IEquations = 10
      IChoice = 1

      REDIM SHARED Key$(IRows, ICols)
      REDIM SHARED SecondKey$(IRows, ICols)
      REDIM SHARED Equation$(IEquations)
      REDIM SHARED Expression$(IEquations)
      REDIM SHARED Prompt$(IEquations)
      REDIM SHARED IASCtoKey(IKeys)

      CALL ReadHHData                       'Read all data statements
      CALL ReadEqFile                       'Read user defined equation file
      CALL VertMenuVars                     'Setup vertical menu variables


HandHeldBegin:

      IPaletteColor = IVertMenuBackGroundColor
      CALL Mask

      CALL InfoBox(0, "U s e r   D e f i n e d   F u n c t i o n s")

'     Display user defined prompt messages

      FOR I = 1 TO 10
             LOCATE IBoxULRow + 2 + I, 2
             CALL QPrint0(RIGHT$(STR$(I), 2) + " " + LEFT$(Prompt$(I), 50), ITextColor)
      NEXT

      ILastChoice = IChoice

      DO
             CALL GetKey(6, 0, 0, Ky$)
             IChoice = ILastChoice       'Use last known choice
             LOCATE 3, 20, 0
             IAction = 0
             CALL VertMenu(IChoice, IMaxLen, IBoxBot, Ky$, IAction)
             ILastChoice = IChoice       'Remember this choice

      LOOP WHILE ASC(Ky$) <> 13

      SELECT CASE IChoice
             CASE 1 ' Hand Held Calculator
                    GOSUB HandHeldCalculator
             CASE 2 ' Edit User Defined Keys
                    CALL DefineUser
                    CALL ReadEqFile
             CASE 3 ' Instructions
                    CALL Instructions
             CASE 4 ' Pan Calendar
                    CALL PanCalendar
             CASE 5 ' Quit
                    GOTO Quit
      END SELECT
      GOTO HandHeldBegin


HandHeldCalculator:

' Setup all variables for calculator

      REDIM Stack$(0)

      DIM SHARED HHPi AS DOUBLE         'Hand Held Pi
      DIM SHARED HHe AS DOUBLE          'Hand Held base of natural logarithm
      DIM SHARED HHW AS DOUBLE          'Hand Held W register non-displayable
      DIM SHARED HHX AS DOUBLE          'Hand Held X register
      DIM SHARED HHY AS DOUBLE          'Hand Held Y register
      DIM SHARED HHZ AS DOUBLE          'Hand Held Z register
      DIM SHARED HHMemory AS DOUBLE     'Hand Held Memory variable
      DIM HHCarry AS DOUBLE             'Carry variable for roll thru carry functions
      DIM SHARED Again AS INTEGER       'Re-entry variable in routine Evaluate
      DIM SHARED IDRG AS INTEGER        'Degree / Radian / Gradient units toggle

      DIM SHARED IClockX AS INTEGER     'Analog clock center position - Horizontial
      DIM SHARED IClockY AS INTEGER     'Analog clock center position - Vertical
      DIM SHARED IStartRow AS INTEGER   'Starting row of Hand Held Calculator

      IDRG = 0                          'Degree / Radian / Gradian toggle
      IFormat = 2                       'Scientific / Engineering / None toggle
      IDecimal = 1                      'Fix / Float toggle
      IFix = 3                          'Number of decimal points
      HHPi = 3.14159265358979#          'Pi
      HHe = 2.71828182845905#           'Base of natural logarithm

      IClockX = 544                     'Center of analog clock  X
      IClockY = 54                      'Center of analog clock  Y
      IClockSecHandLength = 28          'Second hand length
      AspectRatio = 4 * (350 / 640) / 3 'Video aspect ratio, note EGA size 640 by 350
      ISecondColor = IHBrown            'Color of clock hand  second
      IBGColor = IHBlack                'Background color of calculator
      IStartRow = 3                     'Starting row of calculator

      CALL DrawHandHeld                 'Draw the calculator

      FOR J = 0 TO 9                    'Draw the user defined key notation
             LOCATE IStartRow + 30, 7 + J * 7
             PRINT Key$(6, J + 1);
      NEXT J

      GOSUB HHZeroX: HHY = 0: HHZ = 0   'Zero registers

      CALL InitMouse(IMouse)            'Initialize mouse
      IF Cnf.Mouse THEN                 'If attached
             CALL ShowCursor            'Display graphics cursor
             CALL Mouse(7, 0, 48, 590)  'Set horizontal range
             CALL Mouse(8, 0, 88, 270)  'Set vertical range
             CALL SetCursor(590, 270)   'Locate in lower right corner
      END IF

      GOSUB HHShowResults               'Display modes and register values

ClockBegin:                           
      CurrentTime$ = TIME$              'Get system time
      CALL DrawHands                    'Draw the clock hands
      IHour = VAL(LEFT$(CurrentTime$, 2))       'Measure time from hh:00:00
      IMinute = VAL(MID$(CurrentTime$, 4, 2))
      ISecond = VAL(RIGHT$(CurrentTime$, 2))
      IStartSec = IMinute * 60 + ISecond
      IStartMin = IHour * 60 + IMinute

LoopSecond:
      GOSUB SecondUpDate                'Update second hand (dot)
      CALL DigitalTime                  'Update digital clock

HHLoopKey:

      DO                                                                       'ÄÄÄÄÄÄ¿
             CALL GetKey(4, 0, 1, Ky$)                                               '³
                                                                                     '³
             IF Cnf.Mouse THEN CALL GetCursor(IX, IY, IButton)                       '³
                                                                                     '³
             IF Ky$ <> "" THEN                                                       '³
                    GOSUB EraseSecond                                                '³
                    GOTO HHKeyDepressed                                              '³
             END IF                                                                  '³
                                                                                     '³
             IF Cnf.Mouse THEN                                                       '³
                    IF IButton THEN                                                  '³
                           GOSUB EraseSecond                                         '³
                           GOTO HHMouseTrap                                          '³
                    END IF                                                           '³
             END IF                                                                  '³
                                                                                     '³
             LoopTime$ = TIME$                                                       '³
             ILoopSec = VAL(MID$(LoopTime$, 4, 2)) * 60 + VAL(RIGHT$(LoopTime$, 2))  '³
             ILoopMin = VAL(LEFT$(LoopTime$, 2)) * 60 + VAL(MID$(LoopTime$, 4, 2))   '³
                                                                                     '³
      LOOP WHILE (ILoopSec - IStartSec) = 0                                    'ÄÄÄÄÄÄÙ


      GOSUB EraseSecond

      IF (ILoopMin - IStartMin) <> 0 THEN
             CALL EraseHands
             GOTO ClockBegin
      END IF

      NextTime$ = TIME$
      IStartSec = VAL(MID$(NextTime$, 4, 2)) * 60 + VAL(RIGHT$(NextTime$, 2))
      GOTO LoopSecond

HHKeyDepressed:

      Ky$ = UCASE$(Ky$)                             'Upper case
      IF LEN(Ky$) = 1 THEN IStart = 0: IEnd = 49    'Search first fifty positions
      IF LEN(Ky$) = 2 THEN Ky$ = RIGHT$(Ky$, 1): IStart = 50: IEnd = 59  'Search last ten positions
      IKey = ASC(Ky$)                       'ASCII value
      IPosition = 0                         'Zero position
      FOR I = IStart TO IEnd                'Enter loop search
             IF IASCtoKey(I) = IKey THEN    'Is the key in the table?
                    IPosition = I           'Then the position is the loop counter value
                    GOSUB HHCalculate       'Do the calculation
                    GOSUB HHShowResults     'Display the results
                    GOTO HHLoopKey          'Go back for another key
             END IF
      NEXT I                                'End the loop search

HHMouseTrap:
      IF Cnf.Mouse = 0 THEN GOTO HHLoopKey  'Avoid following section if Mouse is not detected

      CALL GetCursor(IX, IY, IButton)

      IF IButton AND IX >= 496 AND IX <= 590 AND IY >= 216 AND IY <= 238 THEN
             IPosition = 48                 'This must be the large <E>nter key
             GOSUB HHCalculate              'Do the calculation
             WHILE IButton > 0              'Wait for button release
                    CALL GetCursor(IX, IY, IButton)
             WEND
             GOSUB HHShowResults            'Display the results
             GOTO HHLoopKey                 'Go back for another key
      END IF

'     Button was depressed that was not on the large <E>nter key
'     Compute values for  ICol (1 - 10)  and  IRow  (1 - 6)
'     If cursor was not on a key one of the values will be zero

      FOR ICount = 1 TO 10                  'Begin search loop
             JMin = 56 * ICount - 8         'Left  edge
             JMax = JMin + 38               'Right edge
             IF IX >= JMin AND IX <= JMax THEN  'On a key column
                    ICol = ICount           'Then the column is the loop counter value
                    GOTO HHColumnExit       'Jump out
             END IF
      NEXT ICount                           'Loop
      ICol = 0                              'Search failed Zero column
HHColumnExit:
                                           
      FOR ICount = 1 TO 6                   'Begin search loop
             JMin = 32 * ICount + 56        'Top edge
             JMax = JMin + 22               'Bottom edge
             IF IY >= JMin AND IY <= JMax THEN  'On a key row
                    IRow = ICount           'Then the row is the loop counter value
                    GOTO HHRowExit          'Jump out
             END IF
      NEXT ICount                           'Loop
      IRow = 0                              'Search failed Zero row
HHRowExit:

      IF IRow * ICol = 0 THEN GOTO HHLoopKey    'If either is zero go back for another key

      IF IButton THEN

             IRow = IRow - 1: ICol = ICol - 1   'Shift to allow zero values
             IPosition = IRow * 10 + ICol       'Compute position (0-59)
             GOSUB HHCalculate                  'Do the calculation
             WHILE IButton > 0                  'Wait for button release
                    CALL GetCursor(IX, IY, IButton)
             WEND
             GOSUB HHShowResults                'Display results

      END IF
      GOTO HHLoopKey                            'Go back for another key

HHCalculate:
      ON ERROR GOTO HHErrorTrap                 'Set a local error trap

      SELECT CASE IPosition                     'These are the calculator buttons
             CASE 0 ' Esc
                    RETURN HandHeldExit
             CASE 1 ' Clr
                    GOSUB HHZeroX: HHY = 0: HHZ = 0
             CASE 2 ' CE
                    GOSUB HHZeroX
             CASE 3 ' DRG
                    IF IDRG = 0 THEN HHX = HHX * HHPi / 180#: Stack$(0) = STR$(HHX)
                    IF IDRG = 1 THEN HHX = HHX * 180# / HHPi * 100# / 90#: Stack$(0) = STR$(HHX)
                    IF IDRG = 2 THEN HHX = HHX * 90# / 100#: Stack$(0) = STR$(HHX)
                    IDRG = IDRG + 1: IDRG = IDRG MOD 3
             CASE 4 '

             CASE 5 ' X¯M
                    HHMemory = HHX: GOSUB HHZeroX
             CASE 6 '7
                    Num = 7: GOSUB HHNumber
             CASE 7 '8
                    Num = 8: GOSUB HHNumber
             CASE 8 '9
                    Num = 9: GOSUB HHNumber
             CASE 9 'ö
                    IF HHX <> 0 THEN HHY = HHY / HHX: GOSUB HHZeroX
             CASE 10 'Sin
                    HHW = HHX
                    IF IDRG = 0 THEN HHW = HHW * HHPi / 180
                    IF IDRG = 2 THEN HHW = HHW * 90 / 100 * HHPi / 180
                    HHX = SIN(HHW): Stack$(0) = STR$(HHX)
             CASE 11 'Cos
                    HHW = HHX
                    IF IDRG = 0 THEN HHW = HHW * HHPi / 180
                    IF IDRG = 2 THEN HHW = HHW * 90 / 100 * HHPi / 180
                    HHX = COS(HHW): Stack$(0) = STR$(HHX)
             CASE 12 'Tan
                    HHW = HHX
                    IF IDRG = 0 THEN HHW = HHW * HHPi / 180
                    IF IDRG = 2 THEN HHW = HHW * 90 / 100 * HHPi / 180
                    HHX = TAN(HHW): Stack$(0) = STR$(HHX)
             CASE 13 'Roll Up
                    HHCarry = HHZ: HHZ = HHY: HHY = HHX: HHX = HHCarry: Stack$(0) = STR$(HHX)
             CASE 14 'Roll Down
                    HHCarry = HHX: HHX = HHY: HHY = HHZ: HHZ = HHCarry: Stack$(0) = STR$(HHX)
             CASE 15 'M¯X
                    HHX = HHMemory: Stack$(0) = STR$(HHX)
             CASE 16 '4
                    Num = 4: GOSUB HHNumber
             CASE 17 '5
                    Num = 5: GOSUB HHNumber
             CASE 18 '6
                    Num = 6: GOSUB HHNumber
             CASE 19 'X
                    HHY = HHY * HHX: GOSUB HHZeroX
             CASE 20 'Ln
                    IF HHX > 0 THEN HHX = LOG(HHX): Stack$(0) = STR$(HHX)
             CASE 21 'Log
                    IF HHX > 0 THEN HHX = LOG(HHX) / LOG(10#): Stack$(0) = STR$(HHX)
             CASE 22 ' yü
                    HHY = HHY ^ HHX: GOSUB HHZeroX
             CASE 23 'Shift Up
                    HHZ = HHY: HHY = HHX: GOSUB HHZeroX
             CASE 24 'Shift Down
                    HHX = HHY: HHY = HHZ: HHZ = 0: Stack$(0) = STR$(HHX)
             CASE 25 'M+
                    HHMemory = HHMemory + HHX: GOSUB HHZeroX
             CASE 26 '1
                    Num = 1: GOSUB HHNumber
             CASE 27 '2
                    Num = 2: GOSUB HHNumber
             CASE 28 '3
                    Num = 3: GOSUB HHNumber
             CASE 29 '-
                    HHY = HHY - HHX: GOSUB HHZeroX
             CASE 30 'eü
                    HHX = EXP(HHX): Stack$(0) = STR$(HHX)
             CASE 31 '10ü
                    HHX = 10 ^ HHX: Stack$(0) = STR$(HHX)
             CASE 32 'üûy
                    IF HHX <> 0 THEN HHY = HHY ^ (1 / HHX): GOSUB HHZeroX
             CASE 33 'x y
                    SWAP HHX, HHY: Stack$(0) = STR$(HHX)
             CASE 34 'View
                    I = HHX
                    SELECT CASE I
                           CASE 1 TO 10
                                  LINE (0, 349 - 4 * 8)-(639, 349), IWhite, BF
                                  COLOR IBlue
                                  LOCATE 41, 1, 0: PRINT "Function Key  "; I;
                                  COLOR IHWhite
                                  LOCATE 42, 1, 0: PRINT Key$(6, I) + "    " + Prompt$(I);
                                  LOCATE 43, 1, 0: PRINT LEFT$(Equation$(I), 79);
                                  GOSUB HHZeroX
                           CASE ELSE
                                  LINE (0, 349 - 4 * 8)-(639, 349), IHBlue, BF
                    END SELECT
             CASE 35 'MC
                    HHMemory = 0
             CASE 36 '0
                    Num = 0: GOSUB HHNumber
             CASE 37 '.
                    IF IPoint = 0 THEN Stack$(0) = Stack$(0) + ".": IPoint = 1
             CASE 38 'Chg sign
                    HHX = -HHX: Stack$(0) = STR$(HHX)
             CASE 39 '+
                    HHY = HHY + HHX: GOSUB HHZeroX
             CASE 40 'ûx
                    IF HHX > 0 THEN HHX = SQR(HHX): Stack$(0) = STR$(HHX)
             CASE 41 'xı
                    HHX = HHX * HHX: Stack$(0) = STR$(HHX)
             CASE 42 '1/x
                    IF HHX <> 0 THEN HHX = 1 / HHX: Stack$(0) = STR$(HHX)
             CASE 43 'ã
                    HHX = HHPi: Stack$(0) = STR$(HHX)
             CASE 44 'e
                    HHX = HHe: Stack$(0) = STR$(HHX)
             CASE 45 'Format
                    IFormat = IFormat + 1
                    IFormat = IFormat MOD 3
             CASE 46 'Fix
                    IF HHX < 0 THEN HHX = 0#
                    IF HHX > 10 THEN HHX = 10#
                    IFix = HHX
             CASE 47 'Float
                    IDecimal = IDecimal + 1
                    IDecimal = IDecimal MOD 2
             CASE 48 'Enter key
                    HHY = HHX
                    GOSUB HHZeroX
             CASE 49 'Enter key
                    HHY = HHX
                    GOSUB HHZeroX
             CASE 50 TO 59 'User Function 1
                    I = IPosition - 49
                    GOSUB EvaluateFunction

             CASE ELSE
      END SELECT
      ON ERROR GOTO 0                   'Suspend local error trapping

      RETURN                            'Return to caller

HHShowResults:
      Display$ = SPACE$(30)
      IF IDRG = 0 THEN LSET Display$ = " Degree     "
      IF IDRG = 1 THEN LSET Display$ = " Radian     "
      IF IDRG = 2 THEN LSET Display$ = " Gradient   "

      MID$(Display$, 13, 5) = "     "

      IF IFormat = 0 THEN MID$(Display$, 18, 6) = "Sci   "
      IF IFormat = 1 THEN MID$(Display$, 18, 6) = "Eng   "
      IF IFormat = 2 THEN MID$(Display$, 18, 6) = "      "

      IF IDecimal = 0 THEN MID$(Display$, 24, 8) = "Fix  " + STR$(IFix)
      IF IDecimal = 1 THEN MID$(Display$, 24, 5) = "Float"

      COLOR IHCyan
      LOCATE IStartRow + 2, 20
      PRINT Display$ + "  -   Mode  "

      COLOR IBlue
      IRow = IStartRow + 3
      HHW = HHMemory
      CALL FormatOutput(IRow, IFormat, IDecimal, IFix)
      PRINT "  -  Memory "
      IRow = IRow + 1

      HHW = HHZ
      CALL FormatOutput(IRow, IFormat, IDecimal, IFix)
      PRINT "  -    Z    "
      IRow = IRow + 1

      HHW = HHY
      CALL FormatOutput(IRow, IFormat, IDecimal, IFix)
      PRINT "  -    Y    "

      LOCATE IStartRow + 6, 20
      Display$ = SPACE$(30)
      RSET Display$ = Stack$(0)
      IF Display$ = SPACE$(30) THEN RSET Display$ = "0"
      PRINT Display$ + "  -    X    "

      RETURN


HHNumber:
      Stack$(0) = LTRIM$(RTRIM$(Stack$(0)))
      IF Stack$(0) = "0" THEN Stack$(0) = ""
      Stack$(0) = Stack$(0) + RIGHT$(STR$(Num), 1)
      HHX = VAL(Stack$(0))
      RETURN

HHZeroX:
      HHX = 0
      Stack$(0) = ""
      IPoint = 0
      RETURN


SecondUpDate:
      SecTime$ = TIME$
      ISecond = VAL(RIGHT$(SecTime$, 2))
      XSecAngle = ISecond * 6 * 3.1459 / 180
      ICurrentDeltaXSec = IClockSecHandLength * SIN(XSecAngle)
      ICurrentDeltaYSec = IClockSecHandLength * COS(XSecAngle) * AspectRatio
      ISecPointX = IClockX + ICurrentDeltaXSec
      ISecPointY = IClockY - ICurrentDeltaYSec
      CIRCLE (ISecPointX, ISecPointY), 1, ISecondColor
      PSET (ISecPointX, ISecPointY), ISecondColor
      RETURN

EraseSecond:

      PSET (ISecPointX, ISecPointY), IBGColor
      CIRCLE (ISecPointX, ISecPointY), 1, IBGColor
      RETURN

HandHeldExit:
      ON ERROR GOTO 0
      IF Cnf.Mouse THEN CALL HideCursor
      SCREEN 0
      WIDTH 80, 25
      COLOR IHWhite, IWhite
      Ky$ = "": IButton = 0
      RETURN HandHeldBegin

EvaluateFunction:
      IF Expression$(I) <> "" THEN
             U# = Evaluate#(Expression$(I))
             IF LEFT$(Expression$(I), 1) = "%" THEN
                    ERROR 255
             ELSE
                    HHX = U#
                    Stack$(0) = STR$(HHX)
             END IF
      END IF
      RETURN




HHErrorTrap:

      CALL HHTurboDuck

      COLOR IBlue
      IF ERR = 6 THEN LOCATE IStartRow + 6, 30: PRINT "      Overflow Error  -    X    "
      IF ERR = 11 THEN LOCATE IStartRow + 6, 30: PRINT "Divide by Zero Error  -    X    "
      IF ERL = 10000 THEN LOCATE IStartRow + 6, 30: PRINT " User Function Error  -    X    "
      PRINT CHR$(7);
      CALL GetKey(2, 0, 1, Ky$)             'Flush Keyboard buffer

      IF Cnf.Mouse THEN                     'Wait for button release
             WHILE IButton = 1: CALL GetCursor(IX, IY, IButton): WEND
      END IF

      Ky$ = ""
      IButton = 0

      DO WHILE LEN(Ky$) = 0
             CALL GetKey(0, 0, 1, Ky$)
             IF Cnf.Mouse THEN
                    CALL GetCursor(IX, IY, IButton)
                    IF IButton THEN EXIT DO
             END IF
      LOOP

      LINE (150, 30)-(489, 73), 7, BF           'Display Box

      GOSUB HHZeroX
      GOSUB HHShowResults
      RESUME HHLoopKey

Quit:
      IPaletteColor = IBlue
      PALETTE IGreen, IPaletteColor
      CALL Logo
      CALL InfoBox(0, "Program Exit Request Input Screen")
      LOCATE IBoxULRow + 3, IBoxULCol + 4
      CALL QPrint0("    P R O G R A M   E X I T   R E Q U E S T", IWarningColor)
      LOCATE IBoxULRow + 10, IBoxULCol + 4
      CALL QPrint0("<N>o will return to Menu . . . . . . . . .", ITextColor)
      LOCATE IBoxULRow + 11, IBoxULCol + 4
      CALL QPrint0("<A>ny other key will proceed to exit . . .", ITextColor)
      LOCATE IBoxULRow + 7, IBoxULCol + 4
      CALL QPrint0("    Proceed with Exit Request   <Y> or <N>     ", IWarningColor)
      CALL GetKey(7, 1, 0, Ky$)
      IF Ky$ = "n" OR Ky$ = "N" THEN GOTO HandHeldBegin
      SCREEN 0
      COLOR IHWhite, IBlue, IBlue
      CLS
      CALL QPrint0("'HandHeld II' terminated normally . . . .", 1 * 16 + 15)
      END

CalendarMonths:
      DATA "January","February","March","April","May","June","July"
      DATA "August","September","October","November","December"

HHASCtoKey:         'ASCII values of buttons
      DATA  27,65,66,67,68,69,55,56,57,47
      DATA  70,71,72,73,74,75,52,53,54,42
      DATA  76,77,78,79,80,81,49,50,51,45
      DATA  82,83,84,85,86,87,48,46,92,43
      DATA  88,89,90,33,64,35,36,37,13,13
      DATA  59,60,61,62,63,64,65,66,67,68

HHKeyDefine:        'Primary key labels
      DATA " OFF "," Clr "," CE  "," DRG ","     "," X¯M ","  7  ","  8  ","  9  ","  ö  "
      DATA " Sin "," Cos "," Tan ","     ","     "," M¯X ","  4  ","  5  ","  6  ","  x  "
      DATA " Ln  "," Log "," y   ","     ","     "," M+  ","  1  ","  2  ","  3  ","  -  "
      DATA " e   "," 10  ","  ûy "," x y ","ViewF"," MC  ","  0  ","  .  ","  ñ  ","  +  "
      DATA " ûx  "," xı  "," 1/x ","  ã  ","  e  "," Fmt "," Fix "," Pts ","     ","     "

HHSecondKeyDefine:  'Secondary key labels
      DATA "  Esc","    a","    b","    c","    d","    e","     ","     ","     ","    /"
      DATA "    f","    g","    h","    i","    j","    k","     ","     ","     ","    *"
      DATA "    l","    m","    n","    o","    p","    q","     ","     ","     ","     "
      DATA "    r","    s","    t","    u","    v","    w","     ","     ","    \","     "
      DATA "    x","    y","    z","    !","    @","    #","    $","    %","     ","     "
      DATA "f   1","f   2","f   3","f   4","f   5","f   6","f   7","f   8","f   9","f  10"

      SUB Calendar (ICurrentYear, ICurrentMonth, ICurrentDay, INew)
' $DYNAMIC
' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Calendar                                                                ³
' ³                                                                         ³
' ³ Subroutine to display calendar in lower right corner of screen          ³
' ³                                                                         ³
' ³ Modification of Crescent Software Inc. routine                          ³
' ³                                                                         ³
' ³          <1> - Provide location for digital clock                       ³
' ³          <2> - European format removed                                  ³
' ³          <3> - Year, month and day passed as arguments                  ³
' ³          <4> - Previous and next month display                          ³
' ³          <5> - QPCCalc subroutine placed inline                         ³
' ³          <6> - INew toggle to avoid redraw in pan mode                  ³
' ³                                                                         ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      SHARED IBoxColor
      SHARED ITextColor
      SHARED ISundayColor
      SHARED IWarningColor
      SHARED Month$()


      IULRow = 11       ' Upper left corner of calendar
      IULCol = 57       ' 13 Rows  X   24 Cols  total area

'     CalendarBorder
      IF INew = 1 THEN
             LOCATE IULRow, IULCol
             CALL QPrint0("É" + STRING$(22, "Í") + "»", IBoxColor)
             LOCATE IULRow + 1, IULCol
             CALL QPrint0("º" + STRING$(22, " ") + "º", IBoxColor)
             LOCATE IULRow + 2, IULCol
             CALL QPrint0("Ì" + STRING$(22, "Í") + "¹", IBoxColor)
             LOCATE IULRow + 3, IULCol
             CALL QPrint0("º" + STRING$(22, " ") + "º", IBoxColor)
             LOCATE IULRow + 4, IULCol
             CALL QPrint0("Ì" + STRING$(22, "Í") + "¹", IBoxColor)
             FOR I = IULRow + 5 TO IULRow + 10
                    LOCATE I, IULCol
                    CALL QPrint0("º" + SPACE$(22) + "º", IBoxColor)
             NEXT

             LOCATE IULRow + 11, IULCol
             CALL QPrint0("Ì" + STRING$(22, "Í") + "¹", IBoxColor)

             LOCATE IULRow + 12, IULCol
             CALL QPrint0("º" + STRING$(22, " ") + "º", IBoxColor)
             LOCATE IULRow + 13, IULCol
             CALL QPrint0("È" + STRING$(22, "Í") + "¼", IBoxColor)

             LOCATE IULRow + 3, IULCol + 1
             CALL QPrint0(" Su Mo Tu We Th Fr Sa ", ITextColor)
             LOCATE IULRow + 12, IULCol + 4
             CALL QPrint0("Time", ITextColor)

      END IF

' Range check arguments

      IF ICurrentMonth < 1 OR ICurrentMonth > 12 OR ICurrentYear < 1900 THEN EXIT SUB


      LOCATE IULRow + 1, IULCol + 3, 0
      CALL QPrint0(" " + LEFT$(Month$(ICurrentMonth) + SPACE$(10), 10) + STR$(ICurrentYear), ITextColor)


' Information on previous month before date passed to routine

      IPreviousMonth = ICurrentMonth - 1
      IPreviousYear = ICurrentYear
      IF IPreviousMonth = 0 THEN
             IPreviousMonth = 12
             IPreviousYear = IPreviousYear - 1
      END IF


      IF IPreviousMonth > 2 THEN
             ITemp1 = IPreviousMonth + 1
             ITemp2 = IPreviousYear
      ELSE
             ITemp1 = IPreviousMonth + 13
             ITemp2 = IPreviousYear - 1
      END IF
      DaysSincePrevious! = INT(365.25 * ITemp2) - 693975 + INT(30.6 * ITemp1)

' Information on date passed to routine

      IF ICurrentMonth > 2 THEN
             ITemp1 = ICurrentMonth + 1
             ITemp2 = ICurrentYear
      ELSE
             ITemp1 = ICurrentMonth + 13
             ITemp2 = ICurrentYear - 1
      END IF
      DaysSinceCurrent! = INT(365.25 * ITemp2) - 693975 + INT(30.6 * ITemp1)
      IDayofWeek = DaysSinceCurrent! - INT(DaysSinceCurrent! / 7) * 7 + 1
      IFirstDayCurrent = IDayofWeek

' Information on next month after date passed to routine

      INextMonth = ICurrentMonth + 1
      INextYear = ICurrentYear
      IF INextMonth = 13 THEN
             INextMonth = 1
             INextYear = INextYear + 1
      END IF


      IF INextMonth > 2 THEN
             ITemp1 = INextMonth + 1
             ITemp2 = INextYear
      ELSE
             ITemp1 = INextMonth + 13
             ITemp2 = INextYear - 1
      END IF
      DaysSinceNext! = INT(365.25 * ITemp2) - 693975 + INT(30.6 * ITemp1)

' Begin display of current month with over-run into next month

      IF IFirstDayCurrent = 7 OR IFirstDayCurrent = 0 THEN
             ILineOffSet = 6
      ELSE
             ILineOffSet = 5
      END IF
      LOCATE IULRow + ILineOffSet, IULCol + 1
      IF IDayofWeek <> 7 THEN
             CALL QPrint0(SPACE$(IDayofWeek * 3), IBoxColor)
             LOCATE , POS(0) + IDayofWeek * 3
      END IF
      IReverseColor = (ITextColor AND 112) / 16 + (ITextColor AND 15) * 16 AND 127
      IReverseSundayColor = (ISundayColor AND 112) / 16 + (ISundayColor AND 15) * 16 AND 127

      IDaysinCurrentMonth = DaysSinceNext! - DaysSinceCurrent!
      FOR I = 1 TO IDaysinCurrentMonth
             CALL QPrint0(" ", ITextColor)
             LOCATE , POS(0) + 1

             IF I <> ICurrentDay THEN
                    IF IDayofWeek MOD 7 = 0 THEN
                           IColor = ISundayColor
                    ELSE
                           IColor = ITextColor
                    END IF
             ELSE
                    IF IDayofWeek MOD 7 = 0 THEN
                           IColor = IReverseSundayColor   'IReverseColor if reverse sunday not wanted
                    ELSE
                           IColor = IReverseColor
                    END IF
             END IF

             CALL QPrint0(RIGHT$(" " + STR$(I), 2), IColor)
             LOCATE , POS(0) + 2
             IDayofWeek = IDayofWeek + 1

             IF INT(IDayofWeek / 7) = IDayofWeek / 7 THEN
                    ILineOffSet = ILineOffSet + 1
                    LOCATE IULRow + ILineOffSet, IULCol + 1
             END IF

      NEXT

      'Display next month information in the remaining space

      IF INT(IDayofWeek / 7) = IDayofWeek / 7 AND ILineOffSet = 11 THEN GOTO NextMonthExit

      FOR I = 1 TO 14
             CALL QPrint0(" ", ITextColor)
             LOCATE , POS(0) + 1
             CALL QPrint0(RIGHT$(" " + STR$(I), 2), IWarningColor)
             LOCATE , POS(0) + 2
             IDayofWeek = IDayofWeek + 1

             IF INT(IDayofWeek / 7) = IDayofWeek / 7 THEN
                    ILineOffSet = ILineOffSet + 1
                    IF ILineOffSet = 11 THEN GOTO NextMonthExit
                    LOCATE IULRow + ILineOffSet, IULCol + 1
             END IF

      NEXT

NextMonthExit:

' Display the end of the previous month

      IDaysinPreviousMonth = DaysSinceCurrent! - DaysSincePrevious!
      IDayOffSet = IDaysinPreviousMonth - (IFirstDayCurrent - 1)
      ILineOffSet = 5
      LOCATE IULRow + 5, IULCol + 1
      FOR I = 0 TO IFirstDayCurrent - 1
             CALL QPrint0(" ", ITextColor)
             LOCATE , POS(0) + 1
             CALL QPrint0(RIGHT$(" " + STR$(IDayOffSet + I), 2), IWarningColor)
             LOCATE , POS(0) + 2
      NEXT I


      EXIT SUB

      END SUB

SUB CardEdit80Col (FileName$, INumLines, IFormat, IRow, INormalColor, IEditColor, IEditChr, INumOnly, Message$, IError)
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ CardEdit80Col    D.Packard  Jul 90                                                                         ³
' ³                                                                                                            ³
' ³ Subroutine to edit 80 column card image files                                                              ³
' ³                                                                                                            ³
' ³ Input:                                                                                                     ³
' ³       FileName$  -  Name of file containing cards.  Extensions of 'CRD' and 'FMT' assumed                  ³
' ³       INumLines  -  Number of cards/lines contained in file                                                ³
' ³         IFormat  -  Formated input  0 - No   1 - Yes                                                       ³
' ³            IRow  -  First screen row to display cards                                                      ³
' ³    INormalColor  -  Color of normal (non edit) characters                                                  ³
' ³      IEditColor  -  Color of edit character                                                                ³
' ³        IEditChr  -  Character to display if blank space encountered                                        ³
' ³        INumOnly  -  Allow numbers only, not text                                                           ³
' ³        Message$  -  Text to be displayed on last line of screen                                            ³
' ³                                                                                                            ³
' ³ Output:             File (filename$) updated                                                               ³
' ³          IError  -  Error flag returned to routine CardSelect                                              ³
' ³                     0 - No Error                                                                           ³
' ³                    11 - Card   file not found                                                              ³
' ³                    12 - Format file not found                                                              ³
' ³                    21 - Attempt to read past end of card   file                                            ³
' ³                    22 - Attempt to read past end of format file                                            ³
' ³                                                                                                            ³
' ³ Notes:                                                                                                     ³
' ³         Colors are input as packed integers;   Foreground Color * 16 + Background Color                    ³
' ³                                                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      ITabPositions = 40
      REDIM Card$(INumLines)
      REDIM ITabPosition(INumLines, ITabPositions)

      Tens$ = "úúúú+úúúú1úúúú+úúúú2úúúú+úúúú3úúúú+úúúú4úúúú+úúúú5úúúú+úúúú6úúúú+úúúú7úúúú+úúúú8"
      IError = 0
      IEditBG = INT(IEditColor / 16)
      IEditFG = IEditColor MOD 16
      INormalBG = INT(INormalColor / 16)
      INormalFG = INormalColor MOD 16

      IDataFileNumber = FREEFILE
      TargetFile$ = FileName$ + ".Crd"
      IF NOT Exist(TargetFile$) THEN IError = 11: EXIT SUB
      OPEN "R", IDataFileNumber, TargetFile$, 80
      FIELD IDataFileNumber, 80 AS Dummy$
      FOR I = 1 TO INumLines
             GET #IDataFileNumber
             Card$(I) = Dummy$
      NEXT
      CLOSE IDataFileNumber

      IF IFormat = 1 THEN
             IDataFileNumber = FREEFILE
             TargetFile$ = FileName$ + ".Fmt"
             IF NOT Exist(TargetFile$) THEN IError = 12: EXIT SUB
             OPEN "I", IDataFileNumber, TargetFile$
             FOR I = 1 TO INumLines
                    IF EOF(IDataFileNumber) THEN IError = 22: EXIT SUB
                    INPUT #IDataFileNumber, ITabPosition(I, 0)
                    FOR J = 1 TO ITabPosition(I, 0)
                           IF EOF(IDataFileNumber) THEN IError = 22: EXIT SUB
                           INPUT #IDataFileNumber, ITabPosition(I, J)
                    NEXT
             NEXT
             IF NOT EOF(IDataFileNumber) THEN IError = 22: EXIT SUB
             CLOSE IDataFileNumber
      END IF
      ICol = 1
      IMinRow = IRow
      IMaxRow = INumLines + IRow - 1

      COLOR INormalFG, INormalBG
      CLS
      LOCATE 1, 1
      PRINT Tens$
      LOCATE 25, 1
      PRINT Message$;
      LOCATE IRow, ICol
      FOR I = 1 TO INumLines
             PRINT Card$(I);
      NEXT

      ICurrentRow = IRow
      ICurrentCol = ICol

CardEdit80ColBegin:

      I = ICurrentRow - IRow + 1
      LOCATE ICurrentRow, ICurrentCol
      IDummy = SCREEN(ICurrentRow, ICurrentCol)
      COLOR IEditFG, IEditBG
      IF IDummy = 32 THEN
             PRINT CHR$(IEditChr);
      ELSE
             PRINT CHR$(IDummy);
      END IF
      ColNum$ = CHR$(SCREEN(1, ICurrentCol))
      LOCATE 1, ICurrentCol
      COLOR IEditFG, IEditBG
      PRINT CHR$(ICurrentCol MOD 10 + 48);
      LOCATE ICurrentRow, ICurrentCol

      CALL GetKey(3, 0, 0, Ky$)

      COLOR INormalFG, INormalBG
      LOCATE 1, ICurrentCol
      PRINT ColNum$;
      LOCATE ICurrentRow, ICurrentCol
      PRINT CHR$(IDummy);

      IF LEN(Ky$) = 2 THEN
             GOTO ExtendedKeys
      ELSE
             GOTO NonExtendedKeys
      END IF


CardEdit80ColEnd:

      IDataFileNumber = FREEFILE
      OPEN "R", IDataFileNumber, FileName$ + ".Crd", 80
      FIELD IDataFileNumber, 80 AS Dummy$
      FOR I = 1 TO INumLines
             LSET Dummy$ = Card$(I)
             PUT IDataFileNumber
      NEXT
      CLOSE IDataFileNumber
      ERASE Card$
      ERASE ITabPosition
      EXIT SUB

AllowedInput:
      MID$(Card$(I), ICurrentCol, 1) = Ky$
      LOCATE ICurrentRow, ICurrentCol
      PRINT Ky$;
      ICurrentCol = ICurrentCol + 1
      IF ICurrentCol > 80 THEN ICurrentCol = 80
      GOTO CardEdit80ColBegin

ExtendedKeys:
      IKey = ASC(RIGHT$(Ky$, 1))
      SELECT CASE IKey
             CASE 15 ' BackwardTab
                    IF IFormat = 1 THEN
                           FOR J = ITabPosition(I, 0) TO 1 STEP -1
                                  IF ITabPosition(I, J) < ICurrentCol THEN
                                         ICurrentCol = ITabPosition(I, J)
                                         GOTO CardEdit80ColBegin
                                  END IF
                           NEXT
                    END IF
             CASE 71, 73 ' Home PageUp
                    ICurrentRow = IMinRow
                    ICurrentCol = 1
             CASE 72 ' Up
                    ICurrentRow = ICurrentRow - 1
                    IF ICurrentRow < IMinRow THEN ICurrentRow = IMinRow
             CASE 75 ' Left
                    ICurrentCol = ICurrentCol - 1
                    IF ICurrentCol < 1 THEN ICurrentCol = 1
             CASE 77 ' Right
                    ICurrentCol = ICurrentCol + 1
                    IF ICurrentCol > 80 THEN ICurrentCol = 80
             CASE 79, 81 ' End PageDown
                    ICurrentRow = IMaxRow
                    ICurrentCol = 1
             CASE 80 ' Down
                    ICurrentRow = ICurrentRow + 1
                    IF ICurrentRow > IMaxRow THEN ICurrentRow = IMaxRow
             CASE 82 ' Insert
                    FOR J = 79 TO ICurrentCol STEP -1
                           MID$(Card$(I), J + 1, 1) = MID$(Card$(I), J, 1)
                    NEXT J
                    MID$(Card$(I), ICurrentCol, 1) = " "
                    LOCATE ICurrentRow, 1: PRINT Card$(I);
                    LOCATE ICurrentRow, ICurrentCol
             CASE 83 ' Delete
                    FOR J = ICurrentCol TO 79
                           MID$(Card$(I), J, 1) = MID$(Card$(I), J + 1, 1)
                    NEXT J
                    MID$(Card$(I), 80, 1) = " "
                    LOCATE ICurrentRow, 1: PRINT Card$(I);
                    LOCATE ICurrentRow, ICurrentCol
             CASE ELSE
      END SELECT
      GOTO CardEdit80ColBegin

NonExtendedKeys:
      IKey = ASC(Ky$)
      SELECT CASE IKey
             CASE 8 ' Backspace
                    ICurrentCol = ICurrentCol - 1
                    IF ICurrentCol < 1 THEN ICurrentCol = 1
             CASE 9 ' ForwardTab
                    IF IFormat = 1 THEN
                           FOR J = 1 TO ITabPosition(I, 0)
                                  IF ITabPosition(I, J) > ICurrentCol THEN
                                         ICurrentCol = ITabPosition(I, J)
                                         GOTO CardEdit80ColBegin
                                  END IF
                           NEXT
                    END IF
             CASE 13 ' Return
                    ICurrentCol = 1
                    ICurrentRow = ICurrentRow + 1
                    IF ICurrentRow > IMaxRow THEN ICurrentRow = IMaxRow
             CASE 27 ' Escape
                    GOTO CardEdit80ColEnd
             CASE 32 TO 126
                    IF INumOnly = 0 THEN
                           GOTO AllowedInput
                    ELSE
                           SELECT CASE IKey
                                  CASE 32, 45, 46, 48 TO 57, 69, 101
                                         GOTO AllowedInput
                                  CASE ELSE
                                         GOTO CardEdit80ColBegin
                           END SELECT
                    END IF
              CASE ELSE
      END SELECT
      GOTO CardEdit80ColBegin

END SUB

      SUB DefineUser
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Define User                                                                                                ³
' ³                                                                                                            ³
' ³ Subroutine to edit user defined function key file 'UsrFnct.Crd'                                            ³
' ³                                                                                                            ³
' ³                                                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      SHARED IEditColor
      SHARED INormalColor
      SHARED IEditChr

      COLOR IBlack, IWhite, IWhite
      CLS
      Message$ = "1st Line  Key Notation  Cols 1-5   Prompt  Cols 10-59    2nd Line  Equation    "
      CALL CardEdit80Col("UsrFnct", 20, 0, 3, INormalColor, IEditColor, IEditChr, INumOnly, Message$, IError)

      EXIT SUB

END SUB

      SUB DigitalTime
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Digital Time                                                                                               ³
' ³                                                                                                            ³
' ³ Subroutine to display digital time                                                                         ³
' ³ Time converted to 12 hour base                                                                             ³
' ³                                                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      DisplayTime$ = TIME$
      IDisplayHour$ = LEFT$(DisplayTime$, 2)
      IDisplayHour = VAL(IDisplayHour$)

      SELECT CASE IDisplayHour
             CASE 0
                  IDisplayHour = 12
             CASE IS > 12
                  IDisplayHour = IDisplayHour - 12
      END SELECT

      IDisplayHour$ = STR$(IDisplayHour)
      IF LEN(IDisplayHour$) > 2 THEN IDisplayHour$ = RIGHT$(IDisplayHour$, 2)
      MID$(DisplayTime$, 1, 2) = IDisplayHour$

      LOCATE IStartRow, 65, 0: PRINT DisplayTime$;

      EXIT SUB

END SUB

      SUB DrawHandHeld
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Draw Hand Held                                                                                             ³
' ³                                                                                                            ³
' ³ Subroutine to draw all static portions of calculator                                                       ³
' ³                                                                                                            ³
' ³                                                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      SHARED AspectRatio
      SHARED ISecondColor

      SCREEN 9                                'EGA
      WIDTH 80, 43                            '80 by 43 mode
                                              'Remap some of the colors
      PALETTE IHRed, 52                       'Logo display color     High red mapped to Orange
      PALETTE IBlue, 0                        'Shadow display color   Blue     mapped to Black

      IX1 = 40
      IX2 = 599
      IY1 = (IStartRow - 1) * 8 - 4
      IY2 = (IStartRow + 32) * 8

' Light Blue field
      LINE (0, 0)-(IX1 - 1, 349), IHBlue, BF
      LINE (IX2 + 1, 0)-(639, 349), IHBlue, BF
      LINE (IX1, 0)-(IX2, IY1 - 1), IHBlue, BF
      LINE (IX1, IY2 + 1)-(IX2, 349), IHBlue, BF

' Gray calculator outline
      LINE (IX1, IY1)-(IX2, IY2), IHBlack, BF

' Black calculator border
      LINE (IX1 + 8, IY2 + 1)-(IX2 + 8, IY2 + 8), IBlue, BF
      LINE (IX2 + 1, IY1 + 8)-(IX2 + 8, IY2 + 8), IBlue, BF

' Angle returns for boarder
      PSET (IX2 + 2, IY1), IBlue
      DRAW "D12R1U11F1D10R1U9 F1D8R1U7 F1D6 "
      PSET (IX1 + 1, IY2 + 1), IBlue
      DRAW "R6D1L5F1R4D1L3F1R2D1L1F1"

' White split line
      LINE (IX2 + 1, IY1)-(IX2 + 1, IY2), IWhite
      LINE -(IX1, IY2), IWhite

' Blank key field
      FOR I = 0 TO 5
              IY1 = (IStartRow + 8 + I * 4) * 8
              IY2 = IY1 + 24
              FOR J = 0 TO 9
                     IX1 = J * 56 + 48
                     IX2 = IX1 + 39
                     LINE (IX1, IY1)-(IX2, IY2), IWhite, BF
                     LINE (IX1 + 4, IY2)-(IX2 + 4, IY2 + 3), IBlue, BF
                     LINE (IX2 + 1, IY1 + 4)-(IX2 + 4, IY2), IBlue, BF
                     PSET (IX1, IY2), IBlue
                     DRAW "R4D1L3F1R2D1L1"
                     PSET (IX2 + 1, IY1), IBlue
                     DRAW "D3R1U2F1D1R1"
              NEXT J
      NEXT I

' Blank ENTER key     I = 4  and  J = 8 and 9  Above

              IY1 = (IStartRow + 24) * 8
              IY2 = IY1 + 24
              IX1 = 496
              IX2 = 591
              LINE (IX1, IY1)-(IX2, IY2), IWhite, BF
              LINE (IX1 + 4, IY2)-(IX2 + 4, IY2 + 3), IBlue, BF
              LINE (IX2 + 1, IY1 + 4)-(IX2 + 4, IY2), IBlue, BF
              PSET (IX1, IY2), IBlue
              DRAW "R4D1L3F1R2D1L1"
              PSET (IX2 + 1, IY1), IBlue
              DRAW "D3R1U2F1D1R1"


      COLOR IHWhite, IWhite                   'High White Key face notation
      FOR I = 0 TO 4                          'Function performed by calculator
            FOR J = 0 TO 9                    'NOT user defined keys, they will change
                    LOCATE IStartRow + 10 + I * 4, 7 + J * 7
                    PRINT Key$(I + 1, J + 1);
             NEXT J
      NEXT I

      COLOR IBrown                            'Brown Alternate Key face notation
      FOR I = 0 TO 5                          'Keyboard key to invoke calculator function
            FOR J = 0 TO 9
                    LOCATE IStartRow + 10 + I * 4 + 1, 7 + J * 7
                    PRINT SecondKey$(I + 1, J + 1);
             NEXT J
      NEXT I

      COLOR IHBrown                           'Yellow Enter key notation
      LOCATE IStartRow + 26, 63
      PRINT " E n t e r  ";

      IX = 52: IY = 62: IC = IBlue: GOSUB HHLogo   'Black Logo shadow
      IX = 53: IY = 62: IC = IBlue: GOSUB HHLogo
      IX = 50: IY = 60: IC = IHRed: GOSUB HHLogo       'High Red Logo
      IX = 51: IY = 60: IC = IHRed: GOSUB HHLogo

      LINE (44, 245)-(597, 277), IRed, B      'Red Box around special function keys

      Expo$ = "R1F4R1H4BR4G4L1E4"             'Superscript x
      COLOR IHWhite
      PSET (64, 190)
      DRAW Expo$
      PSET (128, 190)
      DRAW Expo$
      PSET (168, 190)
      DRAW Expo$
      PSET (176, 158)
      DRAW Expo$

      ArrowDown$ = "D16R1U16BD11BR2L5D1R5G1L3D1R3"   'Arrow Down
      PSET (296, 138)
      DRAW "R1D2L11U16R10" + ArrowDown$
      PSET (290, 156)
      DRAW ArrowDown$

      ArrowUp$ = "D16R1U16BD4BR2L5U1R5H1L3U1R3"      'Arrow Up
      PSET (242, 138)
      DRAW "R1D2L11U16R10" + ArrowUp$
      PSET (234, 156)
      DRAW ArrowUp$

      Swap$ = "R1D1R2D1R7U1R2U1R1BU11L1U1L2U1L7D1L2D1L1"   'Swap notation
      PSET (228, 202)
      DRAW Swap$

      PSET (64, 224)                            'Complete square root symbol
      DRAW "R6"
      PSET (184, 192)
      DRAW "R6"


      LINE (150, 30)-(489, 73), IWhite, BF      'Display Box
      LINE (149, 29)-(490, 74), IBlue, B

      PSET (150, 75), IBlue                 'Display Box shadow
      DRAW "R340U46F1D46L340F1R340U46F1D46L340F1R340U46F1D46L340"

      IClockRadius = 34                          'Clock Rim
      IRimColor = IBrown

      CIRCLE (IClockX, IClockY), IClockRadius - 1, IRimColor
      CIRCLE (IClockX, IClockY), IClockRadius, IRimColor
      CIRCLE (IClockX, IClockY), IClockRadius + 1, IRimColor

      PSET (IClockX, IClockY), ISecondColor     'Hour Marks
      PSET (IClockX - IClockRadius, IClockY), ISecondColor
      PSET (IClockX + IClockRadius, IClockY), ISecondColor
      PSET (IClockX, IClockY + IClockRadius * AspectRatio), ISecondColor
      PSET (IClockX, IClockY - IClockRadius * AspectRatio), ISecondColor
      PSET (IClockX + .866 * IClockRadius, IClockY + .5 * IClockRadius * AspectRatio), ISecondColor
      PSET (IClockX + .5 * IClockRadius, IClockY + .866 * IClockRadius * AspectRatio), ISecondColor
      PSET (IClockX - .866 * IClockRadius, IClockY + .5 * IClockRadius * AspectRatio), ISecondColor
      PSET (IClockX - .5 * IClockRadius, IClockY + .866 * IClockRadius * AspectRatio), ISecondColor
      PSET (IClockX + .866 * IClockRadius, IClockY - .5 * IClockRadius * AspectRatio), ISecondColor
      PSET (IClockX + .5 * IClockRadius, IClockY - .866 * IClockRadius * AspectRatio), ISecondColor
      PSET (IClockX - .866 * IClockRadius, IClockY - .5 * IClockRadius * AspectRatio), ISecondColor
      PSET (IClockX - .5 * IClockRadius, IClockY - .866 * IClockRadius * AspectRatio), ISecondColor

      LINE (512, 15)-(575, 23), IWhite, BF           'Digital Clock Box
      LINE (511, 14)-(576, 24), IBlue, B

      EXIT SUB

HHLogo:
      PSET (IX, IY), IC
      DRAW "E19R11F2D1F1D1G1D1G2L1G2L14"                               'P
      PSET (IX + 12, IY), IC
      DRAW "U2E4R4F1D1G1D1G2L1G1L3BR6BU2F2R1"                          'a
      DRAW "E3U1E3R3F1BL8BD4F2R4"                                      'c
      DRAW "E1R1E1R1U1R1U1R1U1R1U1E1U1E2U1E1U1BD7BL4R5D1G1L2G1D2F1R3"  'k
      DRAW "E7R4F1D1G1D1G2L1G1L3H1U1BR7F2R1"                           'a
      DRAW "E3U1E1U1E1U1D1R4G1D1G1D3F1R1"                              'r
      DRAW "E7R4F1E2U1E1U1E1U1E1BD8BL5D1G1D1G2L1G1L3H2BR8F2R1E2"       'd
      PSET (IX, IY + 8), IC
      DRAW "R80"
      RETURN

END SUB

      SUB DrawHands
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Draw Hands                                                                                                 ³
' ³                                                                                                            ³
' ³ Subroutine to draw hands of analog clock                                                                   ³
' ³                                                                                                            ³
' ³ Shared variables:                                                                                          ³
' ³                                                                                                            ³
' ³   CurrentTime$ - System time                                                                               ³
' ³          IMinX - Minute hand X end point                                                                   ³
' ³          IMinY - Minute hand Y end point                                                                   ³
' ³           IHrX - Hour   hand X end point                                                                   ³
' ³           IHrY - Hour   hand Y end point                                                                   ³
' ³                                                                                                            ³
' ³                                                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      SHARED CurrentTime$
      SHARED IMinX
      SHARED IMinY
      SHARED IHrX
      SHARED IHrY
      SHARED AspectRatio

      IHrColor = IHRed                        'Color of clock hand  hour
      IMinColor = IHRed                       'Color of clock hand  minute

      CONST IClockHrHandLength = 18           'Hour   hand length
      CONST IClockMinHandLength = 22          'Minute hand length

      IHour = VAL(LEFT$(CurrentTime$, 2)) MOD 12
      IMin = VAL(MID$(CurrentTime$, 4, 2))

      XHourAngle = (IHour + IMin / 60) * 30 * 3.14159 / 180
      XMinAngle = IMin * 6 * 3.14159 / 180

      IDeltaXHr = IClockHrHandLength * SIN(XHourAngle)
      IDeltaYHr = IClockHrHandLength * COS(XHourAngle) * AspectRatio
      IDeltaXMin = IClockMinHandLength * SIN(XMinAngle)
      IDeltaYMin = IClockMinHandLength * COS(XMinAngle) * AspectRatio

      IHrX = IClockX + IDeltaXHr
      IHrY = IClockY - IDeltaYHr
      IMinX = IClockX + IDeltaXMin
      IMinY = IClockY - IDeltaYMin

      FOR I = -1 TO 1
             FOR J = -1 TO 1
                    LINE (IClockX + I, IClockY + J)-(IHrX, IHrY), IHrColor
             NEXT J
      NEXT I


      FOR I = -1 TO 1
             FOR J = -1 TO 1
                    LINE (IClockX + I, IClockY + J)-(IMinX, IMinY), IMinColor
             NEXT J
      NEXT I


      EXIT SUB


END SUB

      SUB EditSyntax (Temp$)
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Edit Syntax                                                                                                ³
' ³                                                                                                            ³
' ³ Subroutine to edit syntax of equation                                                                      ³
' ³                                                                                                            ³
' ³    Replace X,Y,Z and M  with underscore _                                                         ³
' ³                                                                                                            ³
' ³    X Y Z M  are calculator register variables                                                            ³
' ³                                                                                                            ³
' ³                                                                                                            ³
' ³                                                                                                            ³
' ³                                                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      FOR IChar = 1 TO LEN(Temp$)
             IF UCASE$(MID$(Temp$, IChar, 3)) = "EXP" THEN MID$(Temp$, IChar, 3) = "E_P"
      NEXT IChar
      FOR IChar = 1 TO LEN(Temp$)
             IF UCASE$(MID$(Temp$, IChar, 6)) = "@TODAY" THEN MID$(Temp$, IChar, 6) = "@TODA_"
      NEXT IChar
      FOR IChar = 1 TO LEN(Temp$)
             IF UCASE$(MID$(Temp$, IChar, 3)) = "XOR" THEN MID$(Temp$, IChar, 6) = "_OR"
      NEXT IChar
      FOR IChar = 1 TO LEN(Temp$)
             IF UCASE$(MID$(Temp$, IChar, 3)) = "IMP" THEN MID$(Temp$, IChar, 6) = "I_P"
      NEXT IChar
      FOR IChar = 1 TO LEN(Temp$)
             IF UCASE$(MID$(Temp$, IChar, 3)) = "MOD" THEN MID$(Temp$, IChar, 6) = "_OD"
      NEXT IChar

      EXIT SUB

END SUB

      SUB EraseHands
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Erase Hands                                                                                                ³
' ³                                                                                                            ³
' ³ Subroutine to erase hands of analog clock                                                                  ³
' ³                                                                                                            ³
' ³ Shared variables:                                                                                          ³
' ³          IMinX - Minute hand X end point                                                                   ³
' ³          IMinY - Minute hand Y end point                                                                   ³
' ³           IHrX - Hour   hand X end point                                                                   ³
' ³           IHrY - Hour   hand Y end point                                                                   ³
' ³                                                                                                            ³
' ³                                                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      SHARED IMinX
      SHARED IMinY
      SHARED IHrX
      SHARED IHrY
      SHARED IBGColor

      FOR I = -1 TO 1
             FOR J = -1 TO 1
                    LINE (IClockX + I, IClockY + J)-(IHrX, IHrY), IBGColor
             NEXT J
      NEXT I


      FOR I = -1 TO 1
             FOR J = -1 TO 1
                    LINE (IClockX + I, IClockY + J)-(IMinX, IMinY), IBGColor
             NEXT J
      NEXT I

      EXIT SUB

END SUB

DEFINT A-H, O-Z
      FUNCTION Evaluate# (FunctionEquation$)
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Evaluate                                                                                                   ³
' ³                                                                                                            ³
' ³ Function to evaluate equation                                                                              ³
' ³                                                                                                            ³
' ³ Modification of Crescent Software Inc. routine                                                             ³
' ³                                                                                                            ³
' ³          <1> - Add dummy operations ???? for future growth                                                 ³
' ³          <2> - All X,Y,Z,M characters replaced by underscore (_)                                           ³
' ³                                                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

'Copyright (c) 1988 Crescent Software
'Copyright (c) 1988 Paul Passarelli
'Revised 02/19/90
'Modified by David Packard 07/04/90

STATIC Again%, OP$(), OP%(), Logic$(), constant$(), constant%()
STATIC Functn%, Position%, LeftSide$, RightSide$, Term#, Term$, RTerm#, LTerm#
STATIC RTerm$, LTerm$, L%, Ticker%, PstnMult%, PstnDiv%, Pstn1%, Pstn2%, Pstn3%
STATIC Valid$, DE$, Digit$, Numeric$, Substitute%

Incoming$ = Compact$((FunctionEquation$))       'work with a copy to leave the
                                        'original alone
IF NOT Again% THEN              'do this initialization stuff only once for
   Again% = -1                  ' the entire program to save time

   '***** The structure for the Constant$ array is set up as follows:
   'The first subscript of the array holds the name as it is used in the
   'expression, and the second subscript holds the value to be inserted.
   '
   '** Important! The array Constant% holds the length of the name.  For
   'example "PI" is two characters, so Constant%(1) = 2.  Furthermore, any
   'constant that is a substring of another MUST follow it in the table.
   'See the OP$ & OP% assignments below.

   '** Important! Constants or functions that contain the letters X Y Z or M
   'are replaced by the underscore character " _ "  Examples @TODA_ and E_P
   'Main module code must correct the input string.

   '***** Assign constants
   Count% = 7                   'how many constants are there
   DIM constant$(Count%, 2)     'two dimensional array
   DIM constant%(Count%)        'table of lengths
   constant%(0) = Count%        'for use during replacements

   constant$(1, 1) = "PI"
   constant$(1, 2) = "3.14159265358979323846"
   constant%(1) = 2

   constant$(2, 1) = "e"
   constant$(2, 2) = "2.71828182845904523536"
   constant%(2) = 1

   constant$(3, 1) = "@TODA_"
   constant$(3, 2) = STR$(Date2Num%(DATE$))
   constant%(3) = 6

   constant$(4, 1) = "X"
   constant$(4, 2) = STR$(HHX#)
   constant%(4) = 1

   constant$(5, 1) = "Y"
   constant$(5, 2) = STR$(HHY#)
   constant%(5) = 1

   constant$(6, 1) = "Z"
   constant$(6, 2) = STR$(HHZ#)
   constant%(6) = 1

   constant$(7, 1) = "M"
   constant$(7, 2) = STR$(HHMemory#)
   constant%(7) = 1


   '***** Define symbols for functions
   DIM OP$(70): DIM OP%(70)

    OP$(1) = "ARCSINH":  OP%(1) = 7
    OP$(2) = "ARCCOSH":  OP%(2) = 7
    OP$(3) = "ARCTANH":  OP%(3) = 7
    OP$(4) = "ARCSECH":  OP%(4) = 7
    OP$(5) = "ARCCSCH":  OP%(5) = 7
    OP$(6) = "ARCCOTH":  OP%(6) = 7
    OP$(7) = "???????":  OP%(7) = 7       'Growth
    OP$(8) = "???????":  OP%(8) = 7
    OP$(9) = "???????":  OP%(9) = 7       'To add additional functions to this routine
   OP$(10) = "???????": OP%(10) = 7       'include the unique syntax here, and the arithmetic
                                          'operation in the Select Case structure below.
   OP$(11) = "ARCSIN": OP%(11) = 6        'Remember X,Y,Z,M characters replaced by underscore (_)
   OP$(12) = "ARCCOS": OP%(12) = 6
   OP$(13) = "ARCTAN": OP%(13) = 6        'Edit file HHHelp.Txt to reflect added function
   OP$(14) = "ARCSEC": OP%(14) = 6
   OP$(15) = "ARCCSC": OP%(15) = 6
   OP$(16) = "ARCCOT": OP%(16) = 6
   OP$(17) = "??????": OP%(17) = 6        'Growth
   OP$(18) = "??????": OP%(18) = 6
   OP$(19) = "??????": OP%(19) = 6
   OP$(20) = "??????": OP%(20) = 6

   OP$(21) = "?????": OP%(21) = 5         'Growth
   OP$(22) = "?????": OP%(22) = 5
   OP$(23) = "?????": OP%(23) = 5
   OP$(24) = "?????": OP%(24) = 5
   OP$(25) = "?????": OP%(25) = 5
   OP$(26) = "?????": OP%(26) = 5
   OP$(27) = "?????": OP%(27) = 5
   OP$(28) = "?????": OP%(28) = 5
   OP$(29) = "?????": OP%(29) = 5
   OP$(30) = "?????": OP%(30) = 5

   OP$(31) = "SINH": OP%(31) = 4
   OP$(32) = "COSH": OP%(32) = 4
   OP$(33) = "TANH": OP%(33) = 4
   OP$(34) = "SECH": OP%(34) = 4
   OP$(35) = "CSCH": OP%(35) = 4
   OP$(36) = "COTH": OP%(36) = 4
   OP$(37) = "CINT": OP%(37) = 4
   OP$(38) = "????": OP%(38) = 4          'Growth
   OP$(39) = "????": OP%(39) = 4
   OP$(40) = "????": OP%(40) = 4

   OP$(41) = "SIN": OP%(41) = 3
   OP$(42) = "COS": OP%(42) = 3
   OP$(43) = "TAN": OP%(43) = 3
   OP$(44) = "SEC": OP%(44) = 3
   OP$(45) = "CSC": OP%(45) = 3
   OP$(46) = "COT": OP%(46) = 3
   OP$(47) = "E_P": OP%(47) = 3
   OP$(48) = "SQR": OP%(48) = 3
   OP$(49) = "LOG": OP%(49) = 3
   OP$(50) = "ABS": OP%(50) = 3
   OP$(51) = "NOT": OP%(51) = 3
   OP$(52) = "INT": OP%(52) = 3
   OP$(53) = "???": OP%(53) = 3           'Growth
   OP$(54) = "???": OP%(54) = 3
   OP$(55) = "???": OP%(55) = 3
   OP$(56) = "???": OP%(56) = 3
   OP$(57) = "???": OP%(57) = 3
   OP$(58) = "???": OP%(58) = 3
   OP$(59) = "???": OP%(59) = 3
   OP$(60) = "???": OP%(60) = 3

   OP$(61) = "LN": OP%(61) = 2
   OP$(62) = "??": OP%(62) = 2            'Growth
   OP$(63) = "??": OP%(63) = 2
   OP$(64) = "??": OP%(64) = 2
   OP$(65) = "??": OP%(65) = 2
   OP$(66) = "??": OP%(66) = 2
   OP$(67) = "??": OP%(67) = 2
   OP$(68) = "??": OP%(68) = 2
   OP$(69) = "??": OP%(69) = 2
   OP$(70) = "??": OP%(70) = 2

   DIM Logic$(5)
   Logic$(1) = "AND"
   Logic$(2) = "_OR"
   Logic$(3) = "EQV"
   Logic$(4) = "I_P"
   Logic$(5) = "OR"

END IF  'Conclusion of the Static Part

   constant%(0) = 7
   constant$(1, 2) = "3.14159265358979323846"
   constant$(2, 2) = "2.71828182845904523536"
   constant$(3, 2) = STR$(Date2Num%(DATE$))
   constant$(4, 2) = STR$(HHX#)
   constant$(5, 2) = STR$(HHY#)
   constant$(6, 2) = STR$(HHZ#)
   constant$(7, 2) = STR$(HHMemory#)

10000  'Line number to detect error in this routine
       'See line HHErrorTrap: in main module

'***** Replace constants with their values
    FOR Substitute% = 1 TO constant%(0)
        DO
           Position% = INSTR(Incoming$, constant$(Substitute%, 1))
           IF Position% THEN
              LeftSide$ = LEFT$(Incoming$, Position% - 1)
              RightSide$ = MID$(Incoming$, Position% + constant%(Substitute%))
              Term$ = constant$(Substitute%, 2)
              Incoming$ = LeftSide$ + Term$ + RightSide$
              LeftSide$ = "": Term$ = "": RightSide$ = ""
           ELSE
              EXIT DO
           END IF
        LOOP
    NEXT
    Incoming$ = UCASE$(Incoming$)

'***** Evaluate Parenthetical Expressions
'      make sure there are a matching number of left and right parentheses
    IF InCount%(Incoming$, "(") <> InCount%(Incoming$, ")") GOTO ErrorExit
    IF ASCII%(Incoming$) = 37 GOTO ErrorExit   'a "%" was added earlier, error
    DO
       Place% = INSTR(1 + Place%, Incoming$, "(")  'find the first open paren
       IF Place% THEN                              'there is one
          EndMrkr% = PMatch(MID$(Incoming$, Place%))  'find the closing paren
          Isolated$ = MID$(Incoming$, Place% + 1, EndMrkr% - 2)  'isolate part
          Paren$ = LTRIM$(STR$(VAL(Isolated$)))
          IF (Isolated$ = Paren$) OR (Isolated$ = "+" + Paren$) THEN
             Incoming$ = LEFT$(Incoming$, Place% - 1) + Paren$ + MID$(Incoming$, Place% + EndMrkr%)
          ELSE
             IsoTerm# = Evaluate#(Isolated$)        'evaluate part
             Incoming$ = LEFT$(Incoming$, Place% - 1) + LTRIM$(STR$(IsoTerm#)) + MID$(Incoming$, Place% + EndMrkr%)
             IF ASCII(Isolated$) = 37 GOTO ErrorExit
          END IF
       ELSE
          EXIT DO
       END IF
    LOOP
    Equat$ = LTRIM$(STR$(VAL(Incoming$)))
    IF (Incoming$ = Equat$) OR (Incoming$ = "+" + Equat$) GOTO Done



'***** Solve Trig. and other single operator functions
    FOR Functn% = 1 TO UBOUND(OP%)
        DO
           Position% = INSTR(Incoming$, OP$(Functn%))
           IF Position% THEN
              LeftSide$ = LEFT$(Incoming$, Position% - 1)
              Position% = Position% + OP%(Functn%) - 1

              GOSUB TermBlockR

'Add growth syntax evauation in case structure

              SELECT CASE Functn%

                CASE 1                  ' ARCSINH
                Term# = LOG(RTerm# + SQR(RTerm# * RTerm# + 1))

                CASE 2                  ' ARCCOSH
                Term# = LOG(RTerm# + SQR(RTerm# * RTerm# - 1))

                CASE 3                  ' ARCTANH
                Term# = LOG((1 + RTerm# / 1 - RTerm#)) / 2

                CASE 4                  ' ARCSECH
                Term# = LOG((SQR(-RTerm# * RTerm# + 1) + 1) / RTerm#)

                CASE 5                  ' ARCCSCH
                Term# = LOG((SGN(RTerm#) * SQR(RTerm# * RTerm# + 1) + 1) / RTerm#)

                CASE 6                  ' ARCCOTH
                Term# = LOG((RTerm# + 1) / (RTerm# - 1)) / 2

                CASE 11                 ' ARCSIN
                Term# = ATN(RTerm# / SQR(-RTerm# * RTerm# + 1))
                IF IDRG = 0 THEN Term# = Term# / HHPi * 180#
                IF IDRG = 2 THEN Term# = Term# / 90# * 100# / HHPi * 180#

                CASE 12                 ' ARCCOS
                Term# = -ATN(RTerm# / SQR(-RTerm# * RTerm# + 1)) + 1.57079637050629#
                IF IDRG = 0 THEN Term# = Term# / HHPi * 180#
                IF IDRG = 2 THEN Term# = Term# / 90# * 100# / HHPi * 180#

                CASE 13                 ' ARCTAN
                Term# = ATN(RTerm#)
                IF IDRG = 0 THEN Term# = Term# / HHPi * 180#
                IF IDRG = 2 THEN Term# = Term# / 90# * 100# / HHPi * 180#

                CASE 14                 ' ARCSEC
                Term# = ATN(RTerm# / SQR(RTerm# * RTerm# - 1)) + SGN(SGN(RTerm#) - 1) * 1.57079637050629#
                IF IDRG = 0 THEN Term# = Term# / HHPi * 180#
                IF IDRG = 2 THEN Term# = Term# / 90# * 100# / HHPi * 180#

                CASE 15                 ' ARCCSC
                Term# = ATN(RTerm# / SQR(RTerm# * RTerm# - 1)) + (SGN(RTerm#) - 1) * 1.57079637050629#
                IF IDRG = 0 THEN Term# = Term# / HHPi * 180#
                IF IDRG = 2 THEN Term# = Term# / 90# * 100# / HHPi * 180#

                CASE 16                 ' ARCCOT
                Term# = ATN(RTerm#) + 2 * ATN(1)
                IF IDRG = 0 THEN Term# = Term# / HHPi * 180#
                IF IDRG = 2 THEN Term# = Term# / 90# * 100# / HHPi * 180#

                CASE 31                 ' SINH
                Term# = (EXP(RTerm#) - EXP(-RTerm#)) / 2

                CASE 32                 ' COSH
                Term# = (EXP(RTerm#) + EXP(-RTerm#)) / 2

                CASE 33                 ' TANH
                Term# = (EXP(RTerm#) - EXP(-RTerm#)) / (EXP(RTerm#) + EXP(-RTerm#))

                CASE 34                 ' SECH
                Term# = 2 / (EXP(RTerm#) + EXP(-RTerm#))

                CASE 35                 ' CSCH
                Term# = 2 / (EXP(RTerm#) - EXP(-RTerm#))

                CASE 36                 ' COTH
                Term# = (EXP(RTerm#) + EXP(-RTerm#)) / (EXP(RTerm#) - EXP(-RTerm#))

                CASE 37                 ' CINT
                RTerm# = RTerm# + .5 * SGN(RTerm#)
                Value$ = STR$(RTerm#)
                I = 0
                I = INSTR(Value$, ".")
                IF I > 1 THEN
                     Value$ = LEFT$(Value$, I)
                ELSE
                     Value$ = "0"
                END IF
                Term# = VAL(Value$)

                CASE 41                 ' SIN
                IF IDRG = 0 THEN RTerm# = RTerm# * HHPi / 180#
                IF IDRG = 2 THEN RTerm# = RTerm# * 90# / 100# * HHPi / 180#
                Term# = SIN(RTerm#)

                CASE 42                 ' COS
                IF IDRG = 0 THEN RTerm# = RTerm# * HHPi / 180#
                IF IDRG = 2 THEN RTerm# = RTerm# * 90# / 100# * HHPi / 180#
                Term# = COS(RTerm#)

                CASE 43                 ' TAN
                IF IDRG = 0 THEN RTerm# = RTerm# * HHPi / 180#
                IF IDRG = 2 THEN RTerm# = RTerm# * 90# / 100# * HHPi / 180#
                Term# = TAN(RTerm#)

                CASE 44                 ' SEC
                IF IDRG = 0 THEN RTerm# = RTerm# * HHPi / 180#
                IF IDRG = 2 THEN RTerm# = RTerm# * 90# / 100# * HHPi / 180#
                Term# = 1 / COS(RTerm#)

                CASE 45                 ' CSC
                IF IDRG = 0 THEN RTerm# = RTerm# * HHPi / 180#
                IF IDRG = 2 THEN RTerm# = RTerm# * 90# / 100# * HHPi / 180#
                Term# = 1 / SIN(RTerm#)

                CASE 46                 ' COT
                IF IDRG = 0 THEN RTerm# = RTerm# * HHPi / 180#
                IF IDRG = 2 THEN RTerm# = RTerm# * 90# / 100# * HHPi / 180#
                Term# = 1 / TAN(RTerm#)

                CASE 47                 ' EXP
                Term# = EXP(RTerm#)

                CASE 48                 ' SQR
                IF RTerm# < 0# GOTO ErrorExit
                Term# = SQR(RTerm#)

                CASE 49                 ' LOG
                IF RTerm# <= 0# GOTO ErrorExit
                Term# = LOG(RTerm#) / LOG(10#)

                CASE 50                 ' ABS
                Term# = ABS(RTerm#)

                CASE 51                 ' NOT
                Term# = NOT RTerm#

                CASE 52                 ' INT
                Term# = INT(RTerm#)

                CASE 61                 ' LN
                IF RTerm# <= 0# GOTO ErrorExit
                Term# = LOG(RTerm#)

              END SELECT

              Incoming$ = LeftSide$ + LTRIM$(STR$(Term#)) + RightSide$
              LeftSide$ = "": Numeric$ = "": RightSide$ = ""
              RTerm# = 0: Term# = 0
           ELSE
              EXIT DO
           END IF
        LOOP
    NEXT




'***** Factorial
    DO
       Position% = INSTR(Incoming$, "!")
       IF Position% THEN
          RightSide$ = MID$(Incoming$, Position% + 1)
          FOR L% = Position% - 1 TO MaxInt(Position% - 5, 1) STEP -1
              IF INSTR("12434567890", MID$(Incoming$, L%, 1)) = 0 THEN EXIT FOR
              IF L% = Position% - 5 GOTO ErrorExit
          NEXT
          Numeric# = VAL(MID$(Incoming$, 1 + L%))
          LeftSide$ = LEFT$(Incoming$, L%)
          IF (Numeric# < 0#) OR (Numeric# > 170) GOTO ErrorExit
          Incoming$ = LeftSide$ + LTRIM$(STR$(Factorial#(CINT(Numeric#)))) + RightSide$
          LeftSide$ = "": Numeric# = 0: RightSide$ = ""
       ELSE
          EXIT DO
       END IF
    LOOP

    GOSUB DoubleSign    'removes occurrences of double signs
                        'for example, "--" is changed to "+"



'***** Exponentiation
    DO
       Position% = INSTR(Incoming$, "^")
       IF Position% = 1 GOTO ErrorExit
       IF Position% > 1 THEN

          GOSUB TermBlock

          Term$ = LTRIM$(STR$(LTerm# ^ RTerm#))
          Incoming$ = LeftSide$ + Term$ + RightSide$
          IF Term$ = Incoming$ THEN
             LeftSide$ = ""
             RightSide$ = ""
             LTerm# = 0
             RTerm# = 0
             Term$ = ""
             GOTO Done
          END IF
       ELSE
          EXIT DO
       END IF
    LOOP



'***** Multiplication & Division
    DO
       QPLenIncoming% = QPLen(Incoming$)
       PstnMult% = INSTR(Incoming$, "*")
       IF PstnMult% = 0 THEN PstnMult% = QPLenIncoming%
       PstnDiv% = INSTR(Incoming$, "/")
       IF PstnDiv% = 0 THEN PstnDiv% = QPLenIncoming%
       PstnIDiv% = INSTR(Incoming$, "\")
       IF PstnIDiv% = 0 THEN PstnIDiv% = QPLenIncoming%
       PstnMod% = INSTR(Incoming$, "_OD")
       IF PstnMod% = 0 THEN PstnMod% = QPLenIncoming%

       Position% = MinInt%(MinInt%(PstnMult%, PstnMod%), MinInt%(PstnDiv%, PstnIDiv%))
       IF Position% = QPLenIncoming% THEN Position% = 0
       IF Position% = 1 GOTO ErrorExit
       IF Position% > 1 THEN

          GOSUB TermBlock

          IF Position% = PstnMult% THEN
             Term# = LTerm# * RTerm#
          ELSEIF Position% = PstnDiv% THEN
             IF RTerm# = 0# GOTO ErrorExit
             Term# = LTerm# / RTerm#
          ELSEIF Position% = PstnIDiv% THEN
             IF RTerm# = 0# GOTO ErrorExit
             Term# = LTerm# \ RTerm#
          ELSEIF Position% = PstnMod% THEN
             Position = Position + 2
             GOSUB TermBlockR
             IF RTerm# = 0# GOTO ErrorExit
             Term# = LTerm# MOD RTerm#
          END IF

          Term$ = LTRIM$(STR$(Term#))
          Incoming$ = LeftSide$ + Term$ + RightSide$
          IF Term$ = Incoming$ THEN
             LeftSide$ = ""
             RightSide$ = ""
             LTerm# = 0
             RTerm# = 0
             Term$ = ""
             Term# = 0
             GOTO Done
          END IF
       ELSE
          EXIT DO
       END IF
    LOOP



'***** Addition & Subtraction
    DO
       PExp% = 2: MExp% = 1: QPLenIncoming% = QPLen(Incoming$)

PFIND: PstnAdd% = INSTR(PExp%, Incoming$, "+")
       IF PstnAdd% THEN
          DE$ = MID$(Incoming$, PstnAdd% - 1, 1)
          IF (DE$ = "D") OR (DE$ = "E") THEN
             PExp% = PstnAdd% + 1
             GOTO PFIND
          END IF
       ELSE
          PstnAdd% = QPLenIncoming%
       END IF

MFIND: Ticker = Ticker + 1
       IF Ticker = QPLenIncoming% + 1 GOTO RelOP
       PstnSub% = INSTR(MExp%, Incoming$, "-")
       IF PstnSub% = 1 THEN
          IF QPLen(STR$(VAL(Incoming$))) = QPLenIncoming% GOTO Done
          MExp% = 2
          GOTO MFIND
       END IF
       IF PstnSub% THEN
          DE$ = MID$(Incoming$, PstnSub% - 1, 1)
          IF (DE$ = "D") OR (DE$ = "E") THEN
             MExp% = PstnSub% + 1
             GOTO MFIND
          END IF
       ELSE
          PstnSub% = QPLenIncoming%
       END IF
       Position% = MinInt%(PstnAdd%, PstnSub%)
       IF Position% = QPLenIncoming% THEN Position% = 0
       IF Position% THEN

          GOSUB TermBlock

          IF Position% = PstnAdd% THEN
             Term# = LTerm# + RTerm#
          ELSEIF Position% = PstnSub% THEN
             Term# = LTerm# - RTerm#
          END IF
          Ticker = 0
          Term$ = LTRIM$(STR$(Term#))
          Incoming$ = LeftSide$ + Term$ + RightSide$
          IF Term$ = Incoming$ OR Incoming$ = "-1=-1" THEN
             LeftSide$ = ""
             RightSide$ = ""
             LTerm# = 0
             RTerm# = 0
             Term$ = ""
             Term# = 0
             DE$ = ""
             GOTO Done
          END IF
       ELSE
          EXIT DO
       END IF
    LOOP


RelOP:
'***** Relational Operators
    DO
       Pstn1% = INSTR(Incoming$, "=")
       Pstn2% = INSTR(Incoming$, ">")
       Pstn3% = INSTR(Incoming$, "<")
       Position% = MaxInt%(Pstn1%, MaxInt%(Pstn2%, Pstn3%))

       IF Position% = 1 GOTO ErrorExit
       IF Position% > 1 THEN

          IF MID$(Incoming$, Position% - 1, 1) = CHR$(34) THEN
             Begin% = QInstrB%(Position% - 2, Incoming$, CHR$(34))
             LTerm$ = MID$(Incoming$, Begin% + 1, Position% - Begin% - 2)
             LeftSide$ = LEFT$(Incoming$, Begin% - 1)
             LastDig% = INSTR(Position% + 2, Incoming$, CHR$(34))
             RTerm$ = MID$(Incoming$, Position% + 2, LastDig% - Position% - 2)
             RightSide$ = MID$(Incoming$, LastDig% + 1)

             IF Position% = Pstn1% THEN
                Term# = LTerm$ = RTerm$
             ELSEIF Position% = Pstn2% THEN
                Term# = LTerm$ > RTerm$
             ELSEIF Position% = Pstn3% THEN
                Term# = LTerm$ < RTerm$
             END IF

          ELSE
             GOSUB TermBlock

             IF Position% = Pstn1% THEN
                Term# = LTerm# = RTerm#
             ELSEIF Position% = Pstn2% THEN
                Term# = LTerm# > RTerm#
             ELSEIF Position% = Pstn3% THEN
                Term# = LTerm# < RTerm#
             END IF

          END IF

          Term$ = LTRIM$(STR$(Term#))
          Incoming$ = LeftSide$ + Term$ + RightSide$

          IF Term$ = Incoming$ THEN
             LeftSide$ = ""
             RightSide$ = ""
             LTerm# = 0
             RTerm# = 0
             Term$ = ""
             Term# = 0
             GOTO Done
          END IF
       ELSE
          EXIT DO
       END IF
    LOOP



'***** Logical Operators

    FOR Functn% = 1 TO UBOUND(Logic$)
        DO
           Position% = INSTR(Incoming$, Logic$(Functn%))
           IF Position% THEN
              SELECT CASE Functn%
                 CASE 1                         'AND
                    GOSUB TermBlock
                    Position% = Position% + LEN(Logic$(Functn%)) - 1
                    GOSUB TermBlockR
                    Term# = LTerm# AND RTerm#
                    IF Term# < 0 THEN Term# = Term# + 256#
                 CASE 2                         'XOR
                    GOSUB TermBlock
                    Position% = Position% + LEN(Logic$(Functn%)) - 1
                    GOSUB TermBlockR
                    Term# = LTerm# XOR RTerm#
                    IF Term# < 0 THEN Term# = Term# + 256#
                 CASE 3                         'EQV
                    GOSUB TermBlock
                    Position% = Position% + LEN(Logic$(Functn%)) - 1
                    GOSUB TermBlockR
                    Term# = LTerm# EQV RTerm#
                    IF Term# < 0 THEN Term# = Term# + 256#
                 CASE 4                         'IMP
                    GOSUB TermBlock
                    Position% = Position% + LEN(Logic$(Functn%)) - 1
                    GOSUB TermBlockR
                    Term# = LTerm# IMP RTerm#
                    IF Term# < 0 THEN Term# = Term# + 256#
                 CASE 5                         'OR
                    GOSUB TermBlock
                    Position% = Position% + LEN(Logic$(Functn%)) - 1
                    GOSUB TermBlockR
                    Term# = LTerm# OR RTerm#
                    IF Term# < 0 THEN Term# = Term# + 256#
                 CASE ELSE
              END SELECT

              Incoming$ = LeftSide$ + LTRIM$(STR$(Term#)) + RightSide$
              LeftSide$ = "": Numeric$ = "": RightSide$ = ""
           ELSE
              EXIT DO
           END IF
        LOOP
    NEXT








'***** Exit the Function
Done:      Evaluate# = VAL(Incoming$)
           EXIT FUNCTION

ErrorExit:
           FunctionEquation$ = "%" + FunctionEquation$
           EXIT FUNCTION


'***** Gosubs

'----- Isolate the left$ & left#, right# & right$ around the operator
TermBlock:
           Valid$ = "1234567890.-+DE"
           Begin% = Position%
           DO
              Valid% = 1
              Begin% = Begin% - 1
              P% = INSTR(Valid$, MID$(Incoming$, Begin%, 1))
              IF P% THEN
                 IF ((P% = 12) OR (P% = 13)) AND (Sign% = 0) AND (Begin% > 1) THEN
                    IF INSTR("1234567890.", MID$(Incoming$, Begin% - 1, 1)) THEN
                       Begin% = Begin% + 1
                       Valid% = 0
                    END IF
                    Sign% = -1
                 ELSEIF (P% = 14) OR (P% = 15) THEN
                    Sign% = 0
                    Valid$ = "1234567890.-+"
                 END IF
              ELSE
                 Begin% = Begin% + 1
                 Valid% = 0
              END IF
           LOOP WHILE (Valid% = 1) AND (Begin% > 1)
           LeftSide$ = LEFT$(Incoming$, Begin% - 1)
           LTerm# = VAL(MID$(Incoming$, Begin%, Position% - Begin%))

TermBlockR:
           RightSide$ = MID$(Incoming$, Position% + 1)
           'RTerm$ = Mid$(Incoming$, Position%
           RTerm# = VAL(RightSide$)
           IF RightSide$ = LTRIM$(STR$(RTerm#)) THEN
              RightSide$ = ""
              GOTO FinisTB
           END IF
           LastDig% = 1: LastDigOld% = 0: EX% = 0: DP% = 0
           QPlenRightSide% = QPLen(RightSide$)
           DO WHILE LastDig% <= QPlenRightSide%
              Digit$ = MID$(RightSide$, LastDig%, 1)
              IF LastDig% = 1 THEN
                 IF INSTR("+-", Digit$) THEN
                    IF (SGN(RTerm#) = 1) AND (Digit$ = "+") THEN
                       LastDig% = LastDig% + 1
                    ELSEIF (SGN(RTerm#) = -1) AND (Digit$ = "-") THEN
                       LastDig% = LastDig% + 1
                    ELSE
                       EXIT DO
                    END IF
                 END IF
              END IF
              IF INSTR("1234567890", Digit$) THEN
                 LastDig% = LastDig% + 1
              ELSEIF Digit$ = "." THEN
                 IF DP% = 0 THEN LastDig% = LastDig% + 1
                 DP% = 1
              ELSEIF INSTR("DE", Digit$) THEN
                 IF EX% = 0 THEN LastDig% = LastDig% + 1
                 EX% = 1
              ELSEIF (INSTR("+-", Digit$) <> 0) AND (INSTR("DE", MID$(RightSide$, MaxInt(1, LastDig% - 1), 1)) <> 0) THEN
                 LastDig% = LastDig% + 1
              END IF
              IF LastDig% = LastDigOld% THEN EXIT DO
              LastDigOld% = LastDig%
           LOOP
           RightSide$ = MID$(RightSide$, LastDig%)

FinisTB:   P% = 0
           Begin% = P%
           Sign% = P%
           LastDig% = P%
           LastDigOld% = P%
           QPlenRightSide% = P%
           EX% = P%
           DP% = P%
           Valid$ = ""
           Digit$ = ""
           RETURN


'----- Remove double occurrences of plus and minus by cancellation
RemoveOne:  Remove% = -1
DoubleSign: PP% = INSTR(Incoming$, "++")
            IF PP% THEN
               MID$(Incoming$, PP%, 2) = "+ "
               GOTO RemoveOne
            END IF
            PM% = INSTR(Incoming$, "+-")
            IF PM% THEN
               MID$(Incoming$, PM%, 2) = " -"
               GOTO RemoveOne
            END IF
            MP% = INSTR(Incoming$, "-+")
            IF MP% THEN
               MID$(Incoming$, MP%, 2) = "- "
               GOTO RemoveOne
            END IF
            MM% = INSTR(Incoming$, "--")
            IF MM% THEN
               MID$(Incoming$, MM%, 2) = "+ "
               GOTO RemoveOne
            END IF
            IF Remove% THEN
               Remove% = 0
               Incoming$ = Compact$(Incoming$)
            END IF
            RETURN

END FUNCTION

DEFSNG A-H, O-Z
      SUB FormatOutput (IRow, IFormat, IDecimal, IFix)
' $DYNAMIC

      ILastCol = 50
      W$ = STR$(HHW)
      IExpo = INSTR(W$, "D")
      IOutputFormat = IFormat * 10 + IDecimal
      IPointPosition = INSTR(W$, ".")
      ILength = LEN(W$)
      IFormatedOutput = 1

Format00:
      Format$ = ""
      F$ = ""
      SELECT CASE IOutputFormat
             CASE 0  'Scientific Fixed
                    ITempFix = IFix
                    IF IFix < 0 THEN ITempFix = 0
                    IF IFix > 10 THEN ITempFix = 10
                    Format$ = "##." + STRING$(ITempFix, "#") + "^^^^^"
             CASE 1  'Scientific Float
                    IF IExpo = 0 THEN
                           ITempExpo = ILength + 1
                    ELSE
                           ITempExpo = IExpo
                    END IF
                    IF IPointPosition = 0 THEN IPointPosition = ILength + 1
                    ITempFix = ITempExpo - IPointPosition - 1
                    IF ITempFix < 0 THEN ITempFix = 0
                    IF ITempFix > 10 THEN ITempFix = 10
                    Format$ = "##." + STRING$(ITempFix, "#") + "^^^^^"
             CASE 10 'Engineering Fixed
                    ITempFix = IFix
                    IF IFix < 0 THEN ITempFix = 0
                    IF IFix > 10 THEN ITempFix = 10
                    Format$ = "#." + STRING$(ITempFix, "#") + "^^^^^"
                    IF HHW = 0 THEN GOTO FormatExit
                    I = INT(LOG(ABS(HHW)) / LOG(10#))
                    I = I MOD 3
                    SELECT CASE I
                           CASE -2
                                  F$ = "##"
                           CASE -1
                                  F$ = "###"
                           CASE 0
                                  F$ = "#"
                           CASE 1
                                  F$ = "##"
                           CASE 2
                                  F$ = "###"
                    END SELECT

                    Format$ = F$ + Format$
             CASE 11 'Engineering Float
                    IF IExpo = 0 THEN
                           ITempExpo = ILength + 1
                    ELSE
                           ITempExpo = IExpo
                    END IF
                    IF IPointPosition = 0 THEN IPointPosition = ILength + 1
                    ITempFix = ITempExpo - IPointPosition - 1
                    IF ITempFix < 0 THEN ITempFix = 0
                    IF ITempFix > 10 THEN ITempFix = 10
                    Format$ = "#." + STRING$(ITempFix, "#") + "^^^^^"
                    IF HHW = 0 THEN GOTO FormatExit
                    I = INT(LOG(ABS(HHW)) / LOG(10#))
                    I = I MOD 3
                    SELECT CASE I
                           CASE -2
                                  F$ = "##"
                           CASE -1
                                  F$ = "###"
                           CASE 0
                                  F$ = "#"
                           CASE 1
                                  F$ = "##"
                           CASE 2
                                  F$ = "###"
                    END SELECT

                    Format$ = F$ + Format$
             CASE 20 'Any Fixed
                    ITempFix = IFix
                    IF ITempFix < 0 THEN ITempFix = 0
                    IF ITempFix > 10 THEN ITempFix = 10

                    IF IExpo THEN IOutputFormat = 0: GOTO Format00
                    Format$ = STRING$(24, "#")
                    MID$(Format$, 24 - ITempFix, 1) = "."
             CASE 21 'Any Float
                    IFormatedOutput = 0
             CASE ELSE
      END SELECT

FormatExit:

      IF IFormatedOutput = 1 AND LEN(Format$) < 25 THEN
             LOCATE IRow, ILastCol - 30
             PRINT SPACE$(30);
             ILength = LEN(Format$)
             LOCATE IRow, ILastCol - ILength
             PRINT USING Format$; HHW;
      ELSE
             W$ = STR$(HHW)
             LOCATE IRow, ILastCol - 30
             W$ = RIGHT$(SPACE$(30) + W$, 30)
             PRINT W$;
      END IF

      EXIT SUB

END SUB

SUB GetKey (IKey, IDisplay, IExtraTrap, Ky$)
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ GetKey         D.Packard  Apr 90                                           ³
' ³                                                                            ³
' ³ Subroutine to poll keyboard, mouse and serial port                                    ³
' ³                                                                            ³
' ³ Input:                                                                     ³
' ³            IKey - Bit mapped control variable                              ³
' ³                   Bit0 - Wait  toggle                                      ³
' ³                   Bit1 - Flush toggle                                      ³
' ³                   Bit2 - No mouse toggle                                   ³
' ³                   Bit3 - Slow mouse toggle                                 ³
' ³                                                                            ³
' ³        IDisplay - Display Time status toggle                               ³
' ³                                                                            ³
' ³      IExtraTrap - One extra toggle variable NOT used in this program       ³
' ³                                                                            ³
' ³ Output:                                                                    ³
' ³             Ky$ - Key depressed                                            ³
' ³ Notes:                                                                     ³
' ³      IExtraTrap normally used as a communications port external interrupt  ³
' ³      IF activity occurs on com port, higher priority task called           ³
' ³      NOT Used in HandHeld . . . .                                          ³
' ³                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ


      ITimeColor = IWhite * 16 + IBlack
      Ky$ = ""

      IWait = (IKey AND 1) / 1
      IFlush = (IKey AND 2) / 2
      INoMouse = (IKey AND 4) / 4
      ISlowMouse = (IKey AND 8) / 8

      IXMouseHit = 8 + 120 * ISlowMouse
      IYMouseHit = 8 + 120 * ISlowMouse

      IF IFlush = 1 THEN
             WHILE INKEY$ <> ""

' IExtraTrap service during key flush ?

             WEND
             IF Cnf.Mouse AND INoMouse = 0 THEN
                    CALL GetCursor(IDummy, JDummy, IButton)
                    WHILE IButton <> 0                                 'Flush buttons
                           CALL GetCursor(IDummy, JDummy, IButton)

' IExtraTrap service during button flush ?

                    WEND
                    CALL GetCursor(IDummy, JDummy, IButton)
             END IF
      END IF


      WHILE Ky$ = ""
GetKeyLoopBegin:
             IF IDisplay = 1 THEN
                    LOCATE 23, 70, 0
                    CALL QPrint0(TIME$, ITimeColor)
             END IF

             Ky$ = INKEY$
             IF Ky$ <> "" THEN GOTO GetKeyLoopExit
             IF Cnf.Mouse AND INoMouse = 0 THEN
                    CALL GetCursor(IXMouse, IYMouse, IButton)
                    IF IButton > 0 THEN
                           IF IButton AND 1 THEN Ky$ = CHR$(13): GOTO GetKeyButtonPress
                           IF IButton AND 2 THEN Ky$ = CHR$(27): GOTO GetKeyButtonPress
                           IF IButton AND 4 THEN Ky$ = CHR$(13): GOTO GetKeyButtonPress
GetKeyButtonPress:
                           KButton = IButton
                           WHILE KButton <> 0
                                  CALL GetCursor(IDummy, JDummy, KButton)
                           WEND
                           GOTO GetKeyLoopExit
                    END IF
                    IXMouseDelta = (IXMouse - IXMouseLast) / IXMouseHit
                    IYMouseDelta = (IYMouse - IYMouseLast) / IYMouseHit

                    IF ABS(IXMouseDelta) < 1 AND ABS(IYMouseDelta) < 1 THEN GOTO GetKeyLoopBegin

                    IF ABS(IXMouseDelta) > ABS(IYMouseDelta) THEN
                           IF IXMouseDelta > 0 THEN Ky$ = CHR$(0) + CHR$(77)
                           IF IXMouseDelta < 0 THEN Ky$ = CHR$(0) + CHR$(75)
                           CALL SetCursor(IXMouseLast, IYMouseLast)

                    ELSE
                           IF IYMouseDelta > 0 THEN Ky$ = CHR$(0) + CHR$(80)
                           IF IYMouseDelta < 0 THEN Ky$ = CHR$(0) + CHR$(72)
                           CALL SetCursor(IXMouseLast, IYMouseLast)
                    END IF

             END IF

' IExtraTrap service during wait loop ?

             IF IWait = 0 THEN GOTO GetKeyLoopExit
      WEND

GetKeyLoopExit:

      EXIT SUB

END SUB

      SUB HHTurboDuck
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ HH Turbo Duck                                                                                              ³
' ³                                                                                                            ³
' ³ Subroutine to draw duck head when system error occures                                                     ³
' ³                                                                                                            ³
' ³                                                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      IXDuck = 164
      IYDuck = 28

      COLOR IBlue                                  'TurboDuck
      PSET (IXDuck + 17, IYDuck + 12)
      PSET (IXDuck + 13, IYDuck + 13): PSET (IXDuck + 16, IYDuck + 13)
      PSET (IXDuck + 12, IYDuck + 14): PSET (IXDuck + 15, IYDuck + 14): PSET (IXDuck + 18, IYDuck + 24)
      PSET (IXDuck + 12, IYDuck + 15): PSET (IXDuck + 14, IYDuck + 15): DRAW "R1BR1R2"
      PSET (IXDuck + 11, IYDuck + 16): DRAW "R6"
      PSET (IXDuck + 11, IYDuck + 17): DRAW "R7"
      PSET (IXDuck + 12, IYDuck + 18): DRAW "R7"
      PSET (IXDuck + 12, IYDuck + 19): DRAW "R8"
      PSET (IXDuck + 12, IYDuck + 20): DRAW "R8"
      PSET (IXDuck + 11, IYDuck + 21): DRAW "R5"
      PSET (IXDuck + 10, IYDuck + 22): DRAW "R5"
      PSET (IXDuck + 10, IYDuck + 23): DRAW "R5"
      PSET (IXDuck + 10, IYDuck + 24): DRAW "R5"
      PSET (IXDuck + 10, IYDuck + 25): DRAW "R5"
      PSET (IXDuck + 11, IYDuck + 26): DRAW "R5BR6R1"
      PSET (IXDuck + 11, IYDuck + 27): DRAW "R5BR4R1"
      PSET (IXDuck + 12, IYDuck + 28): DRAW "R6BR5R1"
      PSET (IXDuck + 12, IYDuck + 29): DRAW "R5BR6R2"
      PSET (IXDuck + 11, IYDuck + 30): DRAW "R5": PSET (IXDuck + 12, IYDuck + 30)
      PSET (IXDuck + 10, IYDuck + 31): PSET (IXDuck + 19, IYDuck + 31): DRAW "R1": PSET (IXDuck + 23, IYDuck + 31): DRAW "R3"
      PSET (IXDuck + 9, IYDuck + 32): PSET (IXDuck + 31, IYDuck + 32): DRAW "R1": PSET (IXDuck + 36, IYDuck + 32)
      PSET (IXDuck + 9, IYDuck + 33): PSET (IXDuck + 14, IYDuck + 33): PSET (IXDuck + 36, IYDuck + 33)
      PSET (IXDuck + 8, IYDuck + 34): PSET (IXDuck + 13, IYDuck + 34): PSET (IXDuck + 15, IYDuck + 34): PSET (IXDuck + 35, IYDuck + 34)
      PSET (IXDuck + 8, IYDuck + 35): PSET (IXDuck + 11, IYDuck + 35): DRAW "R1": PSET (IXDuck + 15, IYDuck + 35): PSET (IXDuck + 21, IYDuck + 35): PSET (IXDuck + 34, IYDuck + 35)
      PSET (IXDuck + 9, IYDuck + 36): PSET (IXDuck + 15, IYDuck + 36): DRAW "R1": PSET (IXDuck + 30, IYDuck + 36): PSET (IXDuck + 32, IYDuck + 36): DRAW "R1"
      PSET (IXDuck + 10, IYDuck + 37): DRAW "R3": PSET (IXDuck + 15, IYDuck + 37): DRAW "R4": PSET (IXDuck + 29, IYDuck + 37)
      PSET (IXDuck + 15, IYDuck + 38): DRAW "R3": PSET (IXDuck + 20, IYDuck + 38): PSET (IXDuck + 28, IYDuck + 38)
      PSET (IXDuck + 15, IYDuck + 39): DRAW "R3": PSET (IXDuck + 21, IYDuck + 39): PSET (IXDuck + 27, IYDuck + 39)
      PSET (IXDuck + 15, IYDuck + 40): DRAW "R3": PSET (IXDuck + 22, IYDuck + 40): PSET (IXDuck + 26, IYDuck + 40)
      PSET (IXDuck + 15, IYDuck + 41): DRAW "R3": PSET (IXDuck + 23, IYDuck + 41): DRAW "R2"
      COLOR IHWhite
      PSET (IXDuck + 15, IYDuck + 42): DRAW "R2"
      PSET (IXDuck + 15, IYDuck + 43): DRAW "R2"
      PSET (IXDuck + 15, IYDuck + 44): DRAW "R2"
      COLOR IBlue
      PSET (IXDuck + 15, IYDuck + 45): DRAW "R3"
      PSET (IXDuck + 15, IYDuck + 46): DRAW "R3"
      COLOR IBrown
      PSET (IXDuck + 25, IYDuck + 29): DRAW "R1"
      PSET (IXDuck + 25, IYDuck + 30): DRAW "R2"
      PSET (IXDuck + 27, IYDuck + 31)
      COLOR IHWhite
      PSET (IXDuck + 17, IYDuck + 21): PSET (IXDuck + 19, IYDuck + 21): DRAW "R2"
      PSET (IXDuck + 16, IYDuck + 22): DRAW "R6"
      PSET (IXDuck + 16, IYDuck + 23): DRAW "R6"
      PSET (IXDuck + 16, IYDuck + 24): DRAW "R6"
      PSET (IXDuck + 16, IYDuck + 25): DRAW "R5"
      PSET (IXDuck + 17, IYDuck + 26): DRAW "R3"
      COLOR IHBrown
      PSET (IXDuck + 21, IYDuck + 26): DRAW "R1"
      PSET (IXDuck + 20, IYDuck + 27): DRAW "R3"
      PSET (IXDuck + 19, IYDuck + 28): DRAW "R4"
      PSET (IXDuck + 18, IYDuck + 29): DRAW "R5"
      PSET (IXDuck + 17, IYDuck + 30): DRAW "R4": PSET (IXDuck + 23, IYDuck + 30): DRAW "R2"
      PSET (IXDuck + 11, IYDuck + 31): DRAW "R16"
      PSET (IXDuck + 10, IYDuck + 32): DRAW "R20": PSET (IXDuck + 23, IYDuck + 32): DRAW "R2"
      PSET (IXDuck + 10, IYDuck + 33): DRAW "R3": PSET (IXDuck + 15, IYDuck + 33): DRAW "R20"
      PSET (IXDuck + 9, IYDuck + 34): DRAW "R3": PSET (IXDuck + 16, IYDuck + 34): DRAW "R18"
      PSET (IXDuck + 9, IYDuck + 35): DRAW "R1": PSET (IXDuck + 13, IYDuck + 35): DRAW "R1": PSET (IXDuck + 16, IYDuck + 35): DRAW "R14": PSET (IXDuck + 32, IYDuck + 35): DRAW "R1"
      PSET (IXDuck + 10, IYDuck + 36): DRAW "R3": PSET (IXDuck + 17, IYDuck + 36): DRAW "R12"
      PSET (IXDuck + 20, IYDuck + 37): DRAW "R8"
      PSET (IXDuck + 21, IYDuck + 38): DRAW "R6"
      PSET (IXDuck + 22, IYDuck + 39): DRAW "R4"
      PSET (IXDuck + 23, IYDuck + 40): DRAW "R2"
      COLOR IBlue
      PSET (IXDuck + 19, IYDuck + 23): PSET (IXDuck + 21, IYDuck + 23)

      EXIT SUB

END SUB

      SUB InfoBox (Info, Message$)
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ InfoBox          D.Packard  Apr 90                                         ³
' ³                                                                            ³
' ³ Subroutine to display standard information box                             ³
' ³                                                                            ³
' ³ Input:                                                                     ³
' ³       Info  -  Toggle for current engine information                       ³
' ³                                                                            ³
' ³                0  -  Suppress information                                  ³
' ³                1  -  Display  information     NOT used in this program     ³
' ³                                                                            ³
' ³   Message$  -  Text of additional information message                      ³
' ³                                                                            ³
' ³ Output:  None                                                              ³
' ³                                                                            ³
' ³ Notes:                                                                     ³
' ³                                                                            ³
' ³                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
      SHARED IBoxColor
      SHARED IFillColor
      SHARED ITextColor

      IBoxChar = 2
      CALL Box0(IBoxULRow, IBoxULCol, IBoxLRRow, IBoxLRCol, IBoxChar, IBoxColor)
      IRow1 = IBoxULRow + 1
      ICol1 = IBoxULCol + 1
      IRow2 = IBoxLRRow - 1
      ICol2 = IBoxLRCol - 1
      CALL ClearScr0(IRow1, ICol1, IRow2, ICol2, IFillColor)

      IF Info = 1 THEN
             Message$ = "Standard InfoBox Message"
      END IF

      LOCATE IBoxULRow + 1, IBoxULCol + ((IBoxLRCol - IBoxULCol - LEN(Message$)) / 2)
      CALL QPrint0(Message$, ITextColor)

      IF Info = 0 THEN EXIT SUB

' Standard message display
' This section of code displays the standard information box messages
' Not used in HandHeld

'      NMessage = 9
'      REDIM M$(NMessage)
'      M$(0) = ""
'      M$(1) = ""
'      M$(2) = ""
'      M$(3) = ""
'      M$(4) = ""
'      M$(5) = ""
'      M$(6) = ""
'      M$(7) = ""
'      M$(8) = ""
'      M$(9) = ""

'      FOR I = 0 TO 9
'             LOCATE IBoxULRow + I + 3, IBoxULCol + 2
'             CALL APrint0(BYVAL VARPTR(M$(I)), 1, 1, LEN(M$(I)), ITextColor)
'      NEXT
'      ERASE M$

      EXIT SUB

END SUB

      SUB Instructions
' $DYNAMIC

      SHARED ITextColor
      SHARED IWarningColor
      SHARED WarningSound$

      IF Exist%("HHHelp.Txt") THEN
             CALL ScrnOut("HHHelp.Txt", 80)
      ELSE
             IPaletteColor = IBlue
             PALETTE IGreen, IPaletteColor
             CALL Logo
             CALL InfoBox(0, "Instruction Manual Request")
             LOCATE IBoxULRow + 5, IBoxULCol + 4
             CALL QPrint0("Disk File 'HHHelp.Txt' NOT found", IWarningColor)
             LOCATE IBoxULRow + 8, IBoxULCol + 4
             CALL QPrint0("Run program HHHelp.Bas or . . . .", ITextColor)
             LOCATE IBoxULRow + 9, IBoxULCol + 4
             CALL QPrint0("Copy file 'HHHelp.Txt' to this directory", ITextColor)
             LOCATE IBoxULRow + 12, IBoxULCol + 4
             CALL QPrint0("Depress any key to continue . . . .", ITextColor)
             PLAY WarningSound$
             CALL GetKey(3, 1, 0, Ky$)
      END IF
      EXIT SUB

END SUB

      SUB Logo
' $DYNAMIC

      ILogoColor = IGreen * 16 + IHWhite

      LOCATE 4, 16
      CALL QPrint0("            Q u i c k C a l c    I I             ", ILogoColor)
      LOCATE 5, 16
      CALL QPrint0("                                                 ", ILogoColor)
      LOCATE 6, 16
      CALL QPrint0("                                                 ", ILogoColor)
      LOCATE 7, 16
      CALL QPrint0("            Quick Calculator Program             ", ILogoColor)
      LOCATE 8, 16
      CALL QPrint0("                                                 ", ILogoColor)
      LOCATE 9, 16
      CALL QPrint0("       Version  01.00.03     February 1991       ", ILogoColor)
      CALL Box0(3, 15, 10, 65, 1, ILogoColor)

      EXIT SUB

END SUB

      SUB Mask
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Mask             D.Packard  Apr 90                                         ³
' ³                                                                            ³
' ³ Subroutine to display standard Quick Calc II background screen mask        ³
' ³                                                                            ³
' ³ Input:  None                                                               ³
' ³                                                                            ³
' ³ Output: Screen cleared to white, mouse buttons and calendar displayed      ³
' ³                                                                            ³
' ³ Notes:                                                                     ³
' ³                                                                            ³
' ³                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      SHARED IPaletteColor

      IMouseColor = IWhite * 16 + IHWhite

      COLOR IHWhite, IWhite
      CLS
      COLOR IBlack
      PALETTE IGreen, IPaletteColor

      IF Cnf.Mouse THEN
             NMouse = 8
             REDIM MouseText$(NMouse)
             SELECT CASE Cnf.MouseButtons
                    CASE 3
                            MouseText$(0) = "           "
                            MouseText$(1) = "   Mouse   "
                            MouseText$(2) = "ÚÄ¿ ÚÄ¿ ÚÄ¿"
                            MouseText$(3) = "³E³ ³ ³ ³ ³"
                            MouseText$(4) = "³n³ ³ ³ ³E³"
                            MouseText$(5) = "³t³ ³ ³ ³s³"
                            MouseText$(6) = "³e³ ³ ³ ³c³"
                            MouseText$(7) = "³r³ ³ ³ ³ ³"
                            MouseText$(8) = "ÀÄÙ ÀÄÙ ÀÄÙ"
                    CASE ELSE
                            MouseText$(0) = "           "
                            MouseText$(1) = "   Mouse   "
                            MouseText$(2) = "ÚÄ¿     ÚÄ¿"
                            MouseText$(3) = "³E³     ³ ³"
                            MouseText$(4) = "³n³     ³E³"
                            MouseText$(5) = "³t³     ³s³"
                            MouseText$(6) = "³e³     ³c³"
                            MouseText$(7) = "³r³     ³ ³"
                            MouseText$(8) = "ÀÄÙ     ÀÄÙ"
             END SELECT

             FOR I = 0 TO 8
                    IRow = I + 2
                    LOCATE IRow, 2
                    CALL QPrint0(MouseText$(I), IMouseColor)
                    LOCATE IRow, 69
                    CALL QPrint0(MouseText$(I), IMouseColor)
             NEXT

      END IF

      ERASE MouseText$
      CurrentDate$ = DATE$
      IYear = VAL(RIGHT$(CurrentDate$, 4))
      IMonth = VAL(LEFT$(CurrentDate$, 2))
      IDay = VAL(MID$(CurrentDate$, 4, 2))
      CALL Calendar(IYear, IMonth, IDay, 1)

      EXIT SUB

END SUB

      SUB PanCalendar
' $DYNAMIC

      SHARED IYear
      SHARED IMonth
      SHARED IDay
      SHARED ISystemYear
      SHARED ISystemMonth
      SHARED ISystemDay
      SHARED ISundayColor
      SHARED ITextColor
      SHARED Month$()

PanCalendarBegin:
      IPaletteColor = IBlue
      PALETTE IGreen, IPaletteColor
      CALL Logo
      CALL InfoBox(0, "P a n   C a l e n d a r")

      LOCATE IBoxULRow + 3, 3: CALL QPrint0("        Key            -    Action", ITextColor)
      LOCATE IBoxULRow + 5, 3: CALL QPrint0(" [PgUp ]   [  " + CHR$(24) + "  ]     -    Previous Month", ITextColor)
      LOCATE IBoxULRow + 6, 3: CALL QPrint0(" [PgDn ]   [  " + CHR$(25) + "  ]     -    Next     Month", ITextColor)
      LOCATE IBoxULRow + 7, 3: CALL QPrint0(" [ Esc ]   [Enter]     -    Main Menu", ITextColor)
      LOCATE IBoxULRow + 8, 3: CALL QPrint0(" [  E  ]               -    Enter specific date", ITextColor)
      LOCATE IBoxULRow + 9, 3: CALL QPrint0(" [  S  ]               -    System date", ITextColor)
      LOCATE IBoxULRow + 10, 3: CALL QPrint0(" [  F  ]               -    Full year format", ITextColor)

PanCalendarKeyLoop:

      CALL GetKey(11, 1, 0, Ky$)

      Ky$ = UCASE$(Ky$)
      IF LEN(Ky$) = 2 THEN Ky$ = RIGHT$(Ky$, 1)
      IKey = ASC(Ky$)

      SELECT CASE IKey
             CASE 13, 27
                    EXIT SUB
             CASE 70 'Full year format
                    GOSUB FullYearFormat
             CASE 69 'Enter date
                    XDate$ = SPACE$(7)
                    Message$ = "Enter Date    mm/yyyy               01/1900 Minimum"
                    LOCATE IBoxULRow + 12, 3: CALL QPrint0(Message$, ITextColor)
                    LOCATE IBoxULRow + 12, 24
                    INPUT "   ", XDate$
                    LOCATE IBoxULRow + 12, 3: CALL QPrint0(SPACE$(LEN(Message$)), ITextColor)

                    IF LEN(XDate$) = 7 THEN
                           ITempYear = VAL(RIGHT$(XDate$, 4))
                           ITempMonth = VAL(LEFT$(XDate$, 2))
                    ELSE
                           GOTO PanCalendarKeyLoop
                    END IF
                    IF ITempMonth > 12 OR ITempMonth < 1 THEN GOTO PanCalendarKeyLoop
                    IF ITempYear < 1900 THEN GOTO PanCalendarKeyLoop
                    IF ITempMonth = ISystemMonth AND ITempYear = ISystemYear THEN
                           ITempDay = ISystemDay
                    ELSE
                           ITempDay = 0
                    END IF
                    IYear = ITempYear
                    IMonth = ITempMonth
                    CALL Calendar(IYear, IMonth, ITempDay, 0)
             CASE 83 ' System Date
                    IYear = ISystemYear
                    IMonth = ISystemMonth
                    IDay = ISystemDay
                    CALL Calendar(IYear, IMonth, IDay, 0)

             CASE 56, 57, 72, 73 'Page up
                    IMonth = IMonth - 1
                    IF IMonth = 0 THEN
                           IMonth = 12
                           IYear = IYear - 1
                    END IF
                    IF IMonth = ISystemMonth AND IYear = ISystemYear THEN
                          ITempDay = ISystemDay
                    ELSE
                          ITempDay = 0
                    END IF
                    CALL Calendar(IYear, IMonth, ITempDay, 0)
             CASE 50, 51, 80, 81 'Page down
                    IMonth = IMonth + 1
                    IF IMonth = 13 THEN
                           IMonth = 1
                           IYear = IYear + 1
                    END IF
                    IF IMonth = ISystemMonth AND IYear = ISystemYear THEN
                          ITempDay = ISystemDay
                    ELSE
                          ITempDay = 0
                    END IF
                    CALL Calendar(IYear, IMonth, ITempDay, 0)

      END SELECT
      GOTO PanCalendarKeyLoop

FullYearFormat:
      REDIM IScreen(4096)
      CALL ScrnSave0(1, 1, 25, 80, SEG IScreen(0))

      ITempDay = 0
      WIDTH 80, 43
      COLOR IBlack, IWhite, IWhite
      CLS
      IReverseColor = (ITextColor AND 112) / 16 + (ITextColor AND 15) * 16 AND 127
      IReverseSundayColor = (ISundayColor AND 112) / 16 + (ISundayColor AND 15) * 16 AND 127
      LOCATE 2, 37
      CALL QPrint0(STR$(IYear), ITextColor)
      FOR I = 0 TO 3
             IULRow = 4 + I * 10
             FOR J = 1 TO 3
                    ILoopMonth = I * 3 + J
                    IULCol = 4 + (J - 1) * 25
                    LOCATE IULRow + 1, IULCol
                    CALL QPrint0(SPACE$((22 - LEN(Month$(ILoopMonth))) / 2) + Month$(ILoopMonth), ITextColor)
                    LOCATE IULRow + 2, IULCol
                    CALL QPrint0(" Su Mo Tu We Th Fr Sa ", ITextColor)

' Information on date passed to routine

                    IF ILoopMonth > 2 THEN
                           ITemp1 = ILoopMonth + 1
                           ITemp2 = IYear
                    ELSE
                           ITemp1 = ILoopMonth + 13
                           ITemp2 = IYear - 1
                    END IF
                    DaysSinceCurrent! = INT(365.25 * ITemp2) - 693975 + INT(30.6 * ITemp1)
                    IDayofWeek = DaysSinceCurrent! - INT(DaysSinceCurrent! / 7) * 7 + 1
                    IFirstDayCurrent = IDayofWeek

' Information on next month after date passed to routine

                    INextMonth = ILoopMonth + 1
                    INextYear = IYear
                    IF INextMonth = 13 THEN
                           INextMonth = 1
                           INextYear = INextYear + 1
                    END IF


                    IF INextMonth > 2 THEN
                           ITemp1 = INextMonth + 1
                           ITemp2 = INextYear
                    ELSE
                           ITemp1 = INextMonth + 13
                           ITemp2 = INextYear - 1
                    END IF
                    DaysSinceNext! = INT(365.25 * ITemp2) - 693975 + INT(30.6 * ITemp1)

                    ILineOffSet = 4
                    LOCATE IULRow + ILineOffSet, IULCol
                    IF IDayofWeek <> 7 THEN
                           CALL QPrint0(SPACE$(IDayofWeek * 3), ITextColor)
                           LOCATE , POS(0) + IDayofWeek * 3
                    END IF

                    IDaysinLoopMonth = DaysSinceNext! - DaysSinceCurrent!
                    FOR K = 1 TO IDaysinLoopMonth
                           CALL QPrint0(" ", ITextColor)
                           LOCATE , POS(0) + 1

                           IF K <> ITempDay OR ILoopMonth <> IMonth THEN
                                  IF IDayofWeek MOD 7 = 0 THEN
                                         IColor = ISundayColor
                                  ELSE
                                         IColor = ITextColor
                                  END IF

                           ELSE
                                  IF IDayofWeek MOD 7 = 0 THEN
                                        IColor = IReverseSundayColor   'IReverseColor if reverse sunday not wanted
                                  ELSE
                                        IColor = IReverseColor
                                  END IF
                           END IF

                           CALL QPrint0(RIGHT$(" " + STR$(K), 2), IColor)
                           LOCATE , POS(0) + 2
                           IDayofWeek = IDayofWeek + 1

                           IF INT(IDayofWeek / 7) = IDayofWeek / 7 THEN
                                  ILineOffSet = ILineOffSet + 1
                                  LOCATE IULRow + ILineOffSet, IULCol
                           END IF

                    NEXT K
             NEXT J
      NEXT I

FullYearFormatKeyLoop:
      LOCATE 1, 1
      CALL QPrint0("                                                              <P>rint hard copy ", ITextColor)
      CALL GetKey(7, 0, 0, Ky$)
      Ky$ = UCASE$(Ky$)

      IF Ky$ = "P" THEN
             LOCATE 1, 1
             CALL QPrint0(SPACE$(80), ITextColor)
             CALL PrtSc(1, -1)
             LPRINT CHR$(12)
             GOTO FullYearFormatKeyLoop
      END IF

      WIDTH 80, 25
      PALETTE IGreen, IPaletteColor
      CALL ScrnRest0(1, 1, 25, 80, SEG IScreen(0))
      GOTO PanCalendarBegin

END SUB

      SUB ReadEqFile
' $DYNAMIC

'   Equation$ = User input form of equation converted to upper case (except e)
' Expression$ = Form of equation with () added to stack variables X,Y,Z,M

      IEquationFile = Exist%("UsrFnct.Crd")
      IF IEquationFile THEN
             IDataFileNumber = FREEFILE
             OPEN "R", IDataFileNumber, "UsrFnct.Crd", 80
             FIELD IDataFileNumber, 80 AS Dummy$
             FOR I = 1 TO 10
                    GET #IDataFileNumber
                    Key$ = LEFT$(Dummy$, 5)
                    Key$ = LTRIM$(RTRIM$(Key$))
                    K = LEN(Key$)
                    IF K = 0 THEN Key$(6, I) = "     "
                    IF K = 1 THEN Key$(6, I) = "  " + Key$ + "  "
                    IF K = 2 THEN Key$(6, I) = " " + Key$ + "  "
                    IF K = 3 THEN Key$(6, I) = " " + Key$ + " "
                    IF K = 4 THEN Key$(6, I) = Key$ + " "
                    IF K = 5 THEN Key$(6, I) = Key$
                    Prompt$(I) = MID$(Dummy$, 10, 51)
                    GET #IDataFileNumber
                    Equation$(I) = Dummy$
                    Equation$(I) = LTRIM$(RTRIM$(Equation$(I)))
             NEXT I
             CLOSE IDataFileNumber
      ELSE
             IDataFileNumber = FREEFILE
             OPEN "R", IDataFileNumber, "UsrFnct.Crd", 80
             FIELD IDataFileNumber, 80 AS Dummy$
             FOR I = 1 TO 20
                    LSET Dummy$ = SPACE$(80)
                    PUT #IDataFileNumber
             NEXT I
             CLOSE IDataFileNumber
      END IF

      FOR I = 1 TO 10
             Temp$ = Equation$(I)
             CALL EditSyntax(Temp$)

             Expression$ = ""
             FOR IChar = 1 TO LEN(Temp$)
                    Char$ = MID$(Temp$, IChar, 1)
                    IF UCASE$(Char$) = "X" THEN Char$ = "(X)"
                    IF UCASE$(Char$) = "Y" THEN Char$ = "(Y)"
                    IF UCASE$(Char$) = "Z" THEN Char$ = "(Z)"
                    IF UCASE$(Char$) = "M" THEN Char$ = "(M)"
                    Expression$ = Expression$ + Char$
             NEXT IChar
             Expression$(I) = Expression$
      NEXT I

      EXIT SUB

END SUB

      SUB ReadHHData
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Read Hand Held Data                                                                                        ³
' ³                                                                                                            ³
' ³ Subroutine to read data statements                                                                         ³
' ³                                                                                                            ³
' ³                                                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      RESTORE HHASCtoKey
      FOR I = 0 TO 59
             READ IASCtoKey(I)
      NEXT I

      RESTORE HHKeyDefine
      FOR I = 1 TO 5
             FOR J = 1 TO 10
                    READ Key$(I, J)
             NEXT J
      NEXT I

      RESTORE HHSecondKeyDefine
      FOR I = 1 TO 6
             FOR J = 1 TO 10
                    READ SecondKey$(I, J)
             NEXT J
      NEXT I

      EXIT SUB

END SUB

SUB ScrnOut (FileName$, IMaxChar)
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ ScrnOut          D.Packard  Apr 90                                         ³
' ³                                                                            ³
' ³ Subroutine to display FileName disk file to screen                         ³
' ³                                                                            ³
' ³ Input:                                                                     ³
' ³       FileName$  -  Name of disk file to be displayed                      ³
' ³       IMaxChar   -  Maximum width of display                               ³
' ³                                                                            ³
' ³ Output:   File displayed                                                   ³
' ³                                                                            ³
' ³ Notes:                                                                     ³
' ³         File must be random excess of the correct record length            ³
' ³                                                                            ³
' ³                                                                            ³
' ³                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      SHARED ISizeofText
      SHARED IHighLighterColor

      IRecord = 1
      NRecord = CINT(FileSize&(FileName$) / IMaxChar)

      IOutfile = FREEFILE
      OPEN "R", IOutfile, FileName$, IMaxChar
      FIELD IOutfile, IMaxChar AS Line$
                                              
      REDIM ILinesofText(3, 3)
      FOR IX = 0 TO 3
             FOR IY = 0 TO 3
                    ILinesofText(IX, IY) = 25
             NEXT IY
      NEXT IX
      ILinesofText(3, 2) = 43
      ILinesofText(2, 3) = 43
      ILinesofText(3, 3) = 50

      SELECT CASE Cnf.MonTyp
             CASE 5
                    INumLines = 2
             CASE 7
                    INumLines = 3
             CASE ELSE
                    INumLines = 1
      END SELECT

      IHighLightLine = 1
      IColor = IWhite * 16 + IBlack
      IHighLightColor = IGreen * 16 + IBlack

      REDIM IScreenHighLightColor(7)
      IScreenHighLightColor(1) = 43
      IScreenHighLightColor(2) = 58
      IScreenHighLightColor(3) = 59
      IScreenHighLightColor(4) = 39
      IScreenHighLightColor(5) = 61
      IScreenHighLightColor(6) = 62
      IScreenHighLightColor(7) = 7

ScrnOutBegin:

      NumLines = ILinesofText(ISizeofText, INumLines)
      IF ISizeofText = 0 THEN IColumns = 40 ELSE IColumns = 80

      REDIM Output$(NumLines)

      FOR I = 0 TO NumLines
             Output$(I) = SPACE$(IMaxChar)
      NEXT

      COLOR IBlack, IWhite, IWhite
      WIDTH IColumns, NumLines
      CLS

      PALETTE IGreen, IScreenHighLightColor(IHighLighterColor)

      IStart = 1
      IEnd = NumLines
      IFirstChar = 1
      IMaxFirstChar = IMaxChar - IColumns + 1
      GOSUB LoadArray

ScrnOutDisplay:
      LOCATE 1, 1
      CALL APrint0(BYVAL VARPTR(Output$(1)), NumLines, IFirstChar, IColumns, IColor)
      LOCATE IHighLightLine, 1
      CALL APrint0(BYVAL VARPTR(Output$(IHighLightLine)), 1, IFirstChar, IColumns, IHighLightColor)

ScrnOutKeyTrap:
      CALL GetKey(3, 0, 1, Ky$)

      IF Ky$ = "L" OR Ky$ = "l" THEN
             ISizeofText = ISizeofText - 1
             IF ISizeofText < 0 THEN ISizeofText = 3
             IHighLightLine = 1
             GOTO ScrnOutBegin
      END IF

      IF Ky$ = "C" OR Ky$ = "c" THEN
             IHighLighterColor = IHighLighterColor + 1
             IF IHighLighterColor > 7 THEN IHighLighterColor = 1
             PALETTE IGreen, IScreenHighLightColor(IHighLighterColor)
             GOTO ScrnOutKeyTrap
      END IF

      IF LEN(Ky$) = 2 THEN
             IScanCode = -ASC(RIGHT$(Ky$, 1))
      ELSE
             IScanCode = ASC(Ky$)
      END IF


      SELECT CASE IScanCode

             CASE 50 'HighLight Down
                    IF IHighLightLine = NumLines THEN
                           GOTO ScrnOutKeyTrap
                    END IF
                    IHighLightLine = IHighLightLine + 1
                    GOTO ScrnOutDisplay

             CASE 56 'HighLight Up
                    IF IHighLightLine = 1 THEN GOTO ScrnOutKeyTrap
                    IHighLightLine = IHighLightLine - 1
                    GOTO ScrnOutDisplay

             CASE -59 ' ScrnOut Help
                    WIDTH 80, 25
                    CLS
                    PRINT "               Screen Output Help"
                    PRINT
                    PRINT "        Key                           Action"
                    PRINT
                    PRINT "      Cursor Up                     Pan up   one line"
                    PRINT "      Cursor Down                   Pan down one line"
                    PRINT "      Cursor Left                   Pan left  one column"
                    PRINT "      Cursor Right                  Pan right one column"
                    PRINT "      Page Up                       Pan up   one page"
                    PRINT "      Page Down                     Pan down one page"
                    PRINT "      Home                          Top of File"
                    PRINT "      End                           End of File"
                    PRINT "      Control + Cursor Left         Pan left  one screen width"
                    PRINT "      Control + Cursor Right        Pan right one screen width"
                    PRINT "      L                             Text size         <L>arge"
                    PRINT "      C                             Highlight color   <C>olor"
                    PRINT "      8  [Shift Cursor Up  ]        Highlight line up"
                    PRINT "      2  [Shift Corsor Down]        Highlight line down"
                    PRINT
                    PRINT "      <Esc>                         Exit screen output module"
                    LOCATE 25, 1
                    PRINT "     Depress any key to continue . . . .";

                    CALL GetKey(3, 0, 1, Ky$)
                    WIDTH IColumns, NumLines
                    CLS
                    PALETTE IGreen, IScreenHighLightColor(IHighLighterColor)
                    GOTO ScrnOutDisplay

             CASE -71 'Home
                    IStart = 1
                    IEnd = NumLines
                    GOSUB LoadArray
                    GOTO ScrnOutDisplay

             CASE -72 'Cursor Up
                    IF IStart = 1 THEN GOTO ScrnOutKeyTrap
                    IStart = IStart - 1
                    IEnd = IStart + NumLines - 1
                    FOR I = NumLines TO 2 STEP -1
                           LSET Output$(I) = Output$(I - 1)
                    NEXT
                    GET IOutfile, IStart
                    LSET Output$(1) = Line$
                    GOTO ScrnOutDisplay

             CASE -73 'Page Up
                    IF IStart = 1 THEN GOTO ScrnOutKeyTrap
                    IStart = IStart - NumLines
                    IF IStart < 1 THEN IStart = 1
                    IEnd = IStart + NumLines - 1
                    GOSUB LoadArray
                    GOTO ScrnOutDisplay

             CASE -75 'Left
                    IFirstChar = IFirstChar - 1
                    IF IFirstChar < 1 THEN IFirstChar = 1
                    GOTO ScrnOutDisplay

             CASE -77 'Right
                    IFirstChar = IFirstChar + 1
                    IF IFirstChar > IMaxFirstChar THEN IFirstChar = IMaxFirstChar
                    GOTO ScrnOutDisplay

             CASE -79 'End
                    IF IEnd = NRecord THEN GOTO ScrnOutKeyTrap
                    IEnd = NRecord
                    IStart = IEnd - NumLines + 1
                    GOSUB LoadArray
                    GOTO ScrnOutDisplay

             CASE -80 'Cursor Down
                    IF IEnd = NRecord THEN GOTO ScrnOutKeyTrap
                    IEnd = IEnd + 1
                    IStart = IEnd - NumLines + 1
                    FOR I = 1 TO NumLines - 1
                           LSET Output$(I) = Output$(I + 1)
                    NEXT
                    GET IOutfile, IEnd
                    LSET Output$(NumLines) = Line$
                    GOTO ScrnOutDisplay

             CASE -81 'Page Down
                    IF IEnd = NRecord THEN GOTO ScrnOutKeyTrap
                    IEnd = IEnd + NumLines
                    IF IEnd > NRecord THEN IEnd = NRecord
                    IStart = IEnd - NumLines + 1
                    GOSUB LoadArray
                    GOTO ScrnOutDisplay

             CASE 52, -115 'Jump Left
                    IFirstChar = 1
                    GOTO ScrnOutDisplay

             CASE 54, -116 'Jump Right
                    IFirstChar = IMaxFirstChar
                    GOTO ScrnOutDisplay

             CASE 13, 27 'Return or Escape
                    GOTO ScrnOutExit

             CASE ELSE
                    GOTO ScrnOutKeyTrap
      END SELECT

ScrnOutExit:

      CLOSE IOutfile
      WIDTH 80, 25
      ERASE ILinesofText
      ERASE IScreenHighLightColor
      ERASE Output$
      EXIT SUB

LoadArray:
      IArray = 1
      FOR I = IStart TO IEnd
             GET IOutfile, I
             LSET Output$(IArray) = Line$
             IArray = IArray + 1
      NEXT
      RETURN

END SUB

      SUB VertMenu (IChoice, IMaxLen, IBoxBot, Ky$, IAction)
' $DYNAMIC

' ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
' ³ Vertical Menu                                                                                              ³
' ³                                                                                                            ³
' ³ Subroutine to display vertical menu                                                                        ³
' ³                                                                                                            ³
' ³ Modification of Crescent Software Inc. routine                                                             ³
' ³                                                                                                            ³
' ³          <1> - Hot key highlight    Key , <Return>   NOT  <Alt> + Key                                      ³
' ³          <2> - Help line display                                                                           ³
' ³                                                                                                            ³
' ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

      SHARED VChoice$()
      SHARED VAltChoice$()
      SHARED IVAltChoice()
      SHARED VHelp$()

      IHelpColor = IBlack * 16 + IHWhite
      IPrevRow = 0
      IPrevChoice = 0
      IHighLighted = 0

      IF IAction = 4 THEN IAction = 1            'See if they forgot to reset
      IOne = 1
      IF IAction < 2 THEN REDIM IScrBuf(IOne)       'Temp Dim the Screen buffer
      IF IAction = 5 AND UBOUND(IScrBuf) > 1 THEN 'Restore the screen if action=5
             CALL ScrnRest0(ITop - 1, ILeft.Margin, IBottom + 2, IRight.Margin + 2, IScrBuf(0))
             ERASE IScrBuf
             EXIT SUB
      END IF

      IF IAction < 2 THEN
             GetVMode 0, 0, 0, IScRows, 0
             INPixLines = 8
             IF IScRows > 43 AND DOSVer% = 1000 THEN INPixLines = 7

             IF IBoxBot = 0 THEN IBoxBot = IScRows - 1
             IBoxBot = MinInt(IBoxBot, IScRows - 1)

       'Find the Inverse box color for the Scroll Bar
             IFgr = Cnf.MenBox MOD 16
             IScrlClr = (Cnf.MenBox \ 16) + 16 * (IFgr - ((IFgr \ 8) * 8))

             Fill$ = SPACE$(IMaxLen + 1)              'used in Dsply to pad spaces

       '---- determine how many choices to display, and where to put the menu
             IMaxNum = UBOUND(VChoice$)                  'find number of active items
             IMaxNum = FindLast%(VARPTR(VChoice$(IMaxNum)), IMaxNum)

             ITop = CSRLIN + 1                         'put the menu at the current
             ILeft.Margin = POS(0)                     '  cursor location
             IBottom = ITop + IMaxNum - 1
             IMore = 0
             IRow = ITop + (IChoice - IOffset) - 1

             IF IBottom >= IBoxBot THEN  '<---/-- BoxBot sets lowest row to display
                    IMore = -1              '   /
                    IBottom = IBoxBot - 1    '<-/
             END IF
             IWinSize = IBottom - ITop + 1
             IRight.Margin = ILeft.Margin + IMaxLen + 3

       '----- Save the underlying screen area
             ISize = (IWinSize + 3) * (IRight.Margin - ILeft.Margin + 3)
             REDIM IScrBuf(ISize + 1)
             CALL ScrnSave0(ITop - 1, ILeft.Margin, IBottom + 2, IRight.Margin + 2, IScrBuf(0))

       '----- Make sure the choice # is in range
             IOffset = 0
             IF IChoice < 0 OR IChoice > IMaxNum THEN
                    IChoice = 0
             END IF

             IF IChoice > IOffset + IWinSize THEN
                    IOffset = IChoice - IWinSize
             END IF

       '----- draw border and display items on the screen
             LOCATE CSRLIN, ILeft.Margin
             CALL QPrint0("É" + STRING$(IMaxLen + 2, "Í") + "»", Cnf.MenBox)
             FOR I = ITop TO IBottom                  'Draw sides, display items
                    IAPos = I - ITop + 1
                    LSET Fill$ = VChoice$(IAPos + IOffset)
                                                   'Left box side and choice
                    LOCATE I, ILeft.Margin
                    CALL QPrint0("º ", Cnf.MenBox)
                    LOCATE I, ILeft.Margin + 2
                    CALL QPrint0(Fill$, Cnf.ActivCh)
                    LOCATE I, ILeft.Margin + IVAltChoice(IAPos + IOffset) + 1
                    CALL QPrint0(VAltChoice$(IAPos + IOffset), Cnf.MenBox)

                    IF I = ITop AND IMore = -1 THEN           'Draw Up arrow box side
                           LOCATE I, IRight.Margin
                           CALL QPrint0(CHR$(24), IScrlClr)
                ELSEIF I = IBottom AND IMore = -1 THEN    'Draw Down arrow box side
                           LOCATE I, IRight.Margin
                           CALL QPrint0(CHR$(25), IScrlClr)
                ELSEIF IMore THEN                        'Draw Scroll bar
                           LOCATE I, IRight.Margin
                           CALL QPrint0("²", IScrlClr)
                ELSE                                    'Draw normal box side
                           LOCATE I, IRight.Margin
                           CALL QPrint0("º", Cnf.MenBox)
                    END IF
       NEXT
                                                   'Draw the bottom of box
       LOCATE I, ILeft.Margin
       CALL QPrint0("È" + STRING$(IMaxLen + 2, "Í") + "¼", Cnf.MenBox)

       CALL PaintBox0(ITop, IRight.Margin + 1, IBottom + 1, IRight.Margin + 2, 8)
       CALL PaintBox0(IBottom + 2, ILeft.Margin + 2, IBottom + 2, IRight.Margin + 2, 8)


       Fill$ = ""
       ILCol = ILeft.Margin + 1                      'Set variables for Hilite
       IRCol = ILCol + IMaxLen + 1
       IOPtr = 0

       GOSUB VertHiLite
    END IF

    IF IAction = 2 THEN                        'Redisplay menu contents
       GOSUB VertDisplay
       GOSUB VertHiLite
    END IF

    IF IAction THEN IAction = 3                 'Reset Action = 3 (Idle state)

    DO

' Begin polling loop

        DO
        IPrevChoice = IChoice
        CALL GetKey(11, 1, 0, Ky$)
        LOOP WHILE Ky$ = "" AND IAction = 0

       IKyLen = LEN(Ky$)

       IF IKyLen = 1 THEN                      'Non-extended keys

VertMenu1:

          SELECT CASE ASC(Ky$)
             CASE 13, 27                      'Enter or Escape key
                IF IAction THEN
                   IAction = 4
                ELSE
                   CALL ScrnRest0(ITop - 1, ILeft.Margin, IBottom + 2, IRight.Margin + 2, IScrBuf(0))
                   ERASE IScrBuf               'Clean up memory
                END IF
                LOCATE , , 1
                EXIT SUB

             CASE 33 TO 122                   'Letter key pressed
                                              'Reset color
                Ky$ = UCASE$(Ky$)             'Upper case version of letter
                K2$ = LCASE$(Ky$)             'dito for lower case
                IPass = -1                     'First pass through choices
                N = IChoice                    'Start at current choice + 1
                DO
                   IPass = IPass + 1
                   IF N < IMaxNum THEN
                      N = N + 1
                   ELSE N = 1
                   END IF

                   WHILE N < IMaxNum AND Ky$ <> VAltChoice$(N) AND K2$ <> VAltChoice$(N)
                      N = N + 1
                   WEND
                   IF Ky$ = VAltChoice$(N) OR K2$ = VAltChoice$(N) THEN
                      IChoice = N
                      IF IChoice > IOffset + (IBottom - ITop + 1) THEN
                         IOffset = IChoice - (IBottom - ITop + 1)
                         GOSUB VertDisplay
                      END IF
                      IF IChoice <= IOffset THEN
                         IOffset = IChoice - 1
                         GOSUB VertDisplay
                      END IF
                      EXIT DO
                   END IF
                LOOP WHILE IChoice > 1 AND IPass = 0
                GOSUB VertHiLite

             CASE ELSE
          END SELECT

       ELSEIF IKyLen = 2 THEN                  'Reset color

          SELECT CASE ASC(RIGHT$(Ky$, 1))

             CASE 72
                IF IChoice > 1 THEN
                   DO
                      IChoice = IChoice - 1
                      IF IRow - 1 < ITop THEN IOffset = IOffset - 1
                   LOOP WHILE LEFT$(VChoice$(IChoice), 1) = "Ä"
                   GOSUB VertDisplay
                ELSEIF IMore = 0 THEN
                   IChoice = 1
                END IF

             CASE 80
                IF IChoice < IMaxNum THEN
                   DO
                      IChoice = IChoice + 1
                      IF IRow + 1 > IBottom THEN IOffset = IOffset + 1
                   LOOP WHILE LEFT$(VChoice$(IChoice), 1) = "Ä"
                   GOSUB VertDisplay
                ELSEIF IMore = 0 THEN
                   IChoice = IMaxNum
                END IF

             CASE 73
                IF IOffset > 0 THEN
                   IF IOffset - IWinSize > 0 THEN
                      IOffset = IOffset - IWinSize
                   ELSE IOffset = 0
                   END IF
                   IChoice = (IRow - ITop + 1) + IOffset
                   GOSUB VertDisplay
                END IF

             CASE 81
                IF IOffset + 2 * IWinSize < IMaxNum THEN
                   IOffset = IOffset + IWinSize
                ELSE IOffset = IMaxNum - IWinSize
                END IF
                IChoice = (IRow - ITop + 1) + IOffset
                GOSUB VertDisplay

             CASE 71
                IChoice = 1
                IOffset = 0
                GOSUB VertDisplay


             CASE 79
                IChoice = IMaxNum
                IOffset = IMaxNum - (IBottom - ITop + 1)
                GOSUB VertDisplay

             CASE 68                      'Function Keys  F1 to F10
                IF IAction THEN
                   IAction = 4
                ELSE
                   CALL ScrnRest0(ITop - 1, ILeft.Margin, IBottom + 2, IRight.Margin + 2, IScrBuf(0))
                   ERASE IScrBuf               'Clean up memory
                END IF
                LOCATE , , 1
                EXIT SUB

             CASE ELSE

          END SELECT
          GOSUB VertHiLite
       END IF

    LOOP WHILE IAction = 0                     'get again if not multi-tasking


    EXIT SUB


VertHiLite:
    IRow = ITop + (IChoice - IOffset) - 1
    IF ILastRow > 0 AND IRow <> ILastRow THEN
          CALL PaintBox0(ILastRow, ILCol, ILastRow, IRCol, Cnf.ActivCh)
    END IF

    IF IHighLighted THEN
             LOCATE IPrevRow, ILeft.Margin + IVAltChoice(IPrevChoice) + 1
             CALL QPrint0(VAltChoice$(IPrevChoice), Cnf.MenBox)
    END IF

    CALL PaintBox0(IRow, ILCol, IRow, IRCol, Cnf.HiLite)   'set inverse color
    LOCATE IRow, ILCol + IVAltChoice(IChoice)
    CALL QPrint0(VAltChoice$(IChoice), Cnf.AltHiLite)

    IPrevRow = IRow
    ILastRow = IRow
    IF IMore THEN
       IPrcnt# = IChoice / (IMaxNum - 1)
       IBarPtr = FIX((IWinSize - 3) * IPrcnt#) + ITop + 1
       IF IBarPtr <> IOPtr THEN
          IF IOPtr THEN
             LOCATE IOPtr, IRight.Margin  'Update the Scroll Bar position
             CALL QPrint0("²", IScrlClr)
          END IF
          LOCATE IBarPtr, IRight.Margin
          CALL QPrint0(CHR$(8), Cnf.MenBox)
       END IF
       IOPtr = IBarPtr
    END IF

'   Display the help message

    LOCATE 25, 1
    CALL QPrint0(VHelp$(IChoice), IHelpColor)
    LOCATE IRow, ILCol + 1
    IHighLighted = 1

RETURN

'----- Print the choices in the window
VertDisplay:
    IF IChoice <= IOffset THEN IOffset = IChoice - 1
    LOCATE ITop, ILeft.Margin + 2
    CALL APrint0(VARPTR(VChoice$(IOffset + 1)), IBottom - ITop + 1, 1, IMaxLen, -1)
       FOR I = 1 TO IWinSize
            J = IOffset + I
           LOCATE ITop + I - 1, ILeft.Margin + IVAltChoice(J) + 1
           CALL QPrint0(VAltChoice$(J), Cnf.MenBox)
       NEXT

RETURN

END SUB

      SUB VertMenuVars
' $DYNAMIC

      SHARED VChoice$()
      SHARED VHelp$()
      SHARED VAltChoice$()
      SHARED IVAltChoice()
      SHARED IMaxLen
      SHARED IBoxBot

      NChoice = 5                           'Dimension VertMenu variables
      REDIM VChoice$(NChoice)
      REDIM VAltChoice$(NChoice)
      REDIM IVAltChoice(NChoice)
      REDIM VHelp$(NChoice)

' Define vertmenu choices

      VChoice$(1) = "     Run Calculator                   "
      VChoice$(2) = "     Edit User Defined Functions      "
      VChoice$(3) = "     Instruction Manual               "
      VChoice$(4) = "     Pan Calendar                     "
      VChoice$(5) = "     Quit Program                     "

' Define help line text

      VHelp$(1) = "Select Hand Held Calculator Option             Run calculator                   "
      VHelp$(2) = "Select Edit User Defined Function Option       Change or View user defined keys "
      VHelp$(3) = "Select Instruction Manual Option               Display instruction manual       "
      VHelp$(4) = "Select Pan Calendar Option                     Display different year / month   "
      VHelp$(5) = "Select Quit Program Option                     End program                      "

' Define alternate hot key characters
' NOTE: Double key strokes [Alt] + key  not supported for handicap users

      VAltChoice$(1) = "C"
      VAltChoice$(2) = "E"
      VAltChoice$(3) = "I"
      VAltChoice$(4) = "P"
      VAltChoice$(5) = "Q"

' Define position in choice text for hot key character

      IVAltChoice(1) = 10
      IVAltChoice(2) = 6
      IVAltChoice(3) = 6
      IVAltChoice(4) = 6
      IVAltChoice(5) = 6

      IMaxLen = 36
      IBoxBot = 23

      
END SUB

