' Program "DEMODATE"                   January 3, 1991
'
' Copyright (c) 1991 Crescent Software
'
' This program demonstrates the procedures for calling the
' QuickPak Scientific date subroutines.
'
' The available date calculations are as follows:
'
'   Calculation of the Julian date from the calendar date
'   Calculation of the calendar date from the Julian date
'   Calculation of the number of days between two dates
'   Calculation of the day of the week from a calendar date
'   Calculation of the day of the year from a calendar date
'   Calculation of the calendar date from the day of the year
'
'***********************************************************

' Variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' Subroutine declarations

DECLARE SUB CDATE (XJDATE, XMONTH, DAY, YEAR)
DECLARE SUB JULIAN (XMONTH, DAY, YEAR, XJD, VDATE$)
DECLARE SUB KEYCHECK ()

' dimension month and day string arrays

DIM MONTH$(12), DAY$(7)

MONTH$(1) = "January"
MONTH$(2) = "February"
MONTH$(3) = "March"
MONTH$(4) = "April"
MONTH$(5) = "May"
MONTH$(6) = "June"
MONTH$(7) = "July"
MONTH$(8) = "August"
MONTH$(9) = "September"
MONTH$(10) = "October"
MONTH$(11) = "November"
MONTH$(12) = "December"

DAY$(1) = "Sunday"
DAY$(2) = "Monday"
DAY$(3) = "Tuesday"
DAY$(4) = "Wednesday"
DAY$(5) = "Thursday"
DAY$(6) = "Friday"
DAY$(7) = "Saturday"

' **********************************************************

' Begin demo program

DO
   DO
      CLS
      LOCATE 5, 1
      PRINT TAB(31); "Program DEMODATE"
      LOCATE 8, 1
      PRINT TAB(12); "< 1 > Calculate the Julian date from a calendar date"
      LOCATE 10, 1
      PRINT TAB(12); "< 2 > Calculate the calendar date from a Julian date"
      LOCATE 12, 1
      PRINT TAB(12); "< 3 > Calculate the number of days between two dates"
      LOCATE 14, 1
      PRINT TAB(12); "< 4 > Calculate the day of the week from a calendar date"
      LOCATE 16, 1
      PRINT TAB(12); "< 5 > Calculate the day of the year from a calendar date"
      LOCATE 18, 1
      PRINT TAB(12); "< 6 > Calculate the calendar date from the day of the year"

      LOCATE 21, 1
      PRINT "Please enter your selection ( 1, 2, 3, 4, 5 or 6)"
      INPUT ISELECT
   LOOP UNTIL (ISELECT >= 1 AND ISELECT <= 6)

   SELECT CASE ISELECT
   CASE 1
      ' Calculate Julian date from calendar date
      DO
         ' request calendar date
         DO
           CLS
           LOCATE 5, 1
           PRINT "Please enter the calendar date"
           LOCATE 7, 1
           PRINT "( month [ 1 - 12 ], day [ 1 - 31 ], year [ YYYY ] )"
           PRINT "< For example, October 21, 1986 is input as 10,21,1986 >"
           PRINT "< B.C. dates are negative, A.D. dates are positive >"
           PRINT "< The day of the month may also include a decimal part >"
           LOCATE 12, 1
           INPUT XMONTH, DAY, YEAR
         LOOP UNTIL (XMONTH >= 1# AND XMONTH <= 12#) AND (DAY >= 1# AND DAY <= 31#)

         CALL JULIAN(XMONTH, DAY, YEAR, XJDAY, VDATE$)

         IF (VDATE$ = "N") THEN
            CLS
            LOCATE 5, 1
            PRINT TAB(26); "This date does not exist !!"
            CALL KEYCHECK
         ELSE
            EXIT DO
         END IF
      LOOP

      CLS
      LOCATE 5, 1
      PRINT TAB(20); "Calculate Julian date from calendar date"
      LOCATE 8, 1
      PRINT TAB(18); "Julian date"; TAB(40); XJDAY
      LOCATE 10, 1
      PRINT TAB(18); "Calendar date"; TAB(41); MONTH$(XMONTH);
      PRINT USING "###.###"; DAY;
      PRINT ", " + STR$(YEAR)
   CASE 2
      ' Calculate the calendar date from the Julian date
      DO
         CLS
         LOCATE 5, 1
         PRINT "Please enter the Julian date"
         INPUT XJDATE
         IF (XJDATE < 0#) THEN
            PRINT
            PRINT "Positive Julian Dates only please !!"
            CALL KEYCHECK
         ELSE
            EXIT DO
         END IF
      LOOP

      CALL CDATE(XJDATE, XMONTH, DAY, YEAR)

      CLS
      LOCATE 5, 1
      PRINT TAB(20); "Calculate calendar date from Julian date"
      LOCATE 8, 1
      PRINT TAB(18); "Julian date"; TAB(40); XJDATE
      LOCATE 10, 1
      PRINT TAB(18); "Calendar date"; TAB(41); MONTH$(XMONTH);
      PRINT USING "###.###"; DAY;
      PRINT ", " + STR$(YEAR)
   CASE 3
      ' Calculate the number of days between two dates
      DO
         CLS
         LOCATE 5, 1
         PRINT "Please enter the first calendar date"
         LOCATE 7, 1
         PRINT "( month [ 1 - 12 ], day [ 1 - 31 ], year [ YYYY ] )"
         PRINT "< For example, October 21, 1986 is input as 10,21,1986 >"
         PRINT "< B.C. dates are negative, A.D. dates are positive >"
         PRINT "< The day of the month may also include a decimal part >"
         LOCATE 12, 1
         INPUT XMONTH1, DAY1, YEAR1

         CALL JULIAN(XMONTH1, DAY1, YEAR1, XJDAY1, VDATE$)

         IF (VDATE$ = "N") THEN
            CLS
            LOCATE 5, 1
            PRINT TAB(26); "This date does not exist !!"
            CALL KEYCHECK
         ELSE
            EXIT DO
         END IF
      LOOP

      DO
         CLS
         LOCATE 5, 1
         PRINT "Please enter the second calendar date"
         LOCATE 7, 1
         PRINT "( month [ 1 - 12 ], day [ 1 - 31 ], year [ YYYY ] )"
         PRINT "< For example, October 21, 1986 is input as 10,21,1986 >"
         PRINT "< B.C. dates are negative, A.D. dates are positive >"
         PRINT "< The day of the month may also include a decimal part >"
         LOCATE 12, 1
         INPUT XMONTH2, DAY2, YEAR2

         CALL JULIAN(XMONTH2, DAY2, YEAR2, XJDAY2, VDATE$)

         IF (VDATE$ = "N") THEN
            CLS
            LOCATE 5, 1
            PRINT TAB(26); "This date does not exist !!"
            CALL KEYCHECK
         ELSE
            EXIT DO
         END IF
      LOOP

      CLS
      LOCATE 5, 1
      PRINT TAB(24); "Number of days between two dates"
      LOCATE 8, 1
      PRINT TAB(18); "Julian date #1"; TAB(40); XJDAY1
      LOCATE 10, 1
      PRINT TAB(18); "Calendar date #1"; TAB(41); MONTH$(XMONTH1);
      PRINT USING "###.###"; DAY1;
      PRINT ", " + STR$(YEAR1)
      LOCATE 13, 1
      PRINT TAB(18); "Julian date #2"; TAB(40); XJDAY2
      LOCATE 15, 1
      PRINT TAB(18); "Calendar date #2"; TAB(41); MONTH$(XMONTH2);
      PRINT USING "###.###"; DAY2;
      PRINT ", " + STR$(YEAR2)
      LOCATE 18, 1
      PRINT TAB(18); "Number of days";
      PRINT TAB(40);
      PRINT USING "########.###"; ABS(XJDAY2 - XJDAY1)
   CASE 4
      ' Calculate the day of the week
      DO
         ' request calendar date
         DO
           CLS
           LOCATE 5, 1
           PRINT "Please enter the calendar date"
           LOCATE 7, 1
           PRINT "( month [ 1 - 12 ], day [ 1 - 31 ], year [ YYYY ] )"
           PRINT "< For example, October 21, 1986 is input as 10,21,1986 >"
           PRINT "< B.C. dates are negative, A.D. dates are positive >"
           PRINT "< The day of the month may also include a decimal part >"
           LOCATE 12, 1
           INPUT XMONTH, DAY, YEAR
         LOOP UNTIL (XMONTH >= 1# AND XMONTH <= 12#) AND (DAY >= 1# AND DAY <= 31#)

         CALL JULIAN(XMONTH, FIX(DAY), YEAR, XJD0, VDATE$)
 
         IF (VDATE$ = "N") THEN
            CLS
            LOCATE 5, 1
            PRINT TAB(26); "This date does not exist !!"
            CALL KEYCHECK
         ELSE
            EXIT DO
         END IF
      LOOP

      XJD = XJD0 + 1.5#

      A = FIX(7 * (XJD / 7# - FIX(XJD / 7#)) + .5#)

      D$ = MONTH$(XMONTH) + STR$(DAY) + "," + STR$(YEAR)

      CLS
      LOCATE 5, 1
      PRINT TAB(25); "Calculate the day of the week"
      LOCATE 8, 1
      PRINT TAB(18); "Julian date"; TAB(40); XJD0
      LOCATE 10, 1
      PRINT TAB(18); "Calendar date"; TAB(41); D$
      LOCATE 12, 1
      PRINT TAB(18); "Day of the week"; TAB(41); DAY$(A + 1)
   CASE 5
      ' Calculate the day of the year subroutine
      DO
        CLS
        LOCATE 5, 1
        PRINT "Please enter the calendar date"
        LOCATE 7, 1
        PRINT "( month [ 1 - 12 ], day [ 1 - 31 ], year [ YYYY ] )"
        PRINT "< For example, October 21, 1986 is input as 10,21,1986 >"
        PRINT "< B.C. dates are negative, A.D. dates are positive >"
        PRINT "< The day of the month may also include a decimal part >"
        LOCATE 12, 1
        INPUT XMONTH, DAY, YEAR
      LOOP UNTIL (XMONTH >= 1# AND XMONTH <= 12#) AND (DAY >= 1# AND DAY <= 31#)

      IF (YEAR / 4# = FIX(YEAR / 4#)) THEN
         N = FIX(275# * XMONTH / 9#) - FIX((XMONTH + 9#) / 12#) + FIX(DAY) - 30#
      ELSE
         N = FIX(275# * XMONTH / 9#) - 2# * FIX((XMONTH + 9#) / 12#) + FIX(DAY) - 30#
      END IF

      D$ = MONTH$(XMONTH) + STR$(DAY) + "," + STR$(YEAR)

      CLS
      LOCATE 5, 1
      PRINT TAB(25); "Calculate the day of the year"
      LOCATE 8, 1
      PRINT TAB(18); "Calendar date"; TAB(41); D$
      LOCATE 10, 1
      PRINT TAB(18); "Day of the year"; TAB(40); N
   CASE 6
      ' Calculate the date from the day number
      CLS
      LOCATE 5, 1
  
      DO
         PRINT "Please enter the day number ( 1 - 366 )"
         INPUT N
      LOOP UNTIL (N >= 1 AND N <= 366)
  
      LOCATE 8, 1
      PRINT "Please enter the calendar year"
      PRINT "< NOTE: include all digits >"
      INPUT YEAR

      IF (YEAR / 4# = FIX(YEAR / 4#)) THEN
         A = 1523#
      ELSE
         A = 1889#
      END IF

      B = FIX((N + A - 122.1#) / 365.25#)
      C = N + A - FIX(365.25# * B)
      E = FIX(C / 30.6001#)

      IF (E < 13.5#) THEN
         XMONTH = E - 1#
      ELSE
         XMONTH = E - 13#
      END IF

      DAY = C - FIX(30.6001# * E)

      D$ = MONTH$(XMONTH) + STR$(DAY) + "," + STR$(YEAR)

      CLS
      LOCATE 5, 1
      PRINT TAB(21); "Calculate the date from the day number"
      LOCATE 8, 1
      PRINT TAB(18); "Calendar date"; TAB(41); D$
      LOCATE 10, 1
      PRINT TAB(18); "Day of the year"; TAB(43); N
   END SELECT

   CALL KEYCHECK

   ' Request another selection

   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

LOOP UNTIL (SLCT$ = "N")

END


