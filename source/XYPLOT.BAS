' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB SCALE (XMIN, XMAX, N, XMINP, XMAXP)
DECLARE SUB KEYCHECK ()
DECLARE FUNCTION ROUND (X, N)

SUB XYPLOT (MODE, NPTS, X(), Y(), XAXIS$, YAXIS$, TITLE$) STATIC

    ' Plot X-Y graphics subroutine

    ' Input

    '  MODE   = graphics mode
    '  ( 1 = CGA, 2 = EGA, 3 = VGA, 4 = Hercules )
    '  NPTS   = number of data points to plot
    '  X()    = vector of X data points ( NPTS rows )
    '  Y()    = vector of Y data points ( NPTS rows )
    '  XAXIS$ = X-axis title
    '  YAXIS$ = Y-axis title
    '  TITLE$ = plot title

    ' NOTE: requires subroutines SCALE.BAS, KEYCHECK.BAS and function ROUND.BAS

    F1$ = "##########"

    ' find minimum and maximum data values

    XMIN = YMIN = 1D+99
    XMAX = YMAX = -1D+99

    FOR I = 1 TO NPTS
        IF (Y(I) > YMAX) THEN YMAX = Y(I)
        IF (Y(I) < YMIN) THEN YMIN = Y(I)
        IF (X(I) > XMAX) THEN XMAX = X(I)
        IF (X(I) < XMIN) THEN XMIN = X(I)
    NEXT I
   
    ' check for sufficient plot resolution

    IF (ABS(XMAX - XMIN) < .0000000001#) THEN
       CLS
       LOCATE 5, 1
       PRINT TAB(18); "Insufficient resolution in the X direction !"
       CALL KEYCHECK
       EXIT SUB
    END IF
  
    IF (ABS(YMAX - YMIN) < .0000000001#) THEN
       CLS
       LOCATE 5, 1
       PRINT TAB(18); "Insufficient resolution in the Y direction !"
       CALL KEYCHECK
       EXIT SUB
    END IF
  
    ' calculate x and y axis ranges

    CALL SCALE(XMIN, XMAX, 20, XMINP, XMAXP)
   
    CALL SCALE(YMIN, YMAX, 10, YMINP, YMAXP)
 
    XMINP = ROUND(XMINP, -10)
    XMAXP = ROUND(XMAXP, -10)
    YMINP = ROUND(YMINP, -10)
    YMAXP = ROUND(YMAXP, -10)

    ' define x format string

    XMID = ROUND(.5# * (XMINP + XMAXP), -10)
   
    LMID = LEN(STR$(ROUND(XMID - FIX(XMID), -10)))
    LMIN = LEN(STR$(ROUND(XMINP - FIX(XMINP), -10)))
    LMAX = LEN(STR$(ROUND(XMAXP - FIX(XMAXP), -10)))

    IF (LMID > LMIN AND LMID > LMAX) THEN
       LXMIN1 = LEN(STR$(FIX(XMID)))
       LXMIN2 = LMID
    ELSEIF (LMIN > LMAX) THEN
       LXMIN1 = LEN(STR$(FIX(XMINP)))
       LXMIN2 = LMIN
    ELSE
       LXMIN1 = LEN(STR$(FIX(XMAXP)))
       LXMIN2 = LMAX
    END IF

    IF (LXMIN2 > 2) THEN
       FX$ = LEFT$(F1$, LXMIN1 + 1) + "." + LEFT$(F1$, LXMIN2 - 2)
    ELSE
       FX$ = LEFT$(F1$, LXMIN1 + 1)
    END IF

    ' define y format string

    YMID = ROUND(.5# * (YMINP + YMAXP), -10)

    LMID = LEN(STR$(ROUND(YMID - FIX(YMID), -10)))
    LMIN = LEN(STR$(ROUND(YMINP - FIX(YMINP), -10)))
    LMAX = LEN(STR$(ROUND(YMAXP - FIX(YMAXP), -10)))

    IF (LMID > LMIN AND LMID > LMAX) THEN
       LYMIN1 = LEN(STR$(FIX(YMID)))
       LYMIN2 = LMID
    ELSEIF (LMIN > LMAX) THEN
       LYMIN1 = LEN(STR$(FIX(YMINP)))
       LYMIN2 = LMIN
    ELSE
       LYMIN1 = LEN(STR$(FIX(YMAXP)))
       LYMIN2 = LMAX
    END IF

    IF (LYMIN2 > 2) THEN
       FY$ = LEFT$(F1$, LYMIN1 + 1) + "." + LEFT$(F1$, LYMIN2 - 2)
    ELSE
       FY$ = LEFT$(F1$, LYMIN1 + 1)
    END IF
    
    ' turn on graphics mode

    YOFFSET = 0

    SELECT CASE MODE
    CASE 1
         ' CGA
         SCREEN 2
    CASE 2
         ' EGA
         SCREEN 9
    CASE 3
         ' VGA
         YOFFSET = 5
         SCREEN 12
    CASE 4
         ' Hercules
         SCREEN 3
    END SELECT

    CLS

    ' define graphics window

    WINDOW (-.5, -6.2)-(10.5, 6.75)

    ' print graph title

    LOCATE 1, 1
    PRINT TAB(41 - .5 * LEN(TITLE$)); TITLE$;
   
    ' print x-axis title

    LOCATE FIX(25 + YOFFSET), 41 - .5 * LEN(XAXIS$)
    PRINT XAXIS$;

    ' print y-axis title

    B = FIX(13 + .5 * YOFFSET - INT(.5 * LEN(YAXIS$)))
  
    FOR I = 1 TO LEN(YAXIS$)
        LOCATE B + I, 1
        PRINT MID$(YAXIS$, I, 1);
    NEXT I

    ' label x axis
   
    LOCATE FIX(24 + YOFFSET), 2
    PRINT USING FX$; XMINP;
   
    LOCATE FIX(24 + YOFFSET), 41 - .5# * LEN(FX$)
    PRINT USING FX$; XMID;
   
    LOCATE FIX(24 + YOFFSET), 80 - LEN(FX$)
    PRINT USING FX$; XMAXP;

    ' label y axis

    LOCATE FIX(4 + .2 * YOFFSET), 6
    PRINT USING FY$; YMAXP;

    LOCATE FIX(13 + .5 * YOFFSET), 6
    PRINT USING FY$; YMID;

    LOCATE FIX(22 + YOFFSET), 6
    PRINT USING FY$; YMINP;
   
    ' draw x and y axes

    LINE (0, -5)-(10, -5)
    LINE (0, -5)-(0, 5)

    ' draw y-axis tic marks

    FOR I = -5 TO 5
        IF (I = 0 OR I = 5) THEN
           LINE (0, I)-(.1, I)
        ELSE
           LINE (0, I)-(.05, I)
        END IF
    NEXT I

    ' draw x-axis tic marks

    FOR XI = .5 TO 10 STEP .5
        IF (XI = 5 OR XI = 10) THEN
           LINE (XI, -5)-(XI, -4.8)
        ELSEIF (XI = 2.5 OR XI = 7.5) THEN
           LINE (XI, -5)-(XI, -4.85)
        ELSE
           LINE (XI, -5)-(XI, -4.9)
        END IF
    NEXT XI

    ' calculate scale factors

    SFX = 10# / (XMAXP - XMINP)
    SFY = 10# / (YMAXP - YMINP)

    ' if necessary, draw y = 0 line

    IF ((SGN(YMAXP) <> SGN(YMINP)) AND YMINP <> 0#) THEN
       FOR XI = .5 TO 10 STEP .5
           LINE (XI - .05, -SFY * YMID)-(XI + .05, -SFY * YMID)
       NEXT XI
    END IF

    ' plot data

    FOR I = 1 TO NPTS
        XP = SFX * (X(I) - XMINP)
        YP = SFY * (Y(I) - YMID)
        PSET (XP, YP)
    NEXT I

    ' wait for keypress

    A$ = ""
    WHILE A$ = ""
      A$ = INKEY$
    WEND

    ' reset to text screen and exit

    SCREEN 0
    WIDTH 80

END SUB

