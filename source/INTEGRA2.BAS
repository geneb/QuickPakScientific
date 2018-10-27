' Copyright (c) 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB FCX (X, F)
DECLARE SUB FDX (X, F)
DECLARE SUB FXY (X, Y, F)

SUB INTEGRA2 (XA, XB, M, N, XJ) STATIC
   
    ' Double integration subroutine

    ' Composite Simpson Method

    ' Input

    '  XA = lower X endpoint
    '  XB = upper X endpoint
    '  M  = number of Y subdivisions
    '  N  = number of X subdivisions

    ' Output

    '  XJ = double integral

    ' NOTE: requires the following subroutines
    '
    '   FCX =>  y = fc(x)      FDX =>  y = fd(x)
    '   and FXY = f(x,y)
    '
    ' These subroutines are in the file USERSUB1.BAS

    H = (XB - XA) / (2# * N)

    XJ1 = 0#
    XJ2 = 0#
    XJ3 = 0#

    FOR I = 0 TO 2 * N
        X = XA + I * H
  
        CALL FDX(X, DX)
        CALL FCX(X, CX)
  
        HX = (DX - CX) / (2# * M)

        CALL FXY(X, CX, FXY1)
        CALL FXY(X, DX, FXY2)
  
        XK1 = FXY1 + FXY2
        XK2 = 0#
        XK3 = 0#
  
        FOR J = 1 TO 2 * M - 1
            Y = CX + J * HX
            CALL FXY(X, Y, FXY3)
            IF ((J / 2) = INT(J / 2)) THEN
               XK2 = XK2 + FXY3
            ELSE
               XK3 = XK3 + FXY3
            END IF
        NEXT J

        XL = (XK1 + 2# * XK2 + 4# * XK3) * HX / 3#

        IF (I = 0 OR I = 2 * N) THEN
           XJ1 = XJ1 + XL
        ELSEIF ((I / 2) = INT(I / 2)) THEN
           XJ2 = XJ2 + XL
        ELSE
           XJ3 = XJ3 + XL
        END IF
    NEXT I

    XJ = (XJ1 + 2# * XJ2 + 4# * XJ3) * H / 3#

END SUB

