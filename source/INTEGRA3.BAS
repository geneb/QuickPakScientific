' Copyright (c) 1989 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB FCX (X, F)
DECLARE SUB FDX (X, F)
DECLARE SUB FAXY (X, Y, F)
DECLARE SUB FBXY (X, Y, F)
DECLARE SUB FXYZ (X, Y, Z, F)

SUB INTEGRA3 (XA, XB, L, M, N, XJ) STATIC

    ' Triple integration subroutine

    ' Composite Simpson Method
   
    ' Input

    '  XA = lower X endpoint
    '  XB = upper X endpoint
    '  L  = number of Z subdivisions
    '  M  = number of Y subdivisions
    '  N  = number of X subdivisions

    ' Output
   
    '  XJ = triple integral

    ' NOTE: requires the following subroutines
    '
    '   FCX =>  y = fc(x)      FDX =>  y = fd(x)
    '   FAXY => z = fa(x,y)    FBXY => z = fb(x,y)
    '   and FXYZ = f(x,y,z)
    '
    ' These subroutines are in the file USERSUB2.BAS

    H = (XB - XA) / (2# * N)

    XJ1 = 0#
    XJ2 = 0#
    XJ3 = 0#

    FOR I = 0 TO 2 * N
        X = XA + I * H
  
        CALL FDX(X, DOFX)
        CALL FCX(X, COFX)
  
        HX = (DOFX - COFX) / (2# * M)
  
        XK1 = 0#
        XK2 = 0#
        XK3 = 0#
  
        FOR J = 1 TO 2 * M
            Y = COFX + J * HX
      
            CALL FBXY(X, Y, FXY1)
            CALL FAXY(X, Y, FXY2)
      
            HY = (FXY1 - FXY2) / (2# * L)
      
            CALL FXYZ(X, Y, FXY2, FXYZ1)
            CALL FXYZ(X, Y, FXY1, FXYZ2)
      
            XL1 = FXYZ1 + FXYZ2
            XL2 = 0#
            XL3 = 0#

            FOR K = 1 TO 2 * L - 1
                Z = FXY2 + K * HY
                CALL FXYZ(X, Y, Z, FQ)
                IF ((K / 2) = INT(K / 2)) THEN
                   XL2 = XL2 + FQ
                ELSE
                   XL3 = XL3 + FQ
                END IF
            NEXT K

            XL = (XL1 + 2# * XL2 + 4# * XL3) * HY / 3#

            IF (J = 0 OR J = 2 * M) THEN
               XK1 = XK1 + XL
            ELSEIF ((J / 2) = INT(J / 2)) THEN
               XK2 = XK2 + XL
            ELSE
               XK3 = XK3 + XL
            END IF
        NEXT J

        XK = (XK1 + 2# * XK2 + 4# * XK3) * HX / 3#

        IF (I = 0 OR I = 2 * N) THEN
           XJ1 = XJ1 + XK
        ELSEIF ((I / 2) = INT(I / 2)) THEN
           XJ2 = XJ2 + XK
        ELSE
           XJ3 = XJ3 + XK
        END IF
    NEXT I

    XJ = (XJ1 + 2# * XJ2 + 4# * XJ3) * H / 3#

END SUB

