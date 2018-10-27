' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB KEYCHECK ()

SUB PRTFFT2 (NN(), X(), XSAVED(), ISIGN) STATIC

    ' Print FFT2 data subroutine

    ' NOTE: requires subroutine KEYCHECK.BAS
  
    ' Input

    '  NN(1) = number of X data points
    '  NN(2) = number of Y data points
    '  X()   = vector of data points ( 2 * NN(1) * NN(2) rows )
    '  ISIGN = type of Fast Fourier transform flag
    '      1 = foward Fast Fourier transform
    '     -1 = inverse Fast Fourier transform

    FORMAT$ = "##.######^^^^"

    IF (ISIGN = 1) THEN
       DEN = 1#
    ELSE
       DEN = 1# / (NN(1) * NN(2))
    END IF
  
    ' print function values

    INUM = 0
   
    FOR J = 1 TO NN(2)
        CLS
        LOCATE 3, 1
        PRINT TAB(31); "Program DEMOFFT2"
        PRINT
        IF (ISIGN = 1) THEN
           PRINT TAB(26); "< Fast Fourier Transform >"
        ELSE
           PRINT TAB(22); "< Inverse Fast Fourier Transform >"
        END IF
        PRINT
        PRINT TAB(30); "( Function Values )"
        PRINT
        PRINT
        PRINT TAB(15); "X ="; J;
        PRINT TAB(31); "f (real)           f (imag)"
        PRINT
        FOR K = 1 TO NN(1)
            INUM = INUM + 1
            L = K + (J - 1) * NN(1)
            LL = 2 * L - 1
            PRINT TAB(15); "Y =";
            PRINT USING "####"; K;
            PRINT "      ";
            IF (ISIGN = 1) THEN
               PRINT USING FORMAT$; XSAVED(LL);
               PRINT "      ";
               PRINT USING FORMAT$; XSAVED(LL + 1);
            ELSE
               PRINT USING FORMAT$; DEN * X(LL);
               PRINT "      ";
               PRINT USING FORMAT$; DEN * X(LL + 1);
            END IF
            IF (INUM = 8 AND K < NN(1)) THEN
               CALL KEYCHECK
               CLS
               LOCATE 3, 1
               PRINT TAB(31); "Program DEMOFFT2"
               PRINT
               IF (ISIGN = 1) THEN
                  PRINT TAB(26); "< Fast Fourier Transform >"
               ELSE
                  PRINT TAB(22); "< Inverse Fast Fourier Transform >"
               END IF
               PRINT
               PRINT TAB(30); "( Function Values )"
               PRINT
               PRINT
               PRINT TAB(15); "X ="; J;
               PRINT TAB(31); "f (real)           f (imag)"
               PRINT
               INUM = 0
            END IF
        NEXT K
        IF (J < 8 OR K < 8) THEN CALL KEYCHECK
    NEXT J

    ' print transform values

    INUM = 0

    FOR J = 1 TO NN(2)
        CLS
        LOCATE 3, 1
        PRINT TAB(31); "Program DEMOFFT2"
        PRINT
        IF (ISIGN = 1) THEN
           PRINT TAB(26); "< Fast Fourier Transform >"
        ELSE
           PRINT TAB(22); "< Inverse Fast Fourier Transform >"
        END IF
        PRINT
        PRINT TAB(29); "( Transform Values )"
        PRINT
        PRINT TAB(15); "Row ="; J
        PRINT
        PRINT TAB(15); "Column"
        PRINT
        FOR K = 1 TO NN(1)
            INUM = INUM + 1
            L = K + (J - 1) * NN(1)
            LL = 2 * L - 1
            IF (ISIGN = 1) THEN
               XTMP = X(LL + 1)
               XABS = ABS(XTMP)
            ELSE
               XTMP = XSAVED(LL + 1)
               XABS = ABS(XTMP)
            END IF
            PRINT TAB(15);
            PRINT USING "####"; K;
            PRINT "      ";
            IF (ISIGN = 1) THEN
               PRINT USING FORMAT$; X(LL);
            ELSE
               PRINT USING FORMAT$; XSAVED(LL);
            END IF
            IF (XTMP < 0#) THEN
               PRINT " -";
            ELSE
               PRINT " +";
            END IF
            PRINT USING FORMAT$; XABS;
            PRINT "i"
            IF (INUM = 8 AND K < NN(1)) THEN
               CALL KEYCHECK
               CLS
               LOCATE 3, 1
               PRINT TAB(31); "Program DEMOFFT2"
               PRINT
               IF (ISIGN = 1) THEN
                  PRINT TAB(26); "< Fast Fourier Transform >"
               ELSE
                  PRINT TAB(22); "< Inverse Fast Fourier Transform >"
               END IF
               PRINT
               PRINT TAB(29); "( Transform Values )"
               PRINT
               PRINT TAB(15); "Row ="; J
               PRINT
               PRINT TAB(15); "Column"
               PRINT
               INUM = 0
            END IF
        NEXT K
        IF (J < 8 OR K < 8) THEN CALL KEYCHECK
    NEXT J
   
END SUB

