' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB KEYCHECK ()

SUB PRTFFT1 (NN, X(), XSAVED(), ISIGN) STATIC

    ' Print FFT1 data subroutine

    ' Input

    '  NN       = number of data points
    '  X()      = vector of data points ( 2 * NN rows )
    '  XSAVED() = vector of saved data points ( 2 * NN rows )
    '  ISIGN    = type of Fast Fourier transform flag
    '         1 = foward Fast Fourier transform
    '        -1 = inverse Fast Fourier transform
    
    ' NOTE: requires subroutine KEYCHECK.BAS

    FORMAT$ = "##.######^^^^"
    A$ = "  J       f (real)          f (imag)         X (real)         X (imag)"
   
    CLS
    LOCATE 1, 31
    PRINT "Program DEMOFFT1"
    PRINT
    IF (ISIGN = 1) THEN
       DEN = 1#
       PRINT TAB(26); "< Fast Fourier Transform >"
    ELSE
       DEN = 1# / NN
       PRINT TAB(22); "< Inverse Fast Fourier Transform >"
    END IF
   
    PRINT

    IPASS = 1
    INUM = 0

    FOR I = 1 TO NN
        IF (IPASS = 1) THEN
           PRINT A$
           PRINT
           IPASS = 0
        END IF

        INUM = INUM + 1
        II = 2 * I - 1
        
        PRINT USING "####"; I - 1;
        PRINT "    ";

        IF (ISIGN = 1) THEN
           PRINT USING FORMAT$; XSAVED(II);
           PRINT "    ";
           PRINT USING FORMAT$; XSAVED(II + 1);
           PRINT "    ";
           PRINT USING FORMAT$; DEN * X(II);
           PRINT "    ";
           PRINT USING FORMAT$; DEN * X(II + 1);
           PRINT
        ELSE
           PRINT USING FORMAT$; DEN * X(II);
           PRINT "    ";
           PRINT USING FORMAT$; DEN * X(II + 1);
           PRINT "    ";
           PRINT USING FORMAT$; XSAVED(II);
           PRINT "    ";
           PRINT USING FORMAT$; XSAVED(II + 1);
           PRINT
        END IF

        IF (INUM = 16) THEN
           CALL KEYCHECK
           CLS
           LOCATE 1, 31
           PRINT "Program DEMOFFT1"
           PRINT
           IF (ISIGN = 1) THEN
              DEN = 1#
              PRINT TAB(26); "< Fast Fourier Transform >"
           ELSE
              DEN = 1# / NN
              PRINT TAB(22); "< Inverse Fast Fourier Transform >"
           END IF
           PRINT
           INUM = 0
           IPASS = 1
        END IF

    NEXT I

    IF (I < 16) THEN CALL KEYCHECK

END SUB

