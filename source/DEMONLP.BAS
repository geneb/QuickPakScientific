' Program "DEMONLP"                    December 26, 1992
'
' Copyright (c) 1991, 1992 Crescent Software
'
' This program demonstrates the procedure for calling
' the subroutines required to solve constrained
' non-linear optimization problems.
'
' This program attempts to solve the minimum cost
' cylindrical tank design problem as follows:
'
'  Minimize
'
'    f(radius,length) = radius^2 + radius * length
'
'  Subject to the equality constraint given by
'
'    volume = pi * radius^2 * height
'
'  and the inequality constraints defined by
'
'    minimum radius <= radius <= maximum radius
'
'    minimum length <= length <= maximum length
'
' ****************************************************

' Variable type definitions

DEFINT I-N
DEFDBL A-H, O-Z

' Function and subroutine declarations

DECLARE FUNCTION DLOG10 (X)
DECLARE FUNCTION DDIV (A, B)

DECLARE SUB FDINT (H, FX, F1, F2, IFRST, ITER, IDONE, HOPT)
DECLARE SUB FINDFD (N, X(), F(), G(), H(), FFD())
DECLARE SUB EXCHANGE (N, W(), F, X(), FX(), GG(), HH(), IER)
DECLARE SUB NLPINZ (X(), F(), G(), H(), IER)
DECLARE SUB NLP (X(), FX(), GG(), HH(), IERR)
DECLARE SUB PENALTY (X(), FX(), GG(), HH(), IERR)
DECLARE SUB PENSET (X(), F(), G(), H(), NPI, NX, NC, IERR)
DECLARE SUB QPENALTY (GG(), HH(), NGG, NHH, HKI, GKI)
DECLARE SUB MDRIVER (X(), F(), G(), H(), IERR)
DECLARE SUB LINEAR (NA, GTG(), RHS(), DET, IERR)
DECLARE SUB USRFUN (X(), F(), G(), H())
DECLARE SUB MINIMIZE (N, X(), FX(), G(), H(), W(), XM(), GG(), HQ(), IER)
DECLARE SUB KEYCHECK ()

' Dimension global arrays

DIM SHARED N(50), METHOD(35), TOL(35)

' algorithm control global variables

COMMON SHARED IPRT, NPI, IFN

FORMAT$ = "##.########^^^^"

' ****************************************************

DO
   CLS
   PRINT TAB(32); "Program DEMONLP"
   PRINT
   PRINT TAB(12); "This program demonstrates the procedure for calling"
   PRINT TAB(12); "the subroutines required to solve constrained"
   PRINT TAB(12); "non-linear optimization problems."
   PRINT
   PRINT TAB(12); "This program attempts to solve the minimum cost"
   PRINT TAB(12); "cylindrical tank design problem as follows:"
   PRINT
   PRINT TAB(12); " Minimize"
   PRINT
   PRINT TAB(12); "   f(radius,length) = radius^2 + radius * length"
   PRINT
   PRINT TAB(12); " Subject to the equality constraint given by"
   PRINT
   PRINT TAB(12); "   volume = pi * radius^2 * height"
   PRINT
   PRINT TAB(12); " and the inequality constraints defined by"
   PRINT
   PRINT TAB(12); "   minimum radius <= radius <= maximum radius"
   PRINT
   PRINT TAB(12); "   minimum length <= length <= maximum length"

   CALL KEYCHECK

   DO
      CLS
      LOCATE 5, 1
      PRINT "Print intermediate results ( y = yes, n = no )"
      INPUT IPRT$
      IPRT$ = UCASE$(IPRT$)
   LOOP UNTIL (IPRT$ = "Y" OR IPRT$ = "N")

   IF (IPRT$ = "Y") THEN
      IPRT = 1
   ELSE
      IPRT = 0
   END IF
  
   ' problem definition

   ' number of control variables

   NX = 2

   ' number of inequality constraints

   NG = 4

   ' number of equality constraints

   NH = 1

   NF = 4

   ' determine maximum array sizes

   MAXF = 12

   ' maximum dimension of G array

   MAXG = 6 * NG

   ' maximum dimension of H array

   MAXH = 5 * NH

   ' NOTE: NC = max(NG + NH, NX)
         
   NC = NG + NH
   IF (NX > NC) THEN NC = NX

   MAXX = NX * (7 + NC) + NC ^ 2 + NX * (NX + 1) / 2

   ' dimension arrays

   DIM X(MAXX), F(MAXF), G(MAXG), H(MAXH)
   DIM SHARED FFD(NX)
   
   ' initialize
  
   N(1) = MAXX
   N(2) = 12
   N(3) = MAXG
   N(4) = MAXH
   N(6) = NX
   N(7) = NF
   N(8) = NG
   N(9) = NH

   CALL NLPINZ(X(), F(), G(), H(), IER)
 
   IF (IER > 0) THEN
      CLS
      LOCATE 5, 1
      SELECT CASE IER
      CASE 2
         PRINT TAB(16); "Number of independent variables is less than 1 !"
      CASE 4
         PRINT TAB(16); "Number of inequality constraints is less than 0 !"
      CASE 5
         PRINT TAB(17); "Number of equality constraints is less than 0 !"
      CASE 7
         PRINT TAB(14); "X-array storage required is greater than available !"
      CASE 8
         PRINT TAB(14); "F-array storage required is greater than available !"
      CASE 9
         PRINT TAB(14); "G-array storage required is greater than available !"
      CASE 10
         PRINT TAB(14); "H-array storage required is greater than available !"
      END SELECT
      GOTO CONTINUE
   END IF

   DO
      CLS
      LOCATE 5, 1
      PRINT TAB(12); "Type of Derivative Computation"
      PRINT
      PRINT
      PRINT TAB(10); "< 1 > One-sided finite difference"
      PRINT
      PRINT TAB(10); "< 2 > Symmetric finite difference"
      PRINT
      PRINT
      PRINT "Selection ( 1 or 2 )"
      INPUT METHOD(4)
   LOOP UNTIL (METHOD(4) = 1 OR METHOD(4) = 2)
 
   DO
      PRINT
      PRINT
      PRINT "Please input the maximum number of nonlinear iterations"
      PRINT "( a value of 20 is recommended )"
      INPUT METHOD(2)
   LOOP UNTIL (METHOD(2) > 0)
  
   CLS
   LOCATE 5, 1
  
   FOR I = 1 TO NX
       PRINT
       PRINT "Please input the initial guess for variable #"; I
       INPUT X(I)
   NEXT I
  
   ' solve NLP problem

   CALL NLP(X(), F(), G(), H(), IERR)

   ' print results

   CLS
   LOCATE 5, 1
   PRINT TAB(31); "Control variables"
   PRINT

   FOR I = 1 TO NX
       PRINT
       PRINT TAB(28); "X("; I; ") = ";
       PRINT USING FORMAT$; X(I)
   NEXT I

   ' evaluate function value at solution

   CALL USRFUN(X(), F(), G(), H())

   PRINT
   PRINT
   PRINT TAB(24); "Function value = ";
   PRINT USING FORMAT$; F(1)

   CALL KEYCHECK

   CLS
   PRINT
   PRINT TAB(29); "Inequality constraints"
   FOR I = 1 TO NG
       PRINT
       PRINT TAB(28); "G("; I; ") =";
       PRINT USING FORMAT$; G(I)
   NEXT I
   PRINT
   PRINT
   PRINT TAB(30); "Equality constraints"
   FOR I = 1 TO NH
       PRINT
       PRINT TAB(28); "H("; I; ") = ";
       PRINT USING FORMAT$; H(I)
   NEXT I

   PRINT
   PRINT
   PRINT TAB(20); "Number of constrained iterations   = "; NPI
   PRINT
   PRINT TAB(20); "Number of unconstrained iterations = "; IFN
   PRINT

CONTINUE:

   CALL KEYCHECK
  
   DO
      CLS
      LOCATE 5, 1
      PRINT "Another selection ( y = yes, n = no )"
      INPUT SLCT$
      SLCT$ = UCASE$(SLCT$)
   LOOP UNTIL (SLCT$ = "Y" OR SLCT$ = "N")

   ERASE X, F, G, H, FFD

LOOP UNTIL (SLCT$ = "N")

END

FUNCTION DDIV (A, B) STATIC

   ' Divided difference subroutine

   FLMIN = 2.23D-308
   FLMAX = 1.79D+308

   IF (A = 0#) THEN
      DDIV = 0#
      EXIT FUNCTION
   END IF

   IF (B = 0#) THEN
      DDIV = SGN(A) * FLMAX
   ELSE
      ABSB = ABS(B)
      IF (ABSB >= 1#) THEN
         IF (ABS(A) >= (ABSB * FLMIN)) THEN
            DDIV = A / B
         ELSE
            DDIV = 0#
         END IF
      ELSE
         IF (ABS(A) <= (ABSB * FLMAX)) THEN
            DDIV = A / B
         ELSE
            DDIV = FLMAX
            IF (A < 0# AND B > 0#) OR (A > 0# AND B < 0#) THEN DDIV = -FLMAX
         END IF
      END IF
   END IF

END FUNCTION

FUNCTION DLOG10 (X) STATIC

    ' Common log function

    IF (X = 0#) THEN
       DLOG10 = -99#
    ELSE
       DLOG10 = .4342944819032518# * LOG(X)
    END IF

END FUNCTION

SUB EXCHANGE (N, W(), F, X(), FX(), GG(), HH(), IER) STATIC

    ' Array exchange subroutine

    FOR I = 1 TO N
        Z = X(I)
        X(I) = W(I)
        W(I) = Z
    NEXT I

    CALL PENALTY(X(), FX(), GG(), HH(), IER)

    F = FX(1)

    FOR I = 1 TO N
        Z = X(I)
        X(I) = W(I)
        W(I) = Z
    NEXT I

END SUB

SUB FDINT (H, FX, F1, F2, IFRST, ITER, IDONE, HOPT) STATIC

    ' Finite difference interval subroutine

    IF (IFRST = 1) THEN
       CDEST = 0#
       SDEST = 0#
    END IF

    BNDLO = .001#
    BNDUP = .1#

    EPSM = 2.22D-16
    EPSR = EPSM ^ .9#
    EPSA = EPSR * (1# + ABS(FX))

    ITER = ITER + 1

    FDEST = DDIV(F1 - FX, H)
    FDEST2 = DDIV(F2 - FX, 2# * H)

    OLDCD = CDEST
    CDEST = DDIV(4# * F1 - 3# * FX - F2, 2# * H)

    OLDSD = SDEST
    SDEST = DDIV(FX - 2# * F1 + F2, H * H)

    AFDMIN = ABS(FDEST)
    IF (AFDMIN > ABS(FDEST2)) THEN AFDMIN = ABS(FDEST2)

    FDCERR = DDIV(EPSA, .5# * ABS(H) * AFDMIN)
    SDCERR = DDIV(EPSA, .25# * ABS(SDEST) * H * H)

    IF (IFRST = 1) THEN
       IFRST = 0

       IF (SDCERR >= BNDLO AND SDCERR <= BNDUP) THEN IDONE = 1

       IF (SDCERR < BNDLO) THEN
          ITE2BIG = 1
       ELSE
          ITE2BIG = 0
       END IF

       IF (SDCERR > BNDUP) THEN
          ICE2BIG = 1
       ELSE
          ICE2BIG = 0
       END IF
      
       IF (FDCERR > BNDUP) THEN
          ICE1BIG = 1
       ELSE
          ICE1BIG = 0
       END IF

       IF (ICE1BIG = 0) THEN
          HSAVE = H
          FDSAVE = FDEST
          CDSAVE = CDEST
          SDSAVE = SDEST
       END IF

       RHO = EPSR ^ (-.16#) / 4#
      
       IF (ITE2BIG = 1) THEN
          RHO = 10# * RHO
          OLDH = H
          H = H / RHO
       ELSEIF (ICE2BIG = 1) THEN
          OLDH = H
          H = H * RHO
       END IF
    ELSEIF (ICE2BIG = 1) THEN
       IF ((ICE1BIG = 1) AND (FDCERR <= BNDUP)) THEN
          ICE1BIG = 0
          HSAVE = H
          FDSAVE = FDEST
          CDSAVE = CDEST
          SDSAVE = SDEST
       END IF

       IF (SDCERR <= BNDUP) THEN IDONE = 1
       
       IF (IDONE = 0) THEN
            OLDH = H
            H = H * RHO
       END IF
    ELSEIF (ITE2BIG = 1) THEN
       IF (SDCERR > BNDUP) THEN IDONE = 1
       IF (IDONE = 1) THEN
          H = OLDH
          SDEST = OLDSD
          CDEST = OLDCD
       ELSE
          IF (FDCERR <= BNDUP) THEN
             ICE1BIG = 0
             HSAVE = H
             FDSAVE = FDEST
             CDSAVE = CDEST
             SDSAVE = SDEST
          END IF

          IF (SDCERR >= BNDLO) THEN IDONE = 1

          IF (IDONE = 0) THEN
             OLDH = H
             H = H / RHO
          END IF
       END IF

    END IF

    IF (IDONE = 1) THEN
       HPHI = ABS(H)
       HOPT = 2# * SQR(EPSA) / SQR(ABS(SDEST))
       ERR1 = HOPT * ABS(SDEST)
       ERR2 = ABS(FDEST - CDEST)
       ERRBND = ERR1
       IF (ERR2 > ERR1) THEN ERRBND = ERR2
       INFORM = 0
       IF (ERRBND > (.5# * ABS(FDEST))) THEN INFORM = 4
    ELSE
       IF (ITER >= 10) THEN IDONE = 1
       IF (IDONE = 1) THEN
          IF (ICE1BIG = 1) THEN
             INFORM = 1
             HPHI = HOPT
             FDEST = 0#
             CDEST = 0#
             SDEST = 0#
             ERRBND = 0#
          ELSEIF (ICE2BIG = 1) THEN
             INFORM = 2
             HPHI = ABS(HSAVE)
             HOPT = HPHI
             FDEST = FDSAVE
             CDEST = CDSAVE
             SDEST = 0#
             ERRBND = 2# * EPSA / HOPT
          ELSE
             INFORM = 3
             HPHI = ABS(HSAVE)
             HOPT = HPHI
             FDEST = FDSAVE
             CDEST = CDSAVE
             SDEST = SDSAVE
             ERRBND = .5# * HOPT * ABS(SDEST) + 2# * EPSA / HOPT
          END IF
       END IF
    END IF

END SUB

SUB FINDFD (N, X(), F(), G(), H(), FFD()) STATIC
   
    ' Finite difference subroutine

    EPSM = 2.22D-16
    EPSRF = EPSM ^ .9#
    
    CALL USRFUN(X(), F(), G(), H())

    FX = F(1)

    FOR I = 1 TO N
        HOPT = 2# * (1# + ABS(X(I))) * SQR(EPSRF)
        H = 10# * HOPT

        XSAVE = X(I)

        IFRST = 1
        ITER = 0
        IDONE = 0
        NITER = 0

        DO
           NITER = NITER + 1

           X(I) = XSAVE + H

           CALL USRFUN(X(), F(), G(), H())
    
           F1 = F(1)

           X(I) = XSAVE + H + H

           CALL USRFUN(X(), F(), G(), H())

           F2 = F(1)
  
           CALL FDINT(H, FX, F1, F2, IFRST, ITER, IDONE, HOPT)
    
           IF (IDONE = 1 OR NITER > 50) THEN EXIT DO
        LOOP

        X(I) = XSAVE

        FFD(I) = HOPT
  
    NEXT I

END SUB

SUB KEYCHECK STATIC

    ' Check user response subroutine

    LOCATE 25, 1
    PRINT TAB(25); "< press any key to continue >";

    A$ = ""
    WHILE A$ = ""
       A$ = INKEY$
    WEND

END SUB

SUB LINEAR (NA, GTG(), RHS(), DET, IERR) STATIC

    ' Solve linear equations subroutine

    DIM TEMP(NA)

    N = NA
    IERR = 1

    IF (N = 1) THEN
       IF (GTG(1, 1) = 0#) THEN
          IERR = 3
          DET = 0#
          GOTO EXITLINEAR
       END IF
       HOLD = GTG(1, 1)
       GTG(1, 1) = RHS(1) / HOLD
       DET = DET * HOLD
       GOTO EXITLINEAR
    END IF

    NM1 = N - 1
    NP1 = N + 1
    DETM = DET

    FOR I = 1 TO N
        TEMP(I) = I
    NEXT I
     
    FOR K = 1 TO NM1
        KP1 = K + 1
        KR = K
        KC = K
        BIGA = ABS(GTG(K, K))
        FOR I = K TO N
            FOR J = K TO N
                IF (BIGA < ABS(GTG(I, J))) THEN
                   BIGA = ABS(GTG(I, J))
                   KR = I
                   KC = J
                END IF
            NEXT J
        NEXT I
        IF (BIGA = 0#) THEN
           IERR = 3
           DET = 0#
           GOTO EXITLINEAR
        END IF
        DETM = DETM * GTG(KR, KC)
        IF (KR <> K) THEN
           FOR I = K TO N
               HOLD = GTG(K, I)
               GTG(K, I) = GTG(KR, I)
               GTG(KR, I) = HOLD
           NEXT I
           HOLD = RHS(K)
           RHS(K) = RHS(KR)
           RHS(KR) = HOLD
           DETM = -DETM
        END IF

        IF (KC <> K) THEN
           FOR J = 1 TO N
               HOLD = GTG(J, K)
               GTG(J, K) = GTG(J, KC)
               GTG(J, KC) = HOLD
           NEXT J
           I = TEMP(K)
           TEMP(K) = TEMP(KC)
           TEMP(KC) = I
           DETM = -DETM
        END IF

        FOR I = KP1 TO N
            GTG(K, I) = GTG(K, I) / GTG(K, K)
        NEXT I
        RHS(K) = RHS(K) / GTG(K, K)
        FOR I = KP1 TO N
            FOR J = KP1 TO N
                GTG(I, J) = GTG(I, J) - GTG(I, K) * GTG(K, J)
            NEXT J
            RHS(I) = RHS(I) - GTG(I, K) * RHS(K)
        NEXT I
    NEXT K

    IF (GTG(N, N) = 0#) THEN
       IERR = 3
       DET = 0#
       GOTO EXITLINEAR
    END IF

    DET = DETM * GTG(N, N)

    RHS(N) = RHS(N) / GTG(N, N)
   
    FOR I = 1 TO NM1
        HOLD = 0#
        J = N - I
        FOR KC = 1 TO I
            K = NP1 - KC
            HOLD = HOLD + GTG(J, K) * RHS(K)
        NEXT KC
        RHS(J) = RHS(J) - HOLD
    NEXT I

    FOR I = 1 TO N
        J = TEMP(I)
        GTG(J, 1) = RHS(I)
    NEXT I

EXITLINEAR:

    IF (IERR = 3) THEN
       CLS
       LOCATE 5, 1
       PRINT TAB(14); "Singular matrix encountered during solution process !"
       CALL KEYCHECK
    END IF

    ERASE TEMP

END SUB

SUB MDRIVER (X(), F(), G(), H(), IERR) STATIC

    ' Minimization driver subroutine
   
    NX = N(6)

    DIM XG(NX), XH(NX * (NX + 1) / 2), XW(4 * NX), XM(NX)

    NXSAVE = N(24)
    NXH = N(19) + 1
    NXG = NXH + NX * (NX + 1) / 2
    NXW = NXG + NX
    NXM = NXW + 4 * NX
    NXFREE = NXM + NX - 1

    IF (NXFREE <= N(1)) THEN
       ' NULL
    ELSE
       IERR = -21
       EXIT SUB
    END IF

    N(24) = NXFREE
    N(20) = NXG - 1

    FOR I = 1 TO NX
        NXMP = NXM - 1 + I
        X(NXMP) = 1#
        X(NXMP) = ABS(X(I))
        IF (X(NXMP) < .001#) THEN X(NXMP) = 1#
     NEXT I

     ' load MINIMIZE working arrays

     FOR I = 1 TO NX
         XG(I) = X(NXG + I - 1)
     NEXT I

     FOR I = 1 TO NX * (NX + 1) / 2
         XH(I) = X(NXH + I - 1)
     NEXT I

     FOR I = 1 TO 4 * NX
         XW(I) = X(NXW + I - 1)
     NEXT I

     FOR I = 1 TO NX
         XM(I) = X(NXM + I - 1)
     NEXT I

     ' perform minimization

     CALL MINIMIZE(NX, X(), F(), XG(), XH(), XW(), XM(), G(), H(), IERR)
    
     N(24) = NXSAVE
    
     ' unload MINIMIZE working arrays

     FOR I = 1 TO NX
         X(NXG + I - 1) = XG(I)
     NEXT I

     FOR I = 1 TO NX * (NX + 1) / 2
         X(NXH + I - 1) = XH(I)
     NEXT I

     FOR I = 1 TO 4 * NX
         X(NXW + I - 1) = XW(I)
     NEXT I
                    
     FOR I = 1 TO NX
         X(NXM + I - 1) = XM(I)
     NEXT I

     ERASE XG, XH, XW, XM

END SUB

SUB MINIMIZE (N, X(), FX(), G(), H(), W(), XM(), GG(), HQ(), IER) STATIC

    ' Unconstrained minimization subroutine
  
    IF (FFD(1) = 0#) THEN CALL FINDFD(N, X(), FX(), GG(), HQ(), FFD())
    
    IBADG = 0
    NA = 2
    DFN = TOL(9)
    EPS = TOL(7)
    HH = TOL(21)
    MAXUMI = METHOD(5)
    MGRAD = METHOD(4)

    IF (MAXUMI <= 0) THEN MAXUMI = N + N

    MAXFN = MAXUMI * (2 * N + 5)
    MODE = METHOD(6)
    IPNLTY = METHOD(7)
    TOLGUM = TOL(8)

    NGG = N(8)
    NGK = N(32)
    NGL = N(33)
    NHH = N(9)
    NHK = N(37)
    NXF = N(15)
    NXS = N(16)

    F = FX(1)
    IER = -1

    NP = N + 1
    N1 = N - 1
    NN = N * NP / 2
    ISS = N
    IU = N
    IV = N + N
    IB = IV + N

    IEXIT = 0

    IF (MODE = 1) THEN
       IJ = NN + 1
       FOR I = 1 TO N
           FOR J = 1 TO I
               IJ = IJ - 1
               H(IJ) = 0#
           NEXT J
           H(IJ) = 1#
       NEXT I
    END IF

    IJ = NP
    DMIN = H(1)

    FOR I = 2 TO N
        IF (H(IJ) >= DMIN) THEN
           ' NULL
        ELSE
           DMIN = H(IJ)
        END IF
        IJ = IJ + NP - I
    NEXT I

    IF (DMIN <= 0#) THEN
       EXIT SUB
    END IF

    Z = F
    ITN = 0

    CALL EXCHANGE(N, X(), F, X(), FX(), GG(), HQ(), IER)

    IF (IER <> 0) THEN
       IER = -4
       GOTO EXITMINIMIZE
    END IF

    IFN = 1
    DF = DFN

    IF (DFN = 0#) THEN
       DF = F - Z
    ELSEIF (DFN < 0#) THEN
       DF = ABS(DF * F)
    ELSEIF (DF <= 0#) THEN
       DF = 1#
    END IF

M17:
    FOR I = 1 TO N
        W(I) = X(I)
    NEXT I

    LINK = 1
    
    SELECT CASE MGRAD
    CASE 1
       GOTO M100
    CASE 2
       GOTO M110
    END SELECT

M18:
    GM = 0#

    FOR I = 1 TO N
        GM = GM + G(I) ^ 2#
    NEXT I
     
    IF (IFN >= MAXFN) GOTO M90

M20:
    ITN = ITN + 1

    W(1) = -G(1)

    FOR I = 2 TO N
        IJ = I
        I1 = I - 1
        Z = -G(I)
        FOR J = 1 TO I1
            Z = Z - H(IJ) * W(J)
            IJ = IJ + N - J
        NEXT J
        W(I) = Z
    NEXT I

    W(ISS + N) = W(N) / H(NN)
    IJ = NN

    FOR I = 1 TO N1
        IJ = IJ - 1
        Z = 0#
        FOR J = 1 TO I
            Z = Z + H(IJ) * W(ISS + NP - J)
            IJ = IJ - 1
        NEXT J
        W(ISS + N - I) = W(N - I) / H(IJ) - Z
    NEXT I

    Z = 0#
    GS0 = 0#

    FOR I = 1 TO N
        IF (Z * XM(I) >= ABS(W(ISS + I))) THEN
           ' NULL
        ELSE
           Z = ABS(W(ISS + I)) / XM(I)
        END IF
           GS0 = GS0 + G(I) * W(ISS + I)
    NEXT I

    AEPS = EPS / Z
    IEXIT = 2

    IF (GS0 < 0#) GOTO M28

    GOTO M92

M28:
    ALPHA = -2# * DF / GS0

    IF (ALPHA > 1#) THEN ALPHA = 1#

    FF = F
    TOT = 0#
    IINT = 0
    IEXIT = 1

M30:
    NOLAST = 0

    IF (IFN >= MAXFN) GOTO M90

    FOR I = 1 TO N
        W(I) = X(I) + ALPHA * W(ISS + I)
    NEXT I

    CALL EXCHANGE(N, W(), F1, X(), FX(), GG(), HQ(), IER)

    IF (IER <> 0) THEN
       IER = -4
       GOTO EXITMINIMIZE
    END IF

    IFN = IFN + 1

    IF (F1 >= F) GOTO M40

    F2 = F
    TOT = TOT + ALPHA

M32:
    FOR I = 1 TO N
        X(I) = W(I)
    NEXT I

    F = F1

    IF (IINT = 1) THEN NOLAST = 1

    IF ((IINT - 1) < 0) THEN
       GOTO M35
    ELSEIF ((IINT - 1) = 0) THEN
       GOTO M49
    ELSEIF ((IINT - 1) > 0) THEN
       GOTO M50
    END IF

M35:
    IF (IFN >= MAXFN) GOTO M90

    FOR I = 1 TO N
        W(I) = X(I) + ALPHA * W(ISS + I)
    NEXT I

    CALL EXCHANGE(N, W(), F1, X(), FX(), GG(), HQ(), IER)

    IF (IER <> 0) THEN
       IER = -4
       GOTO EXITMINIMIZE
    END IF

    IFN = IFN + 1

    IF (F1 >= F) GOTO M36

    IF ((F1 + F2 >= (F + F)) AND ((7# * F1 + 5# * F2) > (12# * F))) THEN IINT = 2

    TOT = TOT + ALPHA
    ALPHA = 2# * ALPHA

    GOTO M32

M36:
    DENOM = F2 + F1 - F - F

    IF (DENOM = 0#) GOTO M49

    Z = .5# * (-4# * F + F1 + 3# * F2) / DENOM - 1#

    IF ((ABS(Z) >= 1#) OR (ABS(Z) < .05#)) GOTO M49

    ALPHA = (Z) * ALPHA
   
    FOR I = 1 TO N
        W(I) = X(I) + ALPHA * W(ISS + I)
    NEXT I

    CALL EXCHANGE(N, W(), F2, X(), FX(), GG(), HQ(), IER)

    IF (IER <> 0) THEN
       IER = -4
       GOTO EXITMINIMIZE
    END IF

    IFN = IFN + 1

    NOLAST = 0

    IF (F2 > F) GOTO M49

    TOT = TOT + ALPHA
    F = F2

    FOR I = 1 TO N
        X(I) = W(I)
    NEXT I

    NOLAST = 1
    GOTO M49

M40:
    IF (ALPHA < AEPS) GOTO M57

    IF (IFN >= MAXFN) GOTO M90

    ALPHA = ALPHA / 2#

    FOR I = 1 TO N
        W(I) = X(I) + ALPHA * W(ISS + I)
    NEXT I

    CALL EXCHANGE(N, W(), F2, X(), FX(), GG(), HQ(), IER)

    IF (IER <> 0) THEN
       IER = -4
       GOTO EXITMINIMIZE
    END IF

    IFN = IFN + 1

    IF (F2 >= F) THEN
       ' NULL
    ELSE
       TOT = TOT + ALPHA
       Z = F
       F = F2
       F2 = Z

       FOR I = 1 TO N
           X(I) = W(I)
       NEXT I
     
       NOLAST = 1

       GOTO M36
    END IF

    Z = .1#

    IF (F1 + F > F2 + F2) THEN Z = 1# + .5# * (F - F1) / (F + F1 - F2 - F2)

    IF (Z < .1#) THEN Z = .1#

    ALPHA = Z * ALPHA
    IINT = 1

    GOTO M30

M49:
    IF (NOLAST = 1) THEN
       ' NULL
    ELSE
       FOR I = 1 TO N
           W(I) = X(I)
       NEXT I

       CALL EXCHANGE(N, W(), F, X(), FX(), GG(), HQ(), IER)
    END IF
   
M50:
    IF (ITN >= MAXUMI) GOTO M90

    ICRIT = 0

M57:
    IF ((IPNLTY <> 7) OR (NGG = 0 AND NHH = 0)) GOTO M69

    SUMDL = 0#
    NA = NHH

    IF (NHH = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NHH
           SUMDL = SUMDL + (2# * HQ(NHK + I) * HQ(I)) ^ 2#
       NEXT I
    END IF

    IF (NGG = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NGG
           IF (GG(NGL + I) <> 0#) THEN NA = NA + 1
           TMP = GG(NGL + I) + 2# * GG(NGK + I) * GG(I)
           IF (TMP > 0#) THEN TMP = 0#
           SUMDL = SUMDL + (GG(NGL + I) - TMP) ^ 2#
       NEXT I
    END IF

    SQRTGM = SQR(GM)
    SUMDL = SQR(SUMDL)

    IF (ITN <> 1) THEN
       ' NULL
    ELSE
       SUMDLO = 0#
       IF (SUMDL = 0#) THEN
          TOLM = 1#
          TOLDL = .0001#
       ELSE
          TOLM = TOLGUM * SQRTGM / SUMDL
          TOLDL = SUMDL * TOLGUM / 100#
       END IF
    END IF

    STEPP = 0#

    FOR I = 1 TO N
        STEPP = STEPP + W(ISS + I) ^ 2#
    NEXT I
     
    STEPP = SQR(STEPP) * TOT

    CALL PENALTY(X(), FX(), GG(), HQ(), IER)

    IF (SQRTGM <= TOLM * SUMDL AND ITN >= NA) THEN ICRIT = 1

    IF ((ABS(SUMDL - SUMDLO) < TOLDL) AND (ITN >= NA) AND (SUMDL <> 0#)) THEN ICRIT = 2

    IF (TOT < AEPS) GOTO M92

    IF (ICRIT <> 0) GOTO M94

    SUMDLO = SUMDL

M69:
    ALPHA = TOT

    FOR I = 1 TO N
        W(I) = X(I)
        W(IB + I) = G(I)
    NEXT I

    LINK = 2
   
    SELECT CASE MGRAD
    CASE 1
       GOTO M100
    CASE 2
       GOTO M110
    END SELECT

M54:
    IF (IFN >= MAXFN) GOTO M90

    GYS = 0#
    GM = 0#

    FOR I = 1 TO N
        W(I) = W(IB + I)
        GM = GM + G(I) ^ 2#
        GYS = GYS + G(I) * W(ISS + I)
    NEXT I
     
    DF = FF - F
    DGS = GYS - GS0

    IF (DGS <= 0#) GOTO M20

    LINK = 1

    IF (DGS + ALPHA * GS0 > 0#) GOTO M52

    FOR I = 1 TO N
        W(IU + I) = G(I) - W(I)
    NEXT I

    SIG = 1# / (ALPHA * DGS)
    GOTO M70

M52:
    ZZ = ALPHA / (DGS - ALPHA * GS0)
   
    Z = DGS * ZZ - 1#

    FOR I = 1 TO N
        W(IU + I) = Z * W(I) + G(I)
    NEXT I
     
    SIG = 1# / (ZZ * DGS ^ 2#)

    GOTO M70

M60:
    LINK = 2

    FOR I = 1 TO N
        W(IU + I) = W(I)
    NEXT I

    IF (DGS + ALPHA * GS0 > 0#) THEN
       SIG = -ZZ
    ELSE
       SIG = 1# / GS0
    END IF

M70:
    W(IV + 1) = W(IU + 1)

    FOR I = 2 TO N
        IJ = I
        I1 = I - 1
        Z = W(IU + I)
        FOR J = 1 TO I1
            Z = Z - H(IJ) * W(IV + J)
            IJ = IJ + N - J
        NEXT J
        W(IV + I) = Z
    NEXT I

    IJ = 1

    FOR I = 1 TO N
        Z = H(IJ) + SIG * W(IV + I) ^ 2#
        IF (Z <= 0#) THEN Z = DMIN
        IF (Z < DMIN) THEN DMIN = Z
        H(IJ) = Z
        W(IB + I) = W(IV + I) * SIG / Z
        SIG = SIG - W(IB + I) ^ 2# * Z
        IJ = IJ + NP - I
    NEXT I

    IJ = 1

    FOR I = 1 TO N1
        IJ = IJ + 1
        I1 = I + 1
        FOR J = I1 TO N
            W(IU + J) = W(IU + J) - H(IJ) * W(IV + I)
            H(IJ) = H(IJ) + W(IB + I) * W(IU + J)
            IJ = IJ + 1
        NEXT J
    NEXT I

    IF (LINK = 1) THEN
       GOTO M60
    ELSEIF (LINK = 2) THEN
       GOTO M20
    END IF

M90:
    IEXIT = 3
    ICRIT = 3

    GOTO M57

M92:
    IF (MGRAD = 2) GOTO M94
    
    MGRAD = 2

    GOTO M17

M94:
    CALL EXCHANGE(N, X(), FX, X(), FX(), GG(), HQ(), IER)

    IFN = IFN + 1

    SELECT CASE IEXIT
    CASE 0
       IER = -1
    CASE 1 TO 3
       IER = 0
    END SELECT

    IF (IPRT = 1) THEN
       CLS
       PRINT
       PRINT TAB(27); "*** Minimization Summary ***"
       PRINT
       PRINT TAB(15); "Number of unconstrained iterations = "; ITN
       PRINT
       PRINT TAB(15); "Number of objective function calls = "; IFN
       PRINT
       PRINT TAB(15); "Augmented function value = "; F
       PRINT
       PRINT TAB(15); "Multiplier update norm   = "; SUMDL
       PRINT
       PRINT
       PRINT TAB(31); "Control variables"
       PRINT
       FOR I = 1 TO N
           PRINT X(I); "  ";
       NEXT I
       PRINT
       PRINT
       PRINT TAB(33); "Gradient values"
       PRINT
       FOR I = 1 TO N
           PRINT G(I); "  ";
       NEXT I
       CALL KEYCHECK
    END IF

    EXIT SUB

M100:
    FOR I = 1 TO N
        IF (HH <= .000000000000001#) THEN
           Z = X(NXF + I) / X(NXS + I)
        ELSE
           Z = FFD(I)
        END IF
       
        W(I) = W(I) + Z
       
        CALL EXCHANGE(N, W(), F1, X(), FX(), GG(), HQ(), IER)
       
        IF (IER <> 0) THEN
           IER = -4
           GOTO EXITMINIMIZE
        END IF
       
        G(I) = (F1 - F) / Z
        W(I) = W(I) - Z
    NEXT I

    IFN = IFN + N

    IF (IPRT = 1) THEN
       FOR I = 1 TO N
           J1 = I + 1
           IF (G(I) = 0#) THEN
              CLS
              PRINT TAB(28); "Zero element in gradient"
              PRINT
              PRINT TAB(23); "potential error in CV definition !"
              FOR J = 1 TO N
                  PRINT
                  PRINT "G("; J; ")", G(J)
              NEXT J
              CALL KEYCHECK
              CLS
              EXIT FOR
           END IF
           FOR J = J1 TO N
               IF (ABS((G(I) - G(J)) / G(I)) < .000001#) THEN
                  CLS
                  LOCATE 3, 1
                  PRINT TAB(18); "Gradient vector has nearly equal elements !!"
                  PRINT
                  FOR K = 1 TO N
                      PRINT
                      PRINT TAB(25); "G("; K; ") ="; TAB(35); G(K)
                  NEXT K
                  CALL KEYCHECK
                  CLS
                  EXIT FOR
               END IF
           NEXT J
       NEXT I

       SUM = 0#

       FOR I = 1 TO N
           SUM = SUM + DLOG10(ABS(G(I)))
       NEXT I

       SUM = SUM / N

       FOR I = 1 TO N
           B = DLOG10(ABS(G(I))) - SUM
           IF (B < -4# OR B > 4#) THEN
              IBADG = IBADG + 1
              CLS
              LOCATE 3, 1
              PRINT TAB(10); "Gradient is out of acceptable range !!"
              PRINT
              FOR J = 1 TO N
                  PRINT
                  PRINT TAB(25); "G("; J; ") ="; TAB(35); G(J)
              NEXT J
              CALL KEYCHECK
              CLS
              IF (IBADG > 2) THEN EXIT FOR
           ELSE
              EXIT FOR
           END IF
       NEXT I
    END IF

    IF (LINK = 1) THEN
       GOTO M18
    ELSEIF (LINK = 2) THEN
       GOTO M54
    END IF

M110:
    FOR I = 1 TO N
        IF (HH <= .000000000000001#) THEN
           Z = X(NXF + I) / X(NXS + I)
        ELSE
           Z = FFD(I)
        END IF
       
        W(I) = W(I) + Z
       
        CALL EXCHANGE(N, W(), F1, X(), FX(), GG(), HQ(), IER)
        
        IF (IER <> 0) THEN
           IER = -4
           GOTO EXITMINIMIZE
        END IF
         
        W(I) = W(I) - 2# * Z
         
        CALL EXCHANGE(N, W(), F2, X(), FX(), GG(), HQ(), IER)
         
        IF (IER <> 0) THEN
           IER = -4
           GOTO EXITMINIMIZE
        END IF
       
        G(I) = (F1 - F2) / (2# * Z)
        W(I) = W(I) + Z
    NEXT I

    IFN = IFN + N + N

    IF (LINK = 1) THEN
       GOTO M18
    ELSEIF (LINK = 2) THEN
       GOTO M54
    END IF

EXITMINIMIZE:

    CLS
    LOCATE 5, 1
    PRINT TAB(14); "Error during unconstrained minimization computation !"
    CALL KEYCHECK

END SUB

SUB NLP (X(), FX(), GG(), HH(), IERR) STATIC

    ' Nonlinear programming subroutine

    NPI = 1
    OLDFX = 0#
    IERR = 0

    TOLFNP = TOL(1)
    TOLHNP = TOL(4)

    MX = N(1)
    N = N(6)
    NGG = N(8)
    NHH = N(9)

    MAXNP = METHOD(2)

    CALL USRFUN(X(), FX(), GG(), HH())

    NXS = N(16)
   
    FOR I = 1 TO N
        X(I) = X(I) / X(NXS + I)
    NEXT I

    CALL PENALTY(X(), FX(), GG(), HH(), IERR)

    IF (IERR <> 0) THEN EXIT SUB

    CNORM = 0#
    OLDFX = FX(1)
    OLDOBJ = FX(2)

    IF (NHH = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NHH
           CNORM = CNORM + HH(I) ^ 2#
       NEXT I
    END IF

    IF (NGG = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NGG
           IF (GG(I) <= 0#) THEN CNORM = CNORM + GG(I) ^ 2#
       NEXT I
    END IF

    OLDCN = SQR(CNORM)

    CALL QPENALTY(GG(), HH(), NGG, NHH, TOL(16), TOL(17))

    DO
       CLS
       LOCATE 5, 1
       PRINT "Nonlinear programming iteration #"; NPI
       
       CALL MDRIVER(X(), FX(), GG(), HH(), IERR)

       FXNEW = FX(1)
       OBJNEW = FX(2)

       IF (IERR <> 0) THEN EXIT DO

       NC = NGG + NHH
       IF (NC > N) THEN NC = N
       NGT = 3 * N + N * NC + 1
       NTEST = NGT + NC * NC - 1
      
       IF (NTEST > MX) THEN EXIT DO

       CALL PENSET(X(), FX(), GG(), HH(), NPI, N, NC, IERR)

       IF (IERR <> 0) THEN EXIT DO

       CNORM = 0#
       NPI = NPI + 1

       IF (NHH = 0) THEN
          ' NULL
       ELSE
          FOR I = 1 TO NHH
              CNORM = CNORM + HH(I) ^ 2#
          NEXT I
       END IF

       IF (NGG = 0) THEN
          ' NULL
       ELSE
          FOR I = 1 TO NGG
              IF (GG(I) <= 0#) THEN CNORM = CNORM + GG(I) ^ 2#
          NEXT I
       END IF

       CNORM = SQR(CNORM)
       FFNORM = ABS(FXNEW)
       OBJNOR = ABS(OBJNEW)

       IF (FFNORM < 1D-38) THEN FFNORM = 1#
       IF (OBJNOR < 1D-38) THEN OBJNOR = 1#

       DELFX = ABS((FXNEW - OLDFX) / FFNORM)
       DELOBJ = ABS((OBJNEW - OLDOBJ) / OBJNOR)

       IF (IPRT = 1) THEN
          CLS
          LOCATE 5, 1
          PRINT TAB(15); "Unconstrained function minimization convergence"
          LOCATE 8, 1
          PRINT TAB(10); "Old value of augmented function = "; OLDFX
          PRINT TAB(10); "Old value of objective function = "; OLDOBJ
          PRINT TAB(10); "Old value of constraint norm    = "; OLDCN
          PRINT
          PRINT TAB(10); "New value of augmented function = "; FXNEW
          PRINT TAB(10); "New value of objective function = "; OBJNEW
          PRINT TAB(10); "New value of constraint norm    = "; CNORM
          PRINT
          PRINT TAB(10); "Function termination criteria   = "; DELFX
          PRINT TAB(10); "Objective function criteria     = "; DELOBJ
          PRINT
       END IF

       IF (DELFX < TOLFNP) THEN
          IF (IPRT = 1) THEN
             PRINT
             PRINT TAB(14); "** Function decrease tolerance satisfied in NLP **"
          END IF
          EXIT DO
       END IF

       IF ((DELOBJ < TOLFNP) AND (CNORM < TOLHNP)) THEN
          IF (IPRT = 1) THEN
             PRINT
             PRINT TAB(24); "** Objective norm criteria satisfied **"
          END IF
          EXIT DO
       END IF

       IF (NPI > MAXNP) THEN
          IF (IPRT = 1) THEN
             PRINT
             PRINT TAB(24); "** Iteration count satisfied in NLP **"
          END IF
          EXIT DO
       END IF

       OLDCN = CNORM
       OLDFX = FXNEW
       OLDOBJ = OBJNEW

    LOOP
    
    IF (IPRT = 1) THEN CALL KEYCHECK
       
END SUB

SUB NLPINZ (X(), F(), G(), H(), IER) STATIC

    ' Initialization subroutine

    IER = 0

    FOR I = 1 TO 35
        METHOD(I) = 1
        TOL(I) = 0#
    NEXT I

    ' define default solution method

    METHOD(1) = 2           ' multiplier update method (2 = byers)
    METHOD(3) = 2           ' method of unconstrained minimization
    METHOD(5) = 50          ' maximum number of unconstrained iterations
    METHOD(6) = 1           ' type of H matrix initialization (1 = identity)
    METHOD(7) = 7           ' type of augmented Lagrange function (7 = standard)
                      
    ' define default tolerances

    TOL(1) = .0000001#      ' convergence tolerance on function decrease
    TOL(4) = .0000001#      ' convergence tolerance on constraint error
    TOL(7) = .0000001#      ' convergence tolerance on step size
    TOL(8) = .01#           ' termination tolerance for the unconstrained method
    TOL(9) = -.01#          ' initial step-length for one-dimensional search
    TOL(16) = 1#            ' initial value of equality constraint penalty constants
    TOL(17) = 1#            ' initial value of inequality constraint penalty constants
    TOL(18) = 0#            ' default value for equality constraint Lagrange multiplier
    TOL(19) = 0#            ' default value for inequality constraint Lagrange multiplier
    TOL(21) = .000001#      ' perturbation fraction
    TOL(22) = 1D+99         ' "big" number
    TOL(23) = 1D-99         ' "small" number
    TOL(25) = .25#          ' tolerance to test for penalty constant increase
    TOL(26) = 5#            ' factor by which quadratic coefficients may be increased
    TOL(27) = .01#          ' tolerance for the Haarhoff-Buys LS update
   
    ' initialization

    SCALD = 1#

    NFI = 2
    NFS = 1
    NGI = 2
    NGK = 3
    NGL = 4
    NGS = 1
    NHI = 2
    NHK = 3
    NHL = 4
    NHS = 1
    NXF = 1
    NXI = 3
    NXH = 4
    NXL = 0
    NXN = 0
    NXS = 2
    NXU = 0
   
    NFFREE = 3
    NGFREE = 5
    NHFREE = 5

    NX = N(6)
   
    IF (NX = 0) THEN
       ' NULL
    ELSE
       L3 = NXF * NX
       L4 = NXS * NX
       FOR I = 1 TO NX
           X(I) = 0#
           X(L3 + I) = TOL(21)
           X(L4 + I) = SCALD
       NEXT I
    END IF

    NF = N(7)

    IF (NF = 0) THEN
       ' NULL
    ELSE
       L1 = NFS * NF
       FOR I = 1 TO NF
           F(L1 + I) = SCALD
       NEXT I
    END IF

    NH = N(9)

    IF (NH = 0) THEN
       ' NULL
    ELSE
       HLAMI = TOL(18)
       HKI = TOL(16)
       L1 = NH * NHS
       L2 = NH * NHK
       L3 = NH * NHL
       FOR I = 1 TO NH
           H(L1 + I) = SCALD
           H(L2 + I) = HKI
           H(L3 + I) = HLAMI
       NEXT I
    END IF

    NG = N(8)
   
    IF (NG = 0) THEN
       ' NULL
    ELSE
       GKI = TOL(17)
       GLAMI = TOL(19)
       L1 = NG * NGS
       L2 = NG * NGK
       L3 = NG * NGL
       FOR I = 1 TO NG
           G(L1 + I) = SCALD
           G(L2 + I) = GKI
           G(L3 + I) = GLAMI
       NEXT I
    END IF

    NX = N(6)
    NF = N(7)
    NG = N(8)
    NH = N(9)

    IF (NX < 1) THEN IER = 2
    IF (NG < 0) THEN IER = 4
    IF (NH < 0) THEN IER = 5

    IF (IER > 0) THEN EXIT SUB

    N(7) = NF
    N(10) = 35
    N(11) = 35
    N(13) = NXU * NX
    N(14) = NXL * NX
    N(15) = NXF * NX
    N(16) = NXS * NX
    N(17) = NXN * NX
    N(18) = NXI * NX
    N(19) = NXH * NX
    N(24) = N(19) + NX * (NX + 1) / 2
    N(25) = 4
    N(26) = NFI * NF
    N(29) = NFFREE * NF
    N(30) = NGS * NG
    N(31) = NGI * NG
    N(32) = NGK * NG
    N(33) = NGL * NG
    N(34) = NGFREE * NG
    N(35) = NHS * NH
    N(36) = NHI * NH
    N(37) = NHK * NH
    N(38) = NHL * NH
    N(39) = NHFREE * NH

    L1 = N(16)

    FOR I = 1 TO NX
        IF (X(L1 + I) = 0#) THEN X(L1 + I) = SCALD
    NEXT I

    L1 = N(25)

    FOR I = 1 TO NF
        IF (F(L1 + I) = 0#) THEN F(L1 + I) = SCALD
    NEXT I

    IF (NG = 0) THEN
       ' NULL
    ELSE
       L1 = N(30)
       FOR I = 1 TO NG
           IF (G(L1 + I) = 0#) THEN G(L1 + I) = SCALD
       NEXT I
    END IF

    IF (NH = 0) THEN
       ' NULL
    ELSE
       L1 = N(35)
       FOR I = 1 TO NH
           IF (H(L1 + I) = 0#) THEN H(L1 + I) = SCALD
       NEXT I
    END IF

    NXSR = N(24)
    NFSR = N(29)
    NGSR = N(34)
    NHSR = N(39)

    NGSR = NGSR + NG
   
    NC = NG + NH
    IF (NC > NX) THEN NC = NX
   
    NXSR = NX * (7 + NC) + NC * NC + NX * (NX + 1) / 2

    NXSR = NXSR - NX

    IF (NXSR > N(1)) THEN IER = 7
    IF (NFSR > N(2)) THEN IER = 8
    IF (NGSR > N(3)) THEN IER = 9
    IF (NHSR > N(4)) THEN IER = 10

END SUB

SUB PENALTY (X(), FX(), GG(), HH(), IERR) STATIC

    ' Penalty function subroutine
   
    IERR = 0
    
    CALL USRFUN(X(), FX(), GG(), HH())
   
    FX(2) = FX(1)

    NGG = N(8)
    NHH = N(9)

    SUMG = 0#
    SUMH = 0#

    NGK = N(32)
    NGL = N(33)
    NHK = N(37)
    NHL = N(38)

    IF (NGG = 0) THEN
       ' NULL
    ELSE
       IF (NGK = 0 OR NGL = 0) THEN
          IERR = -31
          FX(3) = SUMG
          FX(4) = SUMH
          EXIT SUB
       END IF
       FOR I = 1 TO NGG
           GK = GG(NGK + I)
           GLAM = GG(NGL + I)
           IF ((GLAM + 2# * GK * GG(I)) < 0#) THEN
              SUMG = SUMG + GLAM * GG(I) + GK * GG(I) ^ 2#
           ELSE
              SUMG = SUMG - .25# * GLAM ^ 2# / GK
           END IF
       NEXT I
    END IF

    IF (NHH = 0) THEN
       ' NULL
    ELSE
       IF (NHK = 0 OR NHL = 0) THEN
          IERR = -31
          FX(3) = SUMG
          FX(4) = SUMH
          EXIT SUB
       END IF
       FOR I = 1 TO NHH
           HK = HH(NHK + I)
           HLAM = HH(NHL + I)
           SUMH = SUMH + HLAM * HH(I) + HK * HH(I) ^ 2#
       NEXT I
    END IF

    FX(1) = FX(1) + SUMG + SUMH

    FX(3) = SUMG
    FX(4) = SUMH

END SUB

SUB PENSET (X(), F(), G(), H(), NPI, NX, NC, IERR) STATIC

    ' Set penalty function parameters subroutine

    DIM FDP(NX), GX(NX), HP(NX, NC), GTG(NC, NC)

    M = N(6)
    NGG = N(8)
    NHH = N(9)

    DIM RHS(NGG + NHH)

    BETA = TOL(25)
    EPS4 = TOL(27)
    SMALL = TOL(23)
    TAU = TOL(26)

    METHOD(6) = 3
    MUPDAT = METHOD(1)

    NFS = N(25)
    NGIV = N(34)
    NGK = N(32)
    NGL = N(33)
    NGS = N(30)
    NHK = N(37)
    NHL = N(38)
    NHS = N(35)
    NXF = N(15)
    NXS = N(16)

    NA = 0
    AM = 0#

    IF (NGG = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NGG
           A = G(I)
           G(NGIV + I) = NGG + 1
           IF (A <= -G(NGL + I) / (2# * G(NGK + I))) THEN
              NA = NA + 1
              G(NGIV + NA) = I
              IF (ABS(A) > AM) THEN AM = ABS(A)
           END IF
       NEXT I
    END IF

    NA = NHH + NA

    IF (NHH = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NHH
           A = H(I)
           IF (ABS(A) > AM) THEN AM = ABS(A)
       NEXT I
    END IF

    IF ((NA <= M) AND (AM <= EPS4)) GOTO PENSET2

PENSET1:
   
    IF (NHH = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NHH
           H(NHL + I) = H(NHL + I) + 2# * H(NHK + I) * H(I)
       NEXT I
    END IF

    IF (NGG = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NGG
           TMP = G(NGL + I) + 2# * G(NGK + I) * G(I)
           IF (TMP < 0#) THEN
              G(NGL + I) = TMP
           ELSE
              G(NGL + I) = 0#
           END IF
       NEXT I
    END IF

    GOTO PENSET4
       
PENSET2:
   
    IF (NHH = 0) THEN
       ' NULL
    ELSE
       FOR J = 1 TO NHH
           FOR I = 1 TO M
               HP(I, J) = H(J)
           NEXT I
       NEXT J
    END IF

    K = NHH + 1
    KK = 1

    IF (NGG = 0) THEN
       ' NULL
    ELSE
       FOR J = 1 TO NGG
           A = G(J)
           I = G(NGIV + KK)
           IF (J >= I) THEN
              FOR I = 1 TO M
                  HP(I, K) = A
              NEXT I
              K = K + 1
              KK = KK + 1
           END IF
       NEXT J
    END IF

    IF (NA <= 0) GOTO PENSET3
      
    OBJ = F(2)
    FDPCT = TOL(21)

    FOR I = 1 TO M
        S = X(I)
        XMULT = 1#
        IF (ABS(S) > .001#) THEN XMULT = ABS(S)
       
        IF (FDPCT <= .000000000000001#) THEN
           XPERT = X(NXF + I) / X(NXS + I)
        ELSE
           XPERT = FFD(I)
        END IF
       
        X(I) = X(I) + XPERT
      
        CALL PENALTY(X(), F(), G(), H(), IERR)
       
        IF (IERR <> 0) GOTO PENSET5
       
        IF (NHH = 0) THEN
           ' NULL
        ELSE
           FOR J = 1 TO NHH
               HP(I, J) = (H(J) - HP(I, J)) / XPERT
           NEXT J
        END IF
        KK = 1
        K = NHH + 1

        IF (NGG = 0) THEN
           ' NULL
        ELSE
           FOR J = 1 TO NGG
               L = G(NGIV + KK)
               IF (J >= L) THEN
                  HP(I, K) = (G(J) - HP(I, K)) / XPERT
                  KK = KK + 1
                  K = K + 1
               END IF
           NEXT J
        END IF

        OBJ1 = F(2)
        GX(I) = (OBJ1 - OBJ) / XPERT
        X(I) = S
    NEXT I
    
    IF (NA = 0) GOTO PENSET3

    FOR J = 1 TO NA
        RHS(J) = 0#
        FOR I = 1 TO M
            RHS(J) = RHS(J) - HP(I, J) * GX(I)
        NEXT I
    NEXT J
     
    FOR J = 1 TO NA
        FOR I = 1 TO NA
            GTG(I, J) = 0#
            FOR K = 1 TO M
                GTG(I, J) = GTG(I, J) + HP(K, I) * HP(K, J)
            NEXT K
        NEXT I
    NEXT J

    DET = 1#

    CALL LINEAR(NA, GTG(), RHS(), DET, IERR)
        
    IF (DET <= SMALL OR IERR = 3) THEN
       IERR = 0
       GOTO PENSET1
    END IF

    IERR = 0

PENSET3:

    IF (NHH = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NHH
           H(NHL + I) = GTG(I, 1)
       NEXT I
    END IF

    K = NHH + 1
    KK = 1

    IF (NGG = 0) THEN
       ' NULL
    ELSE
       FOR J = 1 TO NGG
           I = G(NGIV + KK)
           IF (J < I) THEN
              G(NGL + J) = 0#
           ELSE
              G(NGL + J) = GTG(K, 1)
              K = K + 1
              KK = KK + 1
           END IF
       NEXT J
    END IF

PENSET4:
   
    IF (NHH = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NHH
           RHS(I) = H(NHL + I) * F(NFS + 1) / H(NHS + I)
       NEXT I
    END IF

    IF (NGG = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NGG
           RHS(I) = G(NGL + I) * F(NFS + 1) / G(NGS + I)
       NEXT I
    END IF

    IF ((IPRT = 1) AND (NHH <> 0) AND (NGG <> 0)) THEN
       CLS
       LOCATE 3, 1
       IF (NHH <> 0) THEN
          PRINT TAB(30); "Equality constraints"
          PRINT
          FOR I = 1 TO NHH
              PRINT H(I); "  ";
          NEXT I
          PRINT
          PRINT
          PRINT TAB(20); "Equality constraint Lagrange multipliers"
          PRINT
          FOR I = 1 TO NHH
              PRINT H(NHL + I); "  ";
          NEXT I
          PRINT
          PRINT
       END IF
       IF (NGG <> 0) THEN
          PRINT
          PRINT TAB(29); "Inequality constraints"
          PRINT
          FOR I = 1 TO NGG
              PRINT G(I); "  ";
          NEXT I
          PRINT
          PRINT
          PRINT TAB(19); "Inequality constraint Lagrange multipliers"
          PRINT
          FOR I = 1 TO NGG
              PRINT G(NGL + I); "  ";
          NEXT I
       END IF
       CALL KEYCHECK
    END IF

    IF (MUPDAT = 1) THEN
       AMOLD = AM
       GOTO PENSET5
    END IF

    IF (NPI = 1) THEN
       AMOLD = AM
       GOTO PENSET5
    END IF

    IF (AM <= BETA * AMOLD) THEN
       AMOLD = AM
       GOTO PENSET5
    END IF

    IF (NHH = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NHH
           A = ABS(H(I))
           IF (A >= BETA * AMOLD) THEN H(NHK + I) = H(NHK + I) * TAU
       NEXT I
    END IF

    K = NHH + 1
    KK = 1

    IF (NGG = 0) THEN
       ' NULL
    ELSE
       FOR J = 1 TO NGG
           I = G(NGIV + KK)
           IF (J >= I) THEN KK = KK + 1
           IF (G(J) < 0#) THEN A = ABS(G(J))
           IF (A >= BETA * AMOLD) THEN G(NGK + J) = G(NGK + J) * TAU
       NEXT J
    END IF

    AMOLD = AM
  
PENSET5:
      
    ERASE FDP, GX, RHS, HP, GTG

END SUB

SUB QPENALTY (GG(), HH(), NGG, NHH, HKI, GKI) STATIC

    ' Quadratic penalty subroutine

    NGK = N(32)
    NHK = N(37)

    R = 1#

    IF (NGG = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NGG
           IF (GG(NGK + I) = 0#) THEN GG(NGK + I) = R
           IF (GKI <> 0#) THEN GG(NGK + I) = GKI
       NEXT I
    END IF

    IF (NHH = 0) THEN
       ' NULL
    ELSE
       FOR I = 1 TO NHH
           IF (HH(NHK + I) = 0#) THEN HH(NHK + I) = R
           IF (HKI <> 0#) THEN HH(NHK + I) = HKI
       NEXT I
    END IF

END SUB

SUB USRFUN (X(), F(), G(), H()) STATIC

    ' Objective function and constraints subroutine

    ' Minimum cost cylindrical tank design

    ' Input

    '  X() = array of control variables ( 2 rows )
     
    ' Output

    '  F() = objective function vector ( 1 element )
    '  G() = vector of inequality constraint functions ( 4 rows )
    '  H() = vector of equality constraint functions ( 1 row )

    PI = 4# * ATN(1#)

    ' user-defined volume of the tank

    VOLUME = 100#

    ' user-defined minimum and maximum tank radius

    RMIN = .1#
    RMAX = 5#

    ' user-defined minimum and maximum tank length

    HMIN = .1#
    HMAX = 10#

    ' current values for cylinder radius and height

    RADIUS = X(1)
    HEIGHT = X(2)

    ' objective (cost) function

    F(1) = RADIUS * RADIUS + RADIUS * HEIGHT

    ' define equality (volume) constraint

    H(1) = PI * RADIUS * RADIUS * HEIGHT - VOLUME

    ' define inequality (radius and length) constraints

    G(1) = RADIUS - RMIN     ' minimum radius
    G(2) = RMAX - RADIUS     ' maximum radius
   
    G(3) = HEIGHT - HMIN     ' minimum height
    G(4) = HMAX - HEIGHT     ' maximum height

END SUB

