' Copyright (c) 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FVAL) 

SUB GAUSLEG (XMIN, XMAX, N, EPS, S) STATIC
 
    ' Gauss-Legendre integration subroutine

    ' Input

    '  XMIN = lower integration limit
    '  XMAX = upper integration limit
    '  N    = number of quadrature points
    '  EPS  = convergence criteria

    ' Output

    '  S = integral of f(x)

    ' Note: requires SUB USERFUNC

    DIM X(N), W(N)

    PI = 4# * ATN(1#)

    M = (N + 1) / 2
   
    XM = .5# * (XMAX + XMIN)
    XL = .5# * (XMAX - XMIN)
   
    FOR I = 1 TO M
        Z = COS(PI * (I - .25#) / (N + .5#))
      
        DO
           P1 = 1#
           P2 = 0#
         
           FOR J = 1 TO N
               P3 = P2
               P2 = P1
               P1 = ((2# * J - 1#) * Z * P2 - (J - 1#) * P3) / J
           NEXT J
        
           ' Newton's method

           PP = N * (Z * P1 - P2) / (Z * Z - 1#)
          
           Z1 = Z
           Z = Z1 - P1 / PP
         
           ' convergence check

           IF (ABS(Z - Z1) <= EPS) THEN EXIT DO
       
         LOOP

         ' abscissas

         X(I) = XM - XL * Z
         X(N + 1 - I) = XM + XL * Z
       
         ' weights

         W(I) = 2# * XL / ((1# - Z * Z) * PP * PP)
         W(N + 1 - I) = W(I)
    NEXT I

    ' evaluate integral

    S = 0#

    FOR I = 1 TO N
        CALL USERFUNC(X(I), Y)
        S = S + W(I) * Y
    NEXT I

    ERASE X, W

END SUB

