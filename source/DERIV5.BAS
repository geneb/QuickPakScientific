' Copyright (c) 1992 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FX)

SUB DERIV5 (X, TOL, DFDX, NITER) STATIC

    ' Numerical derivative subroutine   
   
    ' Input

    '  X   = X value of interest
    '  TOL = convergence criteria

    ' Output

    '  DFDX  = estimate of first derivative at X
    '  NITER = number of algorithm iterations

    ' Note: requires SUB USERFUNC

    IF (ABS(X) > 10 ^ 5) THEN
       H = X * SQR(TOL)
    ELSE
       H = SQR(TOL)
    END IF

    ' first estimate

    CALL USERFUNC(X + H, F1)
    CALL USERFUNC(X - H, F2)

    DFDX0 = (F1 - F2) / (2# * H)

    NITER = 0

    DO
       NITER = NITER + 1
      
       ' next estimate
      
       CALL USERFUNC(X - H, F1)
       CALL USERFUNC(X - .5# * H, F2)
       CALL USERFUNC(X + .5# * H, F3)
       CALL USERFUNC(X + H, F4)

       DFDX = (F1 - 8# * F2 + 8# * F3 - F4) / (6# * H)
      
       ' relative tolerance
      
       TOL1 = TOL * ABS(DFDX)
      
       IF (TOL > TOL1) THEN
          RELTOL = TOL1
       ELSE
          RELTOL = TOL
       END IF
      
       ' convergence test
      
       IF (ABS((DFDX - DFDX0) / 15#) < RELTOL) OR (NITER > 10) THEN EXIT DO
      
       ' new step size
      
       H = .5# * H
       DFDX0 = DFDX
    LOOP

END SUB
