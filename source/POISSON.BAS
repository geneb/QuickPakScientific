' Copyright (c) 1991, 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB FFUN (X, Y, FVAL)
DECLARE SUB GFUN (X, Y, GVAL)

SUB FFUN (X, Y, FVAL) STATIC

    ' "Forcing" function subroutine

    ' z = f(x,y) = x*exp(y)

    ' Input

    '  X = x value in forcing function
    '  Y = y value in forcing function

    ' Output

    '  FVAL = forcing function value at X, Y

    FVAL = X * EXP(Y)

END SUB

SUB GFUN (X, Y, GVAL) STATIC

    ' Boundary conditions subroutine

    ' z = g(x,y) = 0

    ' Input
     
    '  X = x boundary value
    '  Y = y boundary value

    ' Output

    '  GVAL = boundary condition at X, Y

    IF (X = 0#) THEN
       GVAL = 0#
    ELSEIF (X = 2#) THEN
       GVAL = 2# * EXP(Y)
    ELSEIF (Y = 0#) THEN
       GVAL = X
    ELSE
       GVAL = EXP(1#) * X
    END IF

END SUB

SUB POISSON (A, B, C, D, NX, NY, TOL, NITER, X(), Y(), W()) STATIC

    ' Elliptic partial differential equation subroutine

    ' Finite-difference solution of

    '    d2u(x,y)/dx2 + d2u(x,y)/dy2 = f(x,y)

    ' subject to the boundary conditions
 
    '    u(x,y) = g(x,y)   if x = a or x = b and c <= y <= d
 
    '    u(x,y) = g(x,y)   if y = c or y = d and a <= x <= b    

    ' Input

    '  A     = x initial endpoint
    '  B     = x final endpoint
    '  C     = y initial endpoint
    '  D     = y final endpoint
    '  NX    = number of x grid lines
    '  NY    = number of y grid lines
    '  TOL   = convergence tolerance
    '  NITER = input as maximum number of iterations allowed;
    '          output as actual number of iterations required.

    ' Output

    '  X() = vector of mesh point x values ( NX - 1 rows )
    '  Y() = vector of mesh point y values ( NY - 1 rows )
    '  W() = array of solution values ( NX-1 rows by NY-1 columns)

    ' NOTE: requires subroutines FFUN.BAS and GFUN.BAS

    DX = (B - A) / NX
    DY = (D - C) / NY

    ' construct mesh points

    FOR I = 1 TO (NX - 1)
        X(I) = A + I * DX
    NEXT I

    FOR J = 1 TO (NY - 1)
        Y(J) = C + J * DY
    NEXT J

    ' initialize solution array

    FOR I = 1 TO (NX - 1)
        FOR J = 1 TO (NY - 1)
            W(I, J) = 0#
        NEXT J
    NEXT I

    ' preliminary calculations

    XLAMBDA = DX * DX / (DY * DY)

    XMU = 2# * (1# + XLAMBDA)

    L = 1

    WHILE (L <= NITER)
       ' perform Gauss-Seidel iteration
      
       CALL FFUN(X(1), Y(NY - 1), F1)
       CALL GFUN(A, Y(NY - 1), G1)
       CALL GFUN(X(1), D, G2)

       Z = (-DX * DX * F1 + G1 + XLAMBDA * G2 + XLAMBDA * W(1, NY - 2) + W(2, NY - 1)) / XMU

       XNORM = ABS(Z - W(1, NY - 1))

       W(1, NY - 1) = Z

       FOR I = 2 TO (NX - 2)
           CALL FFUN(X(I), Y(NY - 1), F1)
           CALL GFUN(X(I), D, G1)

           Z = (-DX * DX * F1 + XLAMBDA * G1 + W(I - 1, NY - 1) + W(I + 1, NY - 1) + XLAMBDA * W(I, NY - 2)) / XMU

           IF (ABS(W(I, NY - 1) - Z) > XNORM) THEN
              XNORM = ABS(W(I, NY - 1) - Z)
           END IF

           W(I, NY - 1) = Z
       NEXT I

       CALL FFUN(X(NX - 1), Y(NY - 1), F1)
       CALL GFUN(B, Y(NY - 1), G1)
       CALL GFUN(X(NX - 1), D, G2)

       Z = (-DX * DX * F1 + G1 + XLAMBDA * G2 + W(NX - 2, NY - 1) + XLAMBDA * W(NX - 1, NY - 2)) / XMU

       IF (ABS(W(NX - 1, NY - 1) - Z) > XNORM) THEN
          XNORM = ABS(W(NX - 1, NY - 1) - Z)
       END IF

       W(NX - 1, NY - 1) = Z

       FOR J = (NY - 2) TO 2 STEP -1
           CALL FFUN(X(1), Y(J), F1)
           CALL GFUN(A, Y(J), G1)

           Z = (-DX * DX * F1 + G1 + XLAMBDA * W(1, J + 1) + XLAMBDA * W(1, J - 1) + W(2, J)) / XMU

           IF (ABS(W(1, J) - Z) > XNORM) THEN
              XNORM = ABS(W(1, J) - Z)
           END IF

           W(1, J) = Z

           FOR I = 2 TO (NX - 2)
               CALL FFUN(X(I), Y(J), F1)
               Z = (-DX * DX * F1 + W(I - 1, J) + XLAMBDA * W(I, J + 1) + W(I + 1, J) + XLAMBDA * W(I, J - 1)) / XMU
              
               IF (ABS(W(I, J) - Z) > XNORM) THEN
                  XNORM = ABS(W(I, J) - Z)
               END IF

               W(I, J) = Z
           NEXT I

           CALL FFUN(X(NX - 1), Y(J), F1)
           CALL GFUN(B, Y(J), G1)

           Z = (-DX * DX * F1 + G1 + W(NX - 2, J) + XLAMBDA * W(NX - 1, J + 1) + XLAMBDA * W(NX - 1, J - 1)) / XMU

           IF (ABS(W(NX - 1, J) - Z) > XNORM) THEN
              XNORM = ABS(W(NX - 1, J) - Z)
           END IF

           W(NX - 1, J) = Z
       NEXT J

       CALL FFUN(X(1), Y(1), F1)
       CALL GFUN(A, Y(1), G1)
       CALL GFUN(X(1), C, G2)

       Z = (-DX * DX * F1 + G1 + XLAMBDA * G2 + XLAMBDA * W(1, 2) + W(2, 1)) / XMU

       IF (ABS(W(1, 1) - Z) > XNORM) THEN
          XNORM = ABS(W(1, 1) - Z)
       END IF

       W(1, 1) = Z

       FOR I = 2 TO (NX - 2)
           CALL FFUN(X(I), Y(1), F1)
           CALL GFUN(X(I), C, G1)

           Z = (-DX * DX * F1 + XLAMBDA * G1 + W(I - 1, 1) + XLAMBDA * W(I, 2) + W(I + 1, 1)) / XMU

           IF (ABS(W(I, 1) - Z) > XNORM) THEN
              XNORM = ABS(W(I, 1) - Z)
           END IF

           W(I, 1) = Z
       NEXT I

       CALL FFUN(X(NX - 1), Y(1), F1)
       CALL GFUN(B, Y(1), G1)
       CALL GFUN(X(NX - 1), C, G2)

       Z = (-DX * DX * F1 + G1 + XLAMBDA * G2 + W(NX - 2, 1) + XLAMBDA * W(NX - 1, 2)) / XMU

       IF (ABS(W(NX - 1, 1) - Z) > XNORM) THEN
          XNORM = ABS(W(NX - 1, 1) - Z)
       END IF

       W(NX - 1, 1) = Z

       ' check for convergence

       IF (XNORM <= TOL) THEN
          NITER = L
          EXIT SUB
       ELSE
          L = L + 1
       END IF

    WEND

    NITER = L

END SUB

