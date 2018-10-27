' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB FFUN (X, FVAL)
DECLARE SUB GFUN (X, FVAL)

SUB FFUN (X, FVAL) STATIC

    ' Initial conditions function subroutine

    ' y = f(x)

    ' Input

    '  X = x value

    ' Output

    '  FVAL = forcing function value at X, Y

    PI = 4# * ATN(1#)

    FVAL = SIN(PI * X)

END SUB

SUB GFUN (X, FVAL) STATIC

    ' Initial conditions derivative subroutine

    ' y = du/dt(x,0)

    ' Input

    '  X = x value in derivative function

    ' Output

    '  FVAL = derivative value at X

    FVAL = 0#

END SUB

SUB WAVE (ALPHA, NX, NT, XF, TF, W()) STATIC

    ' Hyperbolic partial differential equation subroutine

    ' Finite-difference solution of

    '    d2u(x,t)/dt2 - alpha^2 * d2u(x,t)/dx2 = 0

    '    0 < x < l,  0 < t < T
 
    ' subject to the boundary conditions

    '    u(0,t) = u(l,t) = 0   for  0 < t < T

    ' and the initial conditions

    '    u(x,0) = f(x)   for  0 <= x <= l

    '    du/dt(x,0) = g(x)  for  0 <= x <= l

    ' Input

    '  ALPHA = equation constant
    '  NX    = number of spatial increments
    '  NT    = number of time increments
    '  XF    = final spatial value
    '  TF    = final time value

    ' Output

    '  W() = solution array ( NX rows by NT columns )

    ' NOTE: requires subroutines FFUN.BAS and GFUN.BAS

    ' calculate spatial and time increments

    DX = XF / NX
    DT = TF / NT

    XLAMBDA = ALPHA * DT / DX

    FOR J = 1 TO NT
        W(0, J) = 0#
        W(NX, J) = 0#
    NEXT J

    ' calculate initial values

    CALL FFUN(0#, F)
   
    W(0, 0) = F

    CALL FFUN(XF, F)
   
    W(NX, 0) = F
   
    XLAMBDA2 = XLAMBDA * XLAMBDA

    FOR I = 1 TO (NX - 1)
        X1 = I * DX
        CALL FFUN(X1, F1)
        CALL GFUN(X1, G1)
        W(I, 0) = F1
        X2 = (I + 1) * DX
        CALL FFUN(X2, F2)
        X3 = (I - 1) * DX
        CALL FFUN(X3, F3)
        W(I, 1) = (1# - XLAMBDA2) * F1 + .5# * XLAMBDA2 * (F2 + F3) + DT * G1
    NEXT I

    FOR J = 1 TO (NT - 1)
        FOR I = 1 TO (NX - 1)
            W(I, J + 1) = 2# * (1# - XLAMBDA2) * W(I, J) + XLAMBDA2 * (W(I + 1, J) + W(I - 1, J)) - W(I, J - 1)
        NEXT I
    NEXT J

END SUB

