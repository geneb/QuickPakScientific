' Copyright (c) 1991, 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB FFUN (X, FVAL)

SUB FFUN (X, FVAL) STATIC

    ' "Forcing" function subroutine

    ' y = f(x)

    ' Input

    '  X = x value in forcing function

    ' Output

    '  FVAL = forcing function value at X

    ' "pi" calculations

    PI = 4# * ATN(1#)
    PI2 = 2# * PI

    FVAL = SIN(PI * X) + 3# * SIN(PI2 * X)

END SUB

SUB HEAT (ALPHA, NX, NT, XF, TF, S()) STATIC

    ' Parabolic partial differential equation subroutine

    ' Crank-Nicolson solution of

    '    du(x,t)/dt - alpha^2 * d2u(x,t)/dt2 = 0

    '    0 < x < l,  0 < t < T
 
    ' subject to the boundary conditions

    '    u(0,t) = u(l,t) = 0   for  0 < t < T

    ' and the initial conditions

    '    u(x,0) = f(x)   for 0 <= x <= l

    ' Input

    '  ALPHA = equation constant
    '  NX    = number of spatial increments
    '  NT    = number of time increments
    '  XF    = final spatial value
    '  TF    = final time value

    ' Output

    '  S() = solution array ( NX rows by NT columns )

    ' NOTE: requires subroutine FFUN.BAS

    REDIM U(NX), W(NX), XL(NX), Z(NX)

    ' calculate spatial and time increments

    DX = XF / NX
    DT = TF / NT

    XLAMBDA = ALPHA * ALPHA * DT / (DX * DX)

    W(NX) = 0#

    ' calculate initial values

    FOR I = 1 TO (NX - 1)
        X = I * DX
        CALL FFUN(X, F)
        W(I) = F
    NEXT I

    ' solve tridiagonal linear system

    XL(1) = 1# + XLAMBDA
    U(1) = -XLAMBDA / (2# * XL(1))

    FOR I = 2 TO (NX - 2)
        XL(I) = 1# + XLAMBDA + .5# * XLAMBDA * U(I - 1)
        U(I) = -.5# * XLAMBDA / XL(I)
    NEXT I

    XL(NX - 1) = 1# + XLAMBDA + .5# * XLAMBDA * U(NX - 2)

    FOR J = 1 TO NT
        T = J * DT
        Z(1) = ((1# - XLAMBDA) * W(1) + .5# * XLAMBDA * W(2)) / XL(1)
       
        FOR I = 2 TO (NX - 1)
            Z(I) = ((1# - XLAMBDA) * W(I) + .5# * XLAMBDA * (W(I + 1) + W(I - 1) + Z(I - 1))) / XL(I)
        NEXT I

        W(NX - 1) = Z(NX - 1)
       
        FOR I = (NX - 2) TO 1 STEP -1
            W(I) = Z(I) - U(I) * W(I + 1)
        NEXT I
       
        ' load solution array

        FOR I = 1 TO (NX - 1)
            S(I, J) = W(I)
        NEXT I
    NEXT J

    ' erase working arrays

    ERASE U, W, XL, Z

END SUB

