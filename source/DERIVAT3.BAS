' Copyright (c) 1989, 1990 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

SUB DERIVATIVE (T, Y(), YDOT()) STATIC
   
    ' System of differential equations subroutine

    '   Y1' = -4 * Y(1) - 2 * Y(2) + COS(T) + 4 SIN(T)
    '   Y2' =  3 * Y(1) + Y(2) - 3 SIN(T)

    ' Input

    '  T   = integration time
    '  Y() = vector of integration variables ( 2 rows )

    ' Output

    '  YDOT() = vector of differential equations ( 2 rows )

   YDOT(1) = -4# * Y(1) - 2# * Y(2) + COS(T) + 4# * SIN(T)
  
   YDOT(2) = 3# * Y(1) + Y(2) - 3# * SIN(T)

END SUB

