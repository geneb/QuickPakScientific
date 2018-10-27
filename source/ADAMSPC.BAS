' Copyright (c) 1989, 1990, 1993 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB DERIVATIVE (T, Y(), YDOT())
DECLARE SUB RKF56 (NEQ, TI, TF, H, TOL, Y())

SUB ADAMSPC (INIZ, NEQ, NORD, TIME, DT, Y()) STATIC

    ' Adams-Bashforth/Moulton predictor-corrector subroutine
   
    ' Runge-Kutta-Fehlberg 5(6) starter

    ' Input

    '  INIZ = initialization indicator
    '         ( 1 = first pass, 0 = all others )
    '  NEQ  = number of equations in system
    '  NORD = order of integration
    '  TIME = initial simulation time
    '  DT   = integration step size
    '  Y()  = initial integration vector ( NEQ rows )

    ' Output

    '  TIME = simulation time = TIME + DT
    '  Y()  = integration vector at time = TIME + DT ( NEQ rows )

    ' NOTE: requires SUB RKF56 and SUB DERIVATIVE

    STATIC BB, BBS, G, GG, FF

    REDIM Z(NEQ), YTEMP(NEQ), SUM(NEQ)
    
    IF (INIZ = 1) THEN
       ' perform initialization
      
       REDIM BB(NORD), BBS(NORD), G(NORD), GG(NORD)
       REDIM P(NORD, NORD), FF(NEQ, NORD)

       ' compute Adams P-C coefficients

       FOR J = 1 TO NORD
           BB(J) = 0#
           BBS(J) = 0#
           P(1, J) = 1#
       NEXT J
 
       FOR I = 2 TO NORD
           FOR J = 1 TO NORD
               P(I, J) = -P(I - 1, J) * (J - I + 1#) / (I - 1#)
           NEXT J
       NEXT I
 
       G(1) = 1#
       GG(1) = 1#
 
       FOR I = 2 TO NORD
           G(I) = 1#
           GG(I) = 0#
           FOR J = 2 TO I
               G(I) = G(I) - G(I - J + 1) / J
               GG(I) = GG(I) - GG(I - J + 1) / J
           NEXT J
       NEXT I
 
       FOR M = 1 TO NORD
           FOR J = M TO NORD
               BB(M) = BB(M) + P(M, J) * G(J)
               BBS(M) = BBS(M) + P(M, J) * GG(J)
           NEXT J
       NEXT M

       ERASE P

       ' Runge-Kutta-Fehlberg 5(6) starter

       CALL DERIVATIVE(TIME, Y(), Z())

       FOR N = 1 TO NEQ
           FF(N, 1) = Z(N)
       NEXT N
 
       FOR M = 2 TO NORD
           CALL RKF56(NEQ, TIME, TIME + DT, DT, 1D+99, Y())
           CALL DERIVATIVE(TIME, Y(), Z())
       
           FOR N = 1 TO NEQ
               FF(N, M) = Z(N)
           NEXT N
       
       NEXT M
  
       EXIT SUB
    END IF
   
    ' prediction, shift and correction

    FOR N = 1 TO NEQ
        ZZ = 0#
        FOR J = 1 TO NORD
            ZZ = ZZ + BB(J) * FF(N, NORD - J + 1)
        NEXT J
       
        YTEMP(N) = Y(N)
        Y(N) = YTEMP(N) + DT * ZZ
    NEXT N

    FOR N = 1 TO NEQ
        FOR J = 1 TO NORD - 1
            FF(N, J) = FF(N, J + 1)
        NEXT J
    NEXT N

    TIME = TIME + DT
 
    FOR N = 1 TO NEQ
        SUM(N) = 0#
        FOR J = 2 TO NORD
            SUM(N) = SUM(N) + BBS(J) * FF(N, NORD - J + 1)
        NEXT J
        SUM(N) = YTEMP(N) + DT * SUM(N)
    NEXT N
 
    CALL DERIVATIVE(TIME, Y(), Z())

    FOR N = 1 TO NEQ
        Y(N) = SUM(N) + DT * BBS(1) * Z(N)
        FF(N, NORD) = Z(N)
    NEXT N

    ERASE Z, YTEMP, SUM

END SUB

