' Copyright (c) 1991 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z

DECLARE SUB USERFUNC (X, FX)

SUB ASIMPSON (A, B, ACC, SUM, ESTERR, IFLAG) STATIC

    ' Adaptive Simpson integration subroutine

    ' Input

    '  A   = lower integration limit
    '  B   = upper integration limit
    '  ACC = accuracy
    
    ' Output

    '  SUM    = integral from A to B
    '  ESTERR = relative error
    '  IFLAG  = error flag
    '       1 => no error
    '       2 => more than 30 levels
    '       3 => subinterval too small
    '       4 => more than 2000 function evaluations

    ' NOTE: requires subroutine USERFUNC.BAS

    DIM LORR(30), F1T(30), F2T(30), F3T(30), DAT(30)
    DIM ARESTT(30), ESTT(30), EPST(30), PSUM(30)

    U = 2.23D-308

    ' initialize

    FOURU = 4# * U
    IFLAG = 1
    EPS = ACC
    XERROR = 0#
    LVL = 1
    LORR(LVL) = 1
    PSUM(LVL) = 0#
    ALPHA = A
    DA = B - A
    AREA = 0#
    AREST = 0#
   
    CALL USERFUNC(ALPHA, FV1)
    CALL USERFUNC(ALPHA + .5# * DA, FV3)
    CALL USERFUNC(ALPHA + DA, FV5)
   
    KOUNT = 3
   
    WT = DA / 6#
    EST = WT * (FV1 + 4# * FV3 + FV5)

    ' begin adaptive procedure
   
    DO
       DO
          DX = .5# * DA
   
          CALL USERFUNC(ALPHA + .5# * DX, FV2)
          CALL USERFUNC(ALPHA + 1.5# * DX, FV4)

          KOUNT = KOUNT + 2
   
          WT = DX / 6#
   
          ESTL = WT * (FV1 + 4# * FV2 + FV3)
          ESTR = WT * (FV3 + 4# * FV4 + FV5)

          SUM = ESTL + ESTR

          ARESTL = WT * (ABS(FV1) + ABS(4# * FV2) + ABS(FV3))
          ARESTR = WT * (ABS(FV3) + ABS(4# * FV4) + ABS(FV5))

          AREA = AREA + ARESTL + ARESTR - AREST

          DIFF = EST - SUM
    
          IF (ABS(DIFF) <= EPS * ABS(AREA)) THEN
             EXIT DO
          ELSEIF (ABS(DX) <= FOURU * ABS(ALPHA)) THEN
             IFLAG = 2
             EXIT DO
          ELSEIF (LVL >= 30) THEN
             IFLAG = 3
             EXIT DO
          ELSEIF (KOUNT >= 2000) THEN
             IFLAG = 4
             EXIT DO
          ELSE
             LVL = LVL + 1
             LORR(LVL) = 0
      
             F1T(LVL) = FV3
             F2T(LVL) = FV4
             F3T(LVL) = FV5
      
             DA = DX
             DAT(LVL) = DX
     
             AREST = ARESTL
             ARESTT(LVL) = ARESTR
             EST = ESTL
             ESTT(LVL) = ESTR
             EPS = EPS / 1.4#
             EPST(LVL) = EPS
             FV5 = FV3
             FV3 = FV2
          END IF
   
       LOOP
   
       XERROR = XERROR + DIFF / 15#
    
       DO
          IF (LORR(LVL) = 0) THEN
             PSUM(LVL) = SUM
             LORR(LVL) = 1
             ALPHA = ALPHA + DA
             DA = DAT(LVL)
             FV1 = F1T(LVL)
             FV3 = F2T(LVL)
             FV5 = F3T(LVL)
             AREST = ARESTT(LVL)
             EST = ESTT(LVL)
             EPS = EPST(LVL)
             EXIT DO
          ELSE
             SUM = PSUM(LVL) + SUM
             LVL = LVL - 1
             IF (LVL = 0) THEN EXIT DO
          END IF
       LOOP
   
       IF (LVL = 0) THEN EXIT DO

    LOOP
  
    ESTERR = ABS(XERROR) / ABS(SUM)

    ERASE LORR, F1T, F2T, F3T, DAT, ARESTT, ESTT, EPST, PSUM

END SUB

