' Copyright (c) 1988 Crescent Software

DEFINT I-N
DEFDBL A-H, O-Z
                    

SUB KEYCHECK STATIC

    ' Check user response subroutine

    LOCATE 25, 1
    PRINT TAB(24); "< press any key to continue >";
    WHILE INKEY$ = "": WEND

END SUB

