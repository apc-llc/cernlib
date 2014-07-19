*
* $Id: ucopy2.s,v 1.1.1.1 1996/02/15 17:54:27 mclareni Exp $
*
* $Log: ucopy2.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:27  mclareni
* Kernlib
*
*
        .TITLE  UCOPY2
;
; CERN PROGLIB# V301    UCOPY2          .VERSION KERNDGE  1.01  860217
; ORIG. 03/02/86  M.BERNABE, DGC MADRID
;
        .ENT    UCOPY2
        .DUSR   ARG1 = -12.     ; POINTER TO ARG 1
        .DUSR   ARG2 = -14.     ; POINTER TO ARG 2
        .DUSR   ARG3 = -16.     ; POINTER TO ARG 3
        .NREL   1
UCOPY2:
        WSAVS   0               ; NO STACK USAGE.
        LWLDA   1,@ARG3,3       ; PICK # 32B WORDS TO BE COPIED.
        WLSI    1,1             ; CONVERT TO 16B WORDS.
        LWLDA   2,ARG1,3        ; PICK SOURCE ADDRESS.
        WMOV    3,0             ; SAVE AC3 - WFP.
        LWLDA   3,ARG2,3        ; PICK TARGET
        WSGT    3,2             ; IF T > S, THERE IS
;                                   DESTRUCTIVE OVERLAP,
;                                   AND COPY IS RIGHT TO LEFT,
;                                   COMPLEMENTING AC1,
;                                   AND POSITIONING 2 & 3
;                                   AT END OF FIELD.
        WBR     COPY            ; IF NOT, COPY AS USUAL.
        WADD    1,2             ; POSITION END SOURCE.
        WADD    1,3             ; POSITION END TARGET.
        WCOM    1,1             ; COMPLEMENT AC1 TO 1.
        WADI    1,1             ; COMPLEMENT AC1 TO 2.
COPY:
        WBLM                    ; MAKE THE MOVEMENT.
        WMOV    0,3             ; RESTORE AC3 - WFP.
        WRTN                    ; RETURN TO CALLER.
        .END
#ifdef CERNLIB_TCGEN_UCOPY2
#undef CERNLIB_TCGEN_UCOPY2
#endif
