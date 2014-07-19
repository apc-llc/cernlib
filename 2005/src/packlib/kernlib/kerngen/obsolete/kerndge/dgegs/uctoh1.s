*
* $Id: uctoh1.s,v 1.1.1.1 1996/02/15 17:54:27 mclareni Exp $
*
* $Log: uctoh1.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:27  mclareni
* Kernlib
*
*
        .TITLE  UCTOH1
;
; CERN PROGLIB# M409    UCTOH1          .VERSION KERNDGE  1.01  860217
; ORIG. 03/02/86  M.BERNABE, DGC MADRID
;
        .ENT    UCTOH1
        .DUSR   ARG1 = -12.     ; POINTER TO ARG 1
        .DUSR   ARG2 = -14.     ; POINTER TO ARG 2
        .DUSR   ARG3 = -16.     ; POINTER TO ARG 3
        .NREL   0
SVAC3:  0
INDEX:  0
NUMBT:  0
        .NREL   1
UCTOH1:
        WSAVS   0               ; NO STACK USAGE.
        LWSTA   3,SVAC3         ; SAVE AC3 - WFP.
        LWLDA   0,@ARG3,3       ; PICK # 32B WORDS IN TARGET,
;                                    = # BYTES IN SOURCE.
        LWSTA   0,NUMBT         ; SAVE THAT NUMBER.
        WLSI    2,0             ; CONVERT TO BYTES.
        LWSTA   0,INDEX         ; SAVE # BYTES IN TARGET.
        WSUB    1,1             ; CLEAR 1 TO MOVE SPACES.
        LWLDA   2,ARG2,3        ; PICK TARGET ADDRESS.
        WLSI    1,2             ; CONVERT TO BYTEADDR.
        LWLDA   3,ARG1,3        ; PICK SOURCE ADDRESS.
        WMOV    3,0             ; CHECK FIRST BIT TO SEE IF IT IS
        WSKBO   0               ; A BYTEADDR -A ONE-.
        WLSI    1,3             ; IF NOT, CONVERT TO BYTEADDR.
        LWLDA   0,INDEX         ; RESTORE # BYTES TO CLEAN.
        WCMV                    ; CLEAN TARGET TO SPACES.
        LWSUB   2,INDEX         ; RESTORE TARGET BYTEADDR.
        LWSTA   1,INDEX         ; CLEAR LOOP INDEX.
        LWLDA   0,NUMBT         ; RESTORE # BYTES IN SOURCE.
MLOOP:
        LWDO    0,ENDRT,INDEX   ; LOOP. IF END, END ROUTINE.
        WLDB    3,1             ; PICK BYTE TO BE MLOOPD.
        WSTB    2,1             ; STORE IN TARGET.
        WADI    1,3             ; PICK NEXT SOURCE BYTE,
        WADI    4,2             ; NEXT TARGET 32B WORD,
        LWLDA   0,NUMBT         ; RESET LOOP LIMIT.
        WBR     MLOOP           ; AND CONTINUE THE LOOP.
ENDRT:
        LWLDA   3,SVAC3         ; RESTORE AC3 - WFP.
        WRTN                    ; RETURN TO CALLER.
        .END
#ifdef CERNLIB_TCGEN_UCTOH1
#undef CERNLIB_TCGEN_UCTOH1
#endif
