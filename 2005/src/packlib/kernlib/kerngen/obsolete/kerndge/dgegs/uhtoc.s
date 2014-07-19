*
* $Id: uhtoc.s,v 1.1.1.1 1996/02/15 17:54:27 mclareni Exp $
*
* $Log: uhtoc.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:27  mclareni
* Kernlib
*
*
        .TITLE  UHTOC
;
; CERN PROGLIB# M409    UHTOC           .VERSION KERNDGE  1.01  860217
; ORIG. 03/02/86  M.BERNABE, DGC MADRID
;
        .ENT    UHTOC
        .DUSR   ARG1 = -12.     ; POINTER TO ARG 1
        .DUSR   ARG2 = -14.     ; POINTER TO ARG 2
        .DUSR   ARG3 = -16.     ; POINTER TO ARG 3
        .DUSR   ARG4 = -18.     ; POINTER TO ARG 4
        .NREL   0
SVAC3:  0
NUMBT:  0
NUMBP:  0
INDEX:  0
INDEX1: 0
SRCE:   0
        .NREL   1
UHTOC:
        WSAVS   0               ; NO STACK USAGE.
        LWSTA   3,SVAC3         ; SAVE AC3 - WFP.
        LWLDA   0,@ARG2,3       ; PICK # BYTES IN A PACK.
        LWSTA   0,NUMBP         ; SAVE THAT NUMBER.
        LWLDA   0,@ARG4,3       ; PICK # BYTES IN TARGET.
        LWSTA   0,NUMBT         ; SAVE THAT NUMBER.
        WSUB    1,1             ; CLEAR 1 TO MOVE SPACES.
        LWLDA   2,ARG3,3        ; PICK TARGET ADDRESS.
        WMOV    2,0             ; CHECK FIRST BIT TO SEE IF IT IS
        WSKBO   0               ; A BYTEADDR -A ONE-.
        WLSI    1,2             ; IF NOT, CONVERT TO BYTEADDR.
        LWLDA   3,ARG1,3        ; PICK SOURCE ADDRESS.
        WLSI    1,3             ; CONVERT TO BYTEADDR.
        LWSTA   3,SRCE          ; KEEP SOURCE BYTEADDR.
        LWLDA   0,INDEX         ; RESTORE # BYTES TO CLEAN.
        WCMV                    ; CLEAN TARGET TO SPACES.
        LWLDA   0,NUMBT         ; PICK # BYTES IN TARGET.
        LWSUB   2,INDEX         ; RESTORE TARGET ADDRESS.
        LWLDA   3,SRCE          ; PICK SOURCE ADDRESS.
        LWSTA   1,INDEX         ; CLEAR MAIN INDEX.
        LWSTA   1,INDEX1        ; CLEAR PACK INDEX.
MLOOP:
        LWLDA   0,NUMBT         ; RESTORE # BYTES IN SOURCE.
        LWDO    0,ENDRT,INDEX   ; LOOP. IF END, END ROUTINE.
PLOOP:
        LWLDA   0,NUMBP         ; RESTORE # BYTES IN A PACK.
        LWDO    0,NEXT,INDEX1   ; LOOP. IF END, NEXT PACK.
        WLDB    3,1             ; PICK BYTE TO BE MOVED.
        WSTB    2,1             ; STORE IN TARGET.
        WADI    1,3             ; PICK NEXT PACK BYTE.
        WADI    1,2             ; PICK NEXT TARGET BYTE.
        WBR     MLOOP           ; CONTINUE MAIN LOOP.
NEXT:
        WSUB    1,1             ; CLEAR AC1 TO RESET INDEX.
        LWSTA   1,INDEX1        ; RESET PACK INDEX.
        WADI    4,3             ; PICK NEXT 32B WORD,
        LWSUB   3,NUMBP         ; - # BYTES IN A PACK
;                                   SO, PICK NEXT PACK.
        WBR     PLOOP           ; CONTINUE PACK LOOP.
ENDRT:
        LWLDA   3,SVAC3         ; RESTORE AC3 - WFP.
        WRTN                    ; RETURN TO CALLER.
        .END
#ifdef CERNLIB_TCGEN_UHTOC
#undef CERNLIB_TCGEN_UHTOC
#endif
