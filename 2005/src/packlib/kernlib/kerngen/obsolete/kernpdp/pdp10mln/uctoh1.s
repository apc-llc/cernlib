*
* $Id: uctoh1.s,v 1.1.1.1 1996/02/15 17:53:37 mclareni Exp $
*
* $Log: uctoh1.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:37  mclareni
* Kernlib
*
*
        TITLE   UCTOH1
;
; CERN PROGLIB# M409    UCTOH1          .VERSION KERNPDP  2.03  841128
; ORIG.   J. MOREELS, I.I.H.E. BRUSSELS, 6-NOV-84
;
ENTRY   UCTOH1
 
;       SUBROUTINE UCTOH1(VC,V1,NCH)
 
        EXTERN  UCTOH
 
        SIXBIT /UCTOH1/
UCTOH1: PUSH    17,16           ;SAVE ARGUMENT LIST
        MOVE    1,(16)          ;GET VC
        MOVEM   1,ARGLST
        MOVE    1,1(16)         ;GET V1
        MOVEM   1,V1
        MOVE    1,2(16)         ;GET NCH
        MOVEM   1,NCH
        MOVEI   16,ARGLST       ;CALL UCTOH
        PUSHJ   17,UCTOH
        POP     17,16           ;RESET ARGUMENT LIST
        POPJ    17,0            ;RETURN
 
;       ARGUMENT LIST
 
        -4,,0
ARGLST: 0
V1:     0
        400100,,NH
NCH:    0
 
;       VARIABLES
 
NH:     1
PRGEND
#ifdef CERNLIB_TCGEN_UCTOH1
#undef CERNLIB_TCGEN_UCTOH1
#endif
