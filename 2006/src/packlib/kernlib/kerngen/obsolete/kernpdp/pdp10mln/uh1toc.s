*
* $Id: uh1toc.s,v 1.1.1.1 1996/02/15 17:53:37 mclareni Exp $
*
* $Log: uh1toc.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:37  mclareni
* Kernlib
*
*
        TITLE   UH1TOC
;
; CERN PROGLIB# M409    UH1TOC          .VERSION KERNPDP  2.03  841128
; ORIG.   J. MOREELS, I.I.H.E. BRUSSELS, 6-NOV-84
;
ENTRY   UH1TOC
 
;       SUBROUTINE UH1TOC(V1,VC,NCH)
 
        EXTERN  UHTOC
 
        SIXBIT /UH1TOC/
UH1TOC: PUSH    17,16           ;SAVE ARGUMENT LIST
        MOVE    1,(16)          ;GET V1
        MOVEM   1,V1
        MOVE    1,1(16)         ;GET VC
        MOVEM   1,VC
        MOVE    1,2(16) ;GET NCH
        MOVEM   1,NCH
 
        MOVEI   16,ARGLST       ;CALL UHTOC
        PUSHJ   17,UHTOC
        POP     17,16           ;RESET ARGUMENT LIST
        POPJ    17,0            ;RETURN
 
;       ARGUMENT LIST
 
        -4,,0
V1:
ARGLST: 0
        400100,,NH
VC:     0
NCH:    0
 
;       VARIABLES
 
NH:     1
PRGEND
#ifdef CERNLIB_TCGEN_UH1TOC
#undef CERNLIB_TCGEN_UH1TOC
#endif
