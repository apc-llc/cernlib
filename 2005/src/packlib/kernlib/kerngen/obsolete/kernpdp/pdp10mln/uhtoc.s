*
* $Id: uhtoc.s,v 1.1.1.1 1996/02/15 17:53:38 mclareni Exp $
*
* $Log: uhtoc.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:38  mclareni
* Kernlib
*
*
        TITLE   UHTOC
;
; CERN PROGLIB# M409    UHTOC           .VERSION KERNPDP  2.03  841128
; ORIG.   J. MOREELS, I.I.H.E. BRUSSELS, 6-NOV-84
;
ENTRY UHTOC
 
;       SUBROUTINE UHTOC(VI,NH,VC,NCH)
 
        SIXBIT /UHTOC/
UHTOC:  MOVEI   1,@(16)         ;GET VI
        MOVEM   1,VI
        MOVE    2,@1(16)        ;GET NH
        CAILE   2,5             ;MAXIMUM 5 CHAR/WORD
        MOVEI   2,5
        MOVEM   2,NH
        MOVE    3,@3(16)        ;GET NCH
        MOVEM   3,NCH
        JUMPLE  3,END           ;RETURN IF NCH<=0
 
        MOVE    7,WRD
        MOVE    4,2(16)         ;GET VC
        LDB     4,[270400,,4]   ;GET TYPE
        CAIN    4,15
        JRST    CHAR            ;CHARACTER STRING
        MOVEI   4,@2(16)        ;GET VC
        HRLI    4,440700        ;SET-UP ILDB POINTER
        JRST    COPY
CHAR:   DMOVE   4,@2(16)        ;GET CHARACTER POINTER
 
        CAMGE   5,3
        MOVE    3,5
COPY:   ILDB    6,7             ;GET CHARACTER
        IDPB    6,4
        SOJG    2,NEXT
        MOVE    2,NH
        MOVE    7,WRD
        AOS     VI
NEXT:   SOJG    3,COPY
END:    POPJ    17,0            ;RETURN
 
;       VARIABLES
 
VI:     0
NH:     0
NCH:    0
VC:     0
WRD:    440720,,VI
PRGEND
#ifdef CERNLIB_TCGEN_UHTOC
#undef CERNLIB_TCGEN_UHTOC
#endif
