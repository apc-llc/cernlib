*
* $Id: vfix.s,v 1.1.1.1 1996/02/15 17:53:45 mclareni Exp $
*
* $Log: vfix.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:45  mclareni
* Kernlib
*
*
        TITLE   VFIX
;#
; CERN PROGLIB# F121    VFIX            .VERSION KERNPDP  1.05  771031
;#
;                                                 LAST MODIF. 31/10/77
        ENTRY VFIX
#include "reentr.inc"
        EXTERN IFIX
        SEARCH HELP
        T0==0
        T1==4
        T2==2
        T3==3
        HELLO(VFIX)
        MOVE    T1,@2(16)
        JUMPE   T1,EXIT
        MOVEI   T3,@1(16)
        MOVEI   T2,@0(16)
LOOP:   MOVE    T0,(T2)
        MOVEM   T0,ARGY
        ENTER(IFIX)
        ARG     ARGY
        MOVEM   T0,(T3)
        AOS     T2
        AOS     T3
        SOJN    T1,LOOP
EXIT:   GOODBY
#include "reent1.inc"
ARGY:   0
        PRGEND
#ifdef CERNLIB_TCGEN_VFIX
#undef CERNLIB_TCGEN_VFIX
#endif
