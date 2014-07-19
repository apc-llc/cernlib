*
* $Id: sbit.s,v 1.1.1.1 1996/02/15 17:53:44 mclareni Exp $
*
* $Log: sbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:44  mclareni
* Kernlib
*
*
        TITLE SBIT  (A,X,LX)
;#
; CERN PROGLIB# M421    SBIT            .VERSION KERNPDP  1.00  750903
;#
        ;COPIES BIT 1 FROM A INTO
        ;BIT LX OF X
        TEMP= 1
        BIT= 2
        IPT=3
        SEARCH HELP
#include "reentr.inc"
        HELLO( SBIT)
        MOVE    BIT,@(16)
        HRRZI   IPT,@1(16)
        MOVE    TEMP,@2(16)
ENTR:   SUBI    TEMP,1
        HRLI    IPT,100
        DPB     TEMP,[POINT 6,IPT,5]
        DPB     BIT,IPT
        GOODBY
        HELLO( SBIT1)
        MOVE    TEMP,@1(16)
        HRRZI   IPT,@(16)
        MOVEI   BIT,1
        JUMPA   ENTR
        HELLO( SBIT0)
        HRRZI   IPT,@(16)
        MOVE    TEMP,@1(16)
        SETZ    BIT,
        JUMPA   ENTR
        PRGEND
#ifdef CERNLIB_TCGEN_SBIT
#undef CERNLIB_TCGEN_SBIT
#endif
#ifdef CERNLIB_TCGEN_SBIT0
#undef CERNLIB_TCGEN_SBIT0
#endif
#ifdef CERNLIB_TCGEN_SBIT1
#undef CERNLIB_TCGEN_SBIT1
#endif
