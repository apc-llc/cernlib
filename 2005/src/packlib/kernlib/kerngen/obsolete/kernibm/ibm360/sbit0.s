*
* $Id: sbit0.s,v 1.1.1.1 1996/02/15 17:53:16 mclareni Exp $
*
* $Log: sbit0.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:16  mclareni
* Kernlib
*
*
SBIT0    CSECT
*
* CERN PROGLIB# M421    SBIT0           .VERSION KERNIBM  0.1   720321
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
SBIT0    AMODE ANY
SBIT0    RMODE ANY
#endif
         ENTRY SBIT0
         BC    15,12(15)
         DC    X'07'
         DC    CL7'SBIT0  '
         STM   14,12,12(13)
         LR    11,15
         USING SBIT0,11
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         L     2,0(1)
         L     3,0(2)
         L     4,4(1)
         L     4,0(4)
         BCTR  4,0
         L     6,FE
         L     7,FF
         SLDL  6,0(4)
         NR    3,6
         ST    3,0(2)
         L     13,SA+4
         LM    14,12,12(13)
         BCR   15,14
SA       DS    18F
FE       DC    X'FFFFFFFE'
FF       DC    X'FFFFFFFF'
         END
#ifdef CERNLIB_TCGEN_SBIT0
#undef CERNLIB_TCGEN_SBIT0
#endif
