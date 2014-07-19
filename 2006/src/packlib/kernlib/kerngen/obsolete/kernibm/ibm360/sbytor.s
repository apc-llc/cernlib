*
* $Id: sbytor.s,v 1.1.1.1 1996/02/15 17:53:16 mclareni Exp $
*
* $Log: sbytor.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:16  mclareni
* Kernlib
*
*
SBYTOR   CSECT
*
* CERN PROGLIB# M421    SBYTOR          .VERSION KERNIBM  0.1   720321
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
SBYTOR   AMODE ANY
SBYTOR   RMODE ANY
#endif
         ENTRY SBYTOR
         BC    15,12(15)
         DC    X'07'
         DC    CL7'SBYTOR '
         STM   14,12,12(13)
         LR    11,15
         USING SBYTOR,11
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         L     2,0(1)
         L     2,0(2)
         L     3,4(1)
         L     4,0(3)
         L     5,8(1)
         L     5,0(5)
         L     6,12(1)
         L     6,0(6)
         LA    7,32
         SR    7,6
         SLL   2,0(7)
         SR    7,5
         LA    7,1(7)
         SRL   2,0(7)
         OR    4,2
         ST    4,0(3)
         L     13,SA+4
         LM    14,12,12(13)
         BCR   15,14
SA       DS    18F
         END
#ifdef CERNLIB_TCGEN_SBYTOR
#undef CERNLIB_TCGEN_SBYTOR
#endif
