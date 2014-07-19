*
* $Id: mbytor.s,v 1.1.1.1 1996/02/15 17:53:15 mclareni Exp $
*
* $Log: mbytor.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:15  mclareni
* Kernlib
*
*
MBYTOR   CSECT
*
* CERN PROGLIB# M421    MBYTOR          .VERSION KERNIBM  2.14  851016
* ORIG.  SEPT 85, HRR
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
MBYTOR   AMODE ANY
MBYTOR   RMODE ANY
#endif
         ENTRY MBYTOR
         BC    15,12(15)
         DC    X'07'
         DC    CL7'MBYTOR '
         STM   14,12,12(13)
         LR    11,15
         USING MBYTOR,11
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         L     2,0(1)
         L     2,0(2)
         L     3,4(1)
         L     0,0(3)
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
         OR    0,2
         L     13,SA+4
         LM    14,15,12(13)
         LM    1,12,24(13)
         BCR   15,14
SA       DS    18F
         END
#ifdef CERNLIB_TCGEN_MBYTOR
#undef CERNLIB_TCGEN_MBYTOR
#endif
