*
* $Id: mbytet.s,v 1.1.1.1 1996/02/15 17:53:15 mclareni Exp $
*
* $Log: mbytet.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:15  mclareni
* Kernlib
*
*
MBYTET   CSECT
*
* CERN PROGLIB# M421    MBYTET          .VERSION KERNIBM  2.14  851016
* ORIG.  SEPT 85, HRR
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
MBYTET   AMODE ANY
MBYTET   RMODE ANY
#endif
         ENTRY MBYTET
         BC    15,12(15)
         DC    X'07'
         DC    CL7'MBYTET '
         STM   14,12,12(13)
         LR    11,15
         USING MBYTET,11
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
         L     4,FF
         SLL   4,0(7)
         SR    7,5
         LA    7,1(7)
         SRL   2,0(7)
         SRL   4,0(7)
         X     4,FF
         OR    2,4
         NR    0,2
         L     13,SA+4
         LM    14,15,12(13)
         LM    1,12,24(13)
         BCR   15,14
SA       DS    18F
FF       DC    X'FFFFFFFF'
         END
#ifdef CERNLIB_TCGEN_MBYTET
#undef CERNLIB_TCGEN_MBYTET
#endif
