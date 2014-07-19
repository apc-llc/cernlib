*
* $Id: msbit1.s,v 1.1.1.1 1996/02/15 17:53:15 mclareni Exp $
*
* $Log: msbit1.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:15  mclareni
* Kernlib
*
*
MSBIT1   CSECT
*
* CERN PROGLIB# M421    MSBIT1          .VERSION KERNIBM  2.14  851016
* ORIG.  SEPT 85, HRR
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
MSBIT1   AMODE ANY
MSBIT1   RMODE ANY
#endif
         ENTRY MSBIT1
         BC    15,12(15)
         DC    X'07'
         DC    CL7'MSBIT1 '
         STM   14,12,12(13)
         LR    11,15
         USING MSBIT1,11
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         L     2,0(1)
         L     0,0(2)
         L     4,4(1)
         L     4,0(4)
         BCTR  4,0
         LA    5,1
         SLL   5,0(4)
         OR    0,5
         L     13,SA+4
         LM    14,15,12(13)
         LM    1,12,24(13)
         BCR   15,14
SA       DS    18F
         END
#ifdef CERNLIB_TCGEN_MSBIT1
#undef CERNLIB_TCGEN_MSBIT1
#endif
