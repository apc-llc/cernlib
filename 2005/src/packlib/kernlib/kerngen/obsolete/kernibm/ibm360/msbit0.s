*
* $Id: msbit0.s,v 1.1.1.1 1996/02/15 17:53:15 mclareni Exp $
*
* $Log: msbit0.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:15  mclareni
* Kernlib
*
*
MSBIT0   CSECT
*
* CERN PROGLIB# M421    MSBIT0          .VERSION KERNIBM  2.14  851016
* ORIG.  SEPT 85, HRR
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
MSBIT0   AMODE ANY
MSBIT0   RMODE ANY
#endif
         ENTRY MSBIT0
         BC    15,12(15)
         DC    X'07'
         DC    CL7'MSBIT0 '
         STM   14,12,12(13)
         LR    11,15
         USING MSBIT0,11
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         L     2,0(1)
         L     0,0(2)
         L     4,4(1)
         L     4,0(4)
         BCTR  4,0
         L     6,FE
         L     7,FF
         SLDL  6,0(4)
         NR    0,6
         L     13,SA+4
         LM    14,15,12(13)
         LM    1,12,24(13)
         BCR   15,14
SA       DS    18F
FE       DC    X'FFFFFFFE'
FF       DC    X'FFFFFFFF'
         END
#ifdef CERNLIB_TCGEN_MSBIT0
#undef CERNLIB_TCGEN_MSBIT0
#endif
