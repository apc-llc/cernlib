*
* $Id: jbytet.s,v 1.1.1.1 1996/02/15 17:53:14 mclareni Exp $
*
* $Log: jbytet.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:14  mclareni
* Kernlib
*
*
JBYTET   CSECT
*
* CERN PROGLIB# M421    JBYTET          .VERSION KERNIBM  0.1   720321
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
JBYTET   AMODE ANY
JBYTET   RMODE ANY
#endif
         ENTRY JBYTET
         BC    15,12(15)
         DC    X'07'
         DC    CL7'JBYTET '
         STM   14,12,12(13)
         LR    11,15
         USING JBYTET,11
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         L     2,0(1)
         L     0,0(2)
         L     3,4(1)
         L     3,0(3)
         L     4,8(1)
         L     4,0(4)
         L     5,12(1)
         L     5,0(5)
         BCTR  4,0
         SRL   3,0(4)
         LA    6,32
         SR    6,5
         SLL   0,0(6)
         SRL   0,0(6)
         NR    0,3
         L     13,SA+4
         LM    1,12,24(13)
         BCR   15,14
SA       DS    18F
         END
#ifdef CERNLIB_TCGEN_JBYTET
#undef CERNLIB_TCGEN_JBYTET
#endif
