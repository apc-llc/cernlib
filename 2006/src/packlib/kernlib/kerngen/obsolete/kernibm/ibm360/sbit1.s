*
* $Id: sbit1.s,v 1.1.1.1 1996/02/15 17:53:16 mclareni Exp $
*
* $Log: sbit1.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:16  mclareni
* Kernlib
*
*
SBIT1    CSECT
*
* CERN PROGLIB# M421    SBIT1           .VERSION KERNIBM  0.1   720321
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
SBIT1    AMODE ANY
SBIT1    RMODE ANY
#endif
         ENTRY SBIT1
         BC    15,12(15)
         DC    X'07'
         DC    CL7'SBIT1  '
         STM   14,12,12(13)
         LR    11,15
         USING SBIT1,11
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         L     2,0(1)
         L     3,0(2)
         L     4,4(1)
         L     4,0(4)
         BCTR  4,0
         LA    5,1
         SLL   5,0(4)
         OR    3,5
         ST    3,0(2)
         L     13,SA+4
         LM    14,12,12(13)
         BCR   15,14
SA       DS    18F
         END
#ifdef CERNLIB_TCGEN_SBIT1
#undef CERNLIB_TCGEN_SBIT1
#endif
