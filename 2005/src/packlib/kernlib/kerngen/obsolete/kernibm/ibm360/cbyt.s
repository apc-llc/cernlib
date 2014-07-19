*
* $Id: cbyt.s,v 1.1.1.1 1996/02/15 17:53:11 mclareni Exp $
*
* $Log: cbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:11  mclareni
* Kernlib
*
*
CBYT     CSECT
*
* CERN PROGLIB# M421    CBYT            .VERSION KERNIBM  0.1   720321
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
CBYT     AMODE ANY
CBYT     RMODE ANY
#endif
         ENTRY CBYT
         BC    15,12(15)
         DC    X'07'
         DC    CL7'CBYT   '
         STM   14,12,12(13)
         LR    11,15
         USING CBYT,11
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         L     2,0(1)
         L     2,0(2)
         L     3,4(1)
         L     3,0(3)
         BCTR  3,0
         SRL   2,0(3)
         L     3,8(1)
         L     4,0(3)
         L     5,12(1)
         L     5,0(5)
         BCTR  5,0
         L     6,16(1)
         L     6,0(6)
         LA    7,STOREC
STOREC   STC   2,BYTE
         TM    BYTE,X'01'
         BC    8,ZERO
         LA    8,1
         SLL   8,0(5)
         OR    4,8
         B     TESTR6
ZERO     L     12,FE
         L     13,FF
         SLDL  12,0(5)
         NR    4,12
TESTR6   BCT   6,*+8
         B     STOREX
         LA    5,1(5)
         SRL   2,1
         BCR   15,7
STOREX   ST    4,0(3)
         L     13,SA+4
         LM    14,12,12(13)
         BCR   15,14
SA       DS    18F
FF       DC    X'FFFFFFFF'
FE       DC    X'FFFFFFFE'
BYTE     DS    C
         END
#ifdef CERNLIB_TCGEN_CBYT
#undef CERNLIB_TCGEN_CBYT
#endif
