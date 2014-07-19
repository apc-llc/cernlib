*
* $Id: cbyt.s,v 1.1.1.1 1996/02/15 17:54:36 mclareni Exp $
*
* $Log: cbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:36  mclareni
* Kernlib
*
*
CBYT     CSECT
*
* CERN PROGLIB# M421    CBYT            .VERSION KERNIBX  1.01  900523
*
         USING *,15
         ENTRY _cbyt_
_cbyt_   STM   2,12,16(13)
         LR    12,13              ... added for AIX
         USING STACK,13
         S     13,=A(STEND-STACK) ... added for AIX
         ST    12,4(0,13)         ... added for AIX
         L     6,88(,12)          AIX: address of arg5
         L     5,0(,3)            AIX: value of arg4
         LR    12,2               AIX: address of arg3
         LR    2,0                AIX: address of arg1
         L     2,0(2)             AIX: value of arg1
         L     3,0(,1)            AIX: value of arg2
         BCTR  3,0
         SRL   2,0(3)
         L     4,0(,12)           AIX: value of arg3
         BCTR  5,0
         L     6,0(6)
         LA    7,STOREC
STOREC   STC   2,BYTE
         TM    BYTE,X'01'
         BC    8,ZERO
         LA    8,1
         SLL   8,0(5)
         OR    4,8
         B     TESTR6
ZERO     L     10,FE
         L     11,FF
         SLDL  10,0(5)
         NR    4,10
TESTR6   BCT   6,*+8
         B     STOREX
         LA    5,1(5)
         SRL   2,1
         BCR   15,7
STOREX   ST    4,0(,12)         return arg3
         L     13,4(0,13)
         LM    2,12,16(13)
         BCR   15,14
FF       DC    X'FFFFFFFF'
FE       DC    X'FFFFFFFE'
         LTORG
STACK    DSECT
SAVEAREA DS    18F
AIXOUT   DS    4F
LINKR23  DS    2F
BYTE     DS    C
STEND    DS    0F
         END
#ifdef CERNLIB_TCGEN_CBYT
#undef CERNLIB_TCGEN_CBYT
#endif
