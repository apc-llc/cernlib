*
* $Id: mcbyt.s,v 1.1.1.1 1996/02/15 17:54:40 mclareni Exp $
*
* $Log: mcbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:40  mclareni
* Kernlib
*
*
MCBYT    CSECT
*
* CERN PROGLIB# M421    MCBYT           .VERSION KERNIBX  1.01  900524
* ORIG.  SEPT 85, HRR
* Modified for AIX, Roger Howard, January 1990
*
         USING *,15
         ENTRY _mcbyt_
_mcbyt_  STM   2,1,16(13)
         L     6,88(0,13)           AIX: address of arg5
         L     6,0(,6)              AIX: value of arg5
         LR    4,13                 ... added for AIX
         USING STACK,13
         S     13,=A(STEND-STACK)   ... added for AIX
         ST    4,4(0,13)            ... added for AIX
         L     5,0(,3)              AIX: value of arg4
         L     3,0(,1)              AIX: value of arg2
         LR    1,0                  AIX: address of arg1
         L     0,0(,2)              AIX: value of arg3
         L     2,0(,1)              AIX: value of arg1
         BCTR  3,0
         SRL   2,0(3)
         BCTR  5,0
         LA    7,STOREC
STOREC   STC   2,BYTE
         TM    BYTE,X'01'
         BC    8,ZERO
         LA    8,1
         SLL   8,0(5)
         OR    0,8
         B     TESTR6
ZERO     L     10,FE
         L     11,FF
         SLDL  10,0(5)
         NR    0,10
TESTR6   BCT   6,*+8
         B     STOREX
         LA    5,1(5)
         SRL   2,1
         BCR   15,7
STOREX   L     13,4(0,13)
         LM    2,12,16(13)
         BCR   15,14
         DS    0F
FF       DC    X'FFFFFFFF'
FE       DC    X'FFFFFFFE'
         LTORG
STACK    DSECT
SAVEAREA DS    18F
AIXOUT   DS    0F
LINKR23  DS    2F
BYTE     DS    C
STEND    DS    F
         END
#ifdef CERNLIB_TCGEN_MCBYT
#undef CERNLIB_TCGEN_MCBYT
#endif
