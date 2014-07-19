*
* $Id: iusame.s,v 1.1.1.1 1996/02/15 17:54:39 mclareni Exp $
*
* $Log: iusame.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:39  mclareni
* Kernlib
*
*
IUSAME   CSECT
*
* CERN PROGLIB# M501    IUSAME          .VERSION KERNIBX  1.01  900524
*
         USING *,15
         ENTRY _iusame_
_iusame_ STM   2,9,16(13)
         L     6,88(,13)          AIX: address of arg5=JSAME
         L     5,0(,3)            AIX: value of arg4=MIN
         L     4,0(,2)            AIX: value of arg3=N
         L     3,0(,1)            AIX: value of arg2=JST
         LR    2,0                AIX: address of arg1=IA
         BCTR  3,0
         SLA   3,2
         BCTR  4,0
         SLA   4,2
ZWEI1    SR    8,8
         LR    9,3
ZWEI4    LA    8,1(8)
         CR    3,4
         BE    DREI1
         L     7,0(3,2)
         LA    3,4(3)
         C     7,0(3,2)
         BE    ZWEI4
         CR    8,5
         BL    ZWEI1
         B     DREI2
DREI1    CR    8,5
         BNL   DREI2
         SR    8,8
         LA    9,4(4)
DREI2    LR    0,8
         SRA   9,2
         LA    9,1(9)
         ST    9,0(6)
         LM    2,9,16(13)
         BCR   15,14
         END
#ifdef CERNLIB_TCGEN_IUSAME
#undef CERNLIB_TCGEN_IUSAME
#endif
