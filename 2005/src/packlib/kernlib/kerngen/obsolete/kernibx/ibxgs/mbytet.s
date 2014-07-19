*
* $Id: mbytet.s,v 1.1.1.1 1996/02/15 17:54:40 mclareni Exp $
*
* $Log: mbytet.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:40  mclareni
* Kernlib
*
*
MBYTET   CSECT
*
* CERN PROGLIB# M421    MBYTET          .VERSION KERNIBX  1.01  900524
* ORIG.  SEPT 85, HRR
* Modified for AIX, Roger Howard, January 1990
*
         USING *,15
         ENTRY _mbytet_
_mbytet_ STM   2,7,16(13)
         L     6,0(,3)          AIX: value of arg4
         L     5,0(,2)          AIX: value of arg3
         LR    2,0              AIX: address of arg1
         L     2,0(,2)          AIX: value of arg1
         L     0,0(,1)          AIX: value of arg2
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
         LM    2,7,16(13)
         BCR   15,14
FF       DC    X'FFFFFFFF'
         END
#ifdef CERNLIB_TCGEN_MBYTET
#undef CERNLIB_TCGEN_MBYTET
#endif
