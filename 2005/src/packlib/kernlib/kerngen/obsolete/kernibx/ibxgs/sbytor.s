*
* $Id: sbytor.s,v 1.1.1.1 1996/02/15 17:54:42 mclareni Exp $
*
* $Log: sbytor.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:42  mclareni
* Kernlib
*
*
SBYTOR   CSECT
*
* CERN PROGLIB# M421    SBYTOR          .VERSION KERNIBX  1.01  900524
*
         USING *,15
         ENTRY _sbytor_
_sbytor_ STM   2,7,16(13)
         L     6,0(,3)          AIX: value of arg4
         L     5,0(,2)          AIX: value of arg3
         LR    2,0              AIX: address of arg1
         L     2,0(,2)          AIX: value of arg1
         L     4,0(,1)          AIX: value of arg2
         LA    7,32
         SR    7,6
         SLL   2,0(7)
         SR    7,5
         LA    7,1(7)
         SRL   2,0(7)
         OR    4,2
         ST    4,0(1)          AIX: address of arg2 in GR1
         LM    2,7,16(13)
         BCR   15,14
         END
#ifdef CERNLIB_TCGEN_SBYTOR
#undef CERNLIB_TCGEN_SBYTOR
#endif
