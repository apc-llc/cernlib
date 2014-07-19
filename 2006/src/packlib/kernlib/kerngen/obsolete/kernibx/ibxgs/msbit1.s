*
* $Id: msbit1.s,v 1.1.1.1 1996/02/15 17:54:41 mclareni Exp $
*
* $Log: msbit1.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:41  mclareni
* Kernlib
*
*
MSBIT1   CSECT
*
* CERN PROGLIB# M421    MSBIT1          .VERSION KERNIBX  1.01  900524
* ORIG.  SEPT 85, HRR
* Modified for AIX, Roger Howard, January 1990
*
         USING *,15
         ENTRY _msbit1_
_msbit1_ STM   2,5,16(13)
         LR    2,0              AIX: address of arg1
         L     0,0(,2)          AIX: value of arg1
         L     4,0(,1)          AIX: value of arg2
         BCTR  4,0
         LA    5,1
         SLL   5,0(4)
         OR    0,5
         LM    2,5,16(13)
         BCR   15,14
         END
#ifdef CERNLIB_TCGEN_MSBIT1
#undef CERNLIB_TCGEN_MSBIT1
#endif
