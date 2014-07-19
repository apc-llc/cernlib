*
* $Id: msbit0.s,v 1.1.1.1 1996/02/15 17:54:41 mclareni Exp $
*
* $Log: msbit0.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:41  mclareni
* Kernlib
*
*
MSBIT0   CSECT
*
* CERN PROGLIB# M421    MSBIT0          .VERSION KERNIBX  1.01  900524
* ORIG.  SEPT 85, HRR
* Modified for AIX, Roger Howard, January 1990
*
         USING *,15
         ENTRY _msbit0_
_msbit0_ STM   2,1,16(13)
         LR    4,0                  AIX: address of arg1
         L     0,0(,4)              AIX: value of arg1
         L     4,0(,1)              AIX: value of arg2
         BCTR  4,0
         L     6,FE
         L     7,FF
         SLDL  6,0(4)
         NR    0,6
         LM    2,7,16(13)
         BCR   15,14
         DS    0F
FE       DC    X'FFFFFFFE'
FF       DC    X'FFFFFFFF'
         END
#ifdef CERNLIB_TCGEN_MSBIT0
#undef CERNLIB_TCGEN_MSBIT0
#endif
