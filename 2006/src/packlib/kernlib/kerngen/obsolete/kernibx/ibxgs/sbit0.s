*
* $Id: sbit0.s,v 1.1.1.1 1996/02/15 17:54:41 mclareni Exp $
*
* $Log: sbit0.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:41  mclareni
* Kernlib
*
*
SBIT0    CSECT
*
* CERN PROGLIB# M421    SBIT0           .VERSION KERNIBX  1.01  900524
*
         USING *,15
         ENTRY _sbit0_
_sbit0_  STM   2,7,16(13)
         LR    2,0              AIX: address of arg1
         L     3,0(,2)          AIX: value of arg1
         L     4,0(,1)          AIX: value of arg2
         BCTR  4,0
         L     6,FE
         L     7,FF
         SLDL  6,0(4)
         NR    3,6
         ST    3,0(2)
         LM    2,7,16(13)
         BCR   15,14
FE       DC    X'FFFFFFFE'
FF       DC    X'FFFFFFFF'
         END
#ifdef CERNLIB_TCGEN_SBIT0
#undef CERNLIB_TCGEN_SBIT0
#endif
