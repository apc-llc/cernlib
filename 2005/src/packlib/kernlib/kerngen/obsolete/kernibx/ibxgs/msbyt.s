*
* $Id: msbyt.s,v 1.1.1.1 1996/02/15 17:54:41 mclareni Exp $
*
* $Log: msbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:41  mclareni
* Kernlib
*
*
MSBYT    CSECT
*
* CERN PROGLIB# M421    MSBYT           .VERSION KERNIBX  1.01  900524
* ORIG.  SEPT 85, HRR
* Modified for AIX, Roger Howard, January 1990
*
         USING *,15
         ENTRY _msbyt_
_msbyt_  STM   2,7,16(13)
         L     6,0(,1)          AIX: value of arg4 = X
         L     5,0(,3)          AIX: value of arg4 = NBITS
         L     4,0(,2)          AIX: value of arg3 = LX
         LR    1,0              AIX: address of arg1
         L     0,0(,1)          AIX: value of arg1 = A
         SR    1,1
         SRDL  0,0(5)           R1 HAS LEFT MOST BITS = BYTE  A
         AR    4,5
         SRDL  6,63(4)
         LR    0,6
         SLDL  0,0(5)
         SLL   7,0(5)
         LR    1,7
         SR    4,5
         SLDL  0,63(4)
         LM    2,7,16(13)
         BR    14
         END
#ifdef CERNLIB_TCGEN_MSBYT
#undef CERNLIB_TCGEN_MSBYT
#endif
