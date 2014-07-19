*
* $Id: sbyt.s,v 1.1.1.1 1996/02/15 17:54:42 mclareni Exp $
*
* $Log: sbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:42  mclareni
* Kernlib
*
*
SBYT     CSECT
*
* CERN PROGLIB# M421    SBYT            .VERSION KERNIBX  1.01  900524
*
*    CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*    Modified for AIX, Roger Howard, January 1990
*
         USING *,15
         ENTRY _sbyt_
_sbyt_   STM   2,7,16(13)
         L     5,0(,3)         AIX: value of arg4 = NBITS
         L     4,0(,2)         AIX: value of arg3 = LX
         LR    3,1             AIX: address of arg2 = X
         LR    2,0             AIX: address of arg1
         L     0,0(,2)         AIX: value of arg1 = A
         SR    1,1
         SRDL  0,0(5)          R1 HAS LEFT MOST BITS = BYTE  A
         L     6,0(3)
         AR    4,5
         SRDL  6,63(4)
         LR    0,6
         SLDL  0,0(5)
         SLL   7,0(5)
         LR    1,7
         SR    4,5
         SLDL  0,63(4)
         ST    0,0(3)
         LM    2,7,16(13)
         BR    14
         END
#ifdef CERNLIB_TCGEN_SBYT
#undef CERNLIB_TCGEN_SBYT
#endif
