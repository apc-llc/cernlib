*
* $Id: ibits.s,v 1.1.1.1 1996/02/15 17:54:36 mclareni Exp $
*
* $Log: ibits.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:36  mclareni
* Kernlib
*
*
IBITS    CSECT
*
* CERN PROGLIB# M441    BITPAK          .VERSION KERNIBX  1.01  900523
* ORIG.  MAY 87,  K.M.STORR, CERN/DD
* Modified for AIX, Roger Howard, January 1990
*
*     FUNCTION IBITS : J=IBITS(I,N,L)
*                     is  J=ISHFT(ISHFT(I,32-L-N),L-32)
*
         USING *,15
         ENTRY _ibits_
_ibits_  STM   2,4,16(13)
         LR    4,0
         L     0,0(,4)       AIX: value of arg1 = I
*                            AIX: address of arg2 is in GR1
         L     2,0(,2)       AIX: value of arg3 = L
         L     2,0(,2)       AIX: value of arg3 = L
         LR    4,2           make copy of L
         A     2,0(0,1)      L+N
         LA    3,32
         SR    3,2           32-L-N
         SLL   0,0(3)        shift out unwanted lhs bits
         LA    3,32
         SR    3,4           32-L
         SRL   0,0(3)        FINAL RESULT
         LM    2,4,16(13)
         BR    14
         END
#ifdef CERNLIB_TCGEN_IBITS
#undef CERNLIB_TCGEN_IBITS
#endif
