*
* $Id: jbyt.s,v 1.1.1.1 1996/02/15 17:54:39 mclareni Exp $
*
* $Log: jbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:39  mclareni
* Kernlib
*
*
JBYT     CSECT
*
* CERN PROGLIB# M421    JBYT            .VERSION KERNIBX  1.01  900524
*
*   CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*   Modified for AIX, Roger Howard, January 1990
         USING *,15
         ENTRY _jbyt_
_jbyt_   STM   2,3,16(13)
         LR    3,0             AIX: address of arg1
         L     0,0(,3)         AIX: value of arg1 = A
         L     3,0(,2)         AIX: value of arg3 = NBITS
         L     2,0(,1)         AIX: value of arg2 = LA
         AR    2,3             LA+NBITS
         SRDL  0,63(2)         MOVE LA+NBITS-1 INTO R1
         SR    0,0
         SLDL  0,0(3)          MOVE NBITS BACK INTO R0
         LM    2,3,16(13)
         BR    14
         END
#ifdef CERNLIB_TCGEN_JBYT
#undef CERNLIB_TCGEN_JBYT
#endif
