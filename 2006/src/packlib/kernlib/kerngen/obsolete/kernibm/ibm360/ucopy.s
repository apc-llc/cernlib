*
* $Id: ucopy.s,v 1.1.1.1 1996/02/15 17:53:17 mclareni Exp $
*
* $Log: ucopy.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:17  mclareni
* Kernlib
*
*
UCOPY    CSECT
*
* CERN PROGLIB# V301    UCOPY           .VERSION KERNIBM  0.1   741009
*
*        CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
UCOPY    AMODE ANY
UCOPY    RMODE ANY
#endif
         USING *,15
         B     BEGIN
         DC    X'5'
         DC    CL5'UCOPY'
BEGIN    STM   0,11,20(13)
         LM    8,10,0(1)
         SR    11,11
         L     10,0(10)
         SRDA  10,3
         LTR   10,10
         BNP   BYPASS
LOOP     LM    0,7,0(8)
         STM   0,7,0(9)
         LA    8,32(8)
         LA    9,32(9)
         BCT   10,LOOP
BYPASS   SLDA  10,3
         BNP   END
         LM    0,7,0(8)
         BCTR  10,0
         EX    10,STORE
END      LM    0,11,20(13)
         MVI   12(13),X'FF'
         SR    15,15
         BR    14
STORE    STM   0,0,0(9)
         END
#ifdef CERNLIB_TCGEN_UCOPY
#undef CERNLIB_TCGEN_UCOPY
#endif
