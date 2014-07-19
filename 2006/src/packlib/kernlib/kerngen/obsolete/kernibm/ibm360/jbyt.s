*
* $Id: jbyt.s,v 1.1.1.1 1996/02/15 17:53:14 mclareni Exp $
*
* $Log: jbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:14  mclareni
* Kernlib
*
*
JBYT     CSECT
*
* CERN PROGLIB# M421    JBYT            .VERSION KERNIBM  0.1   741009
*
*        CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
JBYT     AMODE ANY
JBYT     RMODE ANY
#endif
         USING *,15
         B     BEGIN
         DC    X'4'
         DC    CL4'JBYT'
BEGIN    STM   1,4,24(13)
         LM    1,3,0(1)                 R1->A,R2->LA,R3->NBITS
         L     4,0(3)                   R4=NBITS
         L     3,0(2)                   R3=LA
         AR    3,4
         L     0,0(1)
         SRDL  0,63(3)                  MOVE LA+NBITS-1 INTO R1
         SR    0,0
         SLDL  0,0(4)                   MOVE NBITS BACK INTO R0
         LM    1,4,24(13)
         MVI   12(13),X'FF'
         SR    15,15
         BR    14
         END
#ifdef CERNLIB_TCGEN_JBYT
#undef CERNLIB_TCGEN_JBYT
#endif
