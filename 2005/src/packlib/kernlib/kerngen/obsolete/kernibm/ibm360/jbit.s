*
* $Id: jbit.s,v 1.1.1.1 1996/02/15 17:53:14 mclareni Exp $
*
* $Log: jbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:14  mclareni
* Kernlib
*
*
JBIT     CSECT
*
* CERN PROGLIB# M421    JBIT            .VERSION KERNIBM  0.1   741009
*
*        CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
JBIT     AMODE ANY
JBIT     RMODE ANY
#endif
         USING *,15
         B     BEGIN
         DC    X'4'
         DC    CL4'JBIT'
BEGIN    STM   1,2,24(13)
         LM    1,2,0(1)                 R1->A,R2->LA
         L     0,0(1)
         L     2,0(2)                   R2=LA
         SRDL  0,0(2)                   SHIFT SO THAT LA TH BIT IS AT
         SR    0,0                      FAR LEFT OF R1
         SLDL  0,1
         LM    1,2,24(13)
         MVI   12(13),X'FF'
         SR    15,15
         BR    14
         END
#ifdef CERNLIB_TCGEN_JBIT
#undef CERNLIB_TCGEN_JBIT
#endif
