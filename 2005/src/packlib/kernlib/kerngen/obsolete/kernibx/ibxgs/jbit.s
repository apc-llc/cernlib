*
* $Id: jbit.s,v 1.1.1.1 1996/02/15 17:54:39 mclareni Exp $
*
* $Log: jbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:39  mclareni
* Kernlib
*
*
JBIT     CSECT
*
* CERN PROGLIB# M421    JBIT            .VERSION KERNIBX  1.01  900524
*
*    CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*    Modified for AIX, Roger Howard, January 1990
         USING *,15
         ENTRY _jbit_
_jbit_   ST    2,16(,13)
         LR    2,0              AIX: address of arg1
         L     0,0(,2)          AIX: value of arg1 = A
         L     2,0(,1)          AIX: value of arg2 = LA
         SRDL  0,0(2)           SHIFT SO THAT LA TH BIT IS AT
         SR    0,0                      FAR LEFT OF R1
         SLDL  0,1
         L     2,16(,13)
         BR    14
         END
#ifdef CERNLIB_TCGEN_JBIT
#undef CERNLIB_TCGEN_JBIT
#endif
