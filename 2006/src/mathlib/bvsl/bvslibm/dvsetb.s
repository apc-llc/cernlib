*
* $Id: dvsetb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: dvsetb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE DVSETB(NW,BINV,Y,CONST)
*
*       SET Y TO CONST FOR GIVEN INDEX
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
DVSETB  CSECT
#if defined(CERNLIB_QMIBMXA)
DVSETB  AMODE 31
DVSETB  RMODE ANY
#endif
#include "cachesz.inc"
         USING *,15
         STM   0,4,20(13)
* R1 NW, R2 BINV, R3 Y,  R4 ALPHA
         LM    1,4,0(1)
*
*IN R1 NW
         L     1,0(0,1)
*IN F4 CONST
         LD    2,0(0,4)
*
LOOP     VLVCU 1
         VLVM  2
         VLMDQ 0,2
         VSTMD 0,3
         BC    2,LOOP
         VSVMM 0
         LM    0,4,20(13)
         BR    14
*
         END
#endif
