*
* $Id: dscalb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: dscalb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*     SUBROUTINE DSCALB(NW,BINV,Y,ALPHA)
*
*         Y = ALPHA*X
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
DSCALB  CSECT
#if defined(CERNLIB_QMIBMXA)
DSCALB  AMODE 31
DSCALB  RMODE ANY
#endif
#include "cachesz.inc"
         USING *,15
         STM   0,7,20(13)
* R1 NW, R2 BINV, R3 Y, R4 ALPHA
         LM    1,4,0(1)
*
*IN R1 NW
         L     1,0(0,1)
*IN F6 ALPHA
         LD    2,0(0,4)
         LR    7,3
*
         VSVMM 1
LOOP     VLVCU 1
         VLVM  2
         VLMD  0,3
         VMDQ  2,2,0
         VSTMD 2,7
         BC    2,LOOP
         VSVMM 0
         LM    0,7,20(13)
         BR    14
*
         END
#endif
