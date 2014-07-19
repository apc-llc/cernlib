*
* $Id: dcopyb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: dcopyb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE DCOPYB(NW,BINV,Y,X)
*
*         Y(BV) = X(BV)
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
DCOPYB  CSECT
#if defined(CERNLIB_QMIBMXA)
DCOPYB  AMODE 31
DCOPYB  RMODE ANY
#endif
#include "cachesz.inc"
         USING *,15
         STM   0,4,20(13)
* R1 NW, R2 BINV, R3 Y, R4 X
         LM    1,4,0(1)
*
*IN R1 NW
         L     1,0(0,1)
*
LOOP     VLVCU 1
         VLVM  2
         VLMD  4,4
         VSTMD 4,3
         BC    2,LOOP
         LM    0,4,20(13)
         BR    14
*
         END
#endif
