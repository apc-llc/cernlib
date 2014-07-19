*
* $Id: dsxpyb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: dsxpyb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE DSXPYB(NW,BINV,Y,X,ALPHA)
*
*         Y = Y + ALPHA*X
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
DSXPYB  CSECT
#if defined(CERNLIB_QMIBMXA)
DSXPYB  AMODE 31
DSXPYB  RMODE ANY
#endif
#include "cachesz.inc"
         USING *,15
         STM   0,7,20(13)
* R1 NW, R2 BINV, R3 Y, R4 X, R5 ALPHA
         LM    1,5,0(1)
*
*IN R1 NW
         L     1,0(0,1)
*IN F6 ALPHA
         LD    2,0(0,5)
         LR    7,3
*
         VSVMM 1
LOOP     VLVCU 1
         VLVM  2
         VLMD  0,3
         VMADS 0,2,4
         VSTMD 0,7
         BC    2,LOOP
         VSVMM 0
         LM    0,7,20(13)
         BR    14
*
         END
#endif
