*
* $Id: dsxyb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: dsxyb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE DSXYB(NW,BINV,SCAL,Y,X)
*
*         Y = SCAL*X*Y
*
*********************************************************************
DSXYB   CSECT
#if defined(CERNLIB_QMIBMXA)
DSXYB   AMODE 31
DSXYB   RMODE ANY
#endif
#include "cachesz.inc"
         USING *,15
         STM   0,9,20(13)
* R1 NW, R2 BINV, R3 SCAL , R4 Y , R   2
         LM    1,5,0(1)
*
*IN R1 NW
         L     1,0(0,1)
         LD    0,0(0,5)
         LR    7,3
*
         VSVMM 1
LOOP     VLVCU 1
         VLVM  2
         VLMD  0,4
         VMD   2,0,3
         VMDQ  4,0,2
         VSTMD 4,7
         BC    2,LOOP
         VSVMM 0
         LM    0,9,20(13)
         BR    14
*
         END
#endif
