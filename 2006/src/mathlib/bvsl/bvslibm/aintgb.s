*
* $Id: aintgb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: aintgb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE AINTGB(NW,AIN,UVEC,ALVEC,BV,IFOUND)
*
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
AINTGB  CSECT
#if defined(CERNLIB_QMIBMXA)
AINTGB  AMODE 31
AINTGB  RMODE ANY
#endif
#include "cachesz.inc"
         USING *,15
         STM   1,10,24(13)
*
         LM    1,6,0(1)
*
         L     1,0(0,1)
         LR    10,2
*
         SR    8,8
LOOP     VLVCU 1
         LA    7,VMASK
         LR    9,7
         VLD   4,4(0)
         VLD   6,3(0)
         VCD   12,4,2
         VSTVM 7
         VCD   10,6,10
         VNVM  9
         VCOVM 8
         VSTVM 5
         LTR   1,1
         BC    2,LOOP
         ST    8,0(6)
         LM    1,10,24(13)
         BR    14
VMASK    DS    4F
*
         END
#endif
