*
* $Id: drangb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: drangb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE DRANGB(NW,AIN,UBOUND,ALBOUND,INDEX,IFOUND)
*
*
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
DRANGB  CSECT
#if defined(CERNLIB_QMIBMXA)
DRANGB  AMODE 31
DRANGB  RMODE ANY
#endif
#include "cachesz.inc"
         USING *,15
         STM   1,9,24(13)
*
         LM    1,6,0(1)
*
         L     1,0(0,1)
         LD    0,0(0,3)
         LD    2,0(0,4)
*
         SR    8,8
LOOP     VLVCU 1
         LA    7,VMASK
         LR    9,7
         VLD   2,2(0)
         VCDQ  12,2,2
         VSTVM 7
         VCDQ  10,0,2
         VNVM  9
         VCOVM 8
         VSTVM 5
         LTR   1,1
         BC    2,LOOP
         ST    8,0(6)
         LM    1,9,24(13)
         BR    14
VMASK    DS    4F
*
         END
#endif
