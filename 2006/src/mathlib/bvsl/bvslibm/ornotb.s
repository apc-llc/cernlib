*
* $Id: ornotb.s,v 1.1.1.1 1996/03/21 17:19:59 mclareni Exp $
*
* $Log: ornotb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:59  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE ORNOTB(NW,BINV1,BINV2,BINVOU,NWOUT)
*
*    PERFORMS NOT XOR OF TWO BINARY VECTORS
*        NW1 INPUT VECTOR LENGTH
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
ORNOTB   CSECT
#if defined(CERNLIB_QMIBMXA)
ORNOTB   AMODE 31
ORNOTB   RMODE ANY
#endif
         USING *,15
         STM   G1,G6,24(13)
         LM    1,5,0(1)
*
         SR    6,6
         L     1,0(0,1)
LOOP     VLVCU 1
         VLVM  3
         VCVM
         VXVM  2
         VCOVM 6
         VSTVM 4
         LTR   G1,G1         Restore proper OC
         BC    2,LOOP
         ST    6,0(5)
         LM    1,6,24(13)
         BR    14
#include "equats.inc"
         END
#endif
