*
* $Id: andntb.s,v 1.1.1.1 1996/03/21 17:19:59 mclareni Exp $
*
* $Log: andntb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:59  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE ANDNTB(NW,BINV1,BINV2,BINVOU,NWOUT)
*
*    PERFORMS NOT AND OF TWO BINARY VECTORS
*        NW1 INPUT VECTOR LENGTH
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
ANDNTB   CSECT
#if defined(CERNLIB_QMIBMXA)
ANDNTB   AMODE 31
ANDNTB   RMODE ANY
#endif
         USING *,15
         STM   G1,G6,24(13)
* R2 IN, R3 ISRT, R4 ITEST, R5 INDEX, R6 IFOUND
         LM    1,5,0(1)
*
         SR    6,6
         L     1,0(0,1)
LOOP     VLVCU 1
         VLVM  3
         VCVM
         VNVM  2
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
