*
* $Id: oneb.s,v 1.1.1.1 1996/03/21 17:19:59 mclareni Exp $
*
* $Log: oneb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:59  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE ONEB(NW,BV)
*     SET TO 1 THE BV
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*********************************************************************
ONEB     CSECT
#if defined(CERNLIB_QMIBMXA)
ONEB     AMODE 31
ONEB     RMODE ANY
#endif
         USING *,15
         STM   G1,G3,24(13)
* R2 IN, R3 ISRT, R4 ITEST, R5 INDEX, R6 IFOUND
         LM    1,2,0(1)
*
*IN R1 NW
         L     1,0(0,1)
         LA    3,ONE
*
         VLVCU 1
         VLVM  3
LOOP     VSTVM 2
         VLVCU 1
         BC    3,LOOP
         LM    1,3,24(13)
         BR    14
ONE      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
*
#include "equats.inc"
         END
#endif
