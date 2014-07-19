*
* $Id: binvec.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: binvec.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE BINVEC(NW,IBIN,IVEC)
*     CONVERSION FROM BINARY TO INDEX VECTOR
*
* AUTOR: F. ANTONELLI                     LIBRARY: BVSL
*
*********************************************************************
BINVEC  CSECT
#if defined(CERNLIB_QMIBMXA)
BINVEC  AMODE 31
BINVEC  RMODE ANY
#endif
#include "cachesz.inc"
         USING *,15
         STM   1,6,24(13)
* R2 IN, R3 ISRT, R4 ITEST, R5 INDEX, R6 IFOUND
         LM    1,3,0(1)
*
*IN R1 NW - IN R2 IBIN - IN R3 IVEC
         L     5,0(0,1)
         LA    6,1
*
         SR    4,4
LOOP     VLBIX 0,4,0(2)
         VAQ   1,6,0
         VST   1,3(0)
         BC    2,LOOP
         LM    1,6,24(13)
         BR    14
*
         END
#endif
