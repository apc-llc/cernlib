*
* $Id: cntob.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: cntob.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE CNTOB(NW,BV,IFOUND)
*
*********************************************************************
CNTOB   CSECT
#if defined(CERNLIB_QMIBMXA)
CNTOB   AMODE 31
CNTOB   RMODE ANY
#endif
         USING *,15
         STM   1,4,24(13)
* R2 IN, R3 ISRT, R4 ITEST, R5 INDEX, R6 IFOUND
         LM    1,3,0(1)
*
         SR    4,4
         L     1,0(0,1)
LOOP     VLVCU 1
         VLVM  2
         VCOVM 4
         LTR   1,1
         BC    2,LOOP
         ST    4,0(3)
         LM    1,4,24(13)
         BR    14
         END
#endif
