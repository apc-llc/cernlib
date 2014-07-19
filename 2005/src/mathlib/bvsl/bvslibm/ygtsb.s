*
* $Id: ygtsb.s,v 1.1.1.1 1996/03/21 17:19:57 mclareni Exp $
*
* $Log: ygtsb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:57  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*CMZ :          04/05/90  16.17.26  by  Michel Roethlisberger/IBM
*-- Author :
*********************************************************************
*     SUBROUTINE YGTSB(NW,AIN,ATEST,INDEX,IFOUND)
*
*
*
*********************************************************************
YGTSB   CSECT
#if defined(CERNLIB_QMIBMXA)
YGTSB   AMODE 31
YGTSB   RMODE ANY
#endif
         USING *,15
         STM   0,9,20(13)
* R2 AIN, R3 ISRT, R4 ATEST, R5 INDEX, R6 IFOUND
         LM    1,5,0(1)
*
*IN R1 NW - IN R3 ISTR - IN R4 ITEST
         L     8,0(0,1)
         LE    4,0(0,3)
* TEST ON VECTOR SIZE IF VS LE 128 NO SECTIONING
         LA    1,128
         SR    1,8
         BC    2,NOSECT
*
         LA    7,1
         SR    9,9
LOOP     VLVCU 8
         VCES  4,4,2(0)
         VCOVM 9
         VSTVM 4
         LTR   8,8
         BC    2,LOOP
         ST    9,0(5)
         LM    0,9,20(13)
         BR    14
*
NOSECT   VLVCA 0(8)
         LA    7,1
         SR    9,9
*
         VCES  4,4,2(0)
         VCOVM 9
         VSTVM 4
         ST    9,0(5)
         LM    0,9,20(13)
         BR    14
         END
#endif
