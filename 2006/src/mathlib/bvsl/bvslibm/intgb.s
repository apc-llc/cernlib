*
* $Id: intgb.s,v 1.1.1.1 1996/03/21 17:19:59 mclareni Exp $
*
* $Log: intgb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:59  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE INTGB(NW,AIN,UVEC,ALVEC,BV,IFOUND)
*
*
*********************************************************************
INTGB   CSECT
#if defined(CERNLIB_QMIBMXA)
INTGB   AMODE 31
INTGB   RMODE ANY
#endif
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
         VLE   4,4(0)
         VLE   6,3(0)
         VCE   12,4,2
         VSTVM 7
         VCE   10,6,10
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
