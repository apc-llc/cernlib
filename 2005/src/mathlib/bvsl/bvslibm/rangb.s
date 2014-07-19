*
* $Id: rangb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: rangb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE RANGB(NW,AIN,UBOUND,ALBOUND,INDEX,IFOUND)
*
*
*
*********************************************************************
RANGB   CSECT
#if defined(CERNLIB_QMIBMXA)
RANGB   AMODE 31
RANGB   RMODE ANY
#endif
         USING *,15
         STM   1,9,24(13)
*
         LM    1,6,0(1)
*
         L     1,0(0,1)
         LE    2,0(0,3)
         LE    4,0(0,4)
*
         SR    8,8
LOOP     VLVCU 1
         LA    7,VMASK
         LR    9,7
         VLE   2,2(0)
         VCEQ  12,4,2
         VSTVM 7
         VCEQ  10,2,2
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
