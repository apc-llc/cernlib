*
* $Id: zerob.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: zerob.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE ZEROB(NW,BV)
*     SET TO 0 THE BV
*********************************************************************
ZEROB   CSECT
#if defined(CERNLIB_QMIBMXA)
ZEROB   AMODE 31
ZEROB   RMODE ANY
#endif
         USING *,15
         STM   1,3,24(13)
* R2 IN, R3 ISRT, R4 ITEST, R5 INDEX, R6 IFOUND
         LM    1,2,0(1)
*
*IN R1 NW
         L     1,0(0,1)
         LA    3,ZERO
*
         VLVCU 1
         VLVM  3
LOOP     VLVCU 1
         VSTVM 2
         BC    2,LOOP
         LM    1,3,24(13)
         BR    14
ZERO     DC    XL16'00000000000000000000000000000000'
*
         END
#endif
