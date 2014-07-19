*
* $Id: nandb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: nandb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE NANDB(NW,BINV1,BINV2,BINVOU,NWOUT)
*
*    PERFORMS NOT AND OF TWO BINARY VECTORS
*        NW1 INPUT VECTOR LENGTH
*
*********************************************************************
NAND    CSECT
#if defined(CERNLIB_QMIBMXA)
NAND    AMODE 31
NAND    RMODE ANY
#endif
         USING *,15
         STM   1,6,24(13)
* R2 IN, R3 ISRT, R4 ITEST, R5 INDEX, R6 IFOUND
         LM    1,5,0(1)
*
         SR    6,6
         L     1,0(0,1)
LOOP     VLVCU 1
         VLVM  2
         VNVM  3
         VCVM
         VCOVM 6
         VSTVM 4
         LTR   1,1
         BC    2,LOOP
         ST    6,0(5)
         LM    1,6,24(13)
         BR    14
         END
#endif
