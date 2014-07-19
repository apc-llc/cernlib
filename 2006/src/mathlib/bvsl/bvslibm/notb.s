*
* $Id: notb.s,v 1.1.1.1 1996/03/21 17:19:58 mclareni Exp $
*
* $Log: notb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:58  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*********************************************************************
*     SUBROUTINE NOTB(NW,BINV1,BINVOU,NWOUT)
*
*    PERFORMS AND OF TWO BINARY VECTORS
*        NW1 INPUT VECTOR LENGTH
*
*    Modif.
*    25-08-89  M.Roethlisberger    Replaces VLVM 1 + VCVMwith VLCVM1
*
*********************************************************************
NOTB    CSECT
#if defined(CERNLIB_QMIBMXA)
NOTB    AMODE 31
NOTB    RMODE ANY
#endif
         USING *,15
         STM   1,5,24(13)
         LM    1,4,0(1)
*
         SR    5,5
         L     1,0(0,1)
LOOP     VLVCU 1
         VLCVM 2
         VCOVM 5
         VSTVM 3
         LTR   1,1
         BC    2,LOOP
         ST    5,0(4)
         LM    1,5,24(13)
         BR    14
         END
#endif
