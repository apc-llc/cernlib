*
* $Id: iylosb.s,v 1.1.1.1 1996/03/21 17:19:57 mclareni Exp $
*
* $Log: iylosb.s,v $
* Revision 1.1.1.1  1996/03/21 17:19:57  mclareni
* Bvsl
*
*
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QMIBMVF))
*
*     SUBROUTINE IYLOSB(NW,IY,IS,BV,IFOUND,'LO')
*
*
*
*********************************************************************
IYLOSB  CSECT
#if defined(CERNLIB_QMIBMXA)
IYLOSB  AMODE 31
IYLOSB  RMODE ANY
#endif
#include "cachesz.inc"
         USING *,15
         STM   1,8,24(13)
* R1 NW
         LM    1,6,0(1)
         L     1,0(0,1)
*
         LH    6,0(0,6)
         LA    7,CENTRO
         LH    7,0(0,7)
         CR    6,7
         BC    4,MIN0
*
* RESULT 1 > CENTRO
*
         LA    7,LT
         LH    7,0(0,7)
         CR    6,7
         BC    4,MAX1
* RESULT 1 >  = LT
         BC    8,MAX2
* RESULT 1 NE LT END WITH NE
*********************************************************************
*     SUBROUTINE YNESB(NW,IN,ITEST,INDEX,IFOUND)
*
*
*********************************************************************
*
*IN R1 NW - IN R3 ISTR - IN R4 ITEST
         L     3,0(0,3)
* TEST ON VECTOR SIZE IF VS LE 'SZ' NO SECTIONING
         LA    7,SZ
         SR    7,1
         BC    2,NOSNE
*
         SR    8,8
LOONE    VLVCU 1
         VCS   6,3,2(0)
         VCOVM 8
         VSTVM 4
         LTR   1,1
         BC    2,LOONE
         ST    8,0(5)
         LM    1,8,24(13)
         BR    14
*
NOSNE    VLVCA 0(1)
         SR    8,8
*
         VCS   6,3,2(0)
         VCOVM 8
         VSTVM 4
         ST    8,0(5)
         LM    1,8,24(13)
         BR    14
*********************************************************************
* RESULT 1 = LT END WITH LT
*********************************************************************
*     SUBROUTINE YLTSB(NW,Y,S,INDB,IFOUND)
*
*********************************************************************
*
MAX2     L     3,0(0,3)
* TEST ON VECTOR SIZE IF VS LE 'SZ' NO SECTIONING
         LA    7,SZ
         SR    7,1
         BC    2,NOSLT
*
         SR    8,8
LOOLT    VLVCU 1
         VCS   2,3,2(0)
         VCOVM 8
         VSTVM 4
         LTR   1,1
         BC    2,LOOLT
         ST    8,0(5)
         LM    1,8,24(13)
         BR    14
*
NOSLT    VLVCA 0(1)
         SR    8,8
*
         VCS   2,3,2(0)
         VCOVM 8
         VSTVM 4
         ST    8,0(5)
         LM    1,8,24(13)
         BR    14
*********************************************************************
* RESULT 1 > LT END WITH LE
*********************************************************************
*     SUBROUTINE YLESB(NW,AIN,ISTR,ATEST,INDEX,IFOUND)
*
*
*********************************************************************
MAX1     L     3,0(0,3)
* TEST ON VECTOR SIZE IF VS LE 'SZ' NO SECTIONING
         LA    7,SZ
         SR    7,1
         BC    2,NOSLE
*
         SR    8,8
LOOLE    VLVCU 1
         VCS   10,3,2(0)
         VCOVM 8
         VSTVM 4
         LTR   1,1
         BC    2,LOOLE
         ST    8,0(5)
         LM    1,8,24(13)
         BR    14
*
NOSLE    VLVCA 0(1)
         SR    8,8
*
         VCS   10,3,2(0)
         VCOVM 8
         VSTVM 4
         ST    8,0(5)
         LM    1,8,24(13)
         BR    14
*********************************************************************
* RESULT 1 < CENTRO
MIN0     LA    7,GE
         LH    7,0(0,7)
         CR    6,7
         BC    4,MIN1
* RESULT 1 > = GE
         BC    8,MIN2
* RESULT 1 NE GE END WITH GT
*********************************************************************
*     SUBROUTINE YGTSB(NW,AIN,ATEST,INDEX,IFOUND)
*
*
*
*********************************************************************
         L     3,0(0,3)
* TEST ON VECTOR SIZE IF VS LE 'SZ' NO SECTIONING
         LA    7,SZ
         SR    7,1
         BC    2,NOSGT
*
         SR    8,8
LOOGT    VLVCU 1
         VCS   4,3,2(0)
         VCOVM 8
         VSTVM 4
         LTR   1,1
         BC    2,LOOGT
         ST    8,0(5)
         LM    1,8,24(13)
         BR    14
*
NOSGT    VLVCA 0(1)
         SR    8,8
*
         VCS   4,3,2(0)
         VCOVM 8
         VSTVM 4
         ST    8,0(5)
         LM    1,8,24(13)
         BR    14
********************************************************************
* RESULT 1 = GE END WITH GE
*********************************************************************
*     SUBROUTINE YGESB(NW,AIN,ISTR,ATEST,INDEX,IFOUND)
*
*
*********************************************************************
*
*IN R1 NW - IN R3 ISTR - IN R4 ITEST
MIN2     L     3,0(0,3)
* TEST ON VECTOR SIZE IF VS LE 'SZ' NO SECTIONING
         LA    7,SZ
         SR    7,1
         BC    2,NOSGE
*
         SR    8,8
LOOGE    VLVCU 1
         VCS   12,3,2(0)
         VCOVM 8
         VSTVM 4
         LTR   1,1
         BC    2,LOOGE
         ST    8,0(5)
         LM    1,8,24(13)
         BR    14
*
NOSGE    VLVCA 0(1)
         SR    8,8
*
         VCS   12,3,2(0)
         VCOVM 8
         VSTVM 4
         ST    8,0(5)
         LM    1,8,24(13)
         BR    14
*********************************************************************
* END WITH EQ
*********************************************************************
*     SUBROUTINE YEQSB(NW,IN,ITEST,INDEX,IFOUND)
*
*
*********************************************************************
MIN1     L     3,0(0,3)
* TEST ON VECTOR SIZE IF VS LE 'SZ' NO SECTIONING
         LA    7,SZ
         SR    7,1
         BC    2,NOSEQ
*
         SR    8,8
LOOEQ    VLVCU 1
         VCS   8,3,2(0)
         VCOVM 8
         VSTVM 4
         LTR   1,1
         BC    2,LOOEQ
         ST    8,0(5)
         LM    1,8,24(13)
         BR    14
*
NOSEQ    VLVCA 0(1)
         SR    8,8
*
         VCS   8,3,2(0)
         VCOVM 8
         VSTVM 4
         ST    8,0(5)
         LM    1,8,24(13)
         BR    14
*********************************************************************
********************************************************************
EQ       DC    XL2'C5D8'
GE       DC    XL2'C7C5'
GT       DC    XL2'C7E3'
CENTRO   DC    XL2'CCCC'
LE       DC    XL2'D3C5'
LT       DC    XL2'D3E3'
NE       DC    XL2'D5C5'
*
         END
#endif
