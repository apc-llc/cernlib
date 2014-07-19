*
* $Id: iilza.s,v 1.1.1.1 1996/04/01 15:03:02 mclareni Exp $
*
* $Log: iilza.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:02  mclareni
* Mathlib gen
*
*
#if defined(CERNLIB_IBM)
*********************************************************************
*
* CERN PROGLIB# F122    IILZ            .VERSION KERNIBM  2.27  89053
* ORIG. 05/30/89    F. Antonelli/IBM
*
*********************************************************************
*********************************************************************
*     FUNCTION IILZF(NW,INTE,ISTR)
*     DIMENSION INTE(*)
*     IILZF=0
*     INDE=1
*       DO J=1,NW
*         IF(INTE(INDE).EQ.0)THEN
*           IILZF=IILZF+1
*           INDE=INDE+ISTR
*         ELSE
*           GO TO 10
*         ENDIF
*       ENDDO
*10     CONTINUE
*********************************************************************
IILZ     START 0
#if defined(CERNLIB_QMIBMXA)
IILZ     AMODE ANY
IILZ     RMODE ANY
#endif
         USING *,15
         STM   1,9,24(13)
* R2 INTE, R3 ISRT
         LM    1,3,0(1)
*
*IN R1 NW - IN R3 ISTR -
         L     3,0(0,3)
*  SCALAR RUN
         LA    5,4
         MR    4,3
         L     7,0(0,1)
         SLL   7,2
         MR    6,3
         LR    6,5
         LA    5,4
         SR    2,5
         SR    9,9
LOOPS    L     8,0(5,2)
         CR    8,9
         BC    6,OUT
         BXLE  5,6,LOOPS
OUT      LA    6,4
         SR    5,6
         SRL   5,2
         DR    4,3
         LR    0,5
*
* CONCLUSION SCALAR
         LM    1,9,24(13)
         BR    14
         END
#endif
