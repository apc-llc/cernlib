*
* $Id: ilsuma.s,v 1.1.1.1 1996/04/01 15:03:02 mclareni Exp $
*
* $Log: ilsuma.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:02  mclareni
* Mathlib gen
*
*
#if defined(CERNLIB_QMIBMVF)
**********************************************************************
* CERN PROGLIB# F122    ILSUM           .VERSION KERNIBM  2.27  890530
* ORIG. 05/30/89    F. Antonelli/IBM
*
*********************************************************************
*     FUNCTION ILSUM(NW,INTE,ISTR)
*     LOGICAL INTE(*)
*     IILZF=0
*     INDE=1
*       DO J=1,NW
*         IF(INTE(INDE))THEN
*           IILZF=IILZF+1
*         ENDIF
*           INDE=INDE+ISTR
*       ENDDO
*10     CONTINUE
*
*     RETURN
*     END
*********************************************************************
ILSUM    START 0
#if defined(CERNLIB_QMIBMXA)
ILSUM    AMODE ANY
ILSUM    RMODE ANY
#endif
         USING *,15
         STM   1,5,24(13)
* R2 INTE, R3 ISRT
         LM    1,3,0(1)
*
*IN R1 NW - IN R3 ISTR -
         L     1,0(0,1)
         L     3,0(0,3)
* TEST ON VECTOR SIZE IF VS LT 128 NOSECTIONING
         LA    5,128
         SR    5,1
         BC    2,NOSECT
*
* IN R4 0
         LA    5,1
         SR    0,0
LOOP     VLVCU 1
         VCS   8,5,2(3)
         VCOVM 0
         LTR   1,1
         BC    2,LOOP
         LM    1,5,24(13)
         BR    14
NOSECT   LA    5,1
         SR    0,0
         VLVCA 0(1)
         VCS   8,5,2(3)
         VCOVM 0
         LM    1,5,24(13)
         BR    14
         END
#endif
