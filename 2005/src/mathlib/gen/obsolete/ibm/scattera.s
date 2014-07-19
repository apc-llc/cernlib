*
* $Id: scattera.s,v 1.1.1.1 1996/04/01 15:03:02 mclareni Exp $
*
* $Log: scattera.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:02  mclareni
* Mathlib gen
*
*
#if defined(CERNLIB_QMIBMVF)
*********************************************************************
*
* CERN PROGLIB# F122    SCATTER         .VERSION GEN      2.30  890530
* ORIG. 05/30/89    F. Antonelli/IBM
*
*********************************************************************
*********************************************************************
*     SUBROUTINE SCATTER(NW,OUT,INDEX,HIN)
*     DIMENSION OUT(*),HIN(*),INDEX(*)
*       DO J=1,NW
*         OUT(INDEX(J))=HIN(J)
*       ENDDO
*********************************************************************
SCATTER  START 0
#if defined(CERNLIB_QMIBMXA)
SCATTER  AMODE ANY
SCATTER  RMODE ANY
#endif
         USING *,15
*GET ADDRESS OF PARM LIST
         STM   1,6,24(13)
         LM    1,4,0(1)
*
*IN R1 NW
         L     1,0(0,1)
* TEST ON VECTOR SIZE
         LA    6,128
         SR    6,1
         BC    2,NOSECT
         LA    6,4
         SR    2,6
*
*
*IN R2 R3 R4 ADDRESSES OF OUT INDEX HIN
* ADDRESSES OF SUBSEQUENT INDEX SECTION
LOOPV    VLVCU 1
         VLE   0,3
         VLE   1,4
         VSTIE 1,0,0(2)
         BC    2,LOOPV
* CONCLUSION VECTOR
*
         LM    1,6,24(13)
         BR    14
* NO SECTIONING
*
NOSECT   LA    5,4
         SR    2,5
*
         VLVCA 0(1)
         VLE   0,3
         VLE   1,4
         VSTIE 1,0,0(2)
* CONCLUSION NO SECTIONING
*
         LM    1,6,24(13)
         BR    14
*
         END
#endif
