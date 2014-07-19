*
* $Id: gathera.s,v 1.1.1.1 1996/04/01 15:03:02 mclareni Exp $
*
* $Log: gathera.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:02  mclareni
* Mathlib gen
*
*
#if defined(CERNLIB_QMIBMVF)
*********************************************************************
*
* CERN PROGLIB# F122    GATHER          .VERSION GEN      2.30  890530
* ORIG. 05/30/89    F. Antonelli/IBM
*
*********************************************************************
*********************************************************************
*     SUBROUTINE GATHER(NW,OUT,HIN,INDEX)
*     DIMENSION OUT(*),HIN(*),INDEX(*)
*       DO J=1,NW
*         OUT(J)=HIN(INDEX(J))
*       ENDDO
*********************************************************************
GATHER   START 0
#if defined(CERNLIB_QMIBMXA)
GATHER   AMODE ANY
GATHER   RMODE ANY
#endif
         USING *,15
         STM   1,5,24(13)
*GET ADDRESS OF PARM LIST
         LM    1,4,0(1)
*
*IN R1 NW
         L     1,0(0,1)
* TEST ON VECTOR SIZE IF VS LT 128 NOSECTIONING
         LA    5,128
         SR    5,1
         BC    2,NOSECT
*
         LA    5,4
         SR    3,5
*
*IN R2 R3 R4 ADDRESSES OF OUT HIN INDEX
* ADDRESSES OF SUBSEQUENT INDEX SECTION
LOOPV    VLVCU 1
         VLE   2,4
         VLIE  1,2,0(3)
         VSTE  1,2
         BC    2,LOOPV
* CONCLUSION VECTOR
*
         LM    1,5,24(13)
         BR    14
*
* NO SECTIONING
*
NOSECT   LA    5,4
         SR    3,5
*
         VLVCA 0(1)
         VLE   2,4
         VLIE  1,2,0(3)
         VSTE  1,2
* CONCLUSION NO SECTIONING
*
         LM    1,5,24(13)
         BR    14
*
*
         END
#endif
