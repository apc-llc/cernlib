*
* $Id: csetcva.F,v 1.1.1.1 1996/02/26 17:16:32 mclareni Exp $
*
* $Log: csetcva.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 09/11/94  10.46.18  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   31/10/94
      SUBROUTINE CSETCVA(II,IERR)
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
      PARAMETER (  KTINT=1,   KLINT=1, KTREAL=2,  KLREAL=1,
     +  KTCHAR=3,  KTLOG=4,   KLLOG=1, KTDOU=5 )
      PARAMETER (KSDIM=3, NBYTPW=4)
*-
      IERR=0
      I=II
      IF(I.EQ.0)RETURN
      IL=IQ(I+1)
      CALL CSLDLI(IL)
      ISFTG=-1
      NUMGB=NUMGI
      ITYPGI=IABS(ITYPGI)
      IF(ITYPGI.EQ.KTCHAR)THEN
*        IQ(NUMGB+1)=IQ(NUMGB+1)+2
        KGLO=2
      ELSE
*        IQ(NUMGB+1)=IQ(NUMGB+1)+1
        KGLO=1
      ENDIF
      IF(IQ(NUMGB+1).LE.0 .OR. IQ(NUMGB+1).EQ.3)THEN
        IQ(NUMGB+1)=IQ(NUMGB+1)+KGLO
      ENDIF
 1    IF(I.EQ.0)THEN
        CALL CSTLGB(NUMGB,-ISFTG+1)
        RETURN
      ENDIF
      IL=IQ(I+1)
      CALL CSLDLI(IL)
      ITYPGI=IABS(ITYPGI)
      IF(ITYPGI.EQ.KTCHAR)THEN
        IF(KGLO.EQ.1)IERR=-12
      ELSE
        IF(KGLO.EQ.2)IERR=-12
      ENDIF
      IF(MODEGI.GT.1)THEN
*  ARR
        N=IQ(MODEGI+KSDIM)
        LENT=IQ(MODEGI+N+3)*LENEGI
      ELSE
*  VAR
        LENT=LENEGI
        MODEGI=1
      ENDIF
      ISHGI=ISFTG
      IF(ITYPGI.EQ.KTCHAR)THEN
* for mixed arth and char test of nbytpw alignment
        LENT=(LENT-1)/NBYTPW+1
      ENDIF
      CALL CSRTLI(IL)
      ISFTG=ISFTG-LENT
      I=IQ(I)
      GO TO 1
      END
