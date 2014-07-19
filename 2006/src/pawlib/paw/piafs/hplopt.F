*
* $Id: hplopt.F,v 1.1.1.1 1996/03/01 11:39:26 mclareni Exp $
*
* $Log: hplopt.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:26  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/01 03/05/93  15.44.52  by  Alfred Nathaniel
*-- Author :    Alfred Nathaniel   02/05/93
      SUBROUTINE HPLOPT(CHOPTN,N)
      CHARACTER*(*) CHOPTN(*)

#include "paw/quest.inc"

      CHARACTER CHMAIL*80

      IF(N.LT.0) THEN
*
* inquire the status of an option
*
         WRITE(CHMAIL,'(A,I12,A)') 'HPLOPT',N,CHOPTN(1)
         CALL CZPUTA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0) RETURN
         CALL CZGETA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0) RETURN
         CALL PFLOG(9,'HPLOPT',CHMAIL)
         READ(CHMAIL,'(I12)') IQUEST(11)
         RETURN
      ENDIF
*
* set an option
*
      DO 10 I=1,N
         WRITE(CHMAIL,'(A,I12,A)') 'HPLOPT',N,CHOPTN(N)
         CALL CZPUTA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0) RETURN
 10   CONTINUE

      END
