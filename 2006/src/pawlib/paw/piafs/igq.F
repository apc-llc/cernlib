*
* $Id: igq.F,v 1.1.1.1 1996/03/01 11:39:26 mclareni Exp $
*
* $Log: igq.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:26  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/01 03/05/93  15.44.52  by  Alfred Nathaniel
*-- Author :    Alfred Nathaniel   02/05/93
      SUBROUTINE IGQ(PNAME,RVAL)
      CHARACTER*(*) PNAME
      DIMENSION RVAL(*)

      CHARACTER CHMAIL*80

      WRITE(CHMAIL,'(A,E16.8,A)') 'IGQ   ',RVAL(1),PNAME
      CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0) RETURN
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0) RETURN
      CALL PFLOG(9,'IGQ   ',CHMAIL)

      IF(PNAME(:4).EQ.'RGB') THEN
         N=4
      ELSEIF(PNAME(:4).EQ.'TXAL' .OR. PNAME(:4).EQ.'TXFP') THEN
         N=2
      ELSE
         N=1
      ENDIF

      READ(CHMAIL,'(4E16.8)') (RVAL(I),I=1,N)

      END
