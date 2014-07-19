*
* $Id: imfout.F,v 1.1.1.1 1996/02/14 13:11:02 mclareni Exp $
*
* $Log: imfout.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:02  mclareni
* Higz
*
*
#if defined(CERNLIB_MAIL)
#include "higz/pilot.h"
*CMZ :  1.09/00 05/12/89  11.55.12  by  O.Couet
*-- Author :    O.Couet   30/01/89
      SUBROUTINE IMFOUT(N,X)
*.===========>
*.
*.   This routine write a buffer of N floating points
*. on the std output.
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of floating points.
*.  REAL X(N) : Array.
*.
*..==========> (O.Couet)
#include "higz/himail.inc"
      DIMENSION X(*)
      CHARACTER*8 CHTEMP
*.______________________________________
*
      IF(N.EQ.2)THEN
         WRITE (CHMAIL,'(2E16.7)') X(1),X(2)
         CALL IMWRIT(2)
         RETURN
      ENDIF
*
      XMIN=VMIN(X,N)
      XMAX=VMAX(X,N)
      IF(XMIN.EQ.XMAX)THEN
         FACT=-1.
      ELSE
         FACT=(XMAX-XMIN)/99999.
      ENDIF
      WRITE (CHMAIL,'(2E16.7)') FACT,XMIN
      CALL IMWRIT(2)
      IF(FACT.LE.0.)RETURN
      J=0
      DO 10 I=1,N
         IX=(X(I)-XMIN)/FACT
         WRITE (CHTEMP,'(I5)') IX
         K=(5*J)+1
         CHMAIL(K:K+4)=CHTEMP(1:5)
         J=J+1
         IF(J.EQ.16)THEN
            J=0
            CALL IMWRIT(2)
         ENDIF
   10 CONTINUE
      IF(J.NE.0)CALL IMWRIT(2)
*
      END
#endif
