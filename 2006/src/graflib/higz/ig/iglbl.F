*
* $Id: iglbl.F,v 1.1.1.1 1996/02/14 13:10:36 mclareni Exp $
*
* $Log: iglbl.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:36  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.16/09 07/01/93  15.37.36  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGLBL(NLBL,CHLBL)
*.==========>
*.
*.   To change the alphanumeric label of the axis.
*.
*. _Input parameters:
*.
*. INTEGER NLBL : Number of alphanumeric labels in CHLBL
*. CHARACTER CHLBL : New value of the alphanumeric labels
*.
*..=========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hilabs.inc"
      CHARACTER*(*) CHLBL(*)
*._____________________________
*
      IF(NLBL.LE.0.OR.NLBL.GT.MAXLAB)THEN
         CALL IGERR('Wrong number of labels','IGLBL')
         RETURN
      ENDIF
*
      NHILAB = NLBL
      DO 10 I=1,NLBL
         HILABS(I) = CHLBL(I)
   10 CONTINUE
      DO 20 I=NLBL+1,MAXLAB
         HILABS(I) = ' '
   20 CONTINUE
*
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZLBL
#endif
*
      END
