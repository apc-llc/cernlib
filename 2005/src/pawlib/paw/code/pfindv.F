*
* $Id: pfindv.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pfindv.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/20 29/09/94  20.15.48  by  Fons Rademakers
*-- Author :    Fons Rademakers   04/11/92
      SUBROUTINE PFINDV(CHFILE, CADR, IERROR)
*.==========>
*.
*.     Scan the COMIS file for the occurance of Ntuple variables.
*.     Add all found variables to the VAR list.
*.
*..=========> ( A.A.Rademakers )
*
      CHARACTER*(*) CHFILE
      CHARACTER*80  TFILE
      INTEGER       CADR
*
      IERROR = 0
*
      LF = LENOCC(CHFILE)
      IF (LF .NE. 0) THEN
         TFILE = CHFILE
         CALL CUTOL(TFILE)
      ENDIF
*
*-- this should cover the cases .f, .ftn, .for, .fortran
      IF (LF.EQ.0 .OR. INDEXB(TFILE,'.f').NE.0) THEN
         CALL PFINDF(CADR, IERROR)
      ELSEIF (INDEXB(TFILE,' f') .NE. 0) THEN
         CALL PFINDF(CADR, IERROR)
      ELSEIF (TFILE(LF-2:LF) .EQ. '.sl') THEN
         CALL PFINDF(CADR, IERROR)
      ELSEIF (TFILE(LF-1:LF) .EQ. '.c') THEN
         CALL PFINDC(CHFILE, IERROR)
      ELSEIF (TFILE(LF-3:LF) .EQ. '.csl') THEN
         CALL PFINDC(CHFILE(:LF-2), IERROR)
      ELSE
         IERROR = 1
      ENDIF
*
      END
