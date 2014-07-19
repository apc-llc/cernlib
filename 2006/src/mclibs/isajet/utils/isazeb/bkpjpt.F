*
* $Id: bkpjpt.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: bkpjpt.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE BKPJPT(LPJET,NP,LPJPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank PJPT under PJET-
C-
C-   Inputs  : LPJET - PJET BANK POINTER
C-             NP - NUMBER OF PARTONS IN THIS PJET
C-   Outputs : LPJPT - Link of Booked PJPT Bank
C-   Controls: None
C-
C-   Created   7-NOV-1989 18:10:09.84  Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
C
C--   ZEBRA BANKS
#include "zebcom.inc"
#include "izpjpt.inc"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C--   INTERNAL VARIABLES
      INTEGER LPJPT,LPJPTP
      INTEGER LPJET
      INTEGER IXIO
C
      INTEGER NP
C----------------------------------------------------------------------
C
C--   DO PRELIMINARIES ON FIRST CALL
      IF(FIRST)THEN
        CALL MZFORM('PJPT','-I',IXIO)
        FIRST=.FALSE.
      END IF
C
      IF(LPJET.LE.0) CALL GZPJET( )
C
      IF ( LQ(LPJET-IZPJPT).LE.0 ) THEN
        CALL MZBOOK(IXMAIN,LPJPT,LPJET,-IZPJPT,'PJPT',NP+1,0,1,IXIO,0)
        IQ(LPJPT+1)=1         ! Version number
      ENDIF
C
  999 RETURN
      END
