*
* $Id: build_pjet.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: build_pjet.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE BUILD_PJET(RCP_BANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To add PJET banks to existing ISAJET data
C-                        if PJET does not already exist.
C-
C-   Inputs  : RCP_BANK - Name of RCP BANK containing PJET PARAMETERS
C-   Outputs : none
C-   Controls: none
C-
C-   Created  12-JAN-1990   Chip Stewart, Harrison B Prosper
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      CHARACTER*(*) RCP_BANK
C
      INTEGER LISAQ,LPJHD
      INTEGER GZISAQ,GZPJHD
      INTEGER ND,NS,NPJET
C
#include "zebcom.inc"
#include "izpjhd.inc"
C
C----------------------------------------------------------------------
C
C ****  Check if PJET exists; check number of links in ISAE
C
      IF ( LISAE .LE. 0 ) GOTO 999
C
      NS = IQ(LISAE-2)                  ! Number of structural links
C
      IF ( NS .GE. IZPJHD ) GOTO 998    ! make PJET anyway
C
C ****  Make 2 new links
C
      CALL MZPUSH ( IXCOM, LISAE, 2, 0, ' ')   ! MZPUSH 2 NEW REFERENCE LINKS
C
C ****  Make one new reference link
C
      LISAQ = GZISAQ ()                         ! Address of first ISAQ
    1 CALL MZPUSH ( IXCOM, LISAQ, 1, 0, ' ')    ! MZPUSH 1 NEW REFERENCE LINKS
      LISAQ = LQ (LISAQ)                        ! NEXT ISAQ
      IF  ( LISAQ.GT. 0 ) GOTO 1
C
C ****  DO PJET CALCULATIONS - IF  NO RCP FILE THEN
C ****  USE DEFAULTS SET IN PJPGET
C
  998 CALL PJET_RCP(RCP_BANK)
      CALL PJETFL
      LPJHD=GZPJHD ()
      NPJET=IQ(LPJHD+3)
      IQ(LISAE+7)=NPJET             !  Number of of PJET banks
  999 RETURN
      END
