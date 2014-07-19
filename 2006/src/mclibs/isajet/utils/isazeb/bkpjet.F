*
* $Id: bkpjet.F,v 1.11 2001/10/08 14:03:15 mclareni Exp $
*
* $Log: bkpjet.F,v $
* Revision 1.11  2001/10/08 14:03:15  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE BKPJET(LPJHD,LPJET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank PJET -
C-      IF NO PJET BANKS EXIST THEN BOOK THE FIRST ONE
C-      ELSE BOOK NEW PJET BANK AT END OF LINEAR CHAIN OF PJET BANKS
C-
C-   Inputs  : LPJHD - PJET HEADER BANK POINTER
C-   Outputs : LPJET - Link of Booked PJET Bank
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
#include "izpjet.inc"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C--   INTERNAL VARIABLES
      INTEGER LPJET,LPJETP
      INTEGER LPJHD
      INTEGER IXIO
      INTEGER GZPJHD
C
      INTEGER IDN
C----------------------------------------------------------------------
C
C--   DO PRELIMINARIES ON FIRST CALL
      IF(FIRST)THEN
        CALL MZFORM('PJET','1I 9F 2I',IXIO)
        FIRST=.FALSE.
      END IF
C
      IF(LPJHD.LE.0) LPJHD = GZPJHD()
      IF(LPJHD.LE.0) CALL BKPJHD(LPJHD)
C
      LPJETP=LPJHD-IZPJET
      IDN=0
      IF(LQ(LPJETP).EQ.0) THEN
C
C ****  FIRST PJET BANK NEEDS TO BE BOOKED
C
        CALL MZBOOK(IXMAIN,LPJET,LPJHD,-IZPJET,'PJET',2,2,12,IXIO,0)
        IQ(LPJET-5) = 1
      ELSE
C
C ****  LOOK FOR LAST PJET BANK TO ADD SUBSEQUENT PJET BANKS
C
   10   CONTINUE
        IF(LQ(LPJETP).NE.0) THEN
          LPJETP=LQ(LPJETP)
          IDN=IQ(LPJETP-5)
          GOTO 10
        END IF
C
C--         BOOK THE PJET BANKS, SET THE ID NUMBER
        CALL MZBOOK(IXMAIN,LPJET,LPJETP,0,'PJET',2,2,12,IXIO,0)
        IQ(LPJET-5)=IDN+1
      ENDIF
      IQ(LPJET+1)=1         ! Version number
  999 RETURN
      END
