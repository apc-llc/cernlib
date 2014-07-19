*
* $Id: bkpjhd.F,v 1.11 2001/10/08 14:03:15 mclareni Exp $
*
* $Log: bkpjhd.F,v $
* Revision 1.11  2001/10/08 14:03:15  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE BKPJHD(LPJHD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank PJHD
C-      IF NO PJHD BANKS EXIST THEN BOOK THE FIRST ONE
C-      ELSE BOOK NEW PJHD BANK AT END OF LINEAR CHAIN OF PJHD BANKS
C-
C-   Inputs  : NONE
C-   Outputs : LPJHD Link of Booked PJHD Bank
C-   Controls: None
C-
C-   Created   7-NOV-1989 17:57:58.00  Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER LPJHD,LPJHDP
      INTEGER IXIO
      INTEGER IDN
C
C--   ZEBRA BANKS
#include "zebcom.inc"
#include "izpjhd.inc"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LPJHD = 0
      IF(FIRST)THEN
C
        FIRST = .FALSE.
        CALL MZFORM('PJHD','3I2F-I',IXIO)        ! Describe Bank format
C
      ENDIF
C
      IDN=0

C
      LPJHDP = LISAE-IZPJHD
C
      IF(LQ(LPJHDP).EQ.0) THEN
C
C ****  FIRST PJHD BANK NEEDS TO BE BOOKED
C

        CALL MZBOOK(IXMAIN,LPJHD,LISAE,-IZPJHD,'PJHD',2,2,8,IXIO,0)
        IQ(LPJHD-5) = 0
      ELSE
C
C ****  LOOK FOR LAST PJHD BANK TO ADD SUBSEQUENT PJHD BANKS
C
   10   CONTINUE
        IF(LQ(LPJHDP).NE.0) THEN
          LPJHDP=LQ(LPJHDP)
          IDN=IQ(LPJHDP-5)
          GOTO 10
        END IF
C
C--         BOOK THE PJHD BANKS, SET THE ID NUMBER
        CALL MZBOOK(IXMAIN,LPJHD,LPJHDP,0,'PJHD',2,2,8,IXIO,0)
        IQ(LPJHD-5)=IDN+1
      ENDIF
      IQ(LPJHD+1)=2         ! Version number
C
  999 RETURN
      END
