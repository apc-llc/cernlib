*
* $Id: bkiscm.F,v 1.11 2001/10/08 14:03:15 mclareni Exp $
*
* $Log: bkiscm.F,v $
* Revision 1.11  2001/10/08 14:03:15  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE BKISCM(ND,LISCM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank ISCM
C-
C-   Inputs  : ND    = NUMBER OF WORDS IN ISCM
C-   Outputs : LISCM  = Link of Booked ISCM Bank
C-   Controls: None
C-
C-   Created  13-DEC-1989 10:20:16.28  Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER LISCM
      INTEGER IXIO
      INTEGER ND
C
C--   ZEBRA BANKS
#include "zebcom.inc"
#include "iziscm.inc"
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LISCM = 0
      IF(FIRST)THEN
        CALL MZFORM('ISCM','-H',IXIO)        ! Describe Bank format
      ENDIF
C
      CALL MZBOOK
     &  (IXMAIN,LISCM,LISAB,-IZISCM,'ISCM',1,1,ND,IXIO,0)
C
  999 RETURN
      END
