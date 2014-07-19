*
* $Id: czdnet.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: czdnet.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZDNET( IBUF, ICONTR )
************************************************************************
*.
*... CZDNET   hook routine for FZIN and FZOUT
*.
*.
*. Author  : B.Holl
*. Version : 1.00
*. Created : 05-Apr-1990
*. Last Mod:
*.
*. Modification Log.
*.
*.**********************************************************************
*
#include "cspack/czdecnet.inc"
#include "cspack/czsock.inc"
      INTEGER IQUEST
      COMMON /QUEST/ IQUEST(100)
*
      CHARACTER*80 CHMAIL
*
      INTEGER IBUF(1), ICONTR(2), ICASE, NBYTES, NWORD, IK, ISTAT
*
*.......................................................................
*
*-- Get transfer direction and data type
*
      ICASE = ICONTR(1)+1
      IF( ICASE .LT. 3 ) THEN
         NBYTES = 4*LBUF
      ELSE
         NBYTES = 4*ICONTR(2)
      ENDIF
      GO TO ( 100, 200, 300, 400 ), ICASE
      IF( ICASE .LE. 0 .OR. ICASE .GT. 4 ) THEN
        WRITE(LUNCZ,'(1X,A)') '**** Invalid ICASE in CZTCP ****'
        RETURN
      ENDIF
*
*-- Receive Binary
*
100   ISTAT = 0
      NWORD = NBYTES/4
      READ( UNIT=LUNDEC, FMT=*, END=110, ERR=120 )
     +      (IBUF(IK), IK=1, NWORD )
      GO TO 130
110   CONTINUE
      WRITE(LUNCZ,'(1X,A,I4)') '**** CZTCP read error on unit: ',LUNDEC
      ISTAT = 1
      GO TO 130
120   CONTINUE
      WRITE(LUNCZ,'(1X,A,I4)') '**** CZTCP EOF on unit: ', LUNDEC
      ISTAT = 1
130   IQUEST(1) = ISTAT
      GO TO 999
*
*-- Send Binary
*
200   ISTAT = 0
      NWORD = NBYTES/4
      WRITE( UNIT=LUNDEC, FMT=*, ERR=210 )
     +       (IBUF(IK), IK=1, NWORD )
      GO TO 220
210   CONTINUE
      WRITE(LUNCZ,'(1X,A,I4)') '**** CZTCP write error on unit: ',LUNDEC
      ISTAT = 1
220   IQUEST(1) = ISTAT
      GO TO 999
*
*-- Receive Character
*
300   CALL CZDGTA( CHMAIL, ISTAT )
      IF( ISTAT .NE. 0 ) GO TO 310
      CALL UCTOH( CHMAIL, IBUF, 4, 80 )
      ICONTR(1) = 0
      GO TO 999
310   ICONTR(1) = 1
      GO TO 999
*
*-- Send Character
*
400   CHMAIL = ' '
      CALL UHTOC( IBUF, 4, CHMAIL, NBYTES )
      CALL CZDPTA( CHMAIL, ISTAT )
      IF( ISTAT .NE. 0 ) GO TO 410
      ICONTR(1)=0
      GO TO 999
410   ICONTR(1) = 1
*
999   RETURN
      END
