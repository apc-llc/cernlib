*
* $Id: mnmnos.F,v 1.1.1.1 1996/03/07 14:31:30 mclareni Exp $
*
* $Log: mnmnos.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNMNOS(FCN,FUTIL)
#include "minuit/d506dp.inc"
CC        Performs a MINOS error analysis on those parameters for
CC        which it is requested on the MINOS command by calling 
CC        MNMNOT for each parameter requested.
#include "minuit/d506cm.inc"
      EXTERNAL FCN,FUTIL
      IF (NPAR .LE. 0)  GO TO 700
      NGOOD = 0
      NBAD = 0
      NFCNMI = NFCN
C                                      . loop over parameters requested
      DO 570 KNT= 1, NPAR
      IF (INT(WORD7(2)) .EQ. 0) THEN
          ILAX = NEXOFI(KNT)
      ELSE
          IF (KNT .GE. 7)  GO TO 580
          ILAX = INT(WORD7(KNT+1))
          IF (ILAX .EQ. 0)  GO TO 580
          IF (ILAX .GT. 0 .AND. ILAX .LE. NU) THEN
             IF (NIOFEX(ILAX) .GT. 0)  GO TO 565
          ENDIF
          WRITE (ISYSWR,564) ILAX
  564     FORMAT (' PARAMETER NUMBER ',I5,' NOT VARIABLE. IGNORED.')
          GO TO 570
      ENDIF
  565 CONTINUE
C                                         calculate one pair of M E's
      ILAX2 = 0
      CALL MNMNOT(FCN,ILAX,ILAX2,VAL2PL,VAL2MI,FUTIL)
      IF (LNEWMN)  GO TO 650
C                                          update NGOOD and NBAD
      IIN = NIOFEX(ILAX)
      IF (ERP(IIN) .GT. ZERO) THEN
         NGOOD=NGOOD+1
      ELSE
         NBAD=NBAD+1
      ENDIF
      IF (ERN(IIN) .LT. ZERO) THEN
         NGOOD=NGOOD+1
      ELSE
         NBAD=NBAD+1
      ENDIF
  570 CONTINUE
C                                           end of loop . . . . . . .
  580 CONTINUE
C                                        . . . . printout final values .
      CFROM = 'MINOS   '
      NFCNFR = NFCNMI
      CSTATU= 'UNCHANGED '
      IF (NGOOD.EQ.0.AND.NBAD.EQ.0) GO TO 700
      IF (NGOOD.GT.0.AND.NBAD.EQ.0) CSTATU='SUCCESSFUL'
      IF (NGOOD.EQ.0.AND.NBAD.GT.0) CSTATU='FAILURE   '
      IF (NGOOD.GT.0.AND.NBAD.GT.0) CSTATU='PROBLEMS  '
      IF (ISW(5) .GE. 0) CALL MNPRIN(4,AMIN)
      IF (ISW(5) .GE. 2) CALL MNMATU(0)
      GO TO 900
C                                        . . . new minimum found . . . .
  650 CONTINUE
      CFROM = 'MINOS   '
      NFCNFR = NFCNMI
      CSTATU= 'NEW MINIMU'
      IF (ISW(5) .GE. 0) CALL MNPRIN(4,AMIN)
      WRITE (ISYSWR,675)
  675 FORMAT(/50H NEW MINIMUM FOUND.  GO BACK TO MINIMIZATION STEP./1H ,
     +60(1H=)/60X,1HV/60X,1HV/60X,1HV/57X,7HVVVVVVV/58X,5HVVVVV/59X,
     +3HVVV/60X,1HV//)
      GO TO 900
  700 WRITE (ISYSWR,'(A)') ' THERE ARE NO MINOS ERRORS TO CALCULATE.'
  900 RETURN
      END
