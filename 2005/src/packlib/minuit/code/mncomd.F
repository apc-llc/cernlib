*
* $Id: mncomd.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mncomd.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNCOMD(FCN,CRDBIN,ICONDN,FUTIL)
#include "minuit/d506dp.inc"
CC        Called by user.  'Reads' a command string and executes.
CC     Equivalent to MNEXCM except that the command is given as a
CC          character string.
CC
CC     ICONDN = 0: command executed normally
CC              1: command is blank, ignored
CC              2: command line unreadable, ignored
CC              3: unknown command, ignored
CC              4: abnormal termination (e.g., MIGRAD not converged)
CC              5: command is a request to read PARAMETER definitions
CC              6: 'SET INPUT' command
CC              7: 'SET TITLE' command
CC              8: 'SET COVAR' command
CC              9: reserved
CC             10: END command
CC             11: EXIT or STOP command
CC             12: RETURN command
CC
#include "minuit/d506cm.inc"
      DIMENSION PLIST(MAXP)
      CHARACTER COMAND*(MAXCWD)
      CHARACTER CLOWER*26, CUPPER*26
      LOGICAL LEADER
C
      EXTERNAL FCN,FUTIL
      CHARACTER*(*) CRDBIN
      CHARACTER*100 CRDBUF
      DATA CLOWER/'abcdefghijklmnopqrstuvwxyz'/
      DATA CUPPER/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C
      LENBUF = LEN(CRDBIN)
      CRDBUF = CRDBIN
      ICONDN = 0
C     record not case-sensitive, get upper case, strip leading blanks
      LEADER = .TRUE.
      IPOS = 1
         DO 110 I= 1, MIN(MAXCWD,LENBUF)
         IF (CRDBUF(I:I) .EQ. '''') GO TO 111
         IF (CRDBUF(I:I) .EQ. ' ')  THEN
           IF (LEADER) IPOS = IPOS + 1
           GO TO 110
         ENDIF
         LEADER = .FALSE.
           DO 108 IC= 1, 26
           IF (CRDBUF(I:I) .EQ. CLOWER(IC:IC)) CRDBUF(I:I)=CUPPER(IC:IC)
  108      CONTINUE
  110    CONTINUE
  111 CONTINUE
C                     blank or null command
      IF (IPOS .GT. LENBUF)  THEN
         WRITE (ISYSWR,'(A)') ' BLANK COMMAND IGNORED.'
         ICONDN = 1
         GO TO 900
      ENDIF
C                                           . .   preemptive commands
C               if command is 'PARAMETER'
      IF (CRDBUF(IPOS:IPOS+2) .EQ. 'PAR')    THEN
         ICONDN = 5
         LPHEAD = .TRUE.
         GO TO 900
         ENDIF
C               if command is 'SET INPUT'
      IF (CRDBUF(IPOS:IPOS+6) .EQ. 'SET INP')  THEN
         ICONDN = 6
         LPHEAD = .TRUE.
         GO TO 900
         ENDIF
C              if command is 'SET TITLE'
      IF (CRDBUF(IPOS:IPOS+6) .EQ. 'SET TIT')  THEN
         ICONDN = 7
         LPHEAD = .TRUE.
         GO TO 900
         ENDIF
C               if command is 'SET COVARIANCE'
      IF (CRDBUF(IPOS:IPOS+6) .EQ. 'SET COV')   THEN
         ICONDN = 8
         LPHEAD = .TRUE.
         GO TO 900
         ENDIF
C               crack the command . . . . . . . . . . . . . . . .
      CALL MNCRCK(CRDBUF(IPOS:LENBUF),MAXCWD,COMAND,LNC,
     +                            MAXP,  PLIST, LLIST, IERR,ISYSWR)
      IF (IERR .GT. 0) THEN
            WRITE (ISYSWR,'(A)') ' COMMAND CANNOT BE INTERPRETED'
            ICONDN = 2
            GO TO 900
      ENDIF
C
      CALL MNEXCM(FCN,COMAND(1:LNC),PLIST,LLIST,IERR,FUTIL)
      ICONDN = IERR
  900 RETURN
      END
