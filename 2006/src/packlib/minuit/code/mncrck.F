*
* $Id: mncrck.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mncrck.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNCRCK(CRDBUF,MAXCWD,COMAND,LNC,
     +                         MXP,   PLIST, LLIST,IERR,ISYSWR)
#include "minuit/d506dp.inc"
CC
CC       Called from MNREAD.
CC       Cracks the free-format input, expecting zero or more
CC         alphanumeric fields (which it joins into COMAND(1:LNC))
CC         followed by one or more numeric fields separated by
CC         blanks and/or one comma.  The numeric fields are put into
CC         the LLIST (but at most MXP) elements of PLIST.
CC      IERR = 0 if no errors,
CC           = 1 if error(s).
CC      Diagnostic messages are written to ISYSWR
CC
      PARAMETER (MAXELM=25, MXLNEL=19)
      CHARACTER*(*) COMAND, CRDBUF
      CHARACTER CNUMER*13, CELMNT(MAXELM)*(MXLNEL), CNULL*15
      DIMENSION LELMNT(MAXELM),PLIST(MXP)
      DATA CNULL /')NULL STRING   '/
      DATA CNUMER/'123456789-.0+'/
      IELMNT = 0
      LEND = LEN(CRDBUF)
      NEXTB = 1
      IERR = 0
C                                   . . . .  loop over words CELMNT
   10 CONTINUE
      DO 100 IPOS= NEXTB,LEND
         IBEGIN = IPOS
         IF (CRDBUF(IPOS:IPOS).EQ.' ')  GO TO 100
         IF (CRDBUF(IPOS:IPOS).EQ.',')  GO TO 250
         GO TO 150
  100 CONTINUE
         GO TO 300
  150 CONTINUE
C               found beginning of word, look for end
         DO 180 IPOS = IBEGIN+1,LEND
         IF (CRDBUF(IPOS:IPOS).EQ.' ')  GO TO 250
         IF (CRDBUF(IPOS:IPOS).EQ.',')  GO TO 250
  180    CONTINUE
      IPOS = LEND+1
  250 IEND = IPOS-1
      IELMNT = IELMNT + 1
      IF (IEND .GE. IBEGIN) THEN
         CELMNT(IELMNT) = CRDBUF(IBEGIN:IEND)
      ELSE
         CELMNT(IELMNT) = CNULL
      ENDIF
      LELMNT(IELMNT) = IEND-IBEGIN+1
      IF (LELMNT(IELMNT) .GT. MXLNEL)  THEN
         WRITE (ISYSWR, 253) CRDBUF(IBEGIN:IEND),CELMNT(IELMNT)
  253    FORMAT (' MINUIT WARNING: INPUT DATA WORD TOO LONG.'
     +   /'     ORIGINAL:',A
     +   /' TRUNCATED TO:',A)
         LELMNT(IELMNT) = MXLNEL
         ENDIF
      IF (IPOS .GE. LEND) GO TO 300
      IF (IELMNT .GE. MAXELM)  GO TO 300
C                     look for comma or beginning of next word
         DO 280 IPOS= IEND+1,LEND
         IF (CRDBUF(IPOS:IPOS) .EQ. ' ') GO TO 280
         NEXTB = IPOS
         IF (CRDBUF(IPOS:IPOS) .EQ. ',') NEXTB = IPOS+1
         GO TO 10
  280    CONTINUE
C                 All elements found, join the alphabetic ones to
C                                form a command
  300 CONTINUE
      NELMNT = IELMNT
      COMAND = ' '
      LNC = 1
      PLIST(1) = 0.
      LLIST = 0
      IF (IELMNT .EQ. 0)  GO TO 900
      KCMND = 0
         DO 400 IELMNT = 1, NELMNT
         IF (CELMNT(IELMNT) .EQ. CNULL)  GO TO 450
            DO 350 IC= 1, 13
            IF (CELMNT(IELMNT)(1:1) .EQ. CNUMER(IC:IC)) GO TO 450
  350       CONTINUE
         IF (KCMND .GE. MAXCWD) GO TO 400
         LEFT = MAXCWD-KCMND
         LTOADD = LELMNT(IELMNT)
         IF (LTOADD .GT. LEFT) LTOADD=LEFT
         COMAND(KCMND+1:KCMND+LTOADD) = CELMNT(IELMNT)(1:LTOADD)
         KCMND = KCMND + LTOADD
         IF (KCMND .EQ. MAXCWD)  GO TO 400
         KCMND = KCMND + 1
         COMAND(KCMND:KCMND) = ' '
  400    CONTINUE
      LNC = KCMND
      GO TO 900
  450 CONTINUE
      LNC = KCMND
C                      . . . .  we have come to a numeric field
      LLIST = 0
      DO 600 IFLD= IELMNT,NELMNT
      LLIST = LLIST + 1
      IF (LLIST .GT. MXP) THEN
         NREQ = NELMNT-IELMNT+1
         WRITE (ISYSWR,511) NREQ,MXP
  511 FORMAT (/' MINUIT WARNING IN MNCRCK: '/ ' COMMAND HAS INPUT',I5,
     + ' NUMERIC FIELDS, BUT MINUIT CAN ACCEPT ONLY',I3)
         GO TO 900
      ENDIF
      IF (CELMNT(IFLD) .EQ. CNULL)  THEN
          PLIST(LLIST) = 0.
        ELSE
          READ (CELMNT(IFLD), '(BN,F19.0)',ERR=575) PLIST(LLIST)
      ENDIF
      GO TO 600
  575 WRITE (ISYSWR,'(A,A,A)') ' FORMAT ERROR IN NUMERIC FIELD: "',
     + CELMNT(IFLD)(1:LELMNT(IFLD)),'"'
      IERR = 1
      PLIST(LLIST) = 0.
  600 CONTINUE
C                                  end loop over numeric fields
  900 CONTINUE
      IF (LNC .LE. 0)  LNC=1
      RETURN
      END
