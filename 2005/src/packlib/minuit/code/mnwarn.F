*
* $Id: mnwarn.F,v 1.1.1.1 1996/03/07 14:31:32 mclareni Exp $
*
* $Log: mnwarn.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:32  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNWARN(COPT,CORG,CMES)
C     If COPT='W', CMES is a WARning message from CORG.
C     If COPT='D', CMES is a DEBug message from CORG.
C         If SET WARnings is in effect (the default), this routine
C             prints the warning message CMES coming from CORG.
C         If SET NOWarnings is in effect, the warning message is
C             stored in a circular buffer of length MAXMES.
C         If called with CORG=CMES='SHO', it prints the messages in
C             the circular buffer, FIFO, and empties the buffer.
#include "minuit/d506dp.inc"
#include "minuit/d506cm.inc"
      CHARACTER COPT*1, CORG*(*), CMES*(*), CTYP*7
      PARAMETER (MAXMES=10)
      CHARACTER     ORIGIN(MAXMES,2)*10, WARMES(MAXMES,2)*60
      COMMON/MN7WRC/ORIGIN,              WARMES
      COMMON/MN7WRI/NFCWAR(MAXMES,2),ICIRC(2)
      CHARACTER ENGLSH*20
C
      IF (CORG(1:3).EQ.'SHO' .AND. CMES(1:3).EQ.'SHO')  GO TO 200
C             Either print warning or put in buffer
      IF (COPT .EQ. 'W')  THEN
        ITYP = 1
        IF (LWARN) THEN
          WRITE (ISYSWR,'(A,A/A,A)') ' MINUIT WARNING IN ',CORG,
     +              ' ============== ',CMES
          RETURN
        ENDIF
      ELSE
        ITYP = 2
        IF (LREPOR) THEN
          WRITE (ISYSWR,'(A,A/A,A)') ' MINUIT DEBUG FOR  ',CORG,
     +              ' ============== ',CMES
          RETURN
        ENDIF
      ENDIF
C                 if appropriate flag is off, fill circular buffer
         IF (NWRMES(ITYP) .EQ. 0)  ICIRC(ITYP) = 0
         NWRMES(ITYP) = NWRMES(ITYP) + 1
         ICIRC(ITYP) = ICIRC(ITYP) + 1
         IF (ICIRC(ITYP) .GT. MAXMES) ICIRC(ITYP) = 1
         IC = ICIRC(ITYP)
         ORIGIN(IC,ITYP) = CORG
         WARMES(IC,ITYP) = CMES
         NFCWAR(IC,ITYP) = NFCN
      RETURN
C
C             'SHO WARnings', ask if any suppressed mess in buffer
  200 CONTINUE
      IF (COPT .EQ. 'W') THEN
        ITYP = 1
        CTYP = 'WARNING'
      ELSE
        ITYP = 2
        CTYP = '*DEBUG*'
      ENDIF
      IF (NWRMES(ITYP) .GT. 0) THEN
         ENGLSH = ' WAS SUPPRESSED.  '
         IF (NWRMES(ITYP) .GT. 1) ENGLSH = 'S WERE SUPPRESSED.'
         WRITE (ISYSWR,'(/1X,I5,A,A,A,A/)') NWRMES(ITYP),
     +    ' MINUIT ',CTYP,' MESSAGE', ENGLSH
         NM = NWRMES(ITYP)
         IC = 0
         IF (NM .GT. MAXMES) THEN
              WRITE (ISYSWR,'(A,I2,A)')  ' ONLY THE MOST RECENT ',
     +          MAXMES,' WILL BE LISTED BELOW.'
              NM = MAXMES
              IC = ICIRC(ITYP)
         ENDIF
         WRITE (ISYSWR,'(A)') '  CALLS  ORIGIN         MESSAGE'
           DO 300 I= 1, NM
           IC = IC + 1
           IF (IC .GT. MAXMES)  IC = 1
           WRITE (ISYSWR,'(1X,I6,1X,A,1X,A)')
     +           NFCWAR(IC,ITYP),ORIGIN(IC,ITYP),WARMES(IC,ITYP)
 300       CONTINUE
         NWRMES(ITYP) = 0
         WRITE (ISYSWR,'(1H )')
      ENDIF
      RETURN
      END
