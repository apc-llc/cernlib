*
* $Id: fatlog.F,v 1.1.1.1 1996/03/07 15:17:38 mclareni Exp $
*
* $Log: fatlog.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:38  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATLOG
 
      CHARACTER*255 CHLINE
 
#include "fatmen/fatpara.inc"
      DIMENSION     IVECT(NWDSFA)
      CHARACTER*8   CHVID
 
      PARAMETER     (LUNRZ=7)
      PARAMETER     (LUNFZ=8)
 
      LOGICAL       INIT
      DATA          INIT/.FALSE./
*
*     Read FATSERV LOG file and process *FATLOG* lines*
*
      OPEN(1,FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +     STATUS='OLD',IOSTAT=ISTAT)
      IF(ISTAT.NE.0) THEN
         WRITE(6,9001) ISTAT
 9001 FORMAT(' FATLOG. return code ',I6,' opening input logfile')
         STOP
      ENDIF
 
      OPEN(2,FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +     STATUS='NEW',IOSTAT=ISTAT)
      IF(ISTAT.NE.0) THEN
         WRITE(6,9002) ISTAT
 9002 FORMAT(' FATLOG. return code ',I6,' opening output logfile')
         STOP
      ENDIF
 
      NLINE = 0
      NLOG  = 0
 
   10 CONTINUE
 
      READ(1,'(A)',END=20) CHLINE
      LLINE  = LENOCC(CHLINE)
 
      NLINE  = NLINE + 1
      IF(INDEX(CHLINE(1:LLINE),'*FATLOG*').EQ.0) GOTO 10
      NLOG   = NLOG  + 1
 
      ISTART = INDEX(CHLINE(1:LLINE),' //')
      IF(ISTART.EQ.0) GOTO 10
 
      ISTART = ISTART + 1
      IEND   = INDEX(CHLINE(ISTART:LLINE),' ') + ISTART - 1
 
      IF(.NOT.INIT) THEN
         LEND = INDEX(CHLINE(ISTART+2:LLINE),'/') + ISTART + 2
         LEND = LEND + INDEX(CHLINE(LEND:LLINE),'/') - 2
         CALL FMSTRT(LUNRZ,LUNFZ,CHLINE(ISTART:LEND),IRC)
         INIT = .TRUE.
      ENDIF
 
      CALL FMPEEK(CHLINE(ISTART:IEND),IVECT,'G',IRC)
      CALL UHTOC(IVECT(MVIDFA),4,CHVID,NVIDFA)
      ISEQ = IVECT(MFSQFA)
 
      WRITE(CHLINE(LLINE+1:),9003) CHVID,ISEQ
 9003 FORMAT(1X,A8,1X,I3)
 
      WRITE(2,'(A)') CHLINE(1:LLINE+13)
 
      GOTO 10
 
   20 CLOSE(1)
      CLOSE(2)
 
      WRITE(6,9004) NLINE,NLOG
 9004 FORMAT(' FATLOG. processed ',I6,' lines of which ',I6,
     +       ' were *FATLOG* records')
 
      END
