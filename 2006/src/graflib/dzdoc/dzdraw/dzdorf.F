*
* $Id: dzdorf.F,v 1.1.1.1 1996/03/04 16:13:01 mclareni Exp $
*
* $Log: dzdorf.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:01  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDORF
*      IMPLICIT NONE
      INTEGER IQUEST(100)
      COMMON/QUEST/IQUEST
#include "dzdzbrinc.inc"
      CHARACTER*1 CMINIT
      CHARACTER*80 CHDOCB
      SAVE CHDOCB, CMINIT
*--
      INTEGER NCH, NCH1, IRET, IERR, IFLAG, LENOCC
 
      CHARACTER*80 CHFILE
      CHARACTER*4 CHOPT, CHOPT1
*--
      IFLAG=0
      CALL DZDIBR
      CALL RZCDIR(CWDSAV,'R')
      CALL KUGETS(CHFILE,NCH)
      CALL KUGETC(CHOPT,NCH1)
      IF(INDEX(CHOPT,'L').NE.0)THEN
         IF(CTDDOC.NE.' ')THEN
            CALL RZCDIR(CTDDOC,' ')
            CALL RZLDIR(' ',' ')
         ELSE
            WRITE(*,*)'No Rz doc file open'
         ENDIF
         GOTO 999
      ENDIF
      IF(NCH.LE.0)THEN
         WRITE(*,*)'No file name given'
         GOTO 999
      ENDIF
      CMINIT='R'
      IF(NCH.LE.0)THEN
         CHOPT=' '
      ELSE
         IF(   INDEX(CHOPT,'U').NE.0
     &   .OR. INDEX(CHOPT,'N').NE.0
     &   .OR. INDEX(CHOPT,'1').NE.0)CMINIT='U'
      ENDIF
      CHDOCB=CHFILE
      GOTO 10
 
      ENTRY  DZDOR1(CHOPT1,IERR)
 
      IFLAG=1
      IERR=0
      CALL RZCDIR(CWDSAV,'R')
      CHOPT=CHOPT1
      IF(   INDEX(CHOPT,'U').NE.0
     &   .OR. INDEX(CHOPT,'N').NE.0
     &   .OR. INDEX(CHOPT,'1').NE.0)THEN
         IF(CMINIT .NE. 'U')THEN
            WRITE(*,*)
     &      'No documentation RZ-file open for update'
            IERR=1
            GOTO 999
         ENDIF
      ENDIF
 
10    CONTINUE
 
*     close if already open
 
      CALL DZDCLD
      CTDDOC='//RZDOC'
*
      IF    (INDEX(CHOPT,'N').NE.0)THEN
         CALL DZDMRZ(LPDOCS,LPDOCB,' ',CHDOCB,' ',IRET)
         IF(IRET.NE.0)GOTO 888
         LUDOCB=LPDOCB
      ELSE  IF(INDEX(CHOPT,'U').NE.0)THEN
         CALL DZDURZ(LPDOCS,LPDOCB,' ',CHDOCB,' ',IRET)
         IF(IRET.NE.0)GOTO 888
         LUDOCB=LPDOCB
      ELSE
*         IDOCFL=1
         GOTO 777
      ENDIF
*     dont leave it open in update mode
      IF(IFLAG.EQ.0)THEN
         LUDOCB=LPDOCB
         CALL DZDCLD
      ELSE
          GOTO 999
      ENDIF
777   LUDOCB=LPDOCB
      CALL RZOPEN(LPDOCB,CTDDOC(3:),CHDOCB,' ',256,IRET)
      IF(IRET.EQ.0)THEN
         CALL RZFILE(LPDOCB,CTDDOC(3:),' ')
         GOTO 999
      ENDIF
888   CONTINUE
      CTDDOC=' '
      WRITE(*,*)' Error opening bank doc',IRET
      IDOCFL=0
      IF(IFLAG.EQ.1)IERR=IRET
999   CONTINUE
      IF(LENOCC(CWDSAV).GT.0)CALL RZCDIR(CWDSAV,' ')
      END
*********************************************************************
