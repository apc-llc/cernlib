*
* $Id: dzdgrz.F,v 1.1.1.1 1996/03/04 16:13:16 mclareni Exp $
*
* $Log: dzdgrz.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:16  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGRZ(IXSTOR,LTOP,MKEYS,LUN,CHOPTD)
      INTEGER IXSTOR,LTOP,MKEYS,LUN
      CHARACTER*(*) CHOPTD
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/zunit.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
      COMMON/DZDLK3/LNKLOC(3,0:15)
      COMMON/QUEST/IQUEST(100)
      PARAMETER (MSTACK = 100)
      INTEGER STACK(MSTACK), IP
      PARAMETER (MAXDIR=500)
      CHARACTER*16 CHDIR(MAXDIR)
      CHARACTER*80 CURDIR
 
*     bank formats:
 
*     for a directory:
*     IFLAG (=1), OFFSET-TO-NAME, NKEYS, NWKEY,NDIR,NQUOTA
*     KEYFORM((NWKEY+3)/4), KEYTAG (2*NWKEY), NAME
 
*     for a key
*     FLAG (=2), TSTAMP,CYCLE,NDATA,NWKEYS,KEYDEF((NWKEYS+3/4)),
*                KEYS(NWKEYS)
#include "zebra/qstore.inc"
      IST = JQSTOR
      CALL MZLINT(IXSTOR,'/DSDLK3/',
     +LNKLOC(1,IST),LNKLOC(3,IST),LNKLOC(3,IST))
*     IS    stackpointer
*     IP    counts subdirectories
 
      ISTART = 0
      IS = 1
      STACK(IS) = 0
      CALL RZCDIR(CURDIR,'R')
*      WRITE(IQPRNT,*)' Start at ',CURDIR
 
   10 CONTINUE
 
      CALL RZRDIR(MAXDIR,CHDIR,NDIRS)
      IF(NDIRS .GT. MAXDIR)THEN
          IF(IQPRNT.EQ.6)CALL IGTERM
          WRITE(IQPRNT,*)' NDIR > MAXDIR ',NDIRS,MAXDIR
          NDIRS = MAXDIR
      ENDIF
      IF(ISTART .EQ. 0)THEN
          CALL DZDBOK(IXSTOR,MKEYS,CHOPTD,LTOP,1,LNKLOC(3,IST))
          IF(INDEX(CHOPTD,'T').NE.0)THEN
             CALL RZCDIR(CURDIR,'R')
             WRITE(LUN,'(A)')CURDIR
             WRITE(LUN,'(A)')' *---'
          ENDIF
          ISTART = 1
      ENDIF
 
*     next subdirectory
 
      IP= STACK(IS) + 1
 
      IF(IP.GT.NDIRS)THEN
*       all done in this dir
          IF(IS.EQ.1)THEN
*            and back at top
*             WRITE(IQPRNT,*)'All done'
             GOTO 20
          ENDIF
*       step back
          IS = IS - 1
          CALL RZCDIR(BS,' ')
#include "zebra/qstore.inc"
          LNKLOC(3,IST) = LQ(KQS+LNKLOC(3,IST)+1)
          GOTO 10
      ELSE
*        go  down
          CALL RZCDIR(CHDIR(IP),' ')
*          WRITE(IQPRNT,*)'Enter dir: ',CHDIR(IP),' IS,IP ',IS,IP
*          CALL RZRDIR(MAXDIR,CHDIR,NDIRS)
*         remember old position
          STACK(IS) = IP
*         move stack pointer
          IS = IS+1
          IF(IS.GE.MSTACK)THEN
              IF(IQPRNT.EQ.6)CALL IGTERM
              WRITE(IQPRNT,*)'Stack overflow ',IS
              GOTO 20
          ENDIF
          STACK(IS) = 0
          CALL DZDBOK(IXSTOR,MKEYS,CHOPTD,
     &    LNKLOC(3,IST),-IP,LNKLOC(3,IST))
          IF(INDEX(CHOPTD,'T').NE.0)THEN
             CALL RZCDIR(CURDIR,'R')
             WRITE(LUN,'(A)')CURDIR
             WRITE(LUN,'(A)')' *---'
          ENDIF
          GOTO 10
      ENDIF
   20 CONTINUE
      LNKLOC(1,IST) = 0
      END
