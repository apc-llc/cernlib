*
* $Id: dzdfzi.F,v 1.1.1.1 1996/03/04 16:13:04 mclareni Exp $
*
* $Log: dzdfzi.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:04  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDFZI
#include "zebra/mqsys.inc"
#include "dzdzbrinc.inc"
      INTEGER NCH, KNUMB, ISTORE, IIST, MMBK(20)
      CHARACTER*1 CHOPT,CHDS
      CHARACTER*2 CRET
      CHARACTER*80 CMD
      CHARACTER*10 CKNUMB, CMFZIN(5)
      DATA CMFZIN/'S_of_Run', 'E_of_Run','Zebra_EOF', 'Sys_EOF',
     &            'Sys_EOD'/
      CALL KUGETC(CKNUMB,NCH)
      IF(NCH.LE.0)GOTO 999
      CALL DZDCTI(CKNUMB,KNUMB)
      IF(KNUMB.LE.0)GOTO 999
      CALL KUPATL(CMD,NCH)
      IF     (INDEX(CMD,'_HEAD').NE.0)THEN
          CHOPT='S'
      ELSE IF(INDEX(CMD,'_PDS').NE.0)THEN
          CHOPT='A'
      ELSE
          CHOPT=' '
      ENDIF
      CALL KUGETC(CHDS,NCH)
      ISTORE=0
      IIST = 1
      CALL SBYT(ISTORE,IIST,27,6)
*     init link area for store 0
 
      IF(LZEFLG(ISTORE).EQ.0)THEN
         CALL MZLINK(IIST,'DZDZEB',
     &   LZEBLK(1,ISTORE),LZEBLK(3,ISTORE),LZEBLK(3,ISTORE))
         LZEFLG(ISTORE)=1
      ENDIF
      IF(LZEBLK(1,ISTORE).NE.0)CALL MZDROP(IIST,LZEBLK(1,ISTORE),' ')
      LZEBLK(1,ISTORE)=0
      NUH=NUHMAX
      CALL FZIN(KNUMB,IIST,LZEBLK(1,ISTORE),1,CHOPT,NUH,IUHEAD)
      IF(IQUEST(1).NE.0)THEN
         IF     (IQUEST(1).GE.1 .AND. IQUEST(1).LE.5)THEN
            WRITE(*,*)'FZIN:', CMFZIN(IQUEST(1))
         ELSE IF(IQUEST(1).EQ.6)THEN
            WRITE(*,*)'Try ro read beyond EOD, rewind unit',KNUMB
            CALL FZENDI(KNUMB,'I')
*            CLOSE(KNUMB)
         ELSE
            WRITE(*,*)'Error from FZIN', IQUEST(1)
         ENDIF
         NUH=0
         IF(ABS(IQUEST(1)).GT.9)THEN
            IQRET=SIGN(9,IQUEST(1))
         ELSE
            IQRET=IQUEST(1)
         ENDIF
      ELSE
         IQRET=0
         IF(INDEX(CHOPT,'S').NE.0 .OR. LZEBLK(1,ISTORE).EQ.0)THEN
            IF(INDEX(CHOPT,'S').EQ.0 .AND. LZEBLK(1,ISTORE).EQ.0)
     &      WRITE(*,*)'FZIN: Link to d/s is 0!'
            WRITE(*,*)
     &      'FZIN, User header only, words read:',NUH
            IF(NUH.GT.0)THEN
               CALL UCTOH('USHD',MMBK(1),4,4)
               MMBK(2)=0
               MMBK(3)=0
               MMBK(4)=NUH
               MMBK(5)=0
               IF(IQUEST(20).GT.0)THEN
                  CALL UCOPY(IQUEST(21),MMBK(5),IQUEST(20))
               ENDIF
               CALL MZLIFT(IIST,LZEBLK(1,ISTORE),LZEBLK(1,ISTORE),1,
     &         MMBK,0)
               CALL UCOPY(IUHEAD,IQ(LZEBLK(1,ISTORE)+KQSP+1),NUH)
               INMFLG=1
            ENDIF
         ELSE
*            IF(IQUEST(14).EQ.0)THEN
*               WRITE(*,*)
*     &        'FZIN: d/s is empty'
*              GOTO 999
*            ENDIF
            INMFLG=2
            WRITE(*,*)
     &      'FZIN, Words read: ',IQUEST(14), ' into Store 0, Div 1'
            IF(NUH.GT.0)THEN
               NWIOCH=IQUEST(20)
               WRITE(*,*)
     &         'FZIN, NWIOCH=',NWIOCH
               IF(NWIOCH.GT.0)THEN
                  CALL UCOPY(IQUEST(21),IOCHRD,MIN(NWIOCH,10))
               ENDIF
            ENDIF
         ENDIF
         IF(CHDS.EQ.'D')THEN
            CALL IZPICT('DZDISP','SQ')
            CALL IZPICT('DZDISP','M')
            CALL ISTXFP(6,0)
            CALL DZDISP(IIST,LZEBLK(1,ISTORE),CTDDOC,'D'//CCOL//CCACT,
     &      IWKZEB,IWMZEB,ILOZEB, 0)
         ENDIF
      ENDIF
      WRITE(CRET,'(I2)')IQRET
      CALL KUEXEC('ALIAS/CREATE IQUEST_1 '//CRET)
 999  END
**********************************************************************
