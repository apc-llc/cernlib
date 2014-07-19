*
* $Id: dzdibr.F,v 1.1.1.1 1996/03/04 16:13:01 mclareni Exp $
*
* $Log: dzdibr.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:01  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDIBR
*
*     init Zebra browser
*
      LOGICAL KUQSVR
#include "dzdzbrinc.inc"
#include "dzdprm.inc"
      INTEGER INIFLG
      SAVE INIFLG
      EXTERNAL DZDASE
      DATA INIFLG/0/
      IF(INIFLG.NE.0)GOTO 999
*      CALL DZDLUN(LUPTMP,IERR)
*      IF(IERR.NE.0)THEN
*        WRITE(*,*)'Error finding free LUN',IERR
        LUPTMP=67
*      ENDIF
*      CALL DZDLUN(LPDOCB,IERR)
*      IF(IERR.NE.0)THEN
*        WRITE(*,*)'Error finding free LUN',IERR
        LPDOCB=68
*      ENDIF
*      CALL DZDLUN(LPDOCS,IERR)
*      IF(IERR.NE.0)THEN
*        WRITE(*,*)'Error finding free LUN',IERR
        LPDOCS=69
*      ENDIF
      WRITE(*,*)'Init Zebra browser, it will use units:',
     &          65, LUPTMP, LPDOCB, LPDOCS
      CTDDOC=' '
      CWDSAV=' '
      CIDMSK=' '
      CHOPD1=' '
      NUH=0
      INMFLG=0
      LUNTMP=0
      LUDOCB=0
      INIFLG=1
      IWKZEB=1
      IWMZEB=0
      ILOZEB=1
      IDOCFL=0
      NDIADD=0
      NIDMSK=0
      NKYMSK=0
      IAPPFL=0
      DO 3 I=1,9
3     KACMSK(I)=0
      IBCCOL=7
      IBDCOL=3
      IKDSP1=1
      IKDSP2=2
      ICYDSP=1000000
      IDREFL=1
      CCOL  ='C'
      CCACT  ='N'
      GRIDSC=1.
      IDZTFO=0
      IDZTPR=0
      LNEWWI=.TRUE.
      EDITSV=KUQSVR()
      IF(EDITSV)THEN
         CALL KUEUSR(DZDASE)
      ELSE
         WRITE(*,*)'No edit server available, will use KUEDIT'
      ENDIF
      NLKUSE=0
      ISTUSE=0
      IACTST=0
      DO 5 IST=0,15
      LZEFLG(IST)=0
      LTDFLG(IST)=0
5     LTBFLG(IST)=0
999   END
*********************************************************************
*+DECK,DZDLUN.
*      SUBROUTINE DZDLUN(LUN,IERR)
*      INTEGER LUN,IERR
*--
*      IERR=0
*      CALL PALUNF(11,3,LUN)
*      IF(LUN.LE.0)IERR=-1
*      END
*********************************************************************
