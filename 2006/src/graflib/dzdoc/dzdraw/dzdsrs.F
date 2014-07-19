*
* $Id: dzdsrs.F,v 1.1.1.1 1996/03/04 16:13:05 mclareni Exp $
*
* $Log: dzdsrs.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:05  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSRS
#include "zebra/zunit.inc"
#include "dzdzbrinc.inc"
      INTEGER NCH, IFC, LUSAVE, INDEXC,ISTAT
      CHARACTER*20 CHPATH
 
      CALL KUGETC(CHPATH,NCH)
      IF(NCH.LE.0)GOTO 999
      IFC=INDEX(CHPATH,'R_')
      IF(IFC.LE.0)IFC=INDEXC(CHPATH,'/')-2
      LUSAVE=IQPRNT
      LUNTMP=LUPTMP
      CALL KUOPEN(LUNTMP,'dzdoc.scratch1','UNKNOWN',ISTAT)
      IQPRNT=LUNTMP
      CALL RZSTAT('//'//CHPATH(IFC+2:),100,' ')
      CLOSE(LUNTMP)
      LUNTMP=0
      IF(EDITSV)THEN
         CALL KUESVR('dzdoc.scratch1', ISTAT)
      ELSE
         CALL KUEDIT('dzdoc.scratch1', ISTAT)
         LUNTMP=LUPTMP
         CALL KUOPEN(LUNTMP,'dzdoc.scratch1','UNKNOWN',ISTAT)
         IF(ISTAT.EQ.0)CALL KICLOS(LUNTMP,'DELETE',ISTAT)
         LUNTMP=0
      ENDIF
      IQPRNT=LUSAVE
999   END
***********************************************************************
