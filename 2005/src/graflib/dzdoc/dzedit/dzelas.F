*
* $Id: dzelas.F,v 1.1.1.1 1996/03/04 16:13:22 mclareni Exp $
*
* $Log: dzelas.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:22  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZELAS
#include "dzeditkeep.inc"
      CHARACTER*32 CHPFIX
      CHARACTER*4 CHOPTL
 
*     make keep sequences for link assignments
 
      IF(LUNRZ.EQ.0)THEN
         WRITE(LUNOUT,*)' No RZ-file open'
         GOTO 999
      ENDIF
      CALL KUGETC(CHBSBK,NCHA)
      CALL KUGETC(CHBSUP,NCHA)
      CALL KUGETS(CHLIST,NCLIST)
      CALL KUGETC(CHPFIX,NCHA)
      CALL KUGETC(CHOPTL,NCHA)
      IF(NCLIST .NE. 0)THEN
         LUNLST=LUPLST
         IF(ILSTOP .EQ. 0)THEN
            CALL KUOPEN(LUNLST,CHLIST,'UNKNOWN',ISTAT)
            ILSTOP = 1
         ENDIF
      ELSE
         LUNLST=6
      ENDIF
      CALL DZDCOD(CHBSBK//CHBSUP,'LINK','A'//CHOPTL,CHPFIX,LUNLST,0,0)
  999 END
