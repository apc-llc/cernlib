*
* $Id: dzebkk.F,v 1.1.1.1 1996/03/04 16:13:23 mclareni Exp $
*
* $Log: dzebkk.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:23  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEBKK
      CHARACTER*8 CHOPTB,CHCMD
      CHARACTER*2 CHOPTI
#include "dzeditkeep.inc"
 
*     make keep sequences for bank booking
 
      IF(LUNRZ.EQ.0)THEN
         WRITE(LUNOUT,*)' No RZ-file open'
         GOTO 999
      ENDIF
      CALL KUGETC(CHBSBK,NCHA)
      CALL KUGETC(CHBSUP,NCHA)
      CALL KUGETS(CHLIST,NCLIST)
      CALL KUGETC(CHOPTB,NCOPT)
      CALL KUPATL(CHCMD,NPAR)
      IF(NCLIST .NE. 0)THEN
         LUNLST=LUPLST
         IF(ILSTOP .EQ. 0)THEN
            CALL KUOPEN(LUNLST,CHLIST,'UNKNOWN',ISTAT)
            ILSTOP = 1
         ENDIF
      ELSE
         LUNLST=6
      ENDIF
      IF(INDEX(CHOPTB,'S').EQ.0)THEN
          CHOPTI='T '
      ELSE
          CHOPTI='  '
      ENDIF
      IF(   INDEX(CHOPTB,'L').EQ. 0)THEN
         IF(CHCMD.EQ.'BOOK')THEN
            CHOPTI(2:2)='B'
         ELSE
            CHOPTI(2:2)='L'
         ENDIF
      ENDIF
      CALL DZDCOD(CHBSBK//CHBSUP,'BANK',CHOPTI//CHOPTB,' ',LUNLST,0,0)
  999 END
