*
* $Id: dzedof.F,v 1.1.1.1 1996/03/04 16:13:23 mclareni Exp $
*
* $Log: dzedof.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:23  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEDOF
#include "dzeditkeep.inc"
      CHARACTER*32 CHPFIX
      CHARACTER*8 CHOPTD
 
*     make keep sequences for data offsets
 
      IF(LUNRZ.EQ.0)THEN
         WRITE(LUNOUT,*)' No RZ-file open'
         GOTO 999
      ENDIF
      CALL KUGETC(CHBSBK,NCHA)
      CALL KUGETC(CHBSUP,NCHA)
      CALL KUGETS(CHLIST,NCLIST)
      CALL KUGETC(CHPFIX,NCHA)
      CALL KUGETC(CHOPTD,NCHA)
      IPPARA=INDEX(CHOPTD,'P')
      IF(IPPARA.NE.0)CHOPTD(IPPARA:IPPARA)='p'
      CALL KUGETI(IFI)
      CALL KUGETI(ILI)
      IF(NCLIST .NE. 0)THEN
         LUNLST=LUPLST
         IF(ILSTOP .EQ. 0)THEN
            CALL KUOPEN(LUNLST,CHLIST,'UNKNOWN',ISTAT)
            ILSTOP = 1
         ENDIF
      ELSE
         LUNLST=6
      ENDIF
      CALL DZDCOD(CHBSBK//CHBSUP,'DATA',
     +            'O'//CHOPTD,CHPFIX,LUNLST,IFI,ILI)
  999 END
