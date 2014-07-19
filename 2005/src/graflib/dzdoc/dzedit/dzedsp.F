*
* $Id: dzedsp.F,v 1.1.1.1 1996/03/04 16:13:23 mclareni Exp $
*
* $Log: dzedsp.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:23  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEDSP
*     call DZDDIV or DZDISP
#include "dzeditkeep.inc"
      CHARACTER*32 CHCMD
      CHARACTER*4 CDZDOP,COCMD
 
      IF(IWKTYP .LE. 0)THEN
         WRITE(LUNOUT,*)' No graphics available, sorry'
         GOTO 999
      ENDIF
      CALL KUPATL(CHCMD,NPAR)
      CALL KUGETC(COCMD,NCH)
      IF(INDEX(COCMD,'C').NE.0)THEN
         CDZDOP='CMNI'
      ELSE
         CDZDOP='MNI'
      ENDIF
      IF(IXHIGZ.EQ.0)THEN
         IF(IOPWKF .EQ. 0)THEN
            CALL IOPWK(IWKID,ICONID,IWKTYP)
            CALL ISTXFP(1,0)
            IOPWKF = 1
         ENDIF
      ENDIF
      IF(CHCMD.EQ.'DZDISP')THEN
         IF(LTOP.EQ.0)GOTO 999
         CALL ISTXFP(6,0)
         CALL DZDISP(IXDZDS,LTOP,CHDZED,CDZDOP, IWKID,IWMETA,ILOCNR,
     +   IWKTYP)
      ELSEIF(CHCMD.EQ.'DZDIRZ')THEN
         CALL DZDIRZ(IXDZDS,LTOP,1,CHDZED,CDZDOP, IWKID,
     +   IWMETA,ILOCNR)
      ELSE
         CALL ISTXFP(6,0)
         CALL DZDDIV(IXDZDS,LTOP,CHDZED,CDZDOP, IWKID,IWMETA,ILOCNR,
     +   IWKTYP)
      ENDIF
      CALL IGTERM
  999 END
