*
* $Id: zslfil.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zslfil.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZSLFIL
*
*     List RZ files that are currently open
*
#include "cspack/czdir.inc"
#include "cspack/quest.inc"
      CHARACTER*80 CHMAIL
*
*     Find file name corresponding to this TOPDIR
*
 
      IF(NCHRZ.EQ.0) THEN
         CALL CZPUTA('1 No RZ files currently connected',ISTAT)
      ELSE
         CALL CZPUTA('2 List of remotely connected files',ISTAT)
         CALL CZPUTA('2 ================================',ISTAT)
         DO 10 I=1,NCHRZ
 
            IF(LUNRZ(I).NE.0) THEN
               WRITE(CHMAIL,9001) LUNRZ(I),FLNAME(I)
 9001 FORMAT('2 //LUN',I2,' ',A)
               CALL CZPUTA(CHMAIL,ISTAT)
            ENDIF
   10    CONTINUE
         CALL CZPUTA('1',ISTAT)
 
      ENDIF
*
      END
