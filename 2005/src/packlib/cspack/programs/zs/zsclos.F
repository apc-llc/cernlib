*
* $Id: zsclos.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zsclos.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZSCLOS(CHMAIL)
*
*         to close a RZ file remotely
*
#include "cspack/czdir.inc"
#include "cspack/quest.inc"
      CHARACTER*(*) CHMAIL
      CHARACTER*80  CHLINE
      CHARACTER*5   CHDIR
      CHARACTER*64  CHFILE
 
      CHLINE = CHMAIL
      CALL CLTOU(CHLINE)
 
      LLINE = LENOCC(CHLINE)
 
      CHFILE = ' '
 
      IF(CHLINE(1:2).EQ.'//') THEN
         CHDIR = CHLINE(3:7)
      ELSEIF(CHLINE(1:3).EQ.'LUN') THEN
         CHDIR = CHLINE(1:5)
      ELSEIF(ICNUM(CHLINE,1,LLINE).GT.LLINE) THEN
         CHDIR = 'LUN'//CHLINE(1:LLINE)
      ELSE
 
         DO 10 I=1,NCHRZ
            IF(FLNAME(I).EQ.CHMAIL) GOTO 20
   10    CONTINUE
         WRITE(CHLINE,9001) CHMAIL(1:LLINE)
 9001    FORMAT('3 file ',A,' not open')
         CALL CZPUTA(CHLINE,ISTAT)
         GOTO 999
   20    CONTINUE
         CHFILE = CHMAIL
 
      ENDIF
 
      READ(CHDIR,'(3X,I2)') LUN
      CLOSE(LUN)
 
      IF(CHFILE.EQ.' ') THEN
*
*     Find file name corresponding to this TOPDIR
*
      DO 30 I=1,NCHRZ
         IF(LUNRZ(I).NE.LUN) GOTO 30
         CHFILE = FLNAME(I)
         GOTO 40
   30 CONTINUE
 
      WRITE(CHLINE,9002) CHDIR
 9002 FORMAT('3 file with top directory ',A,' not open')
      CALL CZPUTA(CHLINE,ISTAT)
      GOTO 999
 
   40 CONTINUE
 
      ENDIF
 
      CALL RZEND(CHDIR)
 
      CALL CZPUTA('1 Closing remote file '//CHFILE,ISTAT)
 
      LUNRZ(I) = 0
 
      IF(I.EQ.NCHRZ) NCHRZ = NCHRZ - 1
*
  999 END
 
