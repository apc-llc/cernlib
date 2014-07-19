*
* $Id: hrput.F,v 1.2 1996/12/18 11:06:55 couet Exp $
*
* $Log: hrput.F,v $
* Revision 1.2  1996/12/18 11:06:55  couet
* - call now hrendc instead of hrend.
*
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.16/00 27/05/92  10.32.08  by  Unknown
*-- Author :
      SUBROUTINE HRPUT(ID,CHFILE,CHOPT)
*.==========>
*.           To save histogram(s) on file CHFILE
*.            CHOPT='N' Create a New file
*.                 ='U' Update existing file
*.                 ='T' Save complete Tree
*..=========> ( R.Brun )
      CHARACTER*(*) CHFILE,CHOPT
      CHARACTER*255 CHSAVE
      CHARACTER*1 CHOPT1
      CHARACTER*2 CHOPT2
      DIMENSION IOPT(3)
      EQUIVALENCE (IOPTT,IOPT(1)),(IOPTN,IOPT(2)),(IOPTU,IOPT(3))
*.___________________________________________
*
      CALL HUOPTC (CHOPT,'TNU',IOPT)
      IF(IOPTU.EQ.0)IOPTN=1
      IF(IOPTN.NE.0)THEN
         CHOPT1='N'
      ELSE
         CHOPT1='U'
      ENDIF
      IF(IOPTT.NE.0)THEN
         CHOPT2=CHOPT1//'T'
      ELSE
         CHOPT2=CHOPT1
      ENDIF
      NCH=LENOCC(CHFILE)
      IF(NCH.GT.0)THEN
         CALL RZCDIR(CHSAVE,'R')
         LSAVE = LENOCC(CHSAVE)
         CALL HROPEN(88,'HRPUT',CHFILE,CHOPT1,1024,ISTAT)
         IF(ISTAT.NE.0)GO TO 99
         CALL HROUT(ID,ICYCLE,CHOPT2)
         CALL HRENDC('HRPUT')
         CLOSE(88)
         CALL RZCDIR(CHSAVE(1:LSAVE),' ')
      ELSE
         CALL HROUT(ID,ICYCLE,CHOPT2)
      ENDIF
*
  99  END
