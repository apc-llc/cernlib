*
* $Id: hrget.F,v 1.4 1997/01/21 13:19:06 couet Exp $
*
* $Log: hrget.F,v $
* Revision 1.4  1997/01/21 13:19:06  couet
* - Call hrendc instead of hrend
*
* Revision 1.3  1996/03/14 08:33:55  couet
* Mods from Detlef Irmscher <irmscher@rnchp3.lbl.gov> it now puts the tree into
* the directory one is presently in.
*
* Revision 1.2  1996/02/22 09:10:48  couet
* Mod suggested by Rene Brun to reconstruct a file structure in memory.
*
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 11/07/90  18.37.27  by  Rene Brun
*-- Author :
      SUBROUTINE HRGET(ID,CHFILE,CHOPT)
*.==========>
*.           To get histogram(s) from file CHFILE
*.            CHOPT='T' Get complete Tree
*.            CHOPT='A' Add to current histogram in memory
*..=========> ( R.Brun )
      CHARACTER*(*) CHFILE,CHOPT
      CHARACTER*256     DIRECTORY
      COMMON /HRGETDIR/ DIRECTORY, LENDIR
      DIMENSION IOPT(2)
      EQUIVALENCE (IOPTT,IOPT(1)),(IOPTA,IOPT(2))
      EXTERNAL HREADONEDIR
*.___________________________________________
*
      CALL HUOPTC(CHOPT,'TA',IOPT)
      IF(ID.NE.0)IOPTT=0
      IF(IOPTT.NE.0)THEN
         ICYCLE=9999
      ELSE
         ICYCLE=999
      ENDIF
      IF(IOPTA.NE.0)THEN
         IOF=99999
      ELSE
         IOF=0
      ENDIF
      NCH=LENOCC(CHFILE)
      CALL HCDIR (DIRECTORY, 'R')
      LENDIR = LENOCC(DIRECTORY)
      IF(NCH.GT.0)THEN
         CALL HROPEN(88,'HRGET',CHFILE,' ',1024,ISTAT)
         IF(ISTAT.NE.0)GO TO 99
         CALL HRIN(ID,ICYCLE,IOF)
         IF (ICYCLE.EQ.9999) THEN
            CALL RZSCAN(' ',HREADONEDIR)
         ENDIF
         CALL HRENDC('HRGET')
         CLOSE(88)
      ELSE
         CALL HRIN(ID,ICYCLE,IOF)
      ENDIF
*
      CALL HCDIR (DIRECTORY, ' ')
  99  END
