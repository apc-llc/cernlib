*
* $Id: izfile.F,v 1.2 1999/10/13 12:32:22 couet Exp $
*
* $Log: izfile.F,v $
* Revision 1.2  1999/10/13 12:32:22  couet
* - rzmake is now called with 32000
*
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.21/06 27/06/94  17.36.26  by  Rene Brun
*-- Author :
      SUBROUTINE IZFILE(LUN,CHDIR,CHOPT)
*.===========>
*.
*.   This routine create a RZ file .
*.
*. _Input parameters:
*.
*.  INTEGER LUN : Logical unit number .
*.  CHARACTER CHDIR : Top directoy .
*.  CHARACTER CHOPT : Option .
*.
*.                    CHOPT='N' : New RZ file .
*.                    CHOPT='X' : EXchange mode .
*.                    CHOPT=' ' : Open an existing file .
*.                    CHOPT='A' : Pictures are Automatically saved in RZ file .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiflag.inc"
#include "higz/hicur.inc"
      CHARACTER*(*) CHDIR,CHOPT
      CHARACTER*8 CHTAG(5),CHOPTT
      DIMENSION IOPT(9)
      EQUIVALENCE (IOPT(1),IOPTN),(IOPT(2),IOPTX),(IOPT(3),IOPTA)
*.______________________________________
*
      CHOPTT=CHOPT
      CALL CLTOU(CHOPTT)
      CALL UOPTC (CHOPTT,'NXA',IOPT)
*
      IF(IOPTN.NE.0)THEN
         CHTAG(1)=' '
         CHTAG(2)='PICTURE'
         CHTAG(3)='NAME'
         CHTAG(4)=' '
         CHTAG(5)=' '
         I=INDEX(CHOPTT,'N')
         CHOPTT(I:I)='?'
         CALL RZMAKE(LUN,CHDIR,5,'AAAAA',CHTAG,32000,CHOPTT)
      ELSE
         CALL RZFILE(LUN,CHDIR,CHOPTT)
      ENDIF
      CALL RZCDIR(HICDIR,'R')
*
      IF(IOPTA.NE.0)THEN
         ASFLAG=.TRUE.
      ENDIF
*
      END

#endif
