*
* $Id: hntsav.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hntsav.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/02 14/05/93  14.05.15  by  Fons Rademakers
*-- Author :    Fons Rademakers   14/05/93
      SUBROUTINE HNTSAV
*.==========>
*.
*.           Flush RZ directory of current Ntuple to disk.
*.           For the data-structure description see routine HBNT.
*.
*.   This routine assumes that LCID is pointing to the right Ntuple.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*128 CHWOLD, CHDIR, CWDRZ
*
      IERR = 0
*
*-- goto the correct RZ directory
*
      NCHRZ = IQ(LCID+ZNCHRZ)
      CALL RZCDIR(CWDRZ,'R')
      CALL HCDIR(CHWOLD,'R')
      CHDIR = ' '
      CALL UHTOC(IQ(LCID+ZNCHRZ+1),4,CHDIR,NCHRZ)
      IF (CHDIR.NE.CWDRZ) THEN
         CALL HCDIR(CHDIR,' ')
      ENDIF
*
      CALL RZSAVE
*
*-- go back to current working directory
*
      IF (CHDIR.NE.CWDRZ) THEN
         CALL HCDIR(CHWOLD,' ')
         IF (CHWOLD .NE. CWDRZ) THEN
            CALL RZCDIR(CWDRZ,' ')
         ENDIF
      ENDIF
*
      END
