*
* $Id: hnhdwr.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hnhdwr.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/02 14/05/93  14.05.15  by  Fons Rademakers
*-- Author :    Fons Rademakers   14/05/93
      SUBROUTINE HNHDWR(IDD)
*.==========>
*.
*.           Write header structure of N-tuple IDD.
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
      INTEGER       KEYS(2)
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
      LC = LQ(LCID-1)
      IF (JBIT(IQ(LC),1) .NE. 0) THEN
         CALL SBIT0(IQ(LC),1)
         CALL SBIT0(IQ(LC),2)
         KEYS(1) = IDD
         KEYS(2) = 0
         CALL HRZOUT(IHDIV,LCID,KEYS,ICYCLE,' ')
         CALL RZSAVE
      ENDIF
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
