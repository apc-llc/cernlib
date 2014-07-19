*
* $Id: hntdel.F,v 1.1.1.1 1996/01/16 17:07:59 mclareni Exp $
*
* $Log: hntdel.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:59  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/13 28/09/94  08.25.32  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/01/94
      SUBROUTINE HNTDEL(CHDIR)
*.==========>
*.
*.           Delete from //PAWC all Ntuple headers from Ntuples that
*.           reside in file CHDIR. Don't remove memory resident
*.           Ntuples. This routine does not (yet) scan all subdirectories
*.           in //PAWC.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
*
      CHARACTER*(*) CHDIR
      CHARACTER*128 CHRZ
*
*-- Loop over all objects in the current directory in memory
*
      LC   = LENOCC(CHDIR)
      IDPOS=0
*
 10   IDPOS=IDPOS+1
      IF(IDPOS.GT.IQ(LCDIR+KNRH))RETURN
      ID  =IQ(LTAB+IDPOS)
      LCID=LQ(LTAB-IDPOS)
      IF(ID.EQ.0.OR.LCID.EQ.0)RETURN
*
      IF (JBIT(IQ(LCID+KBITS),4) .EQ. 0) GOTO 10
*
      IF (IQ(LCID-2) .NE. ZLINK) THEN
*
*-- RWNtuple
*
         NCHRZ = IQ(LCID+11)
         ICHRZ = 12
      ELSE
*
*-- CWNtuple
*
         NCHRZ = IQ(LCID+ZNCHRZ)
         ICHRZ = ZNCHRZ+1
      ENDIF
*
*-- Memory resident
*
      IF (NCHRZ .LE. 0) GOTO 10
*
      CHRZ = ' '
      CALL UHTOC(IQ(LCID+ICHRZ),4,CHRZ,NCHRZ)
      IF (CHDIR(1:LC) .EQ. CHRZ(3:LC+2)) THEN
         CALL HDELET(ID)
         IDPOS=IDPOS-1
      ENDIF
*
      GOTO 10
*
      END
