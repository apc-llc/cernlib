*
* $Id: hgtdir.F,v 1.1.1.1 1996/01/16 17:07:57 mclareni Exp $
*
* $Log: hgtdir.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 07/04/93  18.32.23  by  Fons Rademakers
*-- Author :    Fons Rademakers   19/09/92
      SUBROUTINE HGTDIR(CHDIR)
*.==========>
*.
*.    Returns in CHDIR the directory of the Ntuple currently in memory.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcntpar.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*(*)  CHDIR
*
      CHDIR = ' '
      IF (IQ(LCID-2) .EQ. ZLINK) THEN
*-- new ntuple
         NCHRZ = IQ(LCID+ZNCHRZ)
         IDIR1 = ZNCHRZ+1
      ELSE
*-- old ntuple
         NCHRZ = IQ(LCID+11)
         IDIR1 = 12
      ENDIF
      IF (NCHRZ.GT.0.AND.NCHRZ.LE.80) THEN
         CALL UHTOC(IQ(LCID+IDIR1),4,CHDIR,NCHRZ)
      ENDIF
*
      END
