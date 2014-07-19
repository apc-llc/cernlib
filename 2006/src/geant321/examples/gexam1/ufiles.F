*
* $Id: ufiles.F,v 1.4 1997/05/23 13:06:52 mclareni Exp $
*
* $Log: ufiles.F,v $
* Revision 1.4  1997/05/23 13:06:52  mclareni
* Simplify the OPEN statements
*
* Revision 1.3  1997/04/25 12:33:16  mclareni
* Typing error ifndef CERNLIB+QFMSOFT corrected
*
* Revision 1.2  1997/04/10 15:33:16  mclareni
* Mods for WINNT
*
* Revision 1.1.1.1.2.1  1997/01/21 11:23:45  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1995/10/24 10:22:09  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE UFILES
*
*            To open FFREAD and HBOOK files
*
      CHARACTER*(*) FILNAM, FSTAT
#if defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX)
      PARAMETER (FILNAM='gexam1.dat')
#endif
#if defined(CERNLIB_IBM)
      PARAMETER (FILNAM='/GEXAM1 DAT *')
#endif
*
#if defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)
      PARAMETER (FSTAT='OLD')
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_IBM)
      PARAMETER (FSTAT='UNKNOWN')
#endif
*
#if defined(CERNLIB_QFMSOFT)
      OPEN(UNIT=4,FILE=FILNAM,STATUS='OLD',FORM='FORMATTED')
#elif  defined(CERNLIB_VAX)
      OPEN(UNIT=4,FILE=FILNAM,STATUS=FSTAT,READONLY,FORM='FORMATTED')
#elif 1
      OPEN(UNIT=4,FILE=FILNAM,STATUS=FSTAT,FORM='FORMATTED')
#endif
      END
