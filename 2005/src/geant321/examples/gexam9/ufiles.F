*
* $Id: ufiles.F,v 1.1.1.1 1995/10/24 10:22:25 cernlib Exp $
*
* $Log: ufiles.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:25  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UFILES
*
*            To open FFREAD and HBOOK files
*
*
*            To open FFREAD and HBOOK files
*
      CHARACTER*(*) FILNAM, FSTAT
#if defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)||defined(CERNLIB_VAX)
      PARAMETER (FILNAM='gexam9.dat')
#endif
#if defined(CERNLIB_IBM)
      PARAMETER (FILNAM='/GEXAM9 DAT *')
#endif
*
#if defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)
      PARAMETER (FSTAT='OLD')
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_IBM)
      PARAMETER (FSTAT='UNKNOWN')
#endif
*
      OPEN(UNIT=4,FILE=FILNAM,STATUS=FSTAT,FORM='FORMATTED')
      END
