*
* $Id: ufiles.F,v 1.2 1996/03/13 17:30:52 ravndal Exp $
*
* $Log: ufiles.F,v $
* Revision 1.2  1996/03/13 17:30:52  ravndal
* Modifications for parallel version testing included
*
c Revision 1.1.1.1  95/10/24  10:22:12  cernlib
c Geant
c 
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
      PARAMETER (FILNAM='gexam3.dat')
#endif
#if defined(CERNLIB_IBM)
      PARAMETER (FILNAM='/GEXAM3 DAT *')
#endif
*
#if defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)
      PARAMETER (FSTAT='OLD')
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_IBM)
      PARAMETER (FSTAT='UNKNOWN')
#endif
*
      OPEN(UNIT=4,FILE=FILNAM,STATUS=FSTAT,
#if defined(CERNLIB_VAX)
     +     READONLY,
#endif
     +     FORM='FORMATTED')

#if !defined(CERNLIB_PARA)
*
*             Open a HBOOK direct access file
*
      CALL HROPEN(34,'HBOOK','gexam3.hist','N',1024,ISTAT)
#endif
#if  defined(CERNLIB_PARA)
*             Do not open a HBOOK file here. It will be done in uglast. 
#endif
      END
