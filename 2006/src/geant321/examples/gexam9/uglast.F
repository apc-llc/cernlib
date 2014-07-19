*
* $Id: uglast.F,v 1.2 1996/02/22 13:32:45 ravndal Exp $
*
* $Log: uglast.F,v $
* Revision 1.2  1996/02/22 13:32:45  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:25  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGLAST
*
************************************************************************
*                                                                      *
*       Termination routine to print histograms and statistics         *
*                                                                      *
************************************************************************
*
#include "geant321/gclist.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcunit.inc"
#include "hboot.inc"
*
      IF (LHBOOT(1) .NE. 1) THEN
         CALL SUMANG
         ENDFILE 21
      END IF
*
      CALL GLAST
*
*             Close I/O buffers
      IF((NGET+NSAVE).GT.0) THEN
         CALL GCLOSE(0,IER)
      ENDIF
*
      CALL HISTDO
*
      END
