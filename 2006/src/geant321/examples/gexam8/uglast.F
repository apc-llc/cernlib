*
* $Id: uglast.F,v 1.2 1996/02/22 13:26:59 ravndal Exp $
*
* $Log: uglast.F,v $
* Revision 1.2  1996/02/22 13:26:59  ravndal
* Cleaning up CARTOCVS interface
*
* Revision 1.1.1.1  1995/10/24 10:22:23  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGLAST
C.    *
C.    *
C.    *      Termination routine to print histograms and statistics
C.    *
C.    *
 
#include "geant321/gcflag.inc"
#include "pvolum.inc"
C
      CALL GLAST
 
C ***          Normalize and print energy distribution
 
C             Save histograms
C
      CALL HROUT(0,ICYCLE,' ')
      CALL HREND('NTUPLE')
C
      END
