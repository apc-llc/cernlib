*
* $Id: pfrange.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pfrange.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 20/08/93  16.56.10  by  Fons Rademakers
*-- Author :    Fons Rademakers   20/08/93
      SUBROUTINE PFRANGE(NSLAV, MYSID, IFIRST, ITO)
************************************************************************
*                                                                      *
* Depending on the number of active slaves and the slave id, calculate *
* the range of events this slave has to analyze.                       *
* IFIRST and ITO are input and output parameters.                      *
*                                                                      *
************************************************************************
*
*-- Total number of events
*
      ITOT = ITO - IFIRST + 1
*
*-- Events per slave
*
      IEV = NINT(FLOAT(ITOT) / FLOAT(NSLAV))
      IF (IEV .EQ. 0) IEV = 1
      IMAX = ITO
*
*-- Find range for this slave
*
      DO 10 I = 1, NSLAV
         ITO = IFIRST + IEV - 1
         IF (I .EQ. MYSID) GOTO 20
         IFIRST = IFIRST + IEV
10    CONTINUE
*
20    IF (ITO .GT. IMAX) ITO = IMAX
      IF (MYSID.EQ.NSLAV .AND. ITO.LT.IMAX) ITO = IMAX
*
      END
