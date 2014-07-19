*
* $Id: pnwork.F,v 1.1.1.1 1996/03/01 11:39:27 mclareni Exp $
*
* $Log: pnwork.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:27  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 24/08/93  17.35.09  by  Fons Rademakers
*-- Author :    Fons Rademakers   12/05/93
      SUBROUTINE PNWORK(IMODE)
************************************************************************
*                                                                      *
*  PNWORK stub called by Piaf server.                                  *
*                                                                      *
************************************************************************
*
#include "paw/pcntwk.inc"
#include "paw/pchost.inc"
#include "paw/pawchn.inc"
#include "hbook/hcpiaf.inc"
*
      CHARACTER*16 CHH
*
      IF (IMODE .EQ. 1) THEN
         IEDONE = 0
         NTCONT = 0
      ENDIF
*
      IF (MASTPF .AND. NSLAVE.GT.0) THEN
         CHH = CRHOST
      ELSE
         CHH = CHHOST
      ENDIF
      WRITE(CHSMPF,'(A6,4I2,5I9,A16)') 'PNWORK', IMODE, NPASS, IPASS,
     +                                  NTCONT, NCHAIN, IMINEV, IMAXEV,
     +                                  MISBYT, IEDONE, CHH
      CALL CZPUTA(CHSMPF, ISTAT)
      CALL CZPUTA(CFILE, ISTAT)
*
      CALL CZGETA(CHSMPF, ISTAT)
      READ(CHSMPF,'(I2)') NTCONT
*
      END
