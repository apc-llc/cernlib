*
* $Id: pspar.F,v 1.1.1.1 1996/03/01 11:39:27 mclareni Exp $
*
* $Log: pspar.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:27  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/14 12/07/94  17.14.42  by  Fons Rademakers
*-- Author :    Fons Rademakers   20/10/93
      SUBROUTINE PSPAR(NSLAV, IDIAL)
************************************************************************
*                                                                      *
* Start NSLAV slaves.                                                  *
* If IDIAL=1 have dialog with client during slave startup.             *
*                                                                      *
************************************************************************
*
#include "hbook/hcpiaf.inc"
*
      IF (.NOT. MASTPF) RETURN
*
      IF (NSLAV .EQ. 0) NSLAV = MXSLPF
*
      IF (NSLAV .GT. 1) THEN
         CALL PSLAVE(NSLAV, IDIAL)
         NSLAVE = NSLAV
         IF (NSLAVE .GT. 0) THEN
            CALL PSMGRP(NSLAVE)
            IF (NSLAVE .LT. 10) THEN
               PRINT 10100, NSLAVE
            ELSE
               PRINT 10200, NSLAVE
            ENDIF
         ELSE
            PRINT *,' Cannot create slaves, piaf server set to',
     +              ' sequential mode'
         ENDIF
      ELSE
         NSLAVE = 0
         PRINT *,' One slave does not make sense, piaf server',
     +           ' set to sequential mode'
      ENDIF
*
10100 FORMAT('  Piaf server set to parallel mode (',I1,' slaves)')
10200 FORMAT('  Piaf server set to parallel mode (',I2,' slaves)')
*
      END
