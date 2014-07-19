*
* $Id: psmgrp.F,v 1.1.1.1 1996/03/01 11:39:28 mclareni Exp $
*
* $Log: psmgrp.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 18/08/93  19.02.55  by  Fons Rademakers
*-- Author :    Fons Rademakers   17/08/93
      SUBROUTINE PSMGRP(NSLAV)
************************************************************************
*                                                                      *
* Send to all active slave servers the current slave group size and    *
* their own unique id.                                                 *
*                                                                      *
************************************************************************
*
#include "hbook/hcpiaf.inc"
*
      ISTAT = 0
*
      DO 10 I = 1, NSLAV
         CALL PFSOCK(I)
         WRITE(CHSMPF,'(A,I5,I5)') 'GROUP:', NSLAV, I
         CALL CZPUTA(CHSMPF, ISTAT)
*-- FIXME: in case of error take bad node out of list of slaves
10    CONTINUE
*
      CALL PFSOCK(0)
*
      END
