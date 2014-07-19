*
* $Id: pscast.F,v 1.1.1.1 1996/03/01 11:39:27 mclareni Exp $
*
* $Log: pscast.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:27  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 13/08/93  18.23.50  by  Fons Rademakers
*-- Author :    Fons Rademakers   13/08/93
      SUBROUTINE PSCAST(CHMSG, NSLAV, ISTAT)
************************************************************************
*                                                                      *
* Multicast the message CHMSG to NSLAV slave servers.                  *
*                                                                      *
* Equivalent semantics as CZPUTA.                                      *
* The array IBAD contains indices into the array SLINPF of dead        *
* slaves. The number of dead slaves is given by ISTAT.                 *
*                                                                      *
************************************************************************
*
#include "hbook/hcpiaf.inc"
*
      CHARACTER*(*)  CHMSG
      INTEGER        IBAD(MXSLPF)
*
      ISTAT = 0
      IF (NSLAV .EQ. 0) RETURN
*
      CALL PSMPUT(CHMSG, NSLAV, SLINPF, IBAD, ISTAT)
*
*-- In case some slave did not reply put it in the list of bad slaves
*
      IF (ISTAT .NE. 0) THEN
         DO 10 I = 1, ISTAT
            CALL PSBADS(SLHOPF(IBAD(I)), 0)
10       CONTINUE
      ENDIF
*
      END
