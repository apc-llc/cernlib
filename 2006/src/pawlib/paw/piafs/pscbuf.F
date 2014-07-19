*
* $Id: pscbuf.F,v 1.1.1.1 1996/03/01 11:39:27 mclareni Exp $
*
* $Log: pscbuf.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:27  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 14/08/93  14.47.18  by  Fons Rademakers
*-- Author :    Fons Rademakers   14/08/93
      SUBROUTINE PSCBUF(NTOT, NSLAV, ISTAT)
************************************************************************
*                                                                      *
* Multicast NTOT characters from CHBUF to NSLAV slave servers.         *
*                                                                      *
* Equivalent semantics as CZPUTC.                                      *
* The array IBAD contains indices into the array SLINPF of dead        *
* slaves. The number of dead slaves is given by ISTAT.                 *
*                                                                      *
************************************************************************
*
#include "hbook/hcpiaf.inc"
*
      INTEGER        IBAD(MXSLPF)
*
      ISTAT = 0
      IF (NSLAV .EQ. 0) RETURN
*
      CALL PSMBUF(NTOT, NSLAV, SLINPF, IBAD, ISTAT)
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
