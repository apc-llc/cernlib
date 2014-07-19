*
* $Id: pfminmax.F,v 1.2 1996/06/03 13:28:50 couet Exp $
*
* $Log: pfminmax.F,v $
* Revision 1.2  1996/06/03 13:28:50  couet
* - add pilot.h
*
* Revision 1.1.1.1  1996/06/03 15:09:49 couet
* Paw
*
*
#include "paw/pilot.h"
      SUBROUTINE PFMINMAX (RMIN,RMAX)
*.==========>
*.
*.     To find lower and upper limit for 1D histo on a PIAF slave
*.
*..=========> ( Dino FM )
*
*-- when running on a Piaf slave server send the lower and upper limits
*-- to the master who will then return the final limits, depending on
*-- the limits found by the other slaves
*
#include "hbook/hcpiaf.inc"
#include "hbook/czbuff.inc"

      INTEGER NVAR,IB,ISTAT

      IB = 1
      NVAR = 1
      WRITE(CHSMPF,'(A,I12)') 'PPROJ0', NVAR
      CALL CZPUTA(CHSMPF, ISTAT)
*
*-- send the limits we found
*
      WRITE(CHBUF(IB:IB+30-1),'(2E15.9)') RMIN, RMAX
      CALL CZPUTC(NVAR*30, ISTAT)
*
*-- get back the lower and upper ranges for the variables
*
      CALL CZGETC(NVAR*30, ISTAT)
      READ(CHBUF(IB:IB+30-1),'(2E15.9)') RMIN,RMAX
      
      END
