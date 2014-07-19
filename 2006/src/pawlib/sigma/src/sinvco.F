*
* $Id: sinvco.F,v 1.1.1.1 1995/12/12 14:36:19 mclareni Exp $
*
* $Log: sinvco.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:19  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SINVCO(IAD)
C
C
C    ..................................................
C
C
C         PURPOSE
C            SERVICE ROUTINE FOR SINVDE
C
C         USAGE
C
C         DESCRIPTION OF PARAMETERS
C            IAD  DYNA ADDRESS OF ARRAY FROM WHICH TRANSFERED
C            N    ROW OR COLUMN LENGTH OF EACH SQUARE MATRIX
C
C         COMM. BLOCKS USED
C            COM1
C
C         REMARKS
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            EXPECTS THE ADDRESS OF ARRAY INTO WHICH TRANSFER
C            TAKES PLACE TO BE IN IADDR
C
C
C         AUTHOR.   JURIS REINFELDS      DATE 13/11/74
C
C
C...  PAW VERSION  ...  MAY 1988
C
C
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
      IA=IAD
      DO 10 I=1,LENGTH
      DYNA(IADDR)=DYNA(IA)
      IADDR=IADDR+1
      IA=IA+1
10    CONTINUE
C
 999  END
