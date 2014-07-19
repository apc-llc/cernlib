*
* $Id: sistak.F,v 1.1.1.1 1995/12/12 14:36:20 mclareni Exp $
*
* $Log: sistak.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:20  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.20  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SISTAK(M,MP,MN)
C
C
C
C    ....................................................
C
C
C         PURPOSE
C            RETURNS MP AND MN OF POSITION -M OF MSTACK
C
C         USAGE
C            CALL SISTAK(M,MP,MN)
C
C         DESCRIPTION OF PARAMETERS
C            MP: FIRST PART OF MSTAK TO BE RETURNED (POINTER TO INDTA)
C            MN: SECOND PART OF MSTAK TO BE RETURNED (NUMBER OF DIM)
C            M : STACK POSITION FROM WHICH TO RETURN CONTENT
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: MSTACK(,)
C
C         REMARKS
C            NOTE THAT CURRENT POSITION OF STAK(TOP OF IT)
C            I S  A D D R E S S E D   B Y       0 (Z E R O )...........
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            TRACE
C            MESSR
C            MESSI
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
C
#include "sigma/sigc.inc"
C
      CALL SITRAC (' SISTAK   ')
C
      MP=MSTACK(1,ISTAPO-M)
      MN=MSTACK(2,ISTAPO-M)
      IF(SITRAK(20).EQ.1)PRINT *,' SISTAK: MP',MP
C
C
 999  END
