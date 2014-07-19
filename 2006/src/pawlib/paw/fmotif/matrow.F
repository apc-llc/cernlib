*
* $Id: matrow.F,v 1.1.1.1 1996/03/01 11:39:05 mclareni Exp $
*
* $Log: matrow.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:05  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/14 01/03/94  17.01.37  by  Fons Rademakers
*-- Author :    Fons Rademakers   22/10/92
      SUBROUTINE MATROW(IEVT)
*
*  Interface routine to matrix_label routine
*
#include "paw/pcmatw.inc"
*
      CALL MATRIX_ROW(IEVT, OUTSTR(1))
*
      END
