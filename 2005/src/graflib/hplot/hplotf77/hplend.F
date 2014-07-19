*
* $Id: hplend.F,v 1.1.1.1 1996/01/19 10:49:58 mclareni Exp $
*
* $Log: hplend.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.04/12 16/01/89  12.08.15  by  O.Couet
*-- Author :
      SUBROUTINE HPLEND
*.==========>
*.       terminates the HIGZ file and writes closing messages
*..=========>
#include "hplot/hpl1.inc"
      CHARACTER*10 STRING
*._____________________________
*
*     Request string (CR) if option PTO selected
*
      IF(LOPTO)THEN
         CALL HPLPTO(' ',STRING)
      ENDIF
      CALL IGEND
*
      END

