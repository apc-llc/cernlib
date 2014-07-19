*
* $Id: hplhip.F,v 1.1.1.1 1996/01/19 10:50:09 mclareni Exp $
*
* $Log: hplhip.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/16 25/08/94  15.26.44  by  O.Couet
*-- Author :
      INTEGER FUNCTION HPLHIP(ITOK,CHOPT)
*.==========>
*.       This function returns the ZEBRA adress
*.       of histogram ID if CHOPT='I' or of the
*.       histogram drawned with transformation NT
*.       if CHOPT='T'
*..=========>
#include "hbook/hcbook.inc"
      CHARACTER*1 CHOPT
*._____________________________
*
      HPLHIP = 0
      IF (CHOPT.EQ.'I') THEN
         IADDR = 1
      ELSE
         IADDR = 2
      ENDIF
*
      LP = LQ(LHPLOT-1)
  10  IF (LP.EQ.0) GOTO 999
      IF (IQ(LP+IADDR).EQ.ITOK) THEN
         HPLHIP = LP
         GOTO 999
      ENDIF
      LP = LQ(LP)
      GOTO 10
*
 999  END
