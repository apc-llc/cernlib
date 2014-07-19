*
* $Id: hplusr.F,v 1.1.1.1 1996/01/19 10:50:12 mclareni Exp $
*
* $Log: hplusr.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.04/12 16/01/89  12.08.26  by  O.Couet
*-- Author :
      SUBROUTINE HPLUSR(ID,ICASE,KID)
*.==========>
*.       this subroutine is supplied as a dummy user routine,so it
*.       can be replaced by the user if required. it is called during
*.       processing of calls to HPLOT/HPLSUR/HPL3D
*.       and is called after each histogram has been plotted. this
*.       enables the user to specify his own axis titles etc. even
*.       with HPLOT(0)
*.
*.       KID=0 if HPLOT(0)/HPLSUR(0)/HPL3D(0) called
*.          =1 if a specific id has been requested e.g. CALL HPLOT(220)
*..=========>
      CHARACTER*4 ICASE
*._____________________________
*
      END


