*
* $Id: hmerg.F,v 1.1.1.1 1996/01/16 17:08:10 mclareni Exp $
*
* $Log: hmerg.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:10  mclareni
* First import
*
*
#include "hbook/pilot.h"
#if defined(CERNLIB_MAIN)
*CMZ :  4.22/07 28/06/94  16.52.36  by  Rene Brun
*-- Author :    Rene Brun, J.C. Marin and Fons Rademakers   26/05/94
      PROGRAM HMERG
*.==========>
*.
*.  This program can be used to merge HBOOK files with identical objects
*.  and directories.
*.
*..=========> (Rene Brun, J.C. Marin, A.A.Rademakers )
*
      INTEGER         MXBOOK
      PARAMETER      (MXBOOK=1000000)
      REAL            PAW
      COMMON/PAWC/    PAW(MXBOOK)
*
      CALL HLIMIT(MXBOOK)
*
      CALL HMERGIN
*
      END
#endif
