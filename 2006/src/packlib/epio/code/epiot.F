*
* $Id: epiot.F,v 1.3 1996/11/27 18:00:44 cernlib Exp $
*
* $Log: epiot.F,v $
* Revision 1.3  1996/11/27 18:00:44  cernlib
* Update Version
*
* Revision 1.2  1996/05/03 16:20:10  mclareni
* Set version stamp in epiot and history file for 1_76
*
* Revision 1.1.1.1  1996/03/08 15:21:43  mclareni
* Epio
*
*
#include "epio/pilot.h"
      SUBROUTINE EPIOT ( LUN)
C.
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       PRINT PAM TITLE ON LUN                                   *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
      WRITE (LUN,1000)
      RETURN
#if defined(CERNLIB_F4)
 1000 FORMAT (33H0PACKAGE COMPILED FROM PAM FILE= ,
#endif
#if !defined(CERNLIB_F4)
 1000 FORMAT ('0PACKAGE COMPILED FROM PAM FILE= ',
#endif
     + 40H EPIO  1_76_01  960411  I101                 
     +/)
      END
