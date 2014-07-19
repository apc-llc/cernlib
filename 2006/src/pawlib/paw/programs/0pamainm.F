*
* $Id: 0pamainm.F,v 1.3 1996/05/09 14:49:54 cernlib Exp $
*
* $Log: 0pamainm.F,v $
* Revision 1.3  1996/05/09 14:49:54  cernlib
* Remove czdummy and make it seperate file
*
* Revision 1.2  1996/04/11 15:17:20  cernlib
* Remove protection on CERNLIB_MAIN and CERNLIB_MOTIF
*
* Revision 1.1.1.1  1996/03/01 11:38:50  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/02 14/06/95  11.22.57  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      PROGRAM PAMAIN
*
#include "paw/pawsiz.inc"
*
      COMMON /PAWC/ PAWCOM(NWPAW)
#if defined(CERNLIB_HPUX)
      ON REAL UNDERFLOW IGNORE
#endif
*
      CALL PAWPP(NWPAW,IWTYP)
*
      IF (IWTYP .EQ. 999) THEN
         CALL KUWHAM('Paw++')
      ELSE
         CALL KUWHAG
      ENDIF
*
      CALL PAEXIT
*
      STOP
      END
      SUBROUTINE QNEXT
      END
#if defined(CERNLIB_VAX)||defined(CERNLIB_QMALPH)
      SUBROUTINE CTL$GL_PCB
      ENTRY CTL$T_ACCOUNT
      ENTRY CTL$T_USERNAME
      END
#endif
