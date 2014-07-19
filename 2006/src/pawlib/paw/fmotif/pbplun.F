*
* $Id: pbplun.F,v 1.1.1.1 1996/03/01 11:39:06 mclareni Exp $
*
* $Log: pbplun.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/08 30/11/93  12.08.54  by  Rene Brun
*-- Author :    Fons Rademakers   23/07/93
      SUBROUTINE PBPLUN(BRCLAS, BRNAME, VARSET)
************************************************************************
*                                                                      *
*   Return the CWD for the piaf files. This routine is called by       *
*   the Kuip Browser.                                                  *
*                                                                      *
************************************************************************
*
#include "hbook/hcpiaf.inc"
*
      CHARACTER*(*) BRCLAS, BRNAME, VARSET
#if defined(CERNLIB_CZ)
*
      IF (BRNAME .EQ. ' ') THEN
         IF (CONNPF) THEN
            BRNAME = 'Piaf'
            CALL CZPUTA('GETCWD', ISTAT)
            CALL CZGETA(CHSMPF, ISTAT)
            VARSET = 'root='//CHSMPF(1:LENOCC(CHSMPF))
         ENDIF
      ELSE
         BRNAME = ' '
      ENDIF
#endif
*
      END
