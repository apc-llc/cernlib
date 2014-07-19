*
* $Id: pbpiaf.F,v 1.1.1.1 1996/03/01 11:39:06 mclareni Exp $
*
* $Log: pbpiaf.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 06/09/93  12.09.02  by  Fons Rademakers
*-- Author :    Fons Rademakers   23/07/93
      SUBROUTINE PBPIAF(BRNAME,BRCLAS,BRPATH,OBNAME,OBCLAS,STEXT,LTEXT)
************************************************************************
*                                                                      *
*  Browser interface to return next file in current piaf directory.    *
*                                                                      *
************************************************************************
*
#include "hbook/hcpiaf.inc"
*
      CHARACTER*(*) BRNAME,BRCLAS,BRPATH,OBNAME,OBCLAS,STEXT,LTEXT
#if defined(CERNLIB_CZ)
      CHARACTER BCLAS*20, LTXT*20, BNAME*40
*
      IF (.NOT. CONNPF) THEN
         OBNAME = ' '
         RETURN
      ENDIF
*
      CHSMPF = 'SCAND:'
      IF (OBNAME .EQ. ' ') CHSMPF(7:) = BRPATH
      CALL CZPUTA(CHSMPF, ISTAT)
*
*-- STEXT (full pathname) is longer than the LTEXT (filetype) in this case!
*
      CALL CZGETA(CHSMPF, ISTAT)
      READ(CHSMPF, '(A40,2A20)') BNAME, BCLAS, LTXT
      OBNAME = BNAME
      OBCLAS = BCLAS
      LTEXT  = LTXT
      IF (OBNAME .NE. ' ') THEN
         CALL CZGETA(CHSMPF, ISTAT)
         STEXT = CHSMPF
      ENDIF
*
#endif
*
      END
