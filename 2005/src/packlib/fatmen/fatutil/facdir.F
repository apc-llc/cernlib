*
* $Id: facdir.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: facdir.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FACDIR(CHPATH,CHOPT)
*CMZ :          23/09/91  14.01.52  by  Jamie Shiers
*-- Author :    Jamie Shiers   23/09/91
*
*     Interface routine to RZCDIR to protect against
*     problems with multiple RZ files
*
      CHARACTER*(*) CHPATH
#include "fatmen/fatsys.inc"
#include "zebra/rzdir.inc"
#include "fatmen/fatopts.inc"
 
      IF(IOPTR+IOPTP.EQ.0) THEN
         LTOP = LENOCC(TOPDIR)
         LCWD = LENOCC(CHCDIR(1))
 
         IF(TOPDIR(3:LTOP).NE.CHCDIR(1)(1:LCWD)) THEN
            CALL RZCDIR(TOPDIR,' ')
         ENDIF
      ENDIF
 
      CALL RZCDIR(CHPATH,CHOPT)
 
      END
