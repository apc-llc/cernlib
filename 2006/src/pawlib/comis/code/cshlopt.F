*
* $Id: cshlopt.F,v 1.1.1.1 1996/02/26 17:16:31 mclareni Exp $
*
* $Log: cshlopt.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:31  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/01 30/03/94  11.58.04  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSHLOPT(TEXT,CHVAR)
      CHARACTER*(*)TEXT,CHVAR
#if defined(CERNLIB_SHL)
#include "comis/cshlnm.inc"
      CALL CUTOL(CHVAR)
      IF(CHVAR(1:1).EQ.'p')THEN
        CHPATH=TEXT
        IF(CHPATH.EQ.' ')CHPATH='./'
        L=LENOCC(CHPATH)
        IF(CHPATH(L:L).NE.'/')CHPATH=CHPATH(:L)//'/'
      ELSEIF(CHVAR(1:1).EQ.'f')THEN
        CHF77=TEXT
      ELSEIF(CHVAR(1:1).EQ.'c')THEN
        CHCC=TEXT
      ELSEIF(CHVAR(1:1).EQ.' ')THEN
        PRINT *,' PATH=',CHPATH(:LENOCC(CHPATH))
        PRINT *,' FORT=',CHF77(:LENOCC(CHF77))
        PRINT *,'   CC=',CHCC(:LENOCC(CHCC))
      ELSE
        PRINT *,' CS.SET: unknown option:',CHVAR
        PRINT *,' possible options are: path, f77, cc'
      ENDIF
#endif
      END
