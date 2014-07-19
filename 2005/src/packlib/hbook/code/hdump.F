*
* $Id: hdump.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hdump.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.47  by  Rene Brun
*-- Author :
      SUBROUTINE HDUMP(ID2)
*.==========>
*.           DUMPS HISTOGRAM ID2
*.           IF ID2=0  DUMPS ALL THE HBOOK AREA IN /PAWC/
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
*.___________________________________________
      IF(ID2.NE.0)THEN
         CALL HFIND(ID2,'HDUMP ')
         IF(LCID.NE.0)THEN
            IF(LFIX.EQ.0)THEN
               CALL DZSHOW('DZSHOW',IHDIV,LCID, 'V',0,0,0,0)
            ELSE
               CALL DZSHOW('DZSHOW',IHDIV,LCONT,'V',0,0,0,0)
            ENDIF
         ELSE
            RETURN
         ENDIF
      ELSE
         IF(LCDIR.NE.0)THEN
            CALL DZSHOW('DZSHOW',IHDIV,LCDIR,'V',0,0,0,0)
         ENDIF
      ENDIF
      END
