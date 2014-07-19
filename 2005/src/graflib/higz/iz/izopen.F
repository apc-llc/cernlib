*
* $Id: izopen.F,v 1.1.1.1 1996/02/14 13:11:12 mclareni Exp $
*
* $Log: izopen.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:12  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.10/03 05/06/90  10.29.03  by  O.Couet
*-- Author :
      SUBROUTINE IZOPEN(LUN,CHDIR,CFNAME,CHOPTT,LRECL,ISTAT)
*.==========>
*.           Open a HIGZ/RZ picture file
*..=========> ( R.Brun )
      CHARACTER*(*) CFNAME,CHDIR,CHOPTT
      CHARACTER*8 CHOPT
*.___________________________________________
*
      CHOPT=CHOPTT
      CALL CLTOU(CHOPT)
*
      CALL RZOPEN(LUN,CHDIR,CFNAME,CHOPT,LRECL,ISTAT)
      IF(ISTAT.NE.0)THEN
         CALL IGERR('Cannot open file','IZOPEN')
         RETURN
      ENDIF
*
      CALL IZFILE(LUN,CHDIR,CHOPT)
*
      END
#endif
