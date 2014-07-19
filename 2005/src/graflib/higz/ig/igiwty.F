*
* $Id: igiwty.F,v 1.1.1.1 1996/02/14 13:10:35 mclareni Exp $
*
* $Log: igiwty.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:35  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/03 23/01/89  15.47.54  by  O.Couet
*-- Author :
      FUNCTION IGIWTY(IWKID)
*.===========>
*.
*.   This function return the workstation type of the workstation
*. identified by IWKID .
*.
*. _Input parameters:
*.
*.  INTEGER IWKID : Workstation identifier .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
      PARAMETER (NBWKOP=10)
*.______________________________________
*
      IGIWTY=0
      DO 10 I=1,NBWKOP
         IF(IWKID.EQ.ILOPWK(I))THEN
            IGIWTY=IWTYL(I)
            RETURN
         ENDIF
   10 CONTINUE
*
      END
