*
* $Id: hbug.F,v 1.1.1.1 1996/01/16 17:07:33 mclareni Exp $
*
* $Log: hbug.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:33  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/13 28/09/94  08.38.37  by  Rene Brun
*-- Author :
      SUBROUTINE HBUG(CHMESS,CHROUT,ID)
*.==========>
*.             To print error message CHMESS called from routine CHROUT
*.
*..=========> ( R.Brun )
#include "hbook/hcunit.inc"
#include "hbook/hcmail.inc"
      CHARACTER*(*) CHMESS,CHROUT
*.___________________________________________
      IF(LERR.LE.0)RETURN
      NCMESS=LENOCC(CHMESS)
      NCROUT=LENOCC(CHROUT)
      IF(CHMESS(1:1).EQ.'+')THEN
         CHMAIL=' *** WARNING in '//CHROUT(1:NCROUT)//' : '
     +          //CHMESS(2:NCMESS)//' : '
         NCMESS=NCMESS+NCROUT+22
      ELSE
         CHMAIL=' ***** ERROR in '//CHROUT(1:NCROUT)//' : '
     +          //CHMESS(1:NCMESS)//' : '
         NCMESS=NCMESS+NCROUT+23
      ENDIF
      IF(ID.NE.0)THEN
         CHMAIL(NCMESS:)='ID='
         NCMESS=NCMESS+3
         WRITE(CHMAIL(NCMESS:),'(I8)')ID
         NCMESS=NCMESS+8
      ENDIF
      WRITE(LERR,1000)CHMAIL(1:NCMESS)
 1000 FORMAT(A)
      END
