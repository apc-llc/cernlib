*
* $Id: hbug.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: hbug.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)
*CMZ :  2.07/00 11/04/95  13.52.59  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE HBUG(CHMESS,CHROUT,ID)
*.==========>
*.             To print error message CHMESS called from routine CHROUT
*.
*..=========> ( R.Brun )
#include "hbook/hcunit.inc"
#include "hbook/hcmail.inc"
#include "paw/pcwk.inc"
#include "paw/quest.inc"
      CHARACTER*(*) CHMESS,CHROUT
*.___________________________________________
      IF(IWK.NE.0)CALL IGSA(1)
      IWARN = 0
      NCMESS=LENOCC(CHMESS)
      NCROUT=LENOCC(CHROUT)
      IF(CHMESS(1:1).EQ.'+')THEN
         CHMAIL=' *** WARNING in '//CHROUT(1:NCROUT)//' : '
     +          //CHMESS(2:NCMESS)//' : '
         NCMESS=NCMESS+NCROUT+22
         IWARN = 1
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
*
#if defined(CERNLIB_IBM)
      IF(IWK.GT.10 .AND. IWARN.EQ.0)THEN
         CALL KUPROC('Type CR to continue or QUIT',CHMAIL,NCMESS)
         IF(NCMESS.GT.0)THEN
            IF(CHMAIL(1:1).EQ.'Q')IQUEST(1)=1
         ENDIF
      ENDIF
#endif
*
      END

#endif
