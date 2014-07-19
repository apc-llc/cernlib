*
* $Id: kiclos.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kiclos.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.05/15 01/08/94  10.35.04  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE KUCLOS(LUN,STATUS,ISTAT)
      ENTRY      KICLOS(LUN,STATUS,ISTAT)
*
********************************************************************************
*
* To close a file
*
*     Input :
*
      CHARACTER*(*) STATUS
      INTEGER       LUN, ISTAT
*
********************************************************************************
*
#if defined(CERNLIB_IBMVM)
      CHARACTER CHLUN*2
#endif
      IF (STATUS .EQ. ' ') THEN
         CLOSE(LUN,IOSTAT=ISTAT)
      ELSE
         CLOSE(LUN,STATUS=STATUS,IOSTAT=ISTAT)
      ENDIF

#if defined(CERNLIB_IBMVM)
      WRITE(CHLUN,'(I2.2)') LUN
      CALL VMCMS('FILEDEF FT'//CHLUN//'F001 CLEAR',IRC)
*     CALL VMCMS('FILEDEF DF@000'//CHLUN//' CLEAR',IRC)
#endif

      END



