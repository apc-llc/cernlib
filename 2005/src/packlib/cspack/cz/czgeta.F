*
* $Id: czgeta.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: czgeta.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZGETA(CHMAIL,ISTAT)
*
*          To get the string CHMAIL from a remote host
*
#include "cspack/czsock.inc"
      CHARACTER*(*) CHMAIL
      DIMENSION IHMAIL(40),ICONTR(2)
*
#if defined(CERNLIB_VAXVMS)
      IF(IPROT.NE.0) THEN
         CALL CZDGTA(CHMAIL,ISTAT)
         RETURN
      ENDIF
#endif
      CALL JUMPST(IADTCP)
      NCH=LEN(CHMAIL)
      IF(NCH.GT.80)NCH=80
      ICONTR(1)=2
      ICONTR(2)=20
      CALL VBLANK(IHMAIL,20)
      CALL JUMPX2(IHMAIL,ICONTR)
*     CALL CZTCP(IHMAIL,ICONTR)
      CALL UHTOC(IHMAIL,4,CHMAIL,NCH)
      ISTAT=ICONTR(1)
*
      END
 
