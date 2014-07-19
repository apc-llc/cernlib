*
* $Id: czputa.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: czputa.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZPUTA(CHMAIL,ISTAT)
*
*          To send the string CHMAIL to a remote host
*
      CHARACTER*(*) CHMAIL
      DIMENSION IHMAIL(40),ICONTR(2)
#include "cspack/czsock.inc"
*
*
#if defined(CERNLIB_VAXVMS)
      IF(IPROT.NE.0) THEN
         CALL CZDPTA(CHMAIL,ISTAT)
         RETURN
      ENDIF
#endif
      CALL JUMPST(IADTCP)
      NCH=LEN(CHMAIL)
      IF(NCH.GT.80)NCH=80
      ICONTR(1)=3
      ICONTR(2)=20
      CALL VBLANK(IHMAIL,20)
      CALL UCTOH(CHMAIL,IHMAIL,4,NCH)
      CALL JUMPX2(IHMAIL,ICONTR)
*     CALL CZTCP(IHMAIL,ICONTR)
      ISTAT=ICONTR(1)
*
      END
