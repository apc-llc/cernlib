*
* $Id: pzopen.F,v 1.1.1.1 1996/03/01 11:38:47 mclareni Exp $
*
* $Log: pzopen.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:47  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  2.04/12 11/02/94  19.26.07  by  Fons Rademakers
*-- Author :
      SUBROUTINE PZOPEN(CHPORT,CHCLUS,CHHOST,CHVERS,PIAFRC,ISTAT)
      CHARACTER*(*) CHPORT,CHCLUS,CHHOST,CHVERS,PIAFRC
*
*****************************************************************
*                                                               *
*          To open socket with remote Piaf server               *
*                                                               *
*****************************************************************
*
#include "hbook/czsock.inc"
#include "paw/rzcxio.inc"
      COMMON/CZUNIT/IXPRNT,IDEBXZ,LUNXZI,LUNXZO,LUNFZI,LUNFZO
      COMMON/CDBUF/DBUF(10)
      INTEGER PZSETUP
      EXTERNAL CZTCP
*
* pass address of /RZCOUNT/ to TCPIAF
*
      CALL PZCXIO(RZXIN,RZXOUT)
*
      IF(IADTCP.EQ.0)THEN
         LUNFZI = 999
         LUNFZO = 998
         LUNCZ  = 0
         LBUFCZ = 270
         CALL FZFILE(LUNFZI,LBUFCZ,'SIOC')
         CALL FZHOOK(LUNFZI,CZTCP,DBUF)
         CALL FZFILE(LUNFZO,LBUFCZ,'SOC')
         CALL FZHOOK(LUNFZO,CZTCP,DBUF)
         IADTCP=JUMPAD(CZTCP)
         CALL JUMPST(IADTCP)
      ENDIF
*
      ISOCK = PZSETUP(CHPORT,CHCLUS,CHHOST,CHVERS,PIAFRC)
      IF(ISOCK.LT.0)THEN
         ISTAT=-ISOCK
      ELSE
         ISTAT=0
         ISKIN=ISOCK
         ISKOUT=ISOCK
      ENDIF

*
      END
#endif
