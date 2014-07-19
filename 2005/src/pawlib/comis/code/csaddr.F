*
* $Id: csaddr.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: csaddr.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/03 09/06/94  17.55.36  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSADDR(CHNAME)
***-------------------------------
*    returns address of comis-routine
***---------------------------
      CHARACTER *(*) CHNAME
      INTEGER CSLTGP,CSITGP
      CHARACTER*32 NAME
#include "comis/cstab.inc"
#if defined(CERNLIB_SHL)
      INTEGER CS_GET_FUNC
      NAME=CHNAME
      NC=LENOCC(NAME)
      CALL CSCHID(NAME(:NC))
      CALL CUTOL(NAME(:NC))
      I=CSLTGP(IPVS)
      IF(I.GT.0)THEN
        IF(IFCS.EQ.0)THEN
          IADGP=CS_GET_FUNC(NAME(1:NC)//'_')
          IF(IADGP.NE.0)THEN
            IFCS=-2
            CALL CSRTGP(I)
          ELSE
            I=0
          ENDIF
        ENDIF
      ELSE
        IADGP=CS_GET_FUNC(NAME(1:NC)//'_')
        IF(IADGP.NE.0)THEN
          IFCS=-2
          ITYPGP=-2
          I=CSITGP(IPVS)
        ENDIF
      END IF
      CSADDR=I
#endif
#if !defined(CERNLIB_SHL)
      NAME=CHNAME
      CALL CSCHID(NAME)
      I=CSLTGP(IPVS)
      IF(I.GT.0)THEN
        IF(IFCS.EQ.0)I=0
      END IF
      CSADDR=I
#endif
      END
