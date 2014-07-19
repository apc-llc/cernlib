*
* $Id: ipzone.F,v 1.2 2005/10/28 12:38:14 couet Exp $
*
* $Log: ipzone.F,v $
* Revision 1.2  2005/10/28 12:38:14  couet
* - SVG driver, workstation type -779
*
* Revision 1.1.1.1  1996/02/14 13:11:08  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/02 30/05/95  14.43.59  by  O.Couet
*-- Author :    O.Couet   13/08/91
      SUBROUTINE IPZONE
#include "higz/hiatt.inc"
#include "higz/hipost.inc"
#include "higz/hiflag.inc"
*.______________________________________
*
      IF(.NOT.LOCLR.OR.TFLAG.OR.VFLAG)RETURN
      LOCLR=.FALSE.
* When IPZONE has been called, LOZONE is TRUE
      LOZONE=.TRUE.
*
      IF(IYZ.GT.NY)THEN
         IYZ=1
         IF(MODE.NE.3)THEN
            CALL IPPSTR('@showpage')
            CALL IPSVRT(-1)
            NPAGES = NPAGES+1
            CALL IPPSTR('@%%Page: number ')
            CALL IPIOUT(NPAGES)
            CALL IPPSTR('@')
         ELSE
            CALL IPPSTF(9,' showpage')
            CALL IPSVRT(-1)
         ENDIF
      ENDIF
*
*              No grestore the first time
*
      IF(MODE.NE.3)THEN
         IF (IXZ.NE.1.OR.IYZ.NE.1) CALL IPSVRT(-1)
         CALL IPSVRT(1)
         CALL IPPSTR('@')
         CALL IPIOUT(IXZ)
         CALL IPIOUT(IYZ)
         CALL IPPSTF(5,' Zone')
         CALL IPPSTR('@')
         IXZ=IXZ+1
         IF(IXZ.GT.NX)THEN
            IXZ=1
            IYZ=IYZ+1
         ENDIF
      ENDIF
*
*              Picture Initialisation
*
      CALL IPSVRT(1)
      CALL IPPSTF(5,'0 0 t')
      IPSLT  = 0
      IPSLW  = 0
      IPSCI  = 0
      IPSMSC = -1
      PSRED  = -1.
      PSGRN  = -1.
      PSBLU  = -1.
      CALL IPSCOL(1)
      CALL IPLTYP(1)
      CALL IPLWID(1)
      LOPRT=.FALSE.
*
*              Force the clipping definition if required.
*
      IF (ICLIP.NE.0) THEN
         IPDXC = -IPDXC
         CALL ISCLIP(ICLIP)
      ENDIF
*
      END
