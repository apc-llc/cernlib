*
* $Id: iopks.F,v 1.34 2004/06/10 07:54:21 couet Exp $
*
* $Log: iopks.F,v $
* Revision 1.34  2004/06/10 07:54:21  couet
* - 1.29/04
*
* Revision 1.33  2003/07/23 09:48:23  couet
* - 1.29/03
*
* Revision 1.32  2003/03/18 13:39:39  couet
* *** empty log message ***
*
* Revision 1.31  2003/02/17 10:04:58  couet
* - 1.29/01
*
* Revision 1.30  2002/12/04 11:01:13  couet
* - 1.29/00
*
* Revision 1.29  2002/05/22 09:18:45  couet
* - 1.28/07
*
* Revision 1.28  2002/05/16 15:32:16  couet
* - 1.28/06
*
* Revision 1.27  2002/04/04 10:23:23  couet
* - 1.28/05
*
* Revision 1.26  2002/03/18 16:25:53  couet
* - 1.28/04
*
* Revision 1.25  2001/12/07 15:52:32  couet
* - 1.28/03
*
* Revision 1.24  2001/11/23 16:09:39  couet
* - 1.28/02
*
* Revision 1.23  2001/10/17 14:42:58  couet
* - 1.28/01
*
* Revision 1.22  2001/10/08 16:18:10  couet
* - 1.28/00
*
* Revision 1.21  2001/05/31 08:08:43  couet
* - 1.27/03
*
* Revision 1.20  2000/08/29 14:04:11  couet
* - 1.27/02
*
* Revision 1.19  2000/07/14 15:19:10  couet
* - 1.27/01
*
* Revision 1.18  2000/04/03 15:41:44  couet
* - 1.27/00
*
* Revision 1.17  1999/10/20 14:17:03  couet
* - 1.26/04
*
* Revision 1.16  1999/10/13 12:35:50  couet
* - 1.26/03
*
* Revision 1.15  1999/10/06 14:05:35  couet
* - 1.26/02
*
* Revision 1.14  1999/06/21 16:48:43  couet
* - 1.26/01
*
* Revision 1.13  1999/05/06 06:59:38  couet
* - version 1.26/00 to tag the following bug fixe in x11int.c:
*  "MapNotify is not supported by all the X11 emulators (problem on Mac !!!)"
*
* Revision 1.12  1999/01/07 15:31:33  couet
* - 1.25/05
*
* Revision 1.11  1998/12/07 13:45:29  couet
* - 1.25/04
*
* Revision 1.10  1998/11/16 11:14:10  couet
* - 1.25/03
*
* Revision 1.9  1998/11/13 10:52:37  couet
* - 1.25/02
*
* Revision 1.8  1998/05/26 09:23:04  couet
* - 1.25/01
*
* Revision 1.7  1998/04/09 13:08:23  couet
* - new version
*
* Revision 1.6  1998/01/28 14:34:44  couet
* - GGDM driver removed
*
* Revision 1.5  1997/01/30 10:55:24  couet
* - version 1.23/09
*
* Revision 1.4  1996/09/25 14:58:47  couet
* - GPR driver removed
*
* Revision 1.3  1996/04/23 08:37:23  couet
* - version number incremented.
*
* Revision 1.2  1996/03/08 17:08:03  couet
* The initialization of the Version string was wrong.
*
* Revision 1.1.1.1  1996/02/14 13:10:56  mclareni
* Higz
*
*
#include "higz/pilot.h"

*-- Author : O.Couet
      SUBROUTINE IOPKS(IERFIL)
      ENTRY      GOPKS(IERFIL)
#include "higz/hilun.inc"
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiatt.inc"
#include "higz/hivers.inc"
      CHARACTER*16 TCARD
*
      CALL UHTOC(
     + 40HVERSION  1.29/04   10/06/04                 
#if !defined(CERNLIB_CRAY)
     +,4,TCARD,16)
#endif
#if defined(CERNLIB_CRAY)
     +,8,TCARD,16)
#endif
      CHVERS = TCARD(9:16)
#if !defined(CERNLIB_NOVERS)
      PRINT*,'Version'//CHVERS//' of HIGZ started'
#endif
*
* ICURWK = Index of the current active workstation
* NODRFL = If .TRUE. the basic graphics package is not called
*          in each primitives of IKERNEL
* ICURCI = Current color
* IOFCOL = Offset in the color table
*
      ICURWK=0
      DO 10 I=1,10
         NODRFL(I)=.TRUE.
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
         ICURCI(I)=-1
#endif
#if defined(CERNLIB_MSDOS)
         ICURCI(I)=0
#endif
   10 CONTINUE
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
      IXLOCO=0
      IYLOCO=0
      IOFCOL=64
      LOINS=.FALSE.
#endif
#if defined(CERNLIB_MSDOS)
      IXLOCO=0
      IYLOCO=0
      IWKNB=0
      IOFCOL=0
      LOINS=.FALSE.
#endif
#if defined(CERNLIB_X11)
      THX11=0.
      IFPX11=0.
#endif
#if defined(CERNLIB_PHIGS)
      CALL POPPH(IERFIL,-1)
      IDIM=3
#endif
      IF(IERFIL.GT.0)LUNERR=IERFIL
      END
