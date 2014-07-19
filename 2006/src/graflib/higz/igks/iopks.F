*
* $Id: iopks.F,v 1.33 2004/06/10 07:54:24 couet Exp $
*
* $Log: iopks.F,v $
* Revision 1.33  2004/06/10 07:54:24  couet
* *** empty log message ***
*
* Revision 1.32  2003/07/23 09:48:21  couet
* - 1.29/03
*
* Revision 1.31  2003/03/18 13:39:37  couet
* - 1.29/02
*
* Revision 1.30  2003/02/17 10:06:04  couet
* - 1.29/01
*
* Revision 1.29  2002/12/04 11:00:49  couet
* - 1.29/00
*
* Revision 1.28  2002/05/22 09:18:43  couet
* - 1.28/07
*
* Revision 1.27  2002/05/16 15:32:14  couet
* - 1.28/06
*
* Revision 1.26  2002/04/04 10:23:19  couet
* - 1.28/05
*
* Revision 1.25  2002/03/18 16:25:11  couet
* - 1.28/04
*
* Revision 1.24  2001/12/07 15:52:29  couet
* - 1.28/03
*
* Revision 1.23  2001/11/23 16:09:32  couet
* - 1.28/02
*
* Revision 1.22  2001/10/17 14:42:57  couet
* - 1.28/01
*
* Revision 1.21  2001/10/08 16:18:09  couet
* - 1.28/00
*
* Revision 1.20  2001/05/31 08:08:40  couet
* - 1.27/03
*
* Revision 1.19  2000/08/29 14:04:08  couet
* - 1.27/02
*
* Revision 1.18  2000/07/14 15:19:04  couet
* - 1.27/01
*
* Revision 1.17  2000/04/03 15:41:42  couet
* - 1.27/00
*
* Revision 1.16  1999/10/20 14:16:58  couet
* - 1.26/04
*
* Revision 1.15  1999/10/13 12:35:39  couet
* - 1.26/03
*
* Revision 1.14  1999/10/06 14:05:30  couet
* - 1.26/02
*
* Revision 1.13  1999/06/21 16:48:40  couet
* - 1.26/01
*
* Revision 1.12  1999/05/06 06:59:34  couet
* - version 1.26/00 to tag the following bug fixe in x11int.c:
*  "MapNotify is not supported by all the X11 emulators (problem on Mac !!!)"
*
* Revision 1.11  1999/01/07 15:31:31  couet
* - 1.25/05
*
* Revision 1.10  1998/12/07 13:45:27  couet
* - 1.25/04
*
* Revision 1.9  1998/11/16 11:15:13  couet
* *** empty log message ***
*
* Revision 1.8  1998/11/16 11:13:49  couet
* - 1.25/03
*
* Revision 1.7  1998/11/13 10:52:34  couet
* - 1.25/02
*
* Revision 1.6  1998/05/26 09:23:00  couet
* - 1.25/01
*
* Revision 1.5  1998/04/09 13:08:14  couet
* - new version
*
* Revision 1.4  1997/01/30 10:55:15  couet
* - version 1.23/09
*
* Revision 1.3  1996/06/05 10:20:35  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.2  1996/04/11 15:22:09  cernlib
* Correct title handling
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.21/03 31/05/94  15.05.05  by  O.Couet
*-- Author :    O.Couet   02/10/90
      SUBROUTINE IOPKS(ERRFIL)
*.===========>
*.
*.   This routine opens the HIGZ kernel system .
*.
*. _Input parameters:
*.
*.  INTEGER ERRFIL : LUN for error file .
*.
*..==========> (O.Couet)
#include "higz/hilun.inc"
#include "higz/hivers.inc"
      CHARACTER*16 TCARD
      INTEGER ERRFIL,IASF(13)
      DATA IASF/13*1/
*.______________________________________
*
      CALL UHTOC(
     + 40HVERSION  1.29/04   10/06/04                                     
#if !defined(CERNLIB_CRAY)
     +,4,TCARD,16)
#else
     +,8,TCARD,16)
#endif
      CHVERS = TCARD(9:16)
*
      LUNERR=ERRFIL
#if defined(CERNLIB_GKSGRAL)||defined(CERNLIB_DECGKS)||defined(CERNLIB_UNIGKS)||defined(CERNLIB_ATCGKS)
      IBUFA=0
      CALL GOPKS(ERRFIL,IBUFA)
#endif
#if defined(CERNLIB_GKSBX)
      CALL GOPKS(ERRFIL,-1)
#endif
#if defined(CERNLIB_PLOT10GKS)||defined(CERNLIB_MGKS)
      CALL GOPKS(ERRFIL)
#endif
#if !defined(CERNLIB_MGKS)
      CALL GSASF(IASF)
#endif
*
      END
#endif
