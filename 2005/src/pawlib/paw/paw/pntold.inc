*
* $Id: pntold.inc,v 1.3 1996/09/13 15:35:26 couet Exp $
*
* $Log: pntold.inc,v $
* Revision 1.3  1996/09/13 15:35:26  couet
* - OLD Qp remove
*
* Revision 1.2  1996/03/01 16:50:28  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:49  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PNTOLD_INC
#define CERNLIB_PAW_PNTOLD_INC
*
* pntold.inc
*
      LOGICAL          NTOLD
      COMMON  /PNTOLD/ NTOLD
#endif
