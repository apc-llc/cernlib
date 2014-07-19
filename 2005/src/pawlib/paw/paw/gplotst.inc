*
* $Id: gplotst.inc,v 1.3 1996/09/13 15:35:16 couet Exp $
*
* $Log: gplotst.inc,v $
* Revision 1.3  1996/09/13 15:35:16  couet
* - OLD Qp remove
*
* Revision 1.2  1996/03/01 16:49:50  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:50  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_GPLOTST_INC
#define CERNLIB_PAW_GPLOTST_INC
*
* gplotst.inc
*
      COMMON /GPLOTST/ RMAXP(4),RMINP(4),ISTCNT,NEW
      LOGICAL       NEW
#endif
