*
* $Id: pcolv.inc,v 1.3 1996/09/13 15:35:24 couet Exp $
*
* $Log: pcolv.inc,v $
* Revision 1.3  1996/09/13 15:35:24  couet
* - OLD Qp remove
*
* Revision 1.2  1996/03/01 16:50:23  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:48  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCOLV_INC
#define CERNLIB_PAW_PCOLV_INC
*
* pcolv.inc
*
      COMMON /PCOLV/ NCOLOR,FMIN,FMAX,IC1,IC2,IC3
#endif
