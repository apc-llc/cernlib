*
* $Id: pcntwk.inc,v 1.3 1996/09/13 15:35:24 couet Exp $
*
* $Log: pcntwk.inc,v $
* Revision 1.3  1996/09/13 15:35:24  couet
* - OLD Qp remove
*
* Revision 1.2  1996/03/01 16:50:22  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:48  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCNTWK_INC
#define CERNLIB_PAW_PCNTWK_INC
*
* pcntwk.inc
*
      CHARACTER*16    CRHOST
      COMMON /PCNTWK/ NPASS, IPASS, IMINEV, IMAXEV, MISBYT, NCHAIN,
     +                NTCONT, IEDONE
      COMMON /PCNTW1/ CRHOST
#endif
