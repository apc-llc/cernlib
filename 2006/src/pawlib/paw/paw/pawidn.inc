*
* $Id: pawidn.inc,v 1.2 1996/03/01 16:50:03 cernlib Exp $
*
* $Log: pawidn.inc,v $
* Revision 1.2  1996/03/01 16:50:03  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:49  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PAWIDN_INC
#define CERNLIB_PAW_PAWIDN_INC
*
*
* pawidn.inc
*
*CMZ :  2.03/18 14/10/93  19.51.31  by  Fons Rademakers
*-- Author :
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,VIDN(10),X(512)
      COMMON/PAWOFF/NEVTID,IVIOFF(13),IWIOFF,IEIOFF
      COMMON/PCHIDN/CHNAMS(512)
      CHARACTER*8 CHNAMS
*

#endif
