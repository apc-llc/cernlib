*
* $Id: pcmode.inc,v 1.3 1996/09/13 15:35:23 couet Exp $
*
* $Log: pcmode.inc,v $
* Revision 1.3  1996/09/13 15:35:23  couet
* - OLD Qp remove
*
* Revision 1.2  1996/03/01 16:50:21  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:47  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCMODE_INC
#define CERNLIB_PAW_PCMODE_INC
*
* pcmode.inc
*
*     IPIAF =1 for the PIAF server
*     IPAWPP=1 if PAW++
*
      COMMON/PCMODE/IPIAF,IPAWPP,NOLOG,BATCH
      LOGICAL NOLOG,BATCH
#endif
