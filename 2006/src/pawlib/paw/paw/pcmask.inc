*
* $Id: pcmask.inc,v 1.3 1996/09/13 15:35:22 couet Exp $
*
* $Log: pcmask.inc,v $
* Revision 1.3  1996/09/13 15:35:22  couet
* - OLD Qp remove
*
* Revision 1.2  1996/03/01 16:50:20  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:49  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCMASK_INC
#define CERNLIB_PAW_PCMASK_INC
*
* pcmask.inc
*
      PARAMETER (MASDIM=9)
      COMMON/PCMASK/LMASK,LMBUF,LM,IMSK(MASDIM),IMRD,MBIT(MASDIM),MBITD,
     +              NUMD,NBUFFD,NBUFFR,NBUFF(MASDIM),IDEC,LREC,NBMASK
      COMMON/PCMAS2/CHMASK
      CHARACTER*32 CHMASK(20)
#endif
