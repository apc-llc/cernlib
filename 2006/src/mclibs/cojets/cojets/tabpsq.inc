*
* $Id: tabpsq.inc,v 1.1.1.1 1996/01/11 14:14:56 mclareni Exp $
*
* $Log: tabpsq.inc,v $
* Revision 1.1.1.1  1996/01/11 14:14:56  mclareni
* Cojets
*
*
#ifndef CERNLIB_COJETS_TABPSQ_INC
#define CERNLIB_COJETS_TABPSQ_INC
*
*
* tabpsq.inc
*
      PARAMETER (MAXTAB=6)
      COMMON/TABPSQ/TABPSQ(MAXTAB,129),QSQMAX,ALMAX,ALMIN(7),DSTEP(7)
     1,NPREC,NTAB,KODTAB(7)

#endif
