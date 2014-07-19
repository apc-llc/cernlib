*
* $Id: eufiles.inc,v 1.1.1.1 1996/03/08 16:58:49 mclareni Exp $
*
* $Log: eufiles.inc,v $
* Revision 1.1.1.1  1996/03/08 16:58:49  mclareni
* Eurodec
*
*
#ifndef CERNLIB_EURODEC_EUFILES_INC
#define CERNLIB_EURODEC_EUFILES_INC
*
*
* eufiles.inc
*
#if defined(CERNLIB_IBMVM)
      CHARACTER*(*) EDFILE,ETFILE
      PARAMETER (EDFILE='/EURODEC DATA  *')
      PARAMETER (ETFILE='/EUTITLE DAT   *')
#endif
      CHARACTER*(*) EDFILE,ETFILE
      PARAMETER (EDFILE='eurodec.dat')
      PARAMETER (ETFILE='eutitle.dat')

#endif
