*
* $Id: momgen.inc,v 1.1.1.1 1996/03/08 16:58:48 mclareni Exp $
*
* $Log: momgen.inc,v $
* Revision 1.1.1.1  1996/03/08 16:58:48  mclareni
* Eurodec
*
*
#ifndef CERNLIB_EURODEC_MOMGEN_INC
#define CERNLIB_EURODEC_MOMGEN_INC
*
*
* momgen.inc
*
      PARAMETER (NGMAX=18)
      COMMON/MOMGEN/NP,IP(NGMAX),APM(NGMAX),PCM(5,NGMAX),IPHEL(NGMAX),
     &PRIVTX(3,NGMAX)

#endif
