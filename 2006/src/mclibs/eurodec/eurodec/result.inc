*
* $Id: result.inc,v 1.1.1.1 1996/03/08 16:58:48 mclareni Exp $
*
* $Log: result.inc,v $
* Revision 1.1.1.1  1996/03/08 16:58:48  mclareni
* Eurodec
*
*
#ifndef CERNLIB_EURODEC_RESULT_INC
#define CERNLIB_EURODEC_RESULT_INC
*
*
* result.inc
*
      PARAMETER (NTMAX=2000)
      COMMON/RESULT/NTEIL,PTEIL(5,NTMAX),INDEXT(NTMAX),IORIGT(NTMAX),
     &IDCAYT(NTMAX),ITHEL(NTMAX),SECVTX(3,NTMAX)

#endif
