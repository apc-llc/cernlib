*
* $Id: dtable.inc,v 1.1.1.1 1996/03/08 16:58:48 mclareni Exp $
*
* $Log: dtable.inc,v $
* Revision 1.1.1.1  1996/03/08 16:58:48  mclareni
* Eurodec
*
*
#ifndef CERNLIB_EURODEC_DTABLE_INC
#define CERNLIB_EURODEC_DTABLE_INC
*
*
* dtable.inc
*
      PARAMETER (NDMAX=2000)
      COMMON/DTABLE/NDP(NDMAX),IDC(6,NDMAX),DBR(NDMAX),ME(NDMAX)

#endif
