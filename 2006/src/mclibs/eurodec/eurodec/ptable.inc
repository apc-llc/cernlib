*
* $Id: ptable.inc,v 1.1.1.1 1996/03/08 16:58:48 mclareni Exp $
*
* $Log: ptable.inc,v $
* Revision 1.1.1.1  1996/03/08 16:58:48  mclareni
* Eurodec
*
*
#ifndef CERNLIB_EURODEC_PTABLE_INC
#define CERNLIB_EURODEC_PTABLE_INC
*
*
* ptable.inc
*
      PARAMETER (NPMAX=400)
      COMMON/PCTABL/NPA,IPC(NPMAX),IPDG(NPMAX),PM(NPMAX),PLT(NPMAX),
     &IDP(NPMAX)
      COMMON/PNTABL/PNA(NPMAX)
      CHARACTER*8 PNA

#endif
