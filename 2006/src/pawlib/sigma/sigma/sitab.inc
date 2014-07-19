*
* $Id: sitab.inc,v 1.1.1.1 1995/12/12 14:36:22 mclareni Exp $
*
* $Log: sitab.inc,v $
* Revision 1.1.1.1  1995/12/12 14:36:22  mclareni
* Imported sources
*
*
#ifndef CERNLIB_SIGMA_SITAB_INC
#define CERNLIB_SIGMA_SITAB_INC
*
*
* sitab.inc
*
*CMZ :  1.09/01 30/07/93  09.22.29  by  Carlo E. Vandoni
*-- Author :
C TABLES & DYNAMIC AREA
      COMMON/SITAB/IPROGR(260)
      DIMENSION IDYNA(4096),XPROGR(260)
      EQUIVALENCE (DYNA(1),IDYNA(1)),(IZMOD(1),IZMODA),
     ,(IZMOD(2),IZMODB),(IZMOD(3),MODE),(IZADA,IZAD(1)),
     ,(IZADB,IZAD(2)),(IADDR,IZAD(3)),(IPROGR(1),XPROGR(1))

#endif
