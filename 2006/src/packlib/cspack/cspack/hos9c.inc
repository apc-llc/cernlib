*
* $Id: hos9c.inc,v 1.1.1.1 1996/03/08 15:44:16 mclareni Exp $
*
* $Log: hos9c.inc,v $
* Revision 1.1.1.1  1996/03/08 15:44:16  mclareni
* Cspack
*
*
#ifndef CERNLIB_CSPACK_HOS9C_INC
#define CERNLIB_CSPACK_HOS9C_INC
*
*
* hos9c.inc
*
      COMMON/HOS9C1/CPU,MODULE
      CHARACTER*20 CPU,MODULE
      PARAMETER(knwid=32,kjcont=33,knw=34,knbprox=35,kjr1=36)
      PARAMETER(kid=37,kad=38,maxhist=400)
      COMMON/HOS9C2/IADPAWD,NHIST,IHEAD(KAD,MAXHIST)
*

#endif
