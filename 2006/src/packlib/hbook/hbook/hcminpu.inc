*
* $Id: hcminpu.inc,v 1.1.1.1 1996/01/16 17:07:50 mclareni Exp $
*
* $Log: hcminpu.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:50  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCMINPU_INC
#define CERNLIB_HBOOK_HCMINPU_INC
*
*
* hcminpu.inc
*
*CMZ :  4.20/03 28/07/93  09.34.19  by  Rene Brun
*-- Author :    Rene Brun   18/06/93
      INTEGER NCMINP,IADINP
      COMMON/HCMINP1/NCMINP,IADINP
      PARAMETER (MCHINP=50)
      CHARACTER*80 CHINPU(MCHINP)
      COMMON/HCMINPU/CHINPU
*

#endif
