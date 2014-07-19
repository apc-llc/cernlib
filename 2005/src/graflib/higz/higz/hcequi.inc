*
* $Id: hcequi.inc,v 1.2 1998/09/25 09:23:54 mclareni Exp $
*
* $Log: hcequi.inc,v $
* Revision 1.2  1998/09/25 09:23:54  mclareni
* Modifications for the Mklinux port flagged by CERNLIB_PPC
*
* Revision 1.1.1.1  1996/02/14 13:10:17  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HCEQUI_INC
#define CERNLIB_HIGZ_HCEQUI_INC
*
*
* hcequi.inc
*
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)||(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))
      PARAMETER         (I2EQUI=2)
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_DECS))&&(!(defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC)))
      PARAMETER         (I2EQUI=1)
#endif

#endif
