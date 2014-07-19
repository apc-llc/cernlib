*
* $Id: hcscrn.inc,v 1.1.1.1 1996/02/14 13:10:16 mclareni Exp $
*
* $Log: hcscrn.inc,v $
* Revision 1.1.1.1  1996/02/14 13:10:16  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HCSCRN_INC
#define CERNLIB_HIGZ_HCSCRN_INC
*
*
* hcscrn.inc
*
      PARAMETER         (NSLIC=1000, NTMAX=100)
      COMMON /HCSCRN/   TN(4,3),TB(4,3),
     &                  NT,X0,DX,U(2,NSLIC),D(2,NSLIC),T(2,NTMAX)

#endif
