*
* $Id: hilut.inc,v 1.1.1.1 1996/02/14 13:10:15 mclareni Exp $
*
* $Log: hilut.inc,v $
* Revision 1.1.1.1  1996/02/14 13:10:15  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HILUT_INC
#define CERNLIB_HIGZ_HILUT_INC
*
*
* hilut.inc
*
*CMZ :  1.18/00 23/04/93  17.27.57  by  O.Couet
*-- Author :
      PARAMETER (NBCLUT=256)
      COMMON /HILUT/ REDLUT(NBCLUT),GRNLUT(NBCLUT),BLULUT(NBCLUT)


#endif
