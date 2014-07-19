*
* $Id: hilut.inc,v 1.2 2005/10/28 12:37:57 couet Exp $
*
* $Log: hilut.inc,v $
* Revision 1.2  2005/10/28 12:37:57  couet
* - SVG driver, workstation type -779
*
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
      PARAMETER (NBCLUT=1024)
      COMMON /HILUT/ REDLUT(NBCLUT),GRNLUT(NBCLUT),BLULUT(NBCLUT)


#endif
