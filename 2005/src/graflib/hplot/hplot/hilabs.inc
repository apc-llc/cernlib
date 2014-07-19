*
* $Id: hilabs.inc,v 1.1.1.1 1996/01/19 10:50:15 mclareni Exp $
*
* $Log: hilabs.inc,v $
* Revision 1.1.1.1  1996/01/19 10:50:15  mclareni
* Hplot
*
*
#ifndef CERNLIB_HPLOT_HILABS_INC
#define CERNLIB_HPLOT_HILABS_INC
*
*
* hilabs.inc
*
*CMZ :          09/11/95  17.18.57  by  O.Couet
*-- Author :
      PARAMETER (MAXLAB=100)
      CHARACTER*32 HILABS
      COMMON/HILBL1/NHILAB
      COMMON/HILBL2/HILABS(MAXLAB)

#endif
