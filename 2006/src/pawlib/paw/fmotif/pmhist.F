*
* $Id: pmhist.F,v 1.1.1.1 1996/03/01 11:39:06 mclareni Exp $
*
* $Log: pmhist.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:06  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/15 13/03/94  12.20.58  by  Fons Rademakers
*-- Author :    Fons Rademakers   13/03/94
      SUBROUTINE PMHIST(ID, TYPE)
*************************************************************************
*                                                                       *
*   Interface routine to the fset_histo routine.                        *
*   The fset_histo routine is a C routine called from Fortran to set    *
*   the correct histo id in the histo style panel.                      *
*                                                                       *
*************************************************************************
*
#include "paw/pchist.inc"
*
      INTEGER        ID, TYPE
*
      CALL FSET_HISTO(ID, TYPE, HPATH, LENOCC(HPATH))
*
      END
