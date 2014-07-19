*
* $Id: polf2i.F,v 1.1.1.1 1996/02/14 13:11:04 mclareni Exp $
*
* $Log: polf2i.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:04  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.49.21  by  Unknown
*-- Author : V.Fine
      SUBROUTINE POLF2I(N,IXY)
*.
*.   by Fine V. from LCTA JINR (fine@main2.jinr.dubna.su)
*.       (based on IGHATC)
*.
      INTEGER IXY(N,2)
*.___________________________________________
*
#include "higz/hikern.inc"
#include "higz/hiatt.inc"
      INTEGER IXY(IBUFSI,2)
      III =IFACI+IOFCOL
#if defined(CERNLIB_NDP)
      IER = POLYFILL (IXY(1,1),IXY(1,2),N,III, III)
#endif
#if !defined(CERNLIB_NDP)
      IER = POLFIL (IXY,N,III, III)
#endif
      END
#endif
