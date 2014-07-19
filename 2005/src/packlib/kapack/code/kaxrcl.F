*
* $Id: kaxrcl.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxrcl.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
#if !defined(CERNLIB_IBM)
      INTEGER FUNCTION KAXRCL(LUNKAF)
*
*.....OBTAIN THE BLOCKSIZE OF A DIRECT ACCESS FILE FOR USE BY KAPACK.
*     THE BLOCKSIZE IS RETURNED IN THE UNITS REQUIRED BY THE FORTRAN
*     OPEN STATEMENT FOR UNFORMATTED DIRECT ACCESS FILES.
*
#include "kapack/kax000.inc"
*
*-----------------------------------------------------------------------
*
      KAXRCL = LWORD * LBUF
*
      END
#endif
