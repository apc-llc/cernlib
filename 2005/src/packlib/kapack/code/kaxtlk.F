*
* $Id: kaxtlk.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxtlk.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXTLK(IROOT,*)
*
*.....TEST A LOCK FOR THE TREE WITH ROOT NUMBER IROOT
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax070.inc"
*
*-----------------------------------------------------------------------
*
*.....SEARCH THE LOCK QUEUE FOR ENTRY IROOT
      DO  1  I = 1, LLOCKQ
         IF ( LOCKI(I).EQ.IROOT  .AND.
     +        LOCKU(I).EQ.LUNKAF .AND.
     +        LOCKN(I).NE.0 )  RETURN
 1    CONTINUE
*
      RETURN 1
*
      END
