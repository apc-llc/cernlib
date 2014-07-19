*
* $Id: igcell.F,v 1.1.1.1 1996/02/14 13:10:33 mclareni Exp $
*
* $Log: igcell.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:33  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.22/08 27/02/95  17.22.05  by  O.Couet
*-- Author :    O.Couet   11/09/89
      REAL FUNCTION IGCELL(NX,NY,V,I,J,IOPT)
*.==========>
*.
*.   Returns the value of cell defined by I and J according to the
*. value of NPAR PAR and CHOPT given to IGTABL. If IOPT=1 the content
*. of the cell I,J is returned, if IOPT=2 the error on the cell I,J is
*. returned. The data can come from an HBOOK histogram (IPACK=1), from
*. an HIGZ picture (IPACK=2) or from a FORTRAN array (IPACK=0).
*.
*..=========> (O.Couet)
#include "higz/hipack.inc"
#include "higz/hipaw.inc"
      DIMENSION V(NX,1)
      REAL IGHCXY
*.___________________________________________
*
*
*              Get the content of the cell I,J (IOPT=1)
*
      IF (IOPT.EQ.1) THEN
         IF (IPACK.EQ.1) THEN
            IGCELL = IGHCXY(INT(V(1,1)),I,J,1)
         ELSEIF (IPACK.EQ.2) THEN
            IF (IDIM1.NE.0) THEN
               IGCELL = Q(INT(V(1,1))+I)
            ELSE
               IGCELL = Q(INT(V(1,1))+(J-1)*NX+I)
            ENDIF
         ELSE
            IF (IDIM1.NE.0) THEN
               IGCELL = V(I,1)
            ELSE
               IGCELL = V(I,J)
            ENDIF
         ENDIF
*
* Adjust the content to ZHIGH and ZLOW
*
         IF( ZHIGH.GT.ZLOW) THEN
            IF (IGCELL.GT.ZHIGH) IGCELL = ZHIGH
            IF (IGCELL.LT.ZLOW) IGCELL = ZLOW
         ENDIF
      ENDIF
*
*              Get the error on the cell I,J (IOPT=2)
*
      IF (IOPT.EQ.2) THEN
         IF (IPACK.EQ.1) THEN
            IGCELL = IGHCXY(INT(V(1,1)),I,J,2)
         ELSEIF (IPACK.EQ.2) THEN
            IF (IDIM1.NE.0) THEN
               IGCELL = SQRT(ABS(Q(INT(V(1,1))+I)))
            ELSE
               IGCELL = SQRT(ABS(Q(INT(V(1,1))+(J-1)*NX+I)))
            ENDIF
         ELSE
            IF (IDIM1.NE.0) THEN
               IGCELL = SQRT(ABS(V(I,1)))
            ELSE
               IGCELL = SQRT(ABS(V(I,J)))
            ENDIF
         ENDIF
      ENDIF
*
*              LOG scale is required
*
      IF (ILOG.NE.0) THEN
         IF (IGCELL.GT.0.) THEN
            IGCELL = LOG10(IGCELL)
         ELSE
            IGCELL = ZLOW
         ENDIF
      ENDIF
*
      END
