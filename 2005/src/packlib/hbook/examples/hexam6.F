*
* $Id: hexam6.F,v 1.2 1996/06/06 09:52:44 cernlib Exp $
*
* $Log: hexam6.F,v $
* Revision 1.2  1996/06/06 09:52:44  cernlib
* Remove #ifdef (CERNLIB_EXAMPLES) and pilot.h where not used
*
* Revision 1.1.1.1  1996/01/16 17:07:50  mclareni
* First import
*
*
#include "pilot.h"
*CMZ :  4.17/02 21/12/92  08.21.10  by  John Allison
*-- Author :
      SUBROUTINE HEXAM6
*.==========>
*.           PARAMETRIZATION      -     SMOOTHING
*..=========> ( R.Brun )
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION COEFF
#endif
      DIMENSION ITERM(15),COEFF(15)
*.___________________________________________
*
      CALL HTITLE('EXAMPLE NO = 6')
*
*             Get hist 110 from data base
*
      CALL HRGET(110,'hexam.dat',' ')
*
*
*       Find best parametrization of histogram in terms of powers
*       of shifted Tchebychev polynomials
*       also produces the corresponding fortran function (here on
*       standard output)
*
*
      CALL HCOPY(110,1,' ')
      CALL HSETPR('PNBX',15.)
      CALL HSETPR('PNCX',15.)
      CALL HSETPR('PLUN',31.)
      CALL HPARAM(1,3011,1.,14,COEFF,ITERM,NCO)
      CALL HPRINT(1)
*
*
*        ID=2 is smoothed with B-splines
*        statistical errors (sqrt of contents) are drawn
*
*
      CALL HCOPY(110,2,' ')
      CALL HSPLI1(2,2,14,3,CHI2)
      CALL HIDOPT(2,'ERRO')
      CALL HPHIST(2,'HIST',1)
*
*
*        ID=3 is smoothed with multiquadrics
*        statistical errors (sqrt of contents) are drawn
*
*
      CALL HCOPY(110,3,' ')
      CALL HQUAD(3,'V',0,0.,0.,NPAR,CHI2,NDF,FMIN,FMAX,IERR)
      LOUT=31
      IF(IERR.NE.0)THEN
         WRITE (LOUT, 10000) IERR
      ELSE
         WRITE (LOUT, 10100) NPAR, CHI2, NDF
         WRITE (LOUT, 10200) FMIN, FMAX
         CALL HIDOPT(3,'ERRO')
         CALL HPHIST(3,'HIST',1)
      END IF
*
*
*        Turn off writing of parameter function.
*
*
      CALL HSETPR('PLUN',0.)
*
*
10000 FORMAT (1X, 'Error', I5, ' in multiquadric smoothing.')
10100 FORMAT (1X, 'Multiquadric smoothing with', I4,  ' parameters.'/
     +3X, 'Chi-squared', G12.5, ' for', I7, ' degrees of freedom.')
10200 FORMAT (3X, 'Min/max event density:', 2G12.5)
      END
