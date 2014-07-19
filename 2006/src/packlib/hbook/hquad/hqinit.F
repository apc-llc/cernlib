*
* $Id: hqinit.F,v 1.1.1.1 1996/01/16 17:08:03 mclareni Exp $
*
* $Log: hqinit.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:03  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 15/12/92  21.48.30  by  Rene Brun
*-- Author :    John Allison   02/09/92
      SUBROUTINE HQINIT (NSIG0, NVHQ, NNXYZ, IHQERR)
      INTEGER NSIG0, NVHQ, NNXYZ (*), IHQERR
* Initialise HQ working space for multiquadric smoothing.
* NSIG0 is the number of existing significant points - usually 0.
* NVHQ = no. of variables, i.e., dimemsion of space being smothed/fitted.
* NNXYZ is a vector of no. of bins to be used in each dimension.  This is
*   either the actual no. of bins in the histogram being smoothed, or it is the
*   no. of bins to be used in the trial histogram for ntuple smoothing.
* The parent LHQUAD bank is created here.
* The LQ (LHQUAD - 1) chain (L1H, etc.) is created in HQLIF1.
* The LQ (LHQUAD - 2) chain is created in HQLIF2, when the number of significant
*   points is known.  Note they contain DOUBLE PRECISION words, so since
*   arithmetic operations are made on them in situ, the data MUST begin on an
*   8-byte boundary.  So HQLIF2 allows an extra word for this.
* The LQ (LHQUAD - 3) chain is created in HQLIF3.
* These banks are deleted at the end of operations, but relevant variables are
*   first copied into the LFUNC and LHFIT banks of the histogram or ntuple as
*   required.
*
*                   |
*                   |  LHQUAD = LQ (LCDIR - 6)
*                   V
* *********************************
* * -5 * -4 * -3 * -2 * -1 * HQUA *
* *********************************
*    |    |    |    |    |
*  (reserved)  |    |    V  L1H                   L1V
*              |    | ***********************   ************************
*              |    | * HQ1H * 1-D contents *-->* HQ1V * 1-D variances *--I
*              |    | ***********************   ************************  |
*              |    |                                                     |
*              |    |         I-------------------------------------------I
*              |    |         |
*              |    |         V  L2H
*              |    |      ***********************
*              |    |      * HQ2H * 2-D contents *--> etc.
*              |    |      ***********************
*              |    |
*              |    |
*              |    V LHQCOV                LHQDER
*              | **********************   **********************
*              | * HQCO * Covariances *-->* HQDE * Derivatives *--I
*              | **********************   **********************  |
*              |                                                  |
*              |         I----------------------------------------I
*              |         |
*              |         V LHQWK1
*              |      **********************   **********************
*              |      * HQW1 * Workspace 1 *-->* HQW2 * Workspace 2 *-->etc.
*              |      **********************   **********************
*              |
*              |
*              V  LHQDJN (if space in /PAWC/)
*           *************************************************
*           * HQDJ * Function values for max. lik. fitting. *
*           *************************************************
*
 
#include "hbook/hcqcom.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
 
      INTEGER I
 
* Clear error flag.
      IHQERR = 0
 
* IMQFUN function flag: = 0, no function evaluated yet.
*                       = 1, linear sum of multiquadrics.
*                       = 2, exponential of linear sum of multiquadrics.
* (Note: HQF... routines take account of this, HQD... always calculate the
*    simple sum of multiquadrics.)
      IMQFUN = 0
 
* Set NSIG
      NSIG = NSIG0
 
* Clear HQMIN etc.
      HQMIN = 0.
      HQMAX = 0.
      HQFMIN = 0.
      HQFMAX = 0.
 
* Fill NDIM, NX, NY,...
      NDIM = NVHQ
      DO 10    I = 1, NDIM
         NBINS (I) = NNXYZ (I)
   10 CONTINUE
 
* Drop any HQ banks which might have been left over after, e.g., CTRL-C,
*   and release link area.
      LHQUAD = LQ (LCDIR - 6)
      IF (LHQUAD .NE. 0) THEN
         CALL MZDROP (IHDIV, LHQUAD, ' ')
         LAREA (1) = 0
      END IF
 
* Inquire if there's spacein /PAWC/ for "parent" bank LHQUAD.
      CALL HSPACE (13, 'HQINIT', IDMQ)
      IF (IERR .NE. 0) GO TO 20
 
* Establish temporary link area.
      CALL MZLINT (IXPAWC, '/HCQCOM/', LAREA, LLAST, LAREA)
 
* Book parent bank LHQUAD.
      CALL MZBOOK (IHDIV, LHQUAD, LCDIR, -6, 'HQUA', 5, 5, 0, 0, 0)
 
      GO TO 30
 
   20 CONTINUE
      CALL HBUG ('Not enough working space in /PAWC/.', 'HQINIT', IDMQ)
      IHQERR = 100
 
   30 CONTINUE
 
      END
