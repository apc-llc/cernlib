*
* $Id: lintra.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: lintra.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/13 27/09/93  09.04.53  by  Rene Brun
*-- Author :
      SUBROUTINE LINTRA(IDN,NVARJ,NVAR,IFROM,ITO,IVARS,INORM,IOPTP)
C
C
C     ******************************************************************
C     *                                                                *
C     *                                                                *
C     *       operates a linear transformation on original             *
C     *       variables.   computes eigenvectors and eigenvalues       *
C     *       corresponding to the matrix of transformation.           *
C     *                    punches the fortran code of the subroutine  *
C     *       permiting the transformations.                           *
C     *                                                                *
C     ******************************************************************
C
C
#include "hbook/hcunit.inc"
#include "paw/pcchar.inc"
#include "paw/pclint.inc"
      DIMENSION IVARS(NVAR)
#if defined(CERNLIB_IBM)
#include "paw/pcwk.inc"
      CHARACTER*8 CW
#endif
*
*     ------------------------------------------------------------------
*
*
*             INITIALISATION
*
      DO 10 I=1,KNTMA2
         AR(I)=0.
         AM(I)=0.
         EIGVEC(I)=0.
   10 CONTINUE
      DO 20 I=1,KNTMAX
         DEIGVA(I)=0.
         DWORK(I)=0.
         SS(I)=0.
         XX(I)=0.
         XA(I)=0.
         EIGVAL(I)=0.
         XSI(I)=0.
         R(I)=0.
         S(I)=0.
   20 CONTINUE
*
*             print constants of program
*
      CALL LMATOU(1,NVAR,IFROM,ITO,NVARJ,INORM)
*
*----         compute transformation to hyperplane
*
      CALL LINEAR(XX     ,XA        ,AM     ,AR      ,EIGVEC     ,
     +            EIGVAL   ,S   ,SS  ,DEIGVA,DWORK   ,NVAR,
     +            IDN,IFROM,ITO,IVARS,INORM)
*
*
*             print of the complete results
*
      IF(IOPTP.NE.0) THEN
         CALL LMATOU(2,NVAR,IFROM,ITO,NVARJ,INORM)
      ENDIF
C
C     restore RMS if no normalisation
C
      IF(INORM.EQ.0)  CALL VFILL( S(1),NVAR,1.)
*
*  CREATE XTOXSI.FORTRAN ROUTINE
*
      CALL PAWJOJ(IVARS,NVAR)
*
      WRITE (LOUT,10000) NVAR,CHFILE,NVAR
*
*             plot of eigenvalues
*
#if defined(CERNLIB_IBM)
      IF(IWK.GT.0) THEN
         CALL KUPROC('Type CR to continue',CW,NCW)
      ENDIF
#endif
      CALL LMATOU(3,NVAR,IFROM,ITO,NVARJ,INORM)
*
#if defined(CERNLIB_UNIX)
#define CERNLIB_NAME_FORMAT A8
#elif defined(CERNLIB_VAXVMS) || defined(CERNLIB_APOLLO)
#define CERNLIB_NAME_FORMAT A10
#elif defined(CERNLIB_IBM)
#define CERNLIB_NAME_FORMAT A14
#else
#define CERNLIB_NAME_FORMAT A14
#endif

10000 FORMAT (///,' ==> PRINCIPAL COMPONENTS ANALYSIS :',/,
     +        '     The principal components XSI(1) to XSI(',I2,') can b
     +e obtained using the function :',/,
     +30X,'-> ',CERNLIB_NAME_FORMAT,'(I) <-',/
     +50X,'with I = 1 to ',I2,///)
*
#undef CERNLIB_NAME_FORMAT
      END
