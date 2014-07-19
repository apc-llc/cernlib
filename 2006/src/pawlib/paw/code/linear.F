*
* $Id: linear.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: linear.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.13/01 05/03/92  17.05.08  by  Rene Brun
*-- Author :
      SUBROUTINE LINEAR (XX,XA,A,AR,EIGVEC,EIGVAL,S,SS,DEIGVA,
     +                   DWORK,NVA,IDN,IFROM,ITO,IVARS,INORM)
C
C
C     ******************************************************************
C     *                                                                *
C     *                                                                *
C     *  purpose                                                       *
C     *       linear fits many points in M dimensions to a             *
C     *       L-(or lower) dimensional hyperplane                      *
C     *       i.e. finds the L most significant                        *
C     *       linear combinations                                      *
C     *                                                                *
C     *  description of parameters                                     *
C     *       XX        ORIGINAL VARIABLES                             *
C     *       XA        mean value of each X variable                  *
C     *       A         matrix of the linear transformation            *
C     *       AR        local array                                    *
C     *       EIGVEC    matrix of eigenvectors                         *
C     *       EIGVAL    matrix of eigenvalues                          *
C     *       DEIGVA    double precision eigenvalues                   *
C     *       DWORK     double precision working space                 *
C     *       NVA       number of dimension in input data              *
C     *                                                                *
C     *  remarks                                                       *
C     *       EIGVEC(M,L) contains the L most significant              *
C     *       linear combinations. The significances are               *
C     *       indicated by EIGVAL(L1) , L1=1,NVA.                      *
C     *                                                                *
C     *                                                                *
C     ******************************************************************
C
C
C
C
#include "paw/pawidn.inc"
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION AR,DWORK,DEIGVA,SS,SSQ,T1
#endif
      DIMENSION SS(NVA),DEIGVA(NVA),DWORK(NVA),S(NVA)
      DIMENSION A(NVA,NVA),AR(NVA,NVA),EIGVEC(NVA,NVA)
      DIMENSION EIGVAL(NVA),XA(NVA),XX(NVA)
      DIMENSION IVARS(NVA)
C
C
C     ------------------------------------------------------------------
C
C
C        find dispersion matrix
C
      IEVENT=IFROM
C
      CALL HGNF(IDN,IEVENT,X,IERROR)
      IF(IERROR.NE.0) RETURN
*
      DO 5 I=1,NVA
      XX(I)=X(IVARS(I))
    5 CONTINUE
*
      CALL UCOPY(XX,XA,NVA)
      CALL VFILL(S,NVA,1.)
*
      AC=1.
      IEVENT=IEVENT+1
  10  CALL HGNF(IDN,IEVENT,X,IERROR)
      IF(IERROR.NE.0) RETURN
      DO 15 I=1,NVA
      XX(I)=X(IVARS(I))
   15 CONTINUE
*
      IEVENT=IEVENT+1
C     IF(IEVENT.GT.ITO) GO TO 30
      AC=AC+1.
      COR=1.-1./AC
C
      DO 20 I=1,NVA
C
      XA(I)=COR*XA(I)+XX(I)/AC
      T1=(1./(AC-1.))*(XX(I)-XA(I))
C
      DO 20 J=1,I
C
 20   AR(I,J)=COR*AR(I,J)+T1*(XX(J)-XA(J))
C
C
      IF(IEVENT.GT.ITO) GO TO 30
      GO TO 10
  30  CONTINUE
C
C
      DO 40 I=1,NVA
      SS(I)= SQRT(AR(I,I))
      S(I)=SS(I)
      IF(INORM.EQ.0)  GO TO 40
      DO 35 J=1,I
  35  AR(I,J)=AR(I,J)/(SS(I)*SS(J))
  40  CONTINUE
  50  SSQ=0.
C
      DO 60 K=1,NVA
 60   SSQ=SSQ+AR(K,K)
      EIGVAL(NVA+1)=SSQ
C
      IM=NVA-1
      DO 70 I=1,IM
      JM=I+1
      DO 70 J=JM,NVA
 70   AR(I,J)=AR(J,I)
C
C        SCALE
C
      DO 80 I=1,NVA
      DO 80 J=1,NVA
      AR(I,J)=AR(I,J)/SSQ
      A(I,J)=AR(I,J)
  80  CONTINUE
C
C        determine NVA eigenvalues & eigenvectors
C
      CALL LTRED2(NVA,NVA,DEIGVA,DWORK,AR)
      CALL LTQL2(NVA,NVA,DEIGVA,DWORK,AR,IERR)
C
      DO 90 I=1,NVA
      II=NVA+1-I
  90  EIGVAL(II)=DEIGVA(I)
C
      DO 100 J=1,NVA
      JJ=NVA+1-J
      DO 100 I=1,NVA
      EIGVEC(I,J)=AR(I,JJ)
 100  CONTINUE
C
      RETURN
      END
