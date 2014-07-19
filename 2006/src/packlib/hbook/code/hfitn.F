*
* $Id: hfitn.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfitn.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HFITN (X,Y,EY,N,NDIM,NVAR,UFUNC,NP,PARAM,CHI2,ICONTR,
     +SIGPAR,COVMAT,STEP0,PARMIN,PARMAX)
*.==========>
*.           FITS THE PARAMETRIC FUNCTION TO THE N-DIMENSIONAL
*.           DISTRIBUTION (X,Y)     (SEE HFITL ALSO)
*.
*.
*.           INPUT     X,Y       = COORDINATES OF POINTS (X= 2-DIM
*.                                 ARRAY OF SIZE N*NDIM, Y= ARRAY
*.                                 OF LENGTH N)
*.           ARGUMENTS
*.                     EY        = ERRORS ON Y (ARRAY OF LENGTH N)
*.
*.                     N         = NUMBER OF POINTS OF THE DISTRIB
*.
*.                     NDIM      = DECLARED FIRST DIMENSION OF X
*.                     NVAR      = DIMENSION OF THE DISTRIBUTION
*.
*.                     UFUNC     = PARAMETRIC FUNCTION (EXTERNAL)
*.
*.                     NP        = NUMBER OF PARAMETERS
*.
*.                     PARAM     = INITIAL VALUES OF PARAMETERS(ARRAY)
*.
*.                     ICONTR    = CONTROL WORD
*.                                   1 - NO SUPERIMPOSING
*.                                   SEE REMARKS
*.
*.                     STEP0   = INITIAL STEP SIZES FOR PARAMETERS
*.                                SEARCH (ARRAY)
*.
*.                     PARMIN  = LOWER
*.                                     PARAMETER BOUNDS (ARRAY)
*.                     PARMAX  = UPPER
*.                                (0. IF UNBOUNDED)                    *
*.
*.           OUTPUT    PARAM     = FINAL VALUES OF PARAMETERS (ARRAY)
*.           ARGUMENTS
*.                     CHI2      = CHISQUARE OF FIT
*.
*.                     SIGPAR    = STANDARD DEVIATIONS OF PARAMETERS
*.                                  (ARRAY)
*.
*.                     COVMAT    = COVARIANCE  MATRIX  OF PARAMETERS
*.                                    (ARRAY L=NPAR*(NPAR+1)/2)
*.
*.
*.    ---------------------------REMARKS------------------------------
*.
*.                ICONTR=M*10**6+F*100000+B*10000+D*1000+W*100+P*10+S
*.
*.           S=1         - NO SUPERIMPOSING
*.
*.           P=1         - PRINTS RESULTS OF THE FINAL ITERATION
*.           P>1         - PRINTS RESULTS OF ITERATIONS N*(P-1),WHERE
*.                                 N=0,1,2,...
*.           P=0         - SUPPRESSES INTERMEDIATE PRINTING
*.
*.           W=0         - ERROR BARS GIVEN BY EY
*.           W=1         - ERRORS COMPUTED AS SQRT(Y)
*.
*.           D=0         - SAVE USER"S FORCE:CALCULATES DERIVATIVES
*.                          FOR PARAMETERS NUMERICALLY
*.           D=1               - REQUIRES USER ROUTINE HDERIN
*.                               FOR COMPUTATION OF DERIVATIVES
*.
*.           B=0         - ALL PARAMETERS MAY VARY FREELY
*.           B=1         - SOME OR ALL PARAMETERS ARE BOUNDED
*.           B=2         - LINEAR EQUALITY CONSTRAINTS BETWEEN PARAMS
*.                       - OPTION PRESENTLY INACTIVE
*.
*.           F=0         - UNDEFINED DIGIT
*.
*.           M=1         - DAVIDON-FLETCHER-POWELL METHOD
*.           M=0         - GAUSS-NEWTON METHOD
*..=========> ( R.Brun ,E.Lessner, D.Lienart)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION PARAM,UFUNC,DQ
#endif
      DIMENSION PARAM(*),SIGPAR(*),COVMAT(*),STEP0(*),PARMIN(*),
     +       PARMAX(*),X(NDIM,NVAR),Y(NDIM),EY(NDIM),DQ(2)
*
#include "hbook/hcfit2.inc"
#include "hbook/hcfit3.inc"
#include "hbook/hcfit6.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcfits.inc"
      EQUIVALENCE (Q(1),DQ(1))
      EXTERNAL UFUNC
      DATA NAME/2HN /
*.___________________________________________
      NUMEP=N
      IHIS=0
      NX=NVAR
      NY=N
      IFLFUN=1
      NAMFUN=NAME
*
      CALL HGETFL(ICONTR)
      ITPR=ITFUM
      ILICO=IFLBUF
      IF (ILICO.EQ.2) THEN
         WRITE (LOUT,'(A)') ' LINEAR EQUALITIES NOT AVAILABLE ',
     +   ' IN THIS VERSION.'
         GO TO 99
      ENDIF
      IMINU=1
*
      FITNAM(1)=' '
      IDIMPN=NVAR+2
      IFLSF=NVAR
      ISUPIM=1
*
      LEXDAT=1
      LMATR=NP*NP
      LXMATR=NX*NY
      LAHFIT=LEXDAT+2*LMATR+LXMATR+11*NP+2*NY+1
      ID=0
      CALL HWORK(LAHFIT,IBQ(1),'HFITN ')
      IF (IBQ(1).EQ.0) GO TO 99
*
*             SET ADDRESSES OF WORKING VECTORS
*
      IF ((IBQ(1)/2)*2.EQ.IBQ(1)) IBQ(1)=IBQ(1)+1
      IBQ(7)=IBQ(1)+2*LMATR+10*NP
      IBQ(1)=(IBQ(1)+1)/2
      DO 5 I=2,6
         IBQ(I)=IBQ(I-1)+NP
    5 CONTINUE
      IBQ(8)=IBQ(7)+LXMATR
      IBQ(9)=IBQ(8)+NY
      IBQ(10)=IBQ(9)+NY
      IDERIV=IBQ(4)
      IDESUM=IBQ(5)
      ICOV=IBQ(6)
      ILXE=IBQ(7)
      ILYE=IBQ(8)
      ILEY=IBQ(9)
      IVARP=IBQ(10)
*
      EPSW=1.E-10
      WGTMAX=0.
      DO 9 L1=1,N
         IF (ABS(Y(L1)).GT.WGTMAX) WGTMAX=ABS(Y(L1))
    9 CONTINUE
*
      DO 12 I=1,NX
         DO 10 J=1,NY
            Q(ILXE+J-1+NY*(I-1))=X(J,I)
   10    CONTINUE
   12 CONTINUE
      DO 15 I=1,N
         Q(ILYE+I-1)=Y(I)
         IF(IWEIGH.EQ.0)THEN
            Q(ILEY+I-1)=EY(I)
         ELSEIF(IWEIGH.EQ.1)THEN
            Q(ILEY+I-1)=1.
         ELSEIF(IWEIGH.EQ.2)THEN
            Q(ILEY+I-1)=WGTMAX
         ELSE
            Q(ILEY+I-1)=WGTMAX/100.
         ENDIF
   15 CONTINUE
*
*             SET BOUNDARY SIZES FOR PARAMETERS
*
      DO 25 I=1,NP
         IF (ILICO.NE.1.OR.PARMIN(I).GT.PARMAX(I)) THEN
            DQ(IBQ(1)+I-1)=0.
            DQ(IBQ(2)+I-1)=0.
         ELSE
            DQ(IBQ(1)+I-1)=PARMIN(I)
            DQ(IBQ(2)+I-1)=PARMAX(I)
         END IF
         DQ(IBQ(3)+I-1)=STEP0(I)
   25 CONTINUE
*
*             OTHER INITIALIZATIONS
*
      NUP=NP
      IMAT=1
*
      CALL HMINUI (UFUNC,PARAM,DQ(IBQ(1)),DQ(IBQ(2)),DQ(IBQ(3)),
     +             DQ(ICOV),IQ(IVARP))
*
      CHI2=CHIM
      DO 30 I=1,NUP
         SIGPAR(I)=DQ(IBQ(3)+I-1)
   30 CONTINUE
*
      IF (ICSTAT.GT.0) THEN
         KFIX=0
         K=0
         DO 40 I=1,NUP
            LC=IQ(IVARP+I-1)
            IF (LC.LE.0) KFIX=KFIX+1
            INTI=I-KFIX
            INTJ=0
            DO 35 J=1,I
               K=K+1
               LCC=IQ(IVARP+J-1)
               IF (LC.LE.0.OR.LCC.LE.0) THEN
                  COVMAT(K)=0.
               ELSE
                  INTJ=INTJ+1
                  COVMAT(K)=DQ(ICOV+NUP*(INTI-1)+(INTJ-1))
               ENDIF
   35       CONTINUE
   40    CONTINUE
      ENDIF
*
   99 END
