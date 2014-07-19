*
* $Id: hfitl.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfitl.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HFITL (IDD,UFUNC,NP,PARAM,CHI2,ICONTR,SIGPAR,COVMAT,
     +STEP0,PARMIN,PARMAX)
*.==========>
*.      NEW FITTING PACKAGE   -   AUTHORS:  E.LESSNER, D.LIENART
*.
*.           FITS THE PARAMETRIC FUNCTION TO THE CONTENTS OF THE
*.           1- OR 2-DIMENSIONAL HISTOGRAM IDD,AND OPTIONALLY SUPER-
*.           IMPOSES IT TO THE 1-DIMENSIONAL HISTOGRAM WHEN EDITING
*.
*.
*.           INPUT     IDD       = HISTOGRAM IDENTIFIER
*.           ARGUMENTS
*.                     UFUNC     = PARAMETRIC FUNCTION (EXTERNAL)
*.
*.                     NP        = NUMBER OF PARAMETERS
*.
*.                     PARAM     = INITIAL VALUES OF PARAMETERS(ARRAY)
*.
*.                     ICONTR    = CONTROL WORD
*.                                   1 - NO SUPERIMPOSING
*.                                   2 - FITTED FUNCTION IS SUPER-
*.                                       IMPOSED TO HISTOGRAM
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
*.           S=2         - SUPERIMPOSES FUNCTION TO HISTOGRAM
*.           S=1         - NO SUPERIMPOSING
*.
*.           P=1         - PRINTS RESULTS OF THE FINAL ITERATION
*.           P>1         - PRINTS RESULTS OF ITERATIONS N*(P-1),WHERE
*.                                 N=0,1,2,...
*.           P=0         - SUPPRESSES INTERMEDIATE PRINTING
*.
*.           W=1         - SETS WEIGHTS EQUAL TO 1.
*.           W=0         -CALCULATES ERROR BAR E=SQRT(CONTENTS)
*.            AND SETS THE WEIGHT EQUAL TO 1./E**2
*.           HOWEVER IF ERROR BARS ARE AVAILABLE (E.G.AFTER CALL TO
*.           HBARX OR HPAKE)THEY ARE USED FOR SETTING OF THE WEIGHTS
*.
*.           D=0         - SAVE USER"S FORCE:CALCULATES DERIVATIVES
*.                          FOR PARAMETERS NUMERICALLY
*.           D=1               - REQUIRES USER ROUTINE HDERI2(2-DIM)
*.                               HDERI1(OTHERWISE) FOR COMPUTATION
*.                               OF DERIVATIVES
*.
*.           B=0         - ALL PARAMETERS MAY VARY FREELY
*.           B=1         - SOME OR ALL PARAMETERS ARE BOUNDED
*.           B=2         - LINEAR EQUALITY CONSTRAINTS BETWEEN PARAMS
*.                       - OPTION PRESENTLY INACTIVE
*.
*.           F>0         - SPECIAL FORMAT:DATA SET FOR F VARIABLES
*.                           (C,SIGMAC,X1,...,XF)
*.           F=0         - USUAL FORMAT OF HISTOGRAMS
*.
*.           M=1         - DAVIDON-FLETCHER-POWELL METHOD
*.           M=0         - GAUSS-NEWTON METHOD
*..=========> ( R.Brun ,E.Lessner,D.Lienart)
      DIMENSION PARAM(*),SIGPAR(*),COVMAT(*),STEP0(*),PARMIN(*),
     +          PARMAX(*),NCHAN(2),DQ(2)
*
#include "hbook/hcfit2.inc"
#include "hbook/hcfit3.inc"
#include "hbook/hcfit6.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcfits.inc"
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION PARAM,DQ,UFUNC
#endif
      EQUIVALENCE (Q(1),DQ(1))
      EXTERNAL UFUNC
      DATA NAME/2HL /
*.___________________________________________
      CALL HFIND(IDD,'HFITL ')
      IF(LCID.EQ.0)GO TO 99
      IF(IQ(LCONT+KNOENT).EQ.0)THEN
         CALL HBUG('Empty histogram','HFITL',IDD)
         GO TO 99
      ENDIF
      CALL HDCOFL
*
      NDIM=1
      NCHAN(1)=IQ(LPRX)
      IF(I1.EQ.0)THEN
         IF(LCONT.EQ.LQ(LCID-1))THEN
            NDIM=2
            NCHAN(2)=IQ(LPRY)
         ENDIF
      ENDIF
*
      FITNAM(1)=' '
      IDIMPN=2+NDIM
      IHIS=1
      NX=1
      NY=1
      IFLFUN=1
      NAMFUN=NAME
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
      NV=1
      IF(IDIMPN.EQ.3.AND.ISUPIM.EQ.2)CALL HSUPIM(UFUNC)
      NV=2
*
      LEXDAT=IDIMPN
      LMATR=NP*NP
      LXMATR=NX*NY
      LAHFIT=LEXDAT+2*LMATR+LXMATR+11*NP+2*NY+1
      CALL HWORK(LAHFIT,IBQ(1),'HFITL ')
      IF (IBQ(1).EQ.0) GO TO 99
*
      CALL HHIPAR (IFLRET)
      IF (IFLRET.EQ.0) GOTO 99
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
      IF(IDIMPN.EQ.3.AND.ISUPIM.EQ.2)CALL HSUPIM(UFUNC)
*
   99 END
