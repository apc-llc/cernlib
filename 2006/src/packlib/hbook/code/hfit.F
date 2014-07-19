*
* $Id: hfit.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfit.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/08 04/07/94  08.58.13  by  Rene Brun
*-- Author :
      SUBROUTINE HFIT  (IDD,UFUNC,NPAR,PARAM,CHI2,ICONTR,SIGPAR,COVMAT,
     +STEP0,PARMIN,PARMAX)
*.==========>
*.           FITS THE PARAMETRIC FUNCTION TO THE CONTENTS OF THE
*.           1- OR 2-DIMENSIONAL HISTOGRAM IDD,AND OPTIONALLY SUPER-
*.           IMPOSES IT TO THE 1-DIMENSIONAL HISTOGRAM WHEN EDITING
*.
*.
*.           INPUT     IDD       = HISTOGRAM IDENTIFIER
*.           ARGUMENTS
*.                     UFUNC     = PARAMETRIC FUNCTION (EXTERNAL)
*.
*.                     NPAR      = NUMBER OF PARAMETERS
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
*.                                (-/+10**4WORDLENGTH BY DEFAULT)
*.
*.           OUTPUT    PARAM     = FINAL VALUES OF PARAMETERS (ARRAY)
*.           ARGUMENTS
*.                     CHI2      = CHISQUARE OF FIT
*.
*.                     SIGPAR    = STANDARD DEVIATIONS OF PARAMETERS
*.                                  (ARRAY)
*.
*.                     COVMAT    = COVARIENCE  MATRIX  OF PARAMETERS
*.                                    (ARRAY L=NPAR*(NPAR+1)/2)
*.
*.            ARGUMENTS FROM CHI2 INCLUDED, ARE OPTIONAL
*.
*.    ---------------------------REMARKS------------------------------
*.
*.                ICONTR=L*10**6+F*100000+B*10000+D*1000+W*100+P*10+S
*.
*.           S=2,DEFAULT - SUPERIMPOSES FUNCTION TO HISTOGRAM
*.           S=1         - NO SUPERIMPOSING
*.
*.           P=1         - PRINTS RESULTS OF THE FINAL ITERATION
*.           P>1         - PRINTS RESULTS OF ITERATIONS N*(P-1),WHERE
*.                                 N=0,1,2,...
*.           P=0,DEFAULT - SUPPRESSES INTERMEDIATE PRINTING
*.
*.           W=1         - SETS WEIGHTS EQUAL TO 1.
*.           W=0,DEFAULT -CALCULATES ERROR BAR E=SQRT(CONTENTS)
*.            AND SETS THE WEIGHT EQUAL TO 1./E**2
*.           HOWEVER IF ERROR BARS ARE AVAILABLE (E.G.AFTER CALL TO
*.           HBARX OR HPAKE)THEY ARE USED FOR SETTING OF THE WEIGHTS
*.
*.           D,F ARE ESSENTIAL ONLY FOR HFIT, OTHER FIT-SUBROUTINES
*.                               IGNORED D,F
*.           D=0,DEFAULT - SAVE USER"S FORCE:CALCULATES DERIVATIVES
*.                          FOR PARAMETERS NUMERICALLY
*.           D=1               - REQUIRES USER ROUTINE HDERI2(2-DIM)
*.                               HDERI1(OTHERWISE) FOR COMPUTATION
*.                               OF DERIVATIVES
*.
*.           B=0,DEFAULT - SAVE MEMORY,USED DIRECTLY HISTOGRAM CON-
*.                                      TENTS
*.           B=1         - SAVE TIME,DEMANDS BUFFER ARRAY FOR FITTED
*.                         DATA:TRANSFORMED HISTOGRAM CONTENTS
*.
*.           F>0         - SPECIAL FORMAT:DATA SET FOR F VARIABLES
*.                           (C,SIGMAC,X1,...,XF)
*.           F=0,DEFAULT - USUAL FORMAT OF HISTOGRAMS
*.
*.           L=1         - LINEAR CASE
*.           L=0,DEFAULT - GENERAL CASE
*..=========> ( R.Brun, I.Ivanchenko )
      DIMENSION PARAM(1),SIGPAR(1),COVMAT(1),STEP0(1),PARMIN(1)
     +,PARMAX(1),NCHAN(2)
#include "hbook/hcfit2.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcbook.inc"
      EXTERNAL UFUNC
      COMMON/HCGARB/NDIM,PLMIN,PLEPS,IBIT,LEXDAT,ISWTCH,GA(11)
      DATA NAME/2H  /
#if defined(CERNLIB_CDC)
      DATA I4000B/O"4000"/
#endif
*.___________________________________________
      CALL HFIND(IDD,'HHFIT ' )
      IF(LCID.EQ.0)GO TO 99
      IF(IQ(LCONT+KNOENT).EQ.0)THEN
         CALL HBUG('Empty histogram','HFIT',IDD)
         GO TO 99
      ENDIF
      CALL HDCOFL
*
      NDIM=1
      NCHAN(1)=IQ(LPRX)
      IF(I1.EQ.0)THEN
         NDIM=2
         NCHAN(2)=IQ(LPRY)
      ENDIF
*
      IDIMPN=2+NDIM
      IFLFUN=1
      NAMFUN=NAME
*
      NARG=11
      CALL NOARG(NARG)
*
      CALL HGETFL(ICONTR)
*
      NV=1
      IF(IDIMPN.EQ.3.AND.ISUPIM.EQ.2)CALL HFUNC(IDD,UFUNC)
      NV=2
*
      LEXDAT=IDIMPN
      DO 2 I=1,NDIM
   2  LEXDAT=NCHAN(I)*LEXDAT
*
      IF(IFLBUF.EQ.0)LEXDAT=IDIMPN
      LMATR=(NPAR*(NPAR+1))/2
      LAHFIT=LEXDAT+8*NPAR+LMATR
      IF(NARG.LT.8)LAHFIT=LAHFIT+LMATR+NPAR
*
      CALL HWORK(LAHFIT,IMATR0,'HFIT  ')
      IF(IMATR0.EQ.0)GO TO 99
      IMATR=IMATR0+LMATR
      ISIGMA=IMATR+LMATR
      IGRADS=ISIGMA+NPAR
      IF(NARG.GE.8)IGRADS=IMATR0+LMATR
      IDERFU=IGRADS+NPAR
      IPL0  =IDERFU+NPAR
      IPLFUM=IPL0+NPAR
      IRFUM =IPLFUM+NPAR
      IDELPA=IRFUM +NPAR
      IMAXPA=IDELPA+NPAR
      IMINPA=IMAXPA+NPAR
      IEXDAT=IMINPA+NPAR
*
*
*             SETS BOUNDARY SIZES FOR THE PARAMETERS
*
      DO 3 I=1,NPAR
         Q(IMINPA+I-1)=-BIGP
         IF(NARG.GE.10)Q(IMINPA+I-1)=PARMIN(I)
         Q(IMAXPA+I-1)=BIGP
         IF(NARG.GE.11)Q(IMAXPA+I-1)=PARMAX(I)
   3  CONTINUE
*
#if defined(CERNLIB_CDC)
*
*             IF(START VALUE OF A PARAMETER IS UNDEFINED)SETS TO 0,MESS
*
      IUNDEF=0
      DO 5 I=1,NPAR
         IBIT=JBYT(PARAM(I),49,12)
         IF(IBIT.NE.I4000B)GO TO 5
         IUNDEF=1
         PARAM(I)=0.
   5  CONTINUE
      IF(IUNDEF.EQ.1)CALL HBUG('Error 727','HFIT',ID)
*
#endif
*
      IF(IFLBUF.EQ.1)CALL HEXDAT(IEXDAT,IFLRET)
      IF(IFLBUF.EQ.0)CALL HHIPAR(IFLRET)
*
      IF(IFLRET.EQ.0)GO TO 99
*
*             SETS INITIAL STEP  SIZES FOR PARAMETERS
*
      IF(NARG.LT.9)THEN
         PLMIN=BINWID
         PLEPS=1.E-9
         DO 15 I=1,NPAR
            Q(IPL0+I-1)=0.3*ABS(PARAM(I))
            IF(Q(IPL0+I-1).LT.PLEPS)Q(IPL0+I-1)=PLMIN
  15     CONTINUE
         IF(LINEAR.EQ.1)CALL VFILL(Q(IPL0),NPAR,BIGP)
      ELSE
         DO 21 I=1,NPAR
            Q(IPL0+I-1)=STEP0(I)
  21     CONTINUE
      ENDIF
*
      IF(NARG.GE.8)
     +CALL HFUMIL(UFUNC,FUMIN,NPAR,ITFUM,MCFUM,COVMAT,Q(IMATR0),
     +Q(IGRADS),PARAM,Q(IDERFU),Q(IPL0),SIGPAR,Q(IPLFUM),
     +Q(IRFUM),Q(IDELPA),Q(IMAXPA),Q(IMINPA),Q(IEXDAT))
      IF(NARG.LT.8)CALL HFUMIL(UFUNC,FUMIN,NPAR,
     +ITFUM,MCFUM,Q(IMATR),Q(IMATR0),Q(IGRADS),
     +PARAM,Q(IDERFU),Q(IPL0),Q(ISIGMA),Q(IPLFUM),Q(IRFUM),
     +Q(IDELPA),Q(IMAXPA),Q(IMINPA),Q(IEXDAT))
*
*
      ISWTCH=NARG-3
      GO TO(36,35,35,32,35,28,28,28),ISWTCH
  28  CONTINUE
      DO 29 I=1,NPAR
         STEP0(I)=Q(IPL0+I-1)
  29  CONTINUE
      GO TO 35
  32  CONTINUE
      DO 33 I=1,NPAR
         SIGPAR(I)=Q(ISIGMA+I-1)
  33  CONTINUE
  35  CHI2=FUMIN+FUMIN
  36  CONTINUE
      IF(IDIMPN.EQ.3.AND.ISUPIM.EQ.2)CALL HFUNC(IDD,UFUNC)
*
  99  RETURN
      END
