*
* $Id: hsupis.F,v 1.1.1.1 1996/01/16 17:07:49 mclareni Exp $
*
* $Log: hsupis.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/05 20/06/94  16.21.17  by  Rene Brun
*-- Author :
      SUBROUTINE HSUPIS (FUNC,ICASE,JC1,JC2)
*.==========>
*.           Stores values of FUNC and/or parameters.
*.           ICASE = 0, drop structure and clear HCBITS flag 12.
*.           ICASE = 1, initialise structure and set HCBITS flag 12.
*.           ICASE = 2, store parameters and, for 1-D, store function values.
*.           ICASE = 3, store parameters and function values for all-D.
*.           JC1, JC2 = lower and upper channel no. of range (1-D only).
*.                      (If JC2 = 0, whole histogram assumed.)
*.           New format for parameter storage (29/07/92) - LHFIT bit 5 is set.
*.
*.      I
*.      I LHFIT=LQ(LFUNC-1) and LFUNC=LQ(LCONT-1) for both 1- and 2-D.
*.      I
*.      V                          LHFCO, LHFNA
*.  **************************   *****************************
*.  * HFIT * Detailed below. *-->* Linear chain - see below. *
*.  **************************   *****************************
*.
*.
*. Details of fit parameter storage banks - LHFIT and linear chain.
*. ---------------------------------------------------------------
*.
*.   ***********************************************************************
*.   * LHFIT bank (name HFIT).                                             *
*.   ***********************************************************************
*.   * word * type * content                                               *
*.   ***********************************************************************
*.   *    1 *   I  * type of parametrisation:                     (IFITTY) *
*.   *      *      *     = 0, no parameters stored.                        *
*.   *      *      *     = 1, polynomial.                                  *
*.   *      *      *     = 2, exponential.                                 *
*.   *      *      *     = 3, gaussian.                                    *
*.   *      *      *     = 4, multiquadric.                                *
*.   *      *      *     = 100, other.                                     *
*.   ***********************************************************************
*.   *    2 *   I  * no. of variable parameters.                  (NFPAR)  *
*.   *    3 *   I  * no. of data points fitted.                   (NPFITS) *
*.   *    4 *   I  * no. of other (fixed) parameters.             (NOTHER) *
*.   *    5 *   I  * not used.                                             *
*.   ***********************************************************************
*.   *    6 *   R  * chi-squared/no. of degrees of freedom.       (FITCHI) *
*.   *    7 *   R  * minimum function value (sampled at bin centres).      *
*.   *    8 *   R  * maximum function value (sampled at bin centres).      *
*.   *    9 *   R  * not used.                                             *
*.   *   10 *   R  * not used.                                             *
*.   ***********************************************************************
*.   * 20ff *   D  * fitted parameters.                 (NFPAR D.P. words) *
*.   *      *      * fixed parameters.                 (NOTHER D.P. words) *
*.   *      *      * errors on fitted parameters.       (NFPAR D.P. words) *
*.   *      *      * (If covariances are available, no errors are stored.) *
*.   ***********************************************************************
*.        I
*.        I
*.        V
*.   ***********************************************************************
*.   * LHSCOV bank (if present) (name HFCO).                               *
*.   ***********************************************************************
*.   *      *   D  * lower triangle of covariance matrix of fitted         *
*.   *      *      *   parameters.          (NFPAR*(NFPAR+1)/2 D.P. words) *
*.   ***********************************************************************
*.        I
*.        I
*.        V
*.   ***********************************************************************
*.   * LHSNAM bank (if present) (name HFNA).                               *
*.   ***********************************************************************
*.   *      *   8H * parameter names.                     (NFPAR 8H words) *
*.   ***********************************************************************
*.
*..=========> ( R.Brun, J.Allison)
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcform.inc"
#include "hbook/hcfits.inc"
      INTEGER IC1, IC2
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW=2)
      DOUBLE PRECISION SS
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW=1)
      REAL SS
#endif
      REAL V(2)
      EQUIVALENCE (X, V(1)), (Y, V(2))
      EXTERNAL FUNC
*.___________________________________________
*
      CALL SBIT0(IQ(LCID),5)
      IF(JC2.EQ.0)THEN
         IC1=1
         IC2=IQ(LCID+KNCX)
      ELSE
         IC1=JC1
         IC2=JC2
      ENDIF
      NCX=IC2-IC1+1
      IF(I230.NE.0)NCY=IQ(LCID+KNCY)
*000000000000000000000000000000000000000000000000000000000000000000000000
      IF(ICASE.EQ.0)THEN
         IF(IQ(LCONT-2).EQ.0)GO TO 100
         LFUNC=LQ(LCONT-1)
         IF(LFUNC.LE.0)GO TO 100
         NTOT=IQ(LFUNC-1)+IQ(LFUNC-3)+10
         IF(IQ(LFUNC-2).NE.0)THEN
            LHFIT=LQ(LFUNC-1)
            IF(LHFIT.NE.0)THEN
               NTOT=NTOT+IQ(LHFIT-1)+10
               L=LHFIT
   10          CONTINUE
               L=LQ(L)
               IF(L.NE.0)THEN
                  NTOT=NTOT+IQ(L-1)+10
                  GO TO 10
               ENDIF
            ENDIF
         ENDIF
         CALL MZDROP(IHDIV,LFUNC,' ')
         CALL HSIFLA(12,0)
         IQ(LCID+KNTOT)=IQ(LCID+KNTOT)-NTOT
*11111111111111111111111111111111111111111111111111111111111111111111111
      ELSEIF(ICASE.EQ.1)THEN
*        If old style 2-D bank, MZPUSH 2 structural links.
         IF(IQ(LCONT-2).EQ.0)THEN
            NNOW=IQ(LCONT-1)+IQ(LCONT-3)+10
            NMORE=2
            NNEW=NNOW+NMORE
            CALL HSPACE(NNOW+NNEW,'HSUPIS',ID)
            IF(IERR.NE.0)GO TO 100
            CALL MZPUSH(IHDIV,LCONT,NMORE,0,' ')
            IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NMORE
         ENDIF
         LFUNC=LQ(LCONT-1)
         IF(LFUNC.EQ.0)THEN
            IF(I1.NE.0)THEN
               NTOT=NCX+13
               CALL HSPACE(NTOT,'HSUPIS',ID)
               IF(IERR.NE.0)GO TO 100
               CALL MZBOOK(IHDIV,LFUNC,LCONT,-1,'HFUN',1,1,NCX+2,
     +         IOCF2,-1)
            ELSE
*              Create short 2-D HFUN bank - MZPUSH later if required.
               NTOT=15
               CALL HSPACE(NTOT,'HSUPIS',ID)
               IF(IERR.NE.0)GO TO 100
               CALL MZBOOK(IHDIV,LFUNC,LCONT,-1,'HFUN',1,1,4,
     +         IOCF4,0)
            ENDIF
            CALL HSIFLA(12,1)
            IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
         ELSEIF(I1.NE.0)THEN
            NMORE=NCX+2-IQ(LFUNC-1)
            IF(NMORE.NE.0)THEN
               NNOW=IQ(LFUNC-1)+IQ(LFUNC-3)+10
               NNEW=NNOW+NMORE
               CALL HSPACE(NNOW+NNEW,'HSUPIS',ID)
               IF(IERR.NE.0)GO TO 100
               CALL MZPUSH(IHDIV,LFUNC,0,NMORE,' ')
               IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NMORE
            ENDIF
         ENDIF
         IF(I1.NE.0)THEN
            IQ(LFUNC+1)=IC1
            IQ(LFUNC+2)=IC2
         ELSE
            IQ(LFUNC+1)=1
            IQ(LFUNC+2)=NCX
            IQ(LFUNC+3)=1
            IQ(LFUNC+4)=NCY
         ENDIF
*2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+
      ELSEIF(ICASE.GE.2)THEN
         IF(IQ(LCONT-2).EQ.0)GO TO 100
         LFUNC=LQ(LCONT-1)
         IF(LFUNC.EQ.0)GO TO 100
         IF(IQ(LFUNC-2).LT.1)GO TO 100
         IFITTY=100
         IF(FITNAM(1).EQ.'A0'.AND.FITNAM(NFPAR+1).EQ.'HFITPO')IFITTY=1
         IF(NFPAR.EQ.2.AND.FITNAM(3).EQ.'HFITEX')IFITTY=2
         IF(NFPAR.EQ.3.AND.FITNAM(4).EQ.'HFITGA')IFITTY=3
         IF(FITNAM(1).EQ.'MQuadric')IFITTY=4
* Calculate no. of words in LHFIT bank and book.
         IF(IFITTY.EQ.4)THEN
            IF(I1.NE.0)THEN
               NOTHER=2*NFPAR+5
            ELSE
               NOTHER=3*NFPAR+5
            ENDIF
            NWERR=0
         ELSE
            NOTHER=0
            NWERR=NFPAR
         ENDIF
         NWFIT=10+NWW*(NFPAR+NOTHER+NWERR)
         NTOT=NWFIT+10
         CALL HSPACE(NTOT,'HSUPIS',ID)
         IF(IERR.NE.0)GO TO 100
         CALL MZBOOK(IHDIV,LHFIT,LFUNC,-1,'HFIT',0,0,NWFIT,IOFIT,0)
         IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
         CALL SBIT1(IQ(LHFIT),5)
* Calculate no. of words in LHFCO bank and add to end of chain if space.
         LHFCO=0
         LHFNA=0
         IF(IFITTY.EQ.4)THEN
            NWCOV=NWW*NFPAR*(NFPAR+1)/2
            NTOT=NWCOV+10
            CALL HSPACE(NTOT,'HSUPIS',ID)
            IF(IERR.EQ.0)THEN
               L=LHFIT
   20          CONTINUE
               LR1=L
               L=LQ(L)
               IF(L.NE.0)GO TO 20
               CALL MZBOOK(IHDIV,LHFCO,LR1,0,'HFCO',0,0,NWCOV,4,-1)
               IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
            ENDIF
         ENDIF
* Calculate no. of words in LHFNA bank and add to end of chain if space.
         IF(IFITTY.GE.100)THEN
            NWNAM=2*NFPAR
            NTOT=NWNAM+10
            CALL HSPACE(NTOT,'HSUPIS',ID)
            IF(IERR.EQ.0)THEN
               L=LHFIT
   30          CONTINUE
               LR1=L
               L=LQ(L)
               IF(L.NE.0)GO TO 30
               CALL MZBOOK(IHDIV,LHFNA,LR1,0,'HFNA',0,0,NWNAM,5,-1)
               IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
            ENDIF
         ENDIF
* Fill banks.
         IQ(LHFIT+1)=IFITTY
         IQ(LHFIT+2)=NFPAR
         IQ(LHFIT+3)=NPFITS
         IQ(LHFIT+4)=NOTHER
         Q(LHFIT+6)=FITCHI
         IF(IFITTY.EQ.4)THEN
            CALL HQPUTF(LHFIT)
         ELSE
            II=11
            DO 40 I=1,NFPAR
*              Note: FITPAR is only single precision.
               SS=FITPAR(I)
               CALL UCOPY(SS,Q(LHFIT+II),NWW)
               II=II+NWW
   40       CONTINUE
            DO 50 I=1,NFPAR
*              Note: FITSIG is only single precision.
               SS=FITSIG(I)
               CALL UCOPY(SS,Q(LHFIT+II),NWW)
               II=II+NWW
   50       CONTINUE
* IF(LHFCO.NE.0)CALL MNEMAT and fill covariance bank.
            IF(LHFNA.NE.0)THEN
               DO 60 I=1,NFPAR
                  CALL UCTOH(FITNAM(I),Q(LHFNA+2*I-1),4,8)
   60          CONTINUE
            ENDIF
         ENDIF
* Now fill HFUN bank with function values at bin centres.
         IF(I1.NE.0)THEN
            DX=(Q(LPRX+2)-Q(LPRX+1))/FLOAT(IQ(LPRX))
            DO 70 I=1,NCX
               IF(I6.EQ.0)THEN
                  X=Q(LPRX+1)+0.5*DX+DX*(IC1+I-2)
               ELSE
                  LBINS=LQ(LCID-2)
                  X=0.5*(Q(LBINS+IC1+I-1)+Q(LBINS+IC1+I))
               ENDIF
               Q(LFUNC+I+2)=FUNC(V)
   70       CONTINUE
         ELSEIF(ICASE.EQ.3)THEN
*           MZPUSH HFUN bank if necessary.
            IF(I230.NE.0.AND.IQ(LFUNC-1).EQ.4)THEN
               NNOW=4+IQ(LFUNC-3)+10
               NMORE=NCX*NCY
               NNEW=NNOW+NMORE
               CALL HSPACE(NNOW+NNEW,'HSUPIS',ID)
               IF(IERR.NE.0)GO TO 100
               LFUNC=LQ(LCONT-1)
               CALL MZPUSH(IHDIV,LFUNC,0,NMORE,' ')
               IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NMORE
            ENDIF
            DX=(Q(LPRX+2)-Q(LPRX+1))/FLOAT(IQ(LPRX))
            DY=(Q(LPRY+2)-Q(LPRY+1))/FLOAT(IQ(LPRY))
            DO 90 IX=1,NCX
               X=Q(LPRX+1)+(IX-0.5)*DX
               DO 80 IY=1,NCY
                  Y=Q(LPRY+1)+(IY-0.5)*DY
                  I=(IY-1)*NCX+IX
                  Q(LFUNC+I+4)=FUNC(V)
   80          CONTINUE
   90       CONTINUE
         ENDIF
      ENDIF
*
  100 RETURN
      END
