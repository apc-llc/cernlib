*
* $Id: hsupin.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hsupin.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 15/12/92  21.52.04  by  Rene Brun
*-- Author :    John Allison   08/09/92
      SUBROUTINE HSUPIN (ICASE)
*.==========>
*.           Stores fit parameters for ntuples.
*.           ICASE  = 0, drop structure.
*.           ICASE  = 1, initialise structure.
*.           ICASE >= 2, store parameters.
*.
*.      I
*.      I LHFIT=LQ(LFUNC-1) and LFUNC=LQ(LCID-5) for ntuples.
*.      I
*.      V
*.  ********************************************
*.  * HFIT chain - see HSUPIS for description. *
*.  ********************************************
*.
*. Note: In the case of 1- and 2-D the LFUNC bank is used to store the
*           channel numbers of any histogram with which it is associated,
*           as, e.g., in PANDRA (which processes the NTUPLE/DRAW commmand in
*           PAW).  The first 4 words are reserved for this.   Otherwise the
*           bank is not used at the moment.
*.
*..=========> ( R.Brun, J.Allison)
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcform.inc"
#include "hbook/hcfits.inc"
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW=2)
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW=1)
#endif
*.___________________________________________
*
*000000000000000000000000000000000000000000000000000000000000000000000000
      CALL SBIT0(IQ(LCID),5)
      IF(ICASE.EQ.0)THEN
         IF(IQ(LCID-2).EQ.2)THEN
* Old ntuple.
            IF(IQ(LCONT-2).EQ.0)GO TO 20
            LFUNC=LQ(LCONT-1)
         ELSEIF(IQ(LCID-2).EQ.6)THEN
* New ntuple.
            LFUNC=LQ(LCID-5)
         ELSE
            GO TO 20
         ENDIF
         IF(LFUNC.EQ.0)GO TO 20
         CALL MZDROP(IHDIV,LFUNC,' ')
*11111111111111111111111111111111111111111111111111111111111111111111111
      ELSEIF(ICASE.EQ.1)THEN
         NTOT=21
         CALL HSPACE(NTOT,'HSUPIN',ID)
         IF(IERR.NE.0)GO TO 20
         IF(IQ(LCID-2).EQ.2)THEN
* Old ntuple.
            IF(IQ(LCONT-2).EQ.0)THEN
               NNOW=IQ(LCONT-1)+IQ(LCONT-3)+10
               NMORE=1
               NNEW=NNOW+NMORE
               CALL HSPACE(NNOW+NNEW,'HSUPIN',ID)
               IF(IERR.NE.0)GO TO 20
               CALL MZPUSH(IHDIV,LCONT,NMORE,0,' ')
            ENDIF
            CALL MZBOOK(IHDIV,LFUNC,LCONT,-1,'HFUN',1,1,10,2,0)
         ELSEIF(IQ(LCID-2).EQ.6)THEN
* New ntuple.
            CALL MZBOOK(IHDIV,LFUNC,LCID,-5,'HFUN',1,1,10,2,0)
         ELSE
            GO TO 20
         ENDIF
*2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+2+
      ELSEIF(ICASE.GE.2)THEN
* Book the LHFIT banks.
         IFITTY=100
         IF(FITNAM(1).EQ.'MQuadric')IFITTY=4
         IF(IFITTY.NE.4)GO TO 20
         NDIM=IQ(LCID+2)
         NOTHER=(NDIM+1)*NFPAR+5
         NWERR=0
         NWFIT=10+NWW*(NFPAR+NOTHER+NWERR)
         NTOT=NWFIT+10
         CALL HSPACE(NTOT,'HSUPIN',ID)
         IF(IERR.NE.0)GO TO 20
         IF(IQ(LCID-2).EQ.2)THEN
* Old ntuple.
            IF(IQ(LCONT-2).EQ.0)THEN
               NNOW=IQ(LCONT-1)+IQ(LCONT-3)+10
               NMORE=1
               NNEW=NNOW+NMORE
               CALL HSPACE(NNOW+NNEW,'HSUPIN',ID)
               IF(IERR.NE.0)GO TO 20
               CALL MZPUSH(IHDIV,LCONT,NMORE,0,' ')
            ENDIF
            LFUNC=LQ(LCONT-1)
         ELSEIF(IQ(LCID-2).EQ.6)THEN
* New ntuple.
            LFUNC=LQ(LCID-5)
         ELSE
            GO TO 20
         ENDIF
         IF(LFUNC.LE.0)GO TO 20
         CALL MZBOOK(IHDIV,LHFIT,LFUNC,-1,'HFIT',0,0,NWFIT,IOFIT,0)
         CALL SBIT1(IQ(LHFIT),5)
* Calculate no. of words in LFCOV bank and add to end of chain if space.
         LFCOV=0
         NWCOV=NWW*NFPAR*(NFPAR+1)/2
         NTOT=NWCOV+10
         CALL HSPACE(NTOT,'HSUPIN',ID)
         IF(IERR.EQ.0)THEN
            L=LHFIT
   10       CONTINUE
            LR1=L
            L=LQ(L)
            IF(L.NE.0)GO TO 10
            CALL MZBOOK(IHDIV,LFCOV,LR1,0,'HFCO',0,0,NWCOV,4,-1)
         ENDIF
* Fill LHFIT banks.
         IQ(LHFIT+1)=IFITTY
         IQ(LHFIT+2)=NFPAR
         IQ(LHFIT+3)=NPFITS
         IQ(LHFIT+4)=NOTHER
         Q(LHFIT+6)=FITCHI
         CALL HQPUTF(LHFIT)
      ENDIF
*
   20 RETURN
      END
