*
* $Id: hsupim.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hsupim.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.21.02  by  Rene Brun
*-- Author :
      SUBROUTINE HSUPIM (FUNC)
*.==========>
*.           cette routine stocke la valeur de la fonction
*.           FUNC pour un unidimensionnel
*..=========> ( R.Brun ,I.Ivanchenko, D.Lienart)
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcform.inc"
#include "hbook/hcfitr.inc"
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION FUNC
#endif
      EXTERNAL FUNC
*.___________________________________________
*
      IF(IFTRNG.NE.0)THEN
         NCX=IFXUP-IFXLOW+1
      ELSE
         NCX=IQ(LPRX)
      ENDIF
      IF(NV.EQ.1)THEN
         IF(LQ(LCONT-1).EQ.0)THEN
            NTOT=NCX+12
            CALL HSPACE(NTOT,'HSUPIM',ID)
            IF(IERR.NE.0)GO TO 99
            CALL MZBOOK(IHDIV,LFUNC,LCONT,-1,'HFUN',0,0,NCX+2,IOCF2,-1)
            CALL HSIFLA(12,1)
            IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
         ELSE
            LFUNC=LQ(LCONT-1)
            NMORE=NCX+2-IQ(LFUNC-1)
            IF(NMORE.NE.0)THEN
               LR1=LFUNC
               CALL MZPUSH(IHDIV,LR1,0,NMORE,' ')
               IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NMORE
            ENDIF
         ENDIF
         LFUNC=LQ(LCONT-1)
         IF(IFTRNG.NE.0)THEN
            IQ(LFUNC+1)=IFXLOW
            IQ(LFUNC+2)=IFXUP
         ELSE
            IQ(LFUNC+1)=1
            IQ(LFUNC+2)=NCX
         ENDIF
      ELSE
         LFUNC=LQ(LCONT-1)
         DX=(Q(LPRX+2)-Q(LPRX+1))/FLOAT(IQ(LPRX))
         X=Q(LPRX+1)-0.5*DX
         IF(IFTRNG.NE.0)X=X+DX*(IFXLOW-1)
         DO 40 I=1,NCX
            X=X+DX
            Q(LFUNC+I+2)=FUNC(X)
  40     CONTINUE
      ENDIF
  99  NV=2
      END
