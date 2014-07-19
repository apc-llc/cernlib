*
* $Id: hprst.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hprst.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.19.47  by  Rene Brun
*-- Author :
      SUBROUTINE HPRST(ALL,ISIGNE,ISIGNF,EXP1,EXP2,EXPL1,EXPL2,CHI)
*.==========>
*.              THIS ROUTINE PRINTS STATISTICS
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcprin.inc"
      INTEGER EXP1,EXP2,EXPL1,EXPL2
      DOUBLE PRECISION SWX(2),XVALM,XRMS,VALMEA
*.___________________________________________
*
*             PRINTING OF SCALE FACTOR IF ANY
*
      IF(EXP2.NE.IDG(41))THEN
         WRITE(LOUT,1000)ISIGNE,EXP1,EXP2
      ENDIF
      IF(EXPL2.NE.IDG(41))THEN
         WRITE(LOUT,2000)ISIGNF,EXPL1,EXPL2
      ENDIF
*
*             CALCULATION OF STATISTICS VALUES
*
      UNDER=HCX(0,1)
      OVER=HCX(ICN+1,1)
      BWID=(Q(LPRX+2)-Q(LPRX+1))/FLOAT(IQ(LPRX))
      X=Q(LPRX+1)-0.5*BWID
*
      IF(I7.NE.0)THEN
         ALLCHA=Q(LCONT+KSTAT1)
         CALL UCOPY(Q(LCONT+KSTAT1+2),SWX,4)
         XVALM=SWX(1)
         VALMEA=0.
         RMS=0.
         EQUIV=0.
         IF(ALLCHA.NE.0.)THEN
            VALMEA=XVALM/ALLCHA
            RMS=SQRT(ABS(SWX(2)/ALLCHA - (SWX(1)/ALLCHA)**2))
            EQN=Q(LCONT+KSTAT1+1)
            IF(EQN.NE.0.)EQUIV=ALLCHA*ALLCHA/EQN
         ENDIF
         WRITE(LOUT,4000)IQ(LCONT+KNOENT),ALL,UNDER,OVER,BWID
     +     ,VALMEA,RMS,EQUIV
      ELSE
         ALLCHA=0.
         XVALM=0.
         XRMS=0.
         VALMEA=0.
         RMS=0.
         DO 10 J=1,ICN
            IF(I6.EQ.0)THEN
               X=X+BWID
            ELSE
               LBINS=LQ(LCID-2)
               X=0.5*(Q(LBINS+J)+Q(LBINS+J+1))
            ENDIF
            W=ABS(HCX(J,1))
            ALLCHA=ALLCHA+W
            XVALM=XVALM+W*X
            XRMS=XRMS+W*X*X
  10     CONTINUE
         IF(ALLCHA.NE.0.)THEN
            VALMEA=XVALM/ALLCHA
            RMS=XRMS/ALLCHA-VALMEA*VALMEA
            RMS=SQRT(ABS(RMS))
         ENDIF
         WRITE(LOUT,3000)IQ(LCONT+KNOENT),ALL,UNDER,OVER,BWID
     +     ,VALMEA,RMS
      ENDIF
*
      IF(CHI.LT.0.)GO TO 999
      WRITE(LOUT,5000)CHI
*
 1000 FORMAT(' * CONTENTS, ETC ARE MULTIPLIED BY 10**',3A1)
 2000 FORMAT(' * LOW EDGE IS MULTIPLIED BY 10**',3A1)
 3000 FORMAT(' * ENTRIES =',I11,6X,'* ALL CHANNELS =',E11.4,6X,
     +'* UNDERFLOW =',E11.4,6X,'* OVERFLOW =',E11.4,/,
     +' * BIN WID =',E11.4,6X,'* MEAN VALUE   =',E11.4,6X,
     +'* R . M . S =',E11.4)
 4000 FORMAT(' * ENTRIES =',I11,6X,'* ALL CHANNELS =',E11.4,6X,
     +'* UNDERFLOW =',E11.4,6X,'* OVERFLOW =',E11.4,/,
     +' * BIN WID =',E11.4,6X,'* MEAN VALUE   =',E11.4,6X,
     +'* R . M . S =',E11.4,6X,'* NEQUIVAL =',E11.4)
 5000 FORMAT(' * CHISQUAR  = ',E11.4)
 999  RETURN
      END
