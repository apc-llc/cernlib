*
* $Id: hfcn.F,v 1.3 1997/09/02 13:09:02 mclareni Exp $
*
* $Log: hfcn.F,v $
* Revision 1.3  1997/09/02 13:09:02  mclareni
* WINNT correction
*
* Revision 1.2  1997/03/14 17:04:22  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:27:58  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HFCN (NPAR,GRAD,F,U,IFLAG,UFCN)
*.==========>
*.       COMPUTES CHI2 OF FIT INTO F
*.       IF USERS' FORCE, COMPUTE GRADIENT
*..=========> ( E.Lessner,D.Lienart )
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION UFCN,F,FU,GRAD,U,DQ,FUSUM,FSUM
#endif
      DIMENSION EXDA(20),DQ(2),GRAD(*),U(*)
#include "hbook/hcfit2.inc"
#include "hbook/hcfit3.inc"
#include "hbook/hcfit6.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
      EQUIVALENCE (Q(1),DQ(1))
      EXTERNAL UFCN
      LOGICAL UGIN
      DATA NAMPO,NAMGA,NAMEX/2HPO,2HGA,2HEX/
*.___________________________________________
#if !defined(CERNLIB_DOUBLE)
      CALL UCOPY(U,XVAR(IOFSET+1),NUP)
#endif
#if defined(CERNLIB_DOUBLE)
      CALL UCOPY(U,XVAR(IOFSET+1),2*NUP)
#endif
      UGIN=.FALSE.
      IF (NAMFUN.NE.NAMPO.OR.NAMFUN.NE.NAMGA.OR.NAMFUN.NE.NAMEX)
     +   UGIN=.TRUE.
*
      NPFIT=0
      F=0.
*
*     INITIALIZE THE SUMS:
      CUSUM=0.
      EUSUM=0.
      FUSUM=0.
*
      IF (IFLAG.EQ.2) THEN
         DO 3 I=1,NUP
            GRAD(I)=0.
            DQ(IDESUM+I-1)=0.
    3    CONTINUE
      ENDIF
*
      IF (IHIS.EQ.1) THEN
         DO 30 L1=1,NUMEP
            CALL HEXDA1 (EXDA,L1,IFLRET)
            DO 10 I=1,IDIMPN-2
               XVAR(I)=EXDA(I+2)
   10       CONTINUE
            CU=EXDA(1)
            EU=EXDA(2)
            IF(EU.LE.0.)GO TO 30
            IF (IDIMPN.EQ.4.AND.IFLSF.EQ.0) THEN
               FU=UFCN(XVAR(1),XVAR(2))
               IF (IFLAG.EQ.2)THEN
                  CALL HDERI2(ID,XVAR(1),XVAR(2),U,DQ(IDERIV))
               ENDIF
            ELSE
#if defined(CERNLIB_MACMPW) || defined(CERNLIB_MSSTDCALL)
               FU=UFCN(XVAR,0.)
#else
               FU=UFCN(XVAR)
#endif
               IF (IFLAG.EQ.2) THEN
                  IF (UGIN) CALL HDERI1(ID,XVAR,U,DQ(IDERIV))
               ENDIF
            END IF
            IF (IFLAG.EQ.2) THEN
               DO 15 K=1,NUP
                  DQ(IDESUM+K-1)=DQ(IDESUM+K-1)+DQ(IDERIV+K-1)
   15          CONTINUE
            ENDIF
            EU=EU*EU
            FUSUM=FUSUM+FU
            CUSUM=CUSUM+CU
            EUSUM=EUSUM+EU
            TEST=CUSUM*CUSUM/EUSUM
            IF (L1.EQ.NUMEP.OR.IWEIGH.NE.0.OR.TEST.GE.0.) THEN
               NPFIT=NPFIT+1
               IF (IFLAG.EQ.2) THEN
                  DO 20 K=1,NUP
                     GRAD(K)=GRAD(K)+2*DQ(IDESUM+K-1)*(FUSUM-CUSUM)/
     +               EUSUM
                     DQ(IDESUM+K-1)=0.
   20             CONTINUE
               ENDIF
               FSUM=(CUSUM-FUSUM)**2/EUSUM
               F=F+FSUM
               CUSUM=0.
               EUSUM=0.
               FUSUM=0.
            ENDIF
   30    CONTINUE
      ELSE
         DO 60 L1=1,NUMEP
            CU=Q(ILYE+L1-1)
            DO 40 I=1,NX
               XVAR(I)=Q(ILXE+L1-1+NUMEP*(I-1))
   40       CONTINUE
            EU=Q(ILEY+L1-1)
            IF(EU.LE.0.)GO TO 60
#if defined(CERNLIB_MACMPW) || defined(CERNLIB_MSSTDCALL)
            FU=UFCN(XVAR,0.)
#else
            FU=UFCN(XVAR)
#endif
            IF (IFLAG.EQ.2) THEN
               CALL HDERIN(XVAR,U,DQ(IDERIV))
               DO 45 K=1,NUP
                  DQ(IDESUM+K-1)=DQ(IDESUM+K-1)+DQ(IDERIV+K-1)
   45          CONTINUE
            ENDIF
            EU=EU*EU
            FUSUM=FUSUM+FU
            CUSUM=CUSUM+CU
            EUSUM=EUSUM+EU
            TEST=CUSUM*CUSUM/EUSUM
            IF (L1.EQ.NUMEP.OR.IWEIGH.NE.0.OR.TEST.GE.0.) THEN
               NPFIT=NPFIT+1
               IF (IFLAG.EQ.2) THEN
                  DO 50 K=1,NUP
                     GRAD(K)=GRAD(K)+2*DQ(IDESUM+K-1)*(FUSUM-CUSUM)/
     +               EUSUM
                     DQ(IDESUM+K-1)=0.
   50             CONTINUE
               ENDIF
               FSUM=(CUSUM-FUSUM)**2/EUSUM
               F=F+FSUM
               CUSUM=0.
               EUSUM=0.
               FUSUM=0.
            ENDIF
   60    CONTINUE
      ENDIF
      END
