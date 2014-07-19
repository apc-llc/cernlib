*
* $Id: geafit.F,v 1.1.1.1 1996/03/06 15:36:26 mclareni Exp $
*
* $Log: geafit.F,v $
* Revision 1.1.1.1  1996/03/06 15:36:26  mclareni
* Add geane321 examples
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.50  by  S.Giani
*-- Author :
      SUBROUTINE GEAFIT
*
*    stearing routine for trak fitting using GEANE
*
*
*
#include "geant321/gcflag.inc"
*
*
      PARAMETER (NPLAN = 8, NSTOP = NPLAN-1, NSTOP5 = 5*NPLAN )
      COMMON /GCFIT/
     +          CSIX(6,0:NSTOP),CSIY(6,0:NSTOP),CSIZ(6,0:NSTOP)
     +         ,WW(5,5,0:NSTOP),DDT(5,5,0:NSTOP)
     +         ,VV(5,5,0:NSTOP),SS(5,5,0:NSTOP)
     +         ,CHI2,CHI2N,CHI2T,CHI2M
     +         ,PLANI(3,4,0:NSTOP)
      DOUBLE PRECISION
     +          CSIX           ,CSIY           ,CSIZ
     +         ,WW             ,DDT
     +         ,VV             ,SS
     +         ,CHI2,CHI2N,CHI2T,CHI2M
*
*     CSIZ            measured values
*     CSIY            predicted values
*     CSIX            fitted  values
*
*     VV              Weight on CSIZ
*     WW              tracking Weight
*     SS              fit covariance
*
*     DDT             trasport matrix (transpose)
*
*     CHI2            first chi2
*     CHI2N           estimated new chi2
*     CHI2T           estimated new chi2 due to tracking
*     CHI2M           estimated new chi2 due to measurements
*
      COMMON /COORD/CTRU(6,10),NMEA(10),charg
*
      REAL XMEA(3,10)
*
      double precision ssi(5,5),dd(5),chi2f,chi2ft
*
      Parameter (NDAT = 8*(6+5+5+1)+1)
      REAL  XNTP(NDAT)
*
      save ifail
      data ifail/0/
*
      data sigmay/0.02/
      data sigmaz/0.05/
cc      data sigmay/0.07/
cc      data sigmaz/0.10/
*
      DATA FIELDM/20.0/
*
*_____________________________________________________________________
*
*
      if (ifail.ne.0)  then
         print *,' previous fit failed '
         idebug=2
         CALL GPRINT('VERT',0)
         CALL GPRINT('KINE',0)
         CALL GPRINT('JXYZ',0)
      endif
*
*      start with simulating a position measurement in y and z
*      with resolution sigmay,sigmaz
*
      DO IPLAN = 1,NPLAN
        IF (NMEA(IPLAN).GT.0) THEN
          CTRU(1,IPLAN) = CTRU(1,IPLAN)/NMEA(IPLAN)
          CTRU(2,IPLAN) = CTRU(2,IPLAN)/NMEA(IPLAN)
          CTRU(3,IPLAN) = CTRU(3,IPLAN)/NMEA(IPLAN)
          CTRU(4,IPLAN) = CTRU(4,IPLAN)/NMEA(IPLAN)
          CTRU(5,IPLAN) = CTRU(5,IPLAN)/NMEA(IPLAN)
          CTRU(6,IPLAN) = CTRU(6,IPLAN)/NMEA(IPLAN)
          XMEA(1,IPLAN) = CTRU(1,IPLAN)
          call rannor(ry,rz)
          XMEA(2,IPLAN) = CTRU(2,IPLAN) + ry*sigmay
          XMEA(3,IPLAN) = CTRU(3,IPLAN) + rz*sigmaz
        ELSE
          PRINT *, 'no measurement at plane ',Iplan
          GO TO 999
        ENDIF
      ENDDO
      IF (IDEBUG.GE.1) THEN
        PRINT *,' GEAFIT,XMEA'
        WRITE(6,'(I4,3E15.5)') (I,(XMEA(J,I),J=1,3),I=1,8)
      ENDIF
*
**    tracking versus   0 = FORWARD 1 = BACKWARD
*
      IVER = MOD(ISWIT(3) - 15,2)
*
**     compute a starting point trajectory parameters
*
*             get helix crossing three points
*     circle is parameterized as:
*    C*[(X-Xp)**2+(Y-Yp)**2] - 2*alpha*(X-Xp) - 2*beta*(Y-Yp) = 0
*    Xp,Yp is a point on the track;
*    C = 1/r0 is the curvature  ( sign of C is charge of particle );
*   alpha & beta are the direction cosines of the radial vector at Xp,Yp
*    i.e.  alpha = C*(X0-Xp),
*          beta  = C*(Y0-Yp),
*    where center of circle is at X0,Y0.
*    Slope dy/dx of tangent at Xp,Yp is -alpha/beta.
*
      X1P = XMEA(1,1)-XMEA(1,4)
      Y1P = XMEA(2,1)-XMEA(2,4)
      D12 = X1P**2 + Y1P**2
      X3P = XMEA(1,8)-XMEA(1,4)
      Y3P = XMEA(2,8)-XMEA(2,4)
      D32 = X3P**2 + Y3P**2
      DET = D12*Y3P-D32*Y1P
      TOP = (X1P*Y3P-Y1P*X3P)         ! top also gives correct sign for CT
      CT  = TOP/DET
      SN  = SIGN(1.,TOP*CT)
      ST2 = (D12*X3P-D32*X1P)/DET
      SEQ = 1.+ST2**2
      AL2 = SN/SQRT(SEQ)
      BE2 = -ST2*AL2
      CT  = 2.*CT*AL2
      ST1 = -(BE2-CT*Y1P)/(AL2-CT*X1P)
      ST3 = -(BE2-CT*Y3P)/(AL2-CT*X3P)
*
      IF ( IVER.EQ.0) THEN
        dydx = 1./ST1
      ELSE
        dydx = 1./ST3
      ENDIF
      dzdx = (xmea(3,8)-xmea(3,1))/(xmea(1,8)-xmea(1,1))
      OVERP = CT/(0.29979251E-3*FIELDM*SQRT(1.+(dzdx*BE2)**2))
*
*
**    start to load parameters
*
      IF (IVER.EQ.0) THEN
        I1 = 1
        INC = 1
      ELSE
        I1 = NPLAN
        INC = -1
      ENDIF
      CALL VZERO(VV,50*NPLAN)
      CALL VZERO(CSIZ,12*NPLAN)
      CALL VZERO(PLANI,6)
      PLANI(2,1,0) =  1.
      PLANI(3,2,0) =  1.
      CALL CROSS(PLANI(1,1,0),PLANI(1,2,0),PLANI(1,4,0))
      IC = I1
      DO IP=0,NSTOP
        IF (IP.GT.0) CALL UCOPY(PLANI(1,1,0),PLANI(1,1,IP),12)
        PLANI(1,3,IP) = XMEA(1,IC)
        PLANI(2,3,IP) = XMEA(2,IC)
        PLANI(3,3,IP) = XMEA(3,IC)
        CSIZ(4,IP) = XMEA(2,IC)
        CSIZ(5,IP) = XMEA(3,IC)
        CSIZ(6,IP) = XMEA(1,IC)
        VV(4,4,IP) = 1.d0/sigmay**2
        VV(5,5,IP) = 1.d0/sigmaz**2
        IC = IC + INC
      ENDDO
*         initial parameters
      CSIX(1,0) = OVERP
      CSIX(2,0) = dydx
      CSIX(3,0) = dzdx
      CSIX(4,0) = XMEA(2,I1)
      CSIX(5,0) = XMEA(3,I1)
      CSIX(6,0) = XMEA(1,I1)
CC      CSIX(4,0) =
CC     +    PLANI(1,1)*XMEA(1,I1)+PLANI(2,1)*XMEA(2,I1)+PLANI(3,1)*XMEA(3,I1)
CC      CSIX(5,0) =
CC     +    PLANI(1,2)*XMEA(1,I1)+PLANI(2,2)*XMEA(2,I1)+PLANI(3,2)*XMEA(3,I1)
CC      CSIX(6,0) =
CC     +    PLANI(1,4)*XMEA(1,I1)+PLANI(2,4)*XMEA(2,I1)+PLANI(3,4)*XMEA(3,I1)
      CALL VZERO(DDT(1,1,0),50)
      CALL VZERO(WW(1,1,0),50)
      DO I=1,5
        DDT(I,I,0) = 1.D0
      ENDDO
      print *,'fitting starting point ',csix(6,0),csix(4,0),csix(5,0)
     +                                 ,csix(1,0),csix(2,0),csix(3,0)
      print *,'starting momentum ',1./csix(1,0)
*
**     call the chosen fitting algorithm
*
      if (ISWIT(3).LE.16) THEN
        CALL GFITMI(iver,ifail)
      elseif (ISWIT(3).LE.18) THEN
        CALL GFITKF(iver,ifail)
      endif
      print *,'fitpoint ',csix(6,0),csix(4,0),csix(5,0)
     +                                 ,csix(1,0),csix(2,0),csix(3,0)
      print *,'fit momentum ',1./csix(1,0)
      if (ifail.ne.0) then
        print *,' Fit failed ',ifail
        go to 999
      endif
*
**    fill histos
*
      p = charg/sqrt(ctru(4,1)**2+ctru(5,1)**2+ctru(6,1)**2)
      d = (csix(1,I1-1)-p)/p
      call hfill(1,d,0.,1.)
      p = ctru(5,1)/ctru(4,1)
      d = csix(2,I1-1)-p
      call hfill(2,d,0.,1.)
      p = ctru(6,1)/ctru(4,1)
      d = csix(3,I1-1)-p
      call hfill(3,d,0.,1.)
      d = csix(4,I1-1)-ctru(2,1)
      call hfill(4,d,0.,1.)
      d = csix(5,I1-1)-ctru(3,1)
      call hfill(5,d,0.,1.)
      IC = I1
      chi2ft = 0.d0
      intp = 0
      do ip=0,nstop
c         print *,' '
C        write(6,'(1X,6E15.5)') (ctru(i,IC),i=1,6)
C        write(6,'(1X,6E15.5)') (csix(i,ip),i=1,6)
C       write(6,'(1X,5E15.5)') (sqrt(ss(i,i,ip)),i=1,5)
        call ucopy(ctru(1,ic),xntp(intp+1),3)
        p = charg/sqrt(ctru(4,IC)**2+ctru(5,IC)**2+ctru(6,IC)**2)
        d = charg*(csix(1,ip)-p)/sqrt(ss(1,1,ip))
        dd(1) = csix(1,ip)-p
        call hfill(1+10*(IC),d,0.,1.)
        xntp(intp+4) = p
        xntp(intp+7) = csix(1,ip)
        xntp(intp+7+5) = sqrt(ss(1,1,ip))
        p = ctru(5,IC)/ctru(4,IC)
        d = (csix(2,ip)-p)/sqrt(ss(2,2,ip))
        dd(2) = csix(2,ip)-p
        xntp(intp+5) = p
        xntp(intp+8) = csix(2,ip)
        xntp(intp+8+5) = sqrt(ss(2,2,ip))
        call hfill(2+10*(IC),d,0.,1.)
        p = ctru(6,IC)/ctru(4,IC)
        d = (csix(3,ip)-p)/sqrt(ss(3,3,ip))
        dd(3) = csix(3,ip)-p
        call hfill(3+10*(IC),d,0.,1.)
        xntp(intp+6) = p
        xntp(intp+9) = csix(3,ip)
        xntp(intp+9+5) = sqrt(ss(3,3,ip))
        d = (csix(4,ip)-ctru(2,IC))/sqrt(ss(4,4,ip))
        dd(4) = csix(4,ip)-ctru(2,IC)
        call hfill(4+10*(IC),d,0.,1.)
        d = (csix(4,ip)-csiz(4,ip))/
     +     sqrt(max(sigmay**2-ss(4,4,ip),1.e-18))
        call hfill(6+10*(IC),d,0.,1.)
        xntp(intp+10) = csix(4,ip)
        xntp(intp+10+5) = sqrt(ss(4,4,ip))
        d = (csix(5,ip)-ctru(3,IC))/sqrt(ss(5,5,ip))
        dd(5) = csix(5,ip)-ctru(3,IC)
        call hfill(5+10*(IC),d,0.,1.)
        d = (csix(5,ip)-csiz(5,ip))/
     +     sqrt(max(sigmaz**2-ss(5,5,ip),1.e-18))
        call hfill(7+10*(IC),d,0.,1.)
        xntp(intp+11) = csix(5,ip)
        xntp(intp+11+5) = sqrt(ss(5,5,ip))
        do i=1,5
          call hfill(1010+i,float(ic)-.5,sngl(sqrt(ss(i,i,ip))),1.)
        enddo
        call ucopy(ss(1,1,ip),ssi,50)
        call dsinv(5,ssi,5,ierr)
        chi2f = 0.d0
        do i=1,5
        do j=1,5
          chi2f = chi2f + dd(i)*ssi(i,j)*dd(j)
        enddo
        enddo
        chi2ft = chi2ft + chi2f
        call hfill(10*ic,prob(sngl(chi2f),5),0.,1.)
        xntp(intp+17) = prob(sngl(chi2f),5)
        IC = IC + INC
        intp = intp + 17
       enddo
       call hfill(1000,prob(sngl(chi2ft),5*NPLAN),0.,1.)
        xntp(intp+1) = prob(sngl(chi2ft),5*NPLAN)
      call hfn(999,xntp)
*
999   CONTINUE
*
      CALL VZERO(CTRU,70)
*
      END
