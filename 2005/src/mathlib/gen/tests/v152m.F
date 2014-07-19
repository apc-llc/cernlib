      SUBROUTINE V152M
C
      CHARACTER*(*) CODE
      PARAMETER (CODE='V152')
C
C     This Subroutine tests the MATHLIB routine V152  FUNLUX and FUNLXP
C
C     PROGRAM FLTST
      DIMENSION XF(200)
      DIMENSION XR(10)
      EXTERNAL FCNSP,TRUINT
      COMMON /fltest/ncalls
      COMMON/CAUGOB/POS(10),WID(10),NRES
      COMMON/FUNINT/XUNI
      COMMON/V152T/ktest

      CALL HEADER(CODE,1)

       ncalls = 0
C                   sum of Breit-Wigners at POS with WID
      WRITE (6,'(//A)') '1  sum of 5 Breit-Wigners at POS with WID  '
      NRES = 5
      WID(1) = .1
      POS(1) = 1.
      WID(2) = 2.
      POS(2) = 5.
      WID(3) = 1.
      POS(3) = 7. 
      WID(4) = .1
      POS(4) = 8. 
      WID(5) = .051
      POS(5) = 9.
      ALIM = -2.
      BLIM = 20. 
C
      call RLUXGO(0,0,0,0)
      do 500 ktest= 1,3
      print *, ' TEST NUMBER',ktest
      n1 = ncalls
      CALL FUNLXP(FCNSP,XF,  ALIM,BLIM)
      n2 = ncalls
      nnew = n2 - n1
C
      TOTINT = TRUINT(ALIM,BLIM)
      print *, '  True integral from',alim,' to',blim,' is',totint
      CHISQ1 = 0.
      NBINS = 99
      HOPE = 1.0/REAL(NBINS)
      DO 100 IBIN= 1, NBINS
      TRU1 = TRUINT(XF(IBIN),XF(IBIN+1))/TOTINT
      DIFF1 = TRU1-HOPE
      CHISQ1 = CHISQ1 + DIFF1**2

  100 CONTINUE
      print *,' Chisq=',chisq1,'     Calls=',nnew
C
C         now test the actual generation (transformation) of r.n.'s
      NN = 10000
      CHISQ2 = 0.
      DO 300 IN= 1, NN
      CALL FUNLUX(XF,XR,1)
      DR = TRUINT(ALIM,XR)/TOTINT - XUNI 
      CHISQ2 = CHISQ2 + DR**2
      if (abs(dr) .gt. 0.01) print *, ' FUNLUX weak for UNI=',xuni,dr
  300 CONTINUE
      print *, ' NN=',nn,'     Chisq =',chisq2
 500  CONTINUE
C     STOP
      IRC= ITEST(CODE,.TRUE.)
      CALL PAGEND(CODE)
      RETURN
      END
      FUNCTION TRUINT(XLO,XHI)
      COMMON/CAUGOB/POS(10),WID(10),NRES
      COMMON/V152T/ktest
      IF (ktest .eq. 1)  then
      ERVAL = 0.
         DO 660 I= 1, NRES
         GAMI = 1.0/WID(I)
         ERVAL = ERVAL +  GAMI*(ATAN(GAMI*(XHI-POS(I))) -
     +                          ATAN(GAMI*(XLO-POS(I))) )      
  660    CONTINUE
      TRUINT = ERVAL
      elseIF (ktest .eq. 2)  then
         truint = exp(-xlo) - exp(-xhi)
      else 
         truint = xhi-xlo - 0.5* (cos(2.*xhi) - cos(2.*xlo))
      endif
      END
      FUNCTION FCNSP(X)
      COMMON/CAUGOB/POS(10),WID(10),NRES
      COMMON /fltest/ncalls
      COMMON/V152T/ktest
      ncalls = ncalls + 1
      if (ktest .eq. 1)  then
      GOBCAU = 0.
         DO 25 I= 1, NRES
         GOBCAU = GOBCAU + 1.0/(WID(I)**2+(X-POS(I))**2)
   25    CONTINUE
      FCNSP = GOBCAU
      else if (ktest .eq. 2)  then
        FCNSP = EXP(-X)
      else
        fcnsp = 1.0 + sin(2.0*x)
      endif
      RETURN
      END
