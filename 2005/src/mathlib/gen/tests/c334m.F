*
* $Id: c334m.F,v 1.1.1.1 1996/04/01 15:01:17 mclareni Exp $
*
* $Log: c334m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:17  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C334M
C     Subroutine to test the operation of the MATHLIB routines GAPNC,
C     DGAPNC,GAGNC,DGAGNC (C334)
      PARAMETER ( NUMBA=9,NUMBX=3 )
#include "gen/def64.inc"
     +   DGAPNC,DGAGNC
#include "gen/def64.inc"
     +   A(NUMBA),X(NUMBX), EXACT(NUMBA,NUMBX,2),SOL(NUMBA,NUMBX,2),
c    +   ERROR(NUMBA,NUMBX,2),ERRMAX(2)
     +                        ERRMAX(2)
      REAL GAPNC, GAGNC, SSOL(NUMBA,NUMBX,2),SERRMAX(2)

C     Set numerical tolerance for testing the S/D versions
      DIMENSION TOL(2)
C
      LOGICAL LTEST

#include "iorc.inc"

      DATA TOL / 5D-5,5D-13 /
C     Set up the test parameters
      DATA (A(J),J=1,NUMBA) / -2.5D0, -1D0, -0.5D0, -0.25D0, 0D0,
     +                        0.25D0,0.5D0, 1D0, 5D0 /
      DATA (X(J),J=1,NUMBX) / 0.2D0, 1D0, 3D0 /
C     Set up the exact values of GAPNC,GAGNC, first index points to A
C     variable, second to X, and third to which routine being tested
C     ******Exact values of functions for X=0.25
      DATA (EXACT(1,1,J),J=1,2) / 0.1829014413075560D+2,
     +                            0.3571134138333013D+0 /
      DATA (EXACT(2,1,J),J=1,2) / 0.1000000000000000D+1,
     +                            0.7013302506135521D+0 /
      DATA (EXACT(3,1,J),J=1,2) / 0.1505793838069019D+1,
     +                            0.9793825468719957D+0 /
      DATA (EXACT(4,1,J),J=1,2) / 0.1298285786634679D+1,
     +                            0.1194243102830858D+1 /
      DATA (EXACT(5,1,J),J=1,2) / 0.1000000000000000D+1,
     +                            0.1493348746932240D+1 /
      DATA (EXACT(6,1,J),J=1,2) / 0.7098510317369823D+0,
     +                            0.2901489682630177D+0 /
      DATA (EXACT(7,1,J),J=1,2) / 0.4729107431344624D+0,
     +                            0.5270892568655376D+0 /
      DATA (EXACT(8,1,J),J=1,2) / 0.1812692469220180D+0,
     +                            0.8187307530779820D+0 /
      DATA (EXACT(9,1,J),J=1,2) / 0.2258190552957787D-5,
     +                            0.9999977418094470D+0 /
C     ******Exact values of functions for X=1
      DATA (EXACT(1,2,J),J=1,2) / 0.1102142978837587D+1,
     +                            0.2624681833913003D+0 /
      DATA (EXACT(2,2,J),J=1,2) / 0.1000000000000000D+1,
     +                            0.4036526376768062D+0 /
      DATA (EXACT(3,2,J),J=1,2) / 0.1050254541660012D+1,
     +                            0.4842556877173750D+0 /
      DATA (EXACT(4,2,J),J=1,2) / 0.1040187658214276D+1,
     +                            0.5354648517668633D+0 /
      DATA (EXACT(5,2,J),J=1,2) / 0.1000000000000000D+1,
     +                            0.5963473623231938D+0 /
      DATA (EXACT(6,2,J),J=1,2) / 0.9320788679898912D+0,
     +                            0.6792113201010884D-1 /
      DATA (EXACT(7,2,J),J=1,2) / 0.8427007929497167D+0,
     +                            0.1572992070502833D+0 /
      DATA (EXACT(8,2,J),J=1,2) / 0.6321205588285577D+0,
     +                            0.3678794411714423D+0 /
      DATA (EXACT(9,2,J),J=1,2) / 0.3659846827343659D-2,
     +                            0.9963401531726564D+0 /
C     ******Exact values of functions for X=3
      DATA (EXACT(1,3,J),J=1,2) / 0.1000560063415294D+1,
     +                            0.1657667608308393D+0 /
      DATA (EXACT(2,3,J),J=1,2) / 0.1000000000000000D+1,
     +                            0.2137487792340328D+0 /
      DATA (EXACT(3,3,J),J=1,2) / 0.1001911512674451D+1,
     +                            0.2357361503461776D+0 /
      DATA (EXACT(4,3,J),J=1,2) / 0.1001916291254332D+1,
     +                            0.2482956229704183D+0 /
      DATA (EXACT(5,3,J),J=1,2) / 0.1000000000000000D+1,
     +                            0.2620837402553086D+0 /
      DATA (EXACT(6,3,J),J=1,2) / 0.9949891040512919D+0,
     +                            0.5010895948708086D-2 /
      DATA (EXACT(7,3,J),J=1,2) / 0.9856941215645710D+0,
     +                            0.1430587843542899D-1 /
      DATA (EXACT(8,3,J),J=1,2) / 0.9502129316321361D+0,
     +                            0.4978706836786394D-1 /
      DATA (EXACT(9,3,J),J=1,2) / 0.1847367554762251D+0,
     +                            0.8152632445237749D+0 /

      CALL HEADER('C334',0)
      LTEST=.TRUE.
C
C---  Number of functions to test
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
C
C     Initialise largest errors, preset to zero
      ERRMAX(1)=0D0
      ERRMAX(2)=0D0
      SERRMAX(1)=0E0
      SERRMAX(2)=0E0
      DO 500 JF=1,NF
      DO 300 K=1,2
#if defined(CERNLIB_DOUBLE)
         IF(JF.EQ.1) THEN
         IF (K .EQ. 1) WRITE(LOUT,'(/5X,''a'',6X,''x'',12X,
     +    '' GAPNC(a,x)'',16X,''P(a,x)'',6X,'' Error'')')
         IF (K .EQ. 2) WRITE(LOUT,'(/5X,''a'',6X,''x'',12X,
     +    '' GAGNC(a,x)'',16X,''G(a,x)'',6X,'' Error'')')
         ENDIF
         IF(JF.EQ.2) THEN
         IF (K .EQ. 1) WRITE(LOUT,'(/5X,''a'',6X,''x'',12X,
     +    ''DGAPNC(a,x)'',16X,''P(a,x)'',6X,'' Error'')')
         IF (K .EQ. 2) WRITE(LOUT,'(/5X,''a'',6X,''x'',12X,
     +    ''DGAGNC(a,x)'',16X,''G(a,x)'',6X,'' Error'')')
         ENDIF
#endif
#if !defined(CERNLIB_DOUBLE)
         IF (K .EQ. 1) WRITE(LOUT,'(/5X,''a'',6X,''x'',12X,
     +    '' GAPNC(a,x)'',16X,''P(a,x)'',6X,'' Error'')')
         IF (K .EQ. 2) WRITE(LOUT,'(/5X,''a'',6X,''x'',12X,
     +    '' GAGNC(a,x)'',16X,''G(a,x)'',6X,'' Error'')')
#endif
         DO 200 J=1,NUMBX
            DO 100 I=1,NUMBA
#if defined(CERNLIB_DOUBLE)
         IF(JF.EQ.1) THEN
               IF (K .EQ. 1)SSOL(I,J,K)= GAPNC(SNGL(A(I)),SNGL(X(J)) )
               IF (K .EQ. 2)SSOL(I,J,K)= GAGNC(SNGL(A(I)),SNGL(X(J)) )
      SERRMAX(K)=MAX(SERRMAX(K),ABS(SSOL(I,J,K)-SNGL(EXACT(I,J,K))))
      WRITE(LOUT,'(1X,F5.2,F7.2,1X,2F22.8,3X,1P,D10.1)') A(I),X(J),
     +SSOL(I,J,K),SNGL(EXACT(I,J,K)),ABS(SSOL(I,J,K)-SNGL(EXACT(I,J,K)))
         ENDIF
         IF(JF.EQ.2) THEN
               IF (K .EQ. 1) SOL(I,J,K)=DGAPNC( A(I),X(J) )
               IF (K .EQ. 2) SOL(I,J,K)=DGAGNC( A(I),X(J) )
      ERRMAX(K)=MAX(ERRMAX(K),ABS(SOL(I,J,K)-EXACT(I,J,K)))
      WRITE(LOUT,'(1X,F5.2,F7.2,1X,2F22.16,3X,1P,D10.1)') A(I),X(J),
     +  SOL(I,J,K),EXACT(I,J,K), ABS( SOL(I,J,K)-EXACT(I,J,K))
         ENDIF
#endif
#if !defined(CERNLIB_DOUBLE)
               IF (K .EQ. 1) SOL(I,J,K)=GAPNC( A(I),X(J) )
               IF (K .EQ. 2) SOL(I,J,K)=GAGNC( A(I),X(J) )
      ERRMAX(K)=MAX(ERRMAX(K),ABS(SOL(I,J,K)-EXACT(I,J,K)))
      WRITE(LOUT,'(1X,F5.2,F7.2,1X,2F22.16,3X,1P,D10.1)') A(I),X(J),
     +  SOL(I,J,K),EXACT(I,J,K), ABS( SOL(I,J,K)-EXACT(I,J,K))
#endif
100         CONTINUE
200      CONTINUE
#if !defined(CERNLIB_DOUBLE)
         ETOL=TOL(JF+1)
        LTEST=LTEST.AND.(ERRMAX(K).LE.ETOL)
#endif
#if defined(CERNLIB_DOUBLE)
         ETOL=TOL(JF  )
         IF(JF.EQ.1)
     +  LTEST=LTEST.AND.(SERRMAX(K).LE.ETOL)
         IF(JF.EQ.2)
     +  LTEST=LTEST.AND.(ERRMAX(K).LE.ETOL)
#endif
300   CONTINUE
500   CONTINUE
      WRITE(LOUT,
     +   '(/'' ******************* TEST SUMMARY ******************''/)')
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(''Largest Error from DGAPNC was'',1P,D10.1)')
     +ERRMAX(1)
      WRITE(LOUT,'(''Largest Error from DGAGNC was'',1P,D10.1)')
     +ERRMAX(2)
      WRITE(LOUT,'(''Largest Error from  GAPNC was'',1P,D10.1)')
     +SERRMAX(1)
      WRITE(LOUT,'(''Largest Error from  GAGNC was'',1P,D10.1)')
     +SERRMAX(2)
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(''Largest Error from  GAPNC was'',1P,D10.1)')
     +ERRMAX(1)
      WRITE(LOUT,'(''Largest Error from  GAGNC was'',1P,D10.1)')
     +ERRMAX(2)
#endif

      WRITE(LOUT,'(1X)')
      IRC=ITEST('C334',LTEST)
      CALL PAGEND('C334')
      RETURN
      END
