*
* $Id: d706m.F,v 1.1 1996/04/17 12:32:09 mclareni Exp $
*
* $Log: d706m.F,v $
* Revision 1.1  1996/04/17 12:32:09  mclareni
* Add d/rfstft.F (D705) and to Imakefile. cfstft.F becomes D706.
* In tests, add d705m.F for rfstft and d706m.F for cfstft and the corresponding
* additions to main.F and Imakefile.
*
*
C     D706 CFSTFT

      SUBROUTINE D706M
 
      IMPLICIT COMPLEX (A-H,I,O-Z)
      REAL PI,R
      LOGICAL LOK
 
      DIMENSION A(0:15),AA(0:15),B(0:15)
      DIMENSION R(0:100)
 
      I=(0.,1.)
      PI=4*ATAN(1.)
      C1=-5*(1+I)
      C2=5*(1+I)
 
      LOK=.TRUE.
      DO 10 M1 = -1,4
      IF(M1 .EQ. -1) THEN
       M=3
       N=8
       DO 11 K = 0,N-1
       A(K)=K+1
   11  B(K)=A(K)
      ELSE
       M=M1
       N=2**M
       IF(M .EQ. 0) WRITE(6,'(1X)')
       CALL RANLUX(R,2*N)
       IF(M .EQ. 0) WRITE(6,'(1X)')
       DO 12 K = 0,N-1
       A(K)=(C1-C2)*(R(K)+I*R(N+K))+C1
   12  B(K)=A(K)
      ENDIF
 
      DO 2 J = 0,N-1
      SK=0
      DO 3 K = 0,N-1
    3 SK=SK+A(K)*EXP(-I*2*PI*K*J/N)
    2 AA(J)=SK
      CALL CFSTFT(-M,B)
      DO 13 J = 0,N-1
   13 LOK=LOK .AND. ABS(AA(J)-B(J)) .LT. 0.0021
 
      DO 4 K = 0,N-1
      SL=0
      DO 5 J = 0,N-1
    5 SL=SL+AA(J)*EXP(I*2*PI*J*K/N)
    4 A(K)=SL/N
      CALL CFSTFT(M,B)
      DO 14 K = 0,N-1
   14 LOK=LOK .AND. ABS(A(K)-B(K)/N) .LT. 0.00025
   10 CONTINUE
 
      IF(LOK) WRITE(6,'(7X,''D706 CFSTST  ** TEST SUCCESFUL **'')')
      IF(.NOT.LOK) WRITE(6,'(7X,''D706 CFSTST  ** TEST FAILED **'')')
      END
