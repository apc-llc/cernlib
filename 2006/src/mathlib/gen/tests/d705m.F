*
* $Id: d705m.F,v 1.2 1996/04/17 12:32:09 mclareni Exp $
*
* $Log: d705m.F,v $
* Revision 1.2  1996/04/17 12:32:09  mclareni
* Add d/rfstft.F (D705) and to Imakefile. cfstft.F becomes D706.
* In tests, add d705m.F for rfstft and d706m.F for cfstft and the corresponding
* additions to main.F and Imakefile.
*
* Revision 1.1  1996/04/10 16:37:41  mclareni
* Add test for cfstst (D705), d705m.F, also to main and Imakefile
*
*
C     D705 RFSTFT

      SUBROUTINE D705M
 
      IMPLICIT COMPLEX (A-H,I,O-Z)
      REAL PI,Y0,Y
      LOGICAL LOK
 
      DIMENSION C0(0:100),Y0(0:100),R(0:100)
      DIMENSION C(0:8),Y(0:17)
      EQUIVALENCE (C,Y)
 
      I=(0.,1.)
      PI=4*ATAN(1.)
 
      LOK=.TRUE.
      DO 10 M1 = -1,4
      IF(M1 .EQ. -1) THEN
       M=3
       N=8
       DO 11 K = 0,N-1
   11  Y(K)=K+1
      ELSE
       M=M1
       N=2**M
       IF(M .EQ. 0) WRITE(6,'(1X)')
       CALL RANLUX(R,2*N)
       IF(M .EQ. 0) WRITE(6,'(1X)')
       DO 12 K = 0,N-1
   12  Y(K)=R(K)
      ENDIF
 
      DO 2 J = 0,N-1
      SK=0
      DO 3 K = 0,N-1
    3 SK=SK+Y(K)*EXP(-I*2*PI*K*J/N)
    2 C0(J)=SK/N
      CALL RFSTFT(-M,C)
      DO 13 J = 0,N-1
      IF(J .LE. N/2) THEN
       CC=C(J)
      ELSE
       CC=CONJG(C(N-J))
      ENDIF
C     WRITE(6,'(1X,I5,6F10.5)') J,C0(J),CC,C0(J)-CC
   13 LOK=LOK .AND. ABS(C0(J)-CC) .LT. 0.0021
 
      DO 15 J = 0,N/2
   15 C(J)=C0(J)
 
      DO 4 K = 0,N-1
      SJ=0
      DO 5 J = 0,N-1
      IF(J .LE. N/2) THEN
       CC=C(J)
      ELSE
       CC=CONJG(C(N-J))
      ENDIF
    5 SJ=SJ+CC*EXP(I*2*PI*J*K/N)
    4 Y0(K)=SJ
      CALL RFSTFT(M,C)
      DO 14 K = 0,N-1
C     WRITE(6,'(1X,I5,3F10.5)') J,Y0(K),Y(K),Y0(K)-Y(K)
   14 LOK=LOK .AND. ABS(Y0(K)-Y(K)) .LT. 0.00025
   10 CONTINUE
 
      IF(LOK) WRITE(6,'(7X,''D705 RFSTST  ** TEST SUCCESFUL **'')')
      IF(.NOT.LOK) WRITE(6,'(7X,''D705 RFSTST  ** TEST FAILED **'')')
      END
