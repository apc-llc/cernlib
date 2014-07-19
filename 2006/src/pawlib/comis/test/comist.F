      PROGRAM COMIST
      EXTERNAL DRTEQ3,RVRAN
      CALL CSINIT(2000)
      CALL CSEXT('DRTEQ3,RVRAN#',
     +            DRTEQ3,RVRAN,P3,P4,P5,P6,P7,P8,P9,P10)
   1  CALL CSPAUS('IFORT')
C     GO TO 1
      END
      SUBROUTINE CSPAWI
      END
      SUBROUTINE DRTEQ3(R,S,T,X,D)
 
      DOUBLE PRECISION R,S,T,X(3),D
      COMPLEX Z(0:2)
      DOUBLE PRECISION QQ,PP,Q1,Q2,Q3
      DOUBLE PRECISION Q,P,U,V,U0,V0,H,H1,H2,H3,Y(0:2)
      DOUBLE PRECISION R1,R2,R3,R4,P2,P3,EPS,DELTA
 
      PARAMETER(R1 = 2D0/27D0, R2 = 1D0/2D0, R3 = 1D0/3D0)
      PARAMETER(Q1 = 2D0/27D0, Q2 = 1D0/2D0, Q3 = 1D0/3D0)
      PARAMETER(R4 = 1.73205 08075 68877 294D0/2D0)
      PARAMETER(P2 = 3.14159 26535 89793 238D0*2D0/3D0)
      PARAMETER(P3 = 2*P2, EPS = 1D-6, DELTA = 1D-15)
 
      IF(S .EQ. 0 .AND. T .EQ. 0) THEN
       X(1)=-R
       X(2)=0
       X(3)=0
       D=0
       RETURN
      END IF
      P=S-R3*R**2
      Q=(R1*R**2-R3*S)*R+T
      D=(R2*Q)**2+(R3*P)**3
      IF(ABS(D) .LE. EPS) THEN
       PP=S-Q3*R**2
       QQ=(Q1*R**2-Q3*S)*R+T
       D=(Q2*QQ)**2+(Q3*PP)**3
       P=PP
       Q=QQ
      END IF
      H=R3*R
      H1=R2*Q
      IF(D .GT. DELTA) THEN
       H2=SQRT(D)
       U0=-H1+H2
       V0=-H1-H2
       U=SIGN(ABS(U0)**R3,U0)
       V=SIGN(ABS(V0)**R3,V0)
       X(1)=U+V-H
       X(2)=-R2*(U+V)-H
       X(3)=R4*ABS(U-V)
       IF(ABS(U0) .LE. EPS .OR. ABS(V0) .LE. EPS) THEN
        Y(0)=X(1)
        DO 1 I = 0,1
    1   Y(I+1)=Y(I)-(((Y(I)+SNGL(R))*Y(I)+SNGL(S))*
     +  Y(I)+T)/((3*Y(I)+SNGL(2*R))*Y(I)+SNGL(S))
        X(1)=Y(2)
        Z(0)=CMPLX(X(2),X(3))
        DO 2 I = 0,1
    2   Z(I+1)=Z(I)-(((Z(I)+SNGL(R))*Z(I)+SNGL(S))*
     +  Z(I)+SNGL(T))/((3*Z(I)+SNGL(2*R))*Z(I)+SNGL(S))
        X(2)=REAL(Z(2))
        X(3)=AIMAG(Z(2))
       END IF
      ELSE IF(ABS(D) .LE. DELTA) THEN
       D=0
       U=SIGN(ABS(H1)**R3,-H1)
       X(1)=U+U-H
       X(2)=-U-H
       X(3)=X(2)
       IF(ABS(H1) .LE. EPS) THEN
        Y(0)=X(1)
        DO 5 I = 0,1
        H1=(3*Y(I)+2*R)*Y(I)+S
        IF(ABS(H1) .GT. DELTA) THEN
         Y(I+1)=Y(I)-(((Y(I)+R)*Y(I)+S)*Y(I)+T)/H1
        ELSE
         X(1)=-R3*R
         X(2)=X(1)
         X(3)=X(1)
         RETURN
        END IF
    5   CONTINUE
        X(1)=Y(2)
        X(2)=-R2*(R+X(1))
        X(3)=X(2)
       END IF
      ELSE
       H3=SQRT(ABS(R3*P)**3)
       H2=R3*ACOS(-H1/H3)
       H1=2*H3**R3
       X(1)=H1*COS(H2)-H
       X(2)=H1*COS(H2+P2)-H
       X(3)=H1*COS(H2+P3)-H
       IF(H3 .LE. EPS .OR. X(1) .LE. EPS .OR. X(2) .LE .EPS .OR.
     1                     X(3) .LE. EPS) THEN
        DO 3 J = 1,3
        Y(0)=X(J)
        DO 4 I = 0,1
    4   Y(I+1)=Y(I)-(((Y(I)+R)*Y(I)+S)*Y(I)+T)/((3*Y(I)+2*R)*Y(I)+S)
    3   X(J)=Y(2)
       END IF
      END IF
      RETURN
      END
