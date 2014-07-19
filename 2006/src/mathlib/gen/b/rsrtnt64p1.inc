      C2=2*C
      IF(DELTA .GT. 0 .OR. DELTA .LT. 0 .AND. C .GT. 0) THEN
       H=LOG(ABS((2*SQRT(C*P(V))+C2*V+B)/
     1           (2*SQRT(C*P(U))+C2*U+B)))/SQRT(C)
      ELSEIF(DELTA .EQ. 0) THEN
       H=ABS(LOG(ABS((C2*V+B)/(C2*U+B))))/SQRT(C)
      ELSE
       H=(ASIN((C2*U+B)/RTD)-ASIN((C2*V+B)/RTD))/SQRT(-C)
      ENDIF
