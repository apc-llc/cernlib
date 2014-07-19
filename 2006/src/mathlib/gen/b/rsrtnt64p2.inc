      IF(C .EQ. 0) THEN
       IF(B .EQ. 0) THEN
        H=LOG(ABS(V/U))/SQRT(A)
       ELSE
        IF(A .LT. 0) THEN
         H=2*(ATAN(SQRT(-(A+B*V)/A))-ATAN(SQRT(-(A+B*U)/A)))/SQRT(-A)
        ELSE
         WA=SQRT(A)
         WU=SQRT(A+B*U)
         WV=SQRT(A+B*V)
         H=LOG(ABS((WV-WA)*(WU+WA)/((WV+WA)*(WU-WA))))/WA
        ENDIF
       ENDIF
      ELSE
       A2=2*A
       IF(DELTA .GT. 0 .OR. DELTA .LT. 0 .AND. A .GT. 0) THEN
        H=LOG(ABS((-2*SQRT(A*P(V))+B*V+A2)*U/
     1           ((-2*SQRT(A*P(U))+B*U+A2)*V)))/SQRT(A)
       ELSEIF(DELTA .EQ. 0) THEN
        H=LOG(ABS((B*U+A2)*V/((B*V+A2)*U)))/SQRT(A)
        IF(U*V .GT. 0) H=SIGN(H,U)
       ELSE
        H=(ASIN((B*V+A2)/(V*RTD))-ASIN((B*U+A2)/(U*RTD)))/SQRT(-A)
        IF(U .LT. 0 .AND. V .LT. 0) H=-H
       ENDIF
      ENDIF
