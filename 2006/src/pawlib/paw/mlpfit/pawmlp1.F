      REAL FUNCTION PAWMLP1(X)
C
C     computes the multi-layer perceptron function in case of one 
C     variable
C
C     J.Schwindling 26-Apr-99      
C      
      REAL RIN(1),RANS(1)
C
      RIN(1) = X
      CALL mlpcompute(RIN,RANS)
      PAWMLP1 = RANS(1)
      END    
      
