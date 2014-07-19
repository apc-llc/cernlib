CDECK  ID>, M_WNIF.
      SUBROUTINE M_WNIF (NU)

C-    Print warning for wrong number of underscores
C.    started  9-dec-91

      CHARACTER    MSG*22
      DATA  MSG    /' 0 underscore too many'/
C-    DATA  MSG    /' 0 underscore missing '/
C-                   _:.=+=.: 1_:.=+=.: 2_:.=


      CALL DPHEAD

      N = NU
      IF (N.LT.0)  THEN
          N = -N
          MSG(15:22) = 'missing '
        ELSE
          MSG(15:22) = 'too many'
        ENDIF

      MSG(1:2) = '  '
      CALL CSETDI (N,MSG,1,2)

      CALL FAILCC (0,MSG)
      END
