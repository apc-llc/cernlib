CDECK  ID>, NA_INPY.
      SUBROUTINE NA_INPY

C-    Pre-set the name stack for Patchy
C.    started  19-july-93

      COMMON /NAMEC/ NA_OCC, NA_CUT, NA_MXSL, NA_MXTX
C--------------    End CDE              --------------------------------

C--   preset slot (0  one blank)
C-                (1  unknown)
C-                 2  CRA*


      CALL NA_LONG ('CRA*')
      CALL NA_LONG ('QCARD1')
      CALL NA_LONG ('QEJECT')
      CALL NA_LONG ('QFTITLE')
      CALL NA_LONG ('QFTITLCH')
      CALL NA_LONG ('QFHEAD')
      CALL NA_LONG ('QFNAME')
      CALL NA_LONG ('QFVERS')
      CALL NA_LONG ('QFVSNUM')
      CALL NA_LONG ('QFVPRIM')
      CALL NA_LONG ('QFVSEC')
      CALL NA_LONG ('QDATE')
      CALL NA_LONG ('QTIME')
      CALL NA_LONG ('QENVIR')
      CALL NA_LONG ('QTERMHD')
      CALL NA_LONG ('DATEQQ')
      CALL NA_LONG ('TIMEQQ')
      RETURN
      END
