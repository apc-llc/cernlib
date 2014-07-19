      PROGRAM PYTST
C
      EXTERNAL PYDATA
#if defined(CERNLIB_GFORTRAN)
      call dummysub(pydata)
#endif

      MPAR = 1
C
      CALL PYTEST(MPAR)
C
      END
#if defined(CERNLIB_GFORTRAN)
      subroutine dummysub(a)
      return
      end
#endif
