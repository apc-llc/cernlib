      PROGRAM JETTST
C
      EXTERNAL LUDATA,PYDATA
#if defined(CERNLIB_GFORTRAN)
      call dummysub(ludata)
      call dummysub(pydata)
#endif 

      MPAR = 1
C
      CALL LUTEST(MPAR)
      CALL PYTEST(MPAR)
C
      END
#if defined(CERNLIB_GFORTRAN)
      subroutine dummysub(a)
      return
      end
#endif 
