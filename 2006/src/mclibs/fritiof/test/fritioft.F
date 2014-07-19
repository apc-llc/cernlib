       external ludata, pydata
#if defined(CERNLIB_GFORTRAN)
       external frdata, ardata

       call dummysub(ludata)
       call dummysub(pydata)
       call dummysub(frdata)
       call dummysub(ardata)
#endif
       call frsample
       end
#if defined(CERNLIB_GFORTRAN)
       subroutine dummysub(a)
       return
       end
#endif
