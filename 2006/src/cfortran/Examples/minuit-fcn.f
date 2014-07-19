      Subroutine FCN(Npar,Grad,Fval,Xval,Iflag,Futil)
      Implicit Double Precision (a-h,o-z)

      Dimension Xval(*), Grad(*)

      Data  Xc,Yc/1.11D0,3.14d0/
* 
*  demo FCN 
*  Gunter Folger, December 94
*

       if ( Iflag.eq.1 ) Then
*
*      Initialaise.
*
       Elseif ( Iflag.eq.2) Then
*
*        derivatives...
*
       Endif

       Fval = (Xval(1)-Xc)**2 + (Xval(2)-Yc)**3*Xval(2)

       Return

*      
       End
