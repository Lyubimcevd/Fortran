      subroutine genslpl(w)
      real r
      integer*4 irand/137462873/,mult/65539/,large/2147483647/
      r=1.0
      irand=irand*mult
      if(irand.lt.0) irand=(irand+large)+1
      w=r*(irand-1)/(large-1)
      end