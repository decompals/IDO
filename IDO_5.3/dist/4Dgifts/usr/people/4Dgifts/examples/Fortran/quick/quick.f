       program quick2D
c
#include "common"
c
       call readat(points,n,caxis)
       call setwin(gid,points,n,caxis)
       call mainlo(gid,points,n,caxis)
       call setclo(gid)
c
       end
