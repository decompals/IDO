       subroutine setque

#include <gl/fdevice.h>
c
c this just sets the keyboard keys we want to use so they are
c recognized by the que
c

       call qdevic(esckey)
       call qdevic(tabkey)
       call qdevic(redraw)
       call qdevic(inptch)
 
       return
       end
