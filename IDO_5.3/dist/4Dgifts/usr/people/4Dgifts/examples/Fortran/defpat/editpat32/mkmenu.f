      function mkmenu ()

#	include <fgl.h>
#	include <fdevice.h>

	integer  mkmenu, menu
	character*30 entries

	entries = 'Menu %t | save | quit'
	menu = newpup()
  	call addtop(menu, entries, len(entries), 0)
	mkmenu = menu
	return
      end
